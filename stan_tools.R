library(rstan)
library(ggmcmc)
library(tidyverse)
library(bayestestR)
library(hBayesDM)
library(gridExtra)

  
set_stan_parallel <- function() {
  rstan_options(auto_write = T)
  options(mc.cores = parallel::detectCores())
}
set_stan_parallel()

fit_to_tibble <- function(fit) {
  to_normal_name <- function(s) {
    m <- gsub("\\[", "_", s)
    gsub("\\]", "", m)
  }
  as_tibble(as.data.frame(fit)) %>% 
    setNames(to_normal_name(names(.)))
}

sample_with_id <- function(..., id_name = "id") sample_n(...) %>% mutate(!!id_name := 1:n())

crossing_with_id <- function(data, grid, id = "id") {
  data %>% 
    mutate(!!id := 1:n()) %>% 
    crossing(grid)
}


make_sample_grid <- function(res, x_rng, x_name = "x", num_line = 20, x_size = 200) {
  sample_n(res, size = num_line) %>% 
    mutate(id = 1:n()) %>% 
    crossing(tibble(x = seq(x_rng[1], x_rng[2], length = x_size))) %>% 
    rename(!!quo_name(x_name) := x)
}

make_line_grid <- function(df, xs, y_func, id = "line_id", x_name = "X", y_name = "Y") {
  y_func = enquo(y_func)
  df %>% 
    crossing_with_id(tibble(..X.. = xs), id = id) %>% 
    rename(!!x_name := ..X..) %>% 
    mutate(..Y.. = !!y_func) %>% 
    rename(!!y_name := ..Y..)
}
#make_line_grid(tibble(a = c(1, 2), b = c(1, 0)), seq(0, 10, length = 10), y_func = a * X + b)

make_tdist_grid <- function(data, point_x, point_y, scale, nu = 100, id = "point_id", x_name = "XX", y_name = "YY", 
                                 d_rng = c(-3, 3), dist_height = 0.3) {
  point_x = enquo(point_x);  point_y = enquo(point_y);  scale = enquo(scale)
  # if need normal dist. use large nu (> 30), default value 100 is abitarily seleted (which meets > 30).
  nu = enquo(nu)  
  data %>% 
    crossing_with_id(tibble(d = seq(d_rng[1], d_rng[2], length = 50)), id = id) %>% 
    mutate(
      !!y_name := !!point_y + d * !!scale, 
      !!x_name := !!point_x - dt(d, !!nu) * calc_max_min_diff(!!point_x) * dist_height, 
      nn = !!nu
    ) 
}
#make_tdist_grid(tibble(X = c(1, 2), Y = c(2, 3), s = c(1, 2), nu = c(2, 50)), X, Y, s, nu = nu, dist_height = 0.2)


################################################################################
# plot
################################################################################

plot_post <- function(data, param_name, rope = NULL, cent_type = "median", title = NULL, 
                      ci = 0.95, dec = 2) {
  param <- data[[param_name]]
  
  central <- if (cent_type == "median") quantile(param, 0.5) else if (cent_type == "mean") mean(param) else NULL
  central_text <- paste0(cent_type, " = ", round(central, dec))
  
  hdi_rng <- hdi(param, ci = ci);  hdi_rng <- c(hdi_rng$CI_low, hdi_rng$CI_high)
  hdi_title <- paste0(ci * 100, "% HDI")
  hdi_texts <- round(hdi_rng, dec)
  
  p <- ggplot(data, aes(param)) +
    geom_histogram(aes(y = ..density..), bins = 40, fill = "skyblue", color = "white", alpha = 0.6) + 
    geom_vline(xintercept = central, color = "grey60", size = 1, alpha = 0.5) +
    annotate("text", x = central, y = Inf, size = 5, label = central_text, vjust = 4) + 
    geom_hdi(color = "black", size = 2, credible_mass = ci) + 
    annotate("text", x = hdi_rng, y = 0, label = hdi_texts, size = 4, hjust = "center", vjust = -1) + 
    annotate("text", x = mean(hdi_rng), y = 0, label = hdi_title, size = 5, vjust = -2) + 
    labs(x = param_name, title = title, y = NULL) + 
    theme_post()
  
  if (!is.null(rope)) {
    p <- p + geom_vline(xintercept = rope, size = .5, color = "purple", alpha = 0.8, linetype = "dashed") +
      geom_vline(xintercept = mean(rope), size = .5, color = "purple", alpha = 0.5, linetype = "solid") +
      annotate("text", x = rope, y = 0, label = rope, size = 4, vjust = -6)
  }
  return(p)
}


# density_func : function(values, idx) -> floats
plot_post_predictive_with_histogram <- function(x, density_func, sample_len, 
                                                num_lines = 20, title = NULL) {
  x_rng <- range(x)
  grid <- tibble(
    Score = seq(x_rng[1], x_rng[2], length.out = 100)
  )
  line_idx <- seq(1, sample_len, by = floor(sample_len / num_lines))
  ss <- list("tibble", num_lines)
  for (idx in 1:num_lines) {
    ss[[idx]] <- grid %>% 
      mutate(
        density = density_func(Score, idx), 
        idx = idx
      )
  }
  grid0 <- bind_rows(ss)
  ggplot(raw_data) + 
    geom_histogram(aes(Score, ..density..), fill = "skyblue", bins = 30) + 
    geom_line(data = grid0, aes(Score, density, group = idx), color = "red", size = 0.2, alpha = 0.5) + 
    labs(title = title)
}


theme_post <- function() {
    theme_light() + 
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}


################################################################################
# geom_hdi
# from https://bjsmith.github.io/post/geom_hdi_for_ggplot2/

geom_hdi <- function(mapping = NULL, data = NULL, stat = "hdi",
                     position = "identity", na.rm = TRUE, show.legend = NA,
                     inherit.aes = TRUE,
                     credible_mass=0.95, ...) {
  layer(
    `geom` = GeomHdi,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(credible_mass=credible_mass,
                  ...)
  )
}

GeomHdi <- ggproto("GeomHdi", GeomSegment,
                   required_aes = c("x"),
                   default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA)
)

stat_hdi <- function(mapping = NULL, data = NULL, `geom` = "segment",
                     position = "identity", na.rm = TRUE, show.legend = NA, inherit.aes = TRUE,
                     credible_mass=0.95, ...) {
  layer(
    stat = StatHdi,
    data = data,
    mapping = mapping,
    `geom` = `geom`,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(credible_mass=credible_mass,
                  ...
    )
  )
}

StatHdi <- ggproto("StatHdi", Stat,
                   
                   required_aes = c("x"),
                   
                   compute_group = function(self, data, scales, params,
                                            credible_mass=0.95) {
                     require(hBayesDM)
                     hdi.data<-HDIofMCMC(data$x,credible_mass)
                     data.frame(x=hdi.data[1],xend=hdi.data[2],y=0,yend=0)
                   }
)

################################################################################
