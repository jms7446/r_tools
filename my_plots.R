
ms_plot_pair <- function(n, diag_plot, upper_plot, lower_plot) {
  plots <- list()
  for (i in 1:n) {
    for (j in 1:n) {
      idx <- (i - 1) * n + j
      if (i == j) {
        plots[[idx]] <- diag_plot(i, j)
      } else if (i < j) {
        plots[[idx]] <- upper_plot(i, j)
      } else {
        plots[[idx]] <- lower_plot(i, j)
      }
    }
  }
  grid.arrange(grobs = plots, ncol = n)
}

piechart <- function(data, mapping) {
  ggplot(data, mapping) + 
    geom_bar(width = 1) + 
    coord_polar(theta = "y") + 
    labs(x = NULL, y = NULL)
}
piechart(mpg, aes(factor(1), fill = class))

pcp_data <- function(df) {
  is_numeric <- vapply(df, is.numeric, logical(1))
  rescale01 <- function(x) {
    rng <- range(x, na.rm = T)
    (x - rng[1]) / (rng[2] - rng[1])
  }
  df[is_numeric] <- lapply(df[is_numeric], rescale01)
  
  df$.row <- rownames(df)
  
  gather(df, "variable", "value", names(df)[is_numeric])
}

pcp <- function(df, ...) {
  df <- pcp_data(df)
  ggplot(df, aes(variable, value, group = .row)) + geom_line(...)
}
