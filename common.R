library(tidyverse)


calc_max_min_diff <- function(xs) {
  rng = range(xs)
  rng[2] - rng[1]
}

make_plot_file_name <- function(...) {
  name <- paste(..., sep = "_")
  paste0("plot/", name, ".pdf")
}

multi_gather <- function(data, ..., col_name = "c", ptn = "(.*)_([^_]*)", use_prefix = T) {
  cols <- if (use_prefix) c("..param..", col_name) else c(col_name, "..param..")
  data %>% 
    gather(..key.., ..value.., ...) %>% 
    tidyr::extract(..key.., cols, ptn, convert = T) %>% 
    spread(..param.., ..value..) 
}

install_missing_packages <- function(pkgs) {
  have = pkgs %in% rownames(installed.packages())
  if ( any(!have) ) { install.packages( want[!have] ) }
}


# save multiple plot
#d <- tibble(x = 1:10, y = x * 2)
#p1 <- ggplot(data = d, aes(x, y)) + geom_line()
#p2 <- ggplot(data = d, aes(x, y)) + geom_point()
#p <- gridExtra::grid.arrange(p1, p2, nrow = 1)
#p <- egg::ggarrange(p1, p2, widths = c(1.5, 2))
#plots <- list(p1, p2)
#l = mget(plots)
#ggsave("tmp_output.pdf", gridExtra::grid.arrange(p1, p2, nrow = 1))


################################################################################
# Multiple Assignment
# from : https://stackoverflow.com/questions/7519790/assign-multiple-new-variables-on-lhs-in-a-single-line
# from : https://strugglingthroughproblems.wordpress.com/2010/08/27/matlab-style-multiple-assignment-in%C2%A0r/
################################################################################

# Generic form
'%=%' = function(l, r, ...) UseMethod('%=%')

# Binary Operator
'%=%.lbunch' = function(l, r, ...) {
  Envir = as.environment(-1)
  
  if (length(r) > length(l))
    warning("RHS has more args than LHS. Only first", length(l), "used.")
  
  if (length(l) > length(r))  {
    warning("LHS has more args than RHS. RHS will be repeated.")
    r <- extendToMatch(r, l)
  }
  
  for (II in 1:length(l)) {
    do.call('<-', list(l[[II]], r[[II]]), envir=Envir)
  }
}

# Used if LHS is larger than RHS
extendToMatch <- function(source, destin) {
  s <- length(source)
  d <- length(destin)
  
  # Assume that destin is a length when it is a single number and source is not
  if(d==1 && s>1 && !is.null(as.numeric(destin)))
    d <- destin
  
  dif <- d - s
  if (dif > 0) {
    source <- rep(source, ceiling(d/s))[1:d]
  }
  return (source)
}

# Grouping the left hand side
g = function(...) {
  List = as.list(substitute(list(...)))[-1L]
  class(List) = 'lbunch'
  return(List)
}

################################################################################