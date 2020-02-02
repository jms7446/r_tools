as_tibble_with_name <- function(data, col_name = NULL, row_name = NULL, ...) {
    if (!is.null(col_name)) colnames(data) <- col_name
    if (!is.null(row_name)) rownames(data) <- row_name
    as_tibble(data, ...)
}

standardize <- function(x) (x - mean(x)) / sd(x)

default_if_null <- function(x, default) if (!is.null(x)) x else default

calc_max_min_diff <- function(xs) {
  rng = range(xs)
  rng[2] - rng[1]
}

make_plot_file_name <- function(...) {
  name <- paste(..., sep = "_")
  paste0("plot/", name, ".pdf")
}

make_save_file_name <- function(...) {
  name <- paste(..., sep = "_")
  paste0("save/", name, ".RData")
}

name_to_coef <- function(names, levels, coef_prefix = "beta_") {
    "beta_" %+% as.integer(factor(names, levels = x_levels))   
}

ggsave_plot <- function(plot, filename, ...) ggsave(filename, plot, ...)

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

null_func <- function(...) return (NULL)


################################################################################
# Operators
################################################################################

`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) {
    lhs
  } else {
    rhs
  }
}
 
`%+%` <- function(a, b) paste0(a, b)

`%s%` <- function(x, y) {

  do.call(sprintf, c(list(x), y))

}


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


################################################################################
# Shape parameters from central tendency and scale:
# from John Kruschke
################################################################################

betaABfromMeanKappa = function( mean , kappa ) {
  if ( mean <=0 | mean >= 1) stop("must have 0 < mean < 1")
  if ( kappa <=0 ) stop("kappa must be > 0")
  a = mean * kappa
  b = ( 1.0 - mean ) * kappa
  return( list( a=a , b=b ) )
}

betaABfromModeKappa = function( mode , kappa ) {
  if ( mode <=0 | mode >= 1) stop("must have 0 < mode < 1")
  if ( kappa <=2 ) stop("kappa must be > 2 for mode parameterization")
  a = mode * ( kappa - 2 ) + 1
  b = ( 1.0 - mode ) * ( kappa - 2 ) + 1
  return( list( a=a , b=b ) )
}

betaABfromMeanSD = function( mean , sd ) {
  if ( mean <=0 | mean >= 1) stop("must have 0 < mean < 1")
  if ( sd <= 0 ) stop("sd must be > 0")
  kappa = mean*(1-mean)/sd^2 - 1
  if ( kappa <= 0 ) stop("invalid combination of mean and sd")
  a = mean * kappa
  b = ( 1.0 - mean ) * kappa
  return( list( a=a , b=b ) )
}

gammaShRaFromMeanSD = function( mean , sd ) {
  if ( mean <=0 ) stop("mean must be > 0")
  if ( sd <=0 ) stop("sd must be > 0")
  shape = mean^2/sd^2
  rate = mean/sd^2
  return( list( shape=shape , rate=rate ) )
}

gammaShRaFromModeSD = function( mode , sd ) {
  if ( mode <=0 ) stop("mode must be > 0")
  if ( sd <=0 ) stop("sd must be > 0")
  rate = ( mode + sqrt( mode^2 + 4 * sd^2 ) ) / ( 2 * sd^2 )
  shape = 1 + mode * rate
  return( list( shape=shape , rate=rate ) )
}

################################################################################
