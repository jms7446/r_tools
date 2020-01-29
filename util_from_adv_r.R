
abort_bad_argument <- function(arg, must, not = NULL) {
    msg <- glue("`{arg}` must {must}")
    if (!is.null(not)) {
        not <- typeof(not)
        msg <- glue("{msg}; not {not}.")
    }
    
    abort(
        "error_bad_argument", 
        message = msg, 
        must = must, 
        not = not
    )
}

fail_with <- function(expr, value = NULL) {
  tryCatch(
    error = function(cnd) value,
    expr
  )
}

pick <- function(i) {
    force(i)
    function(x) x[[i]]
}

binwidth_bins <- function(n) {
  force(n)
  
  function(x) {
    (max(x) - min(x)) / n
  }
}

base_bins <- function(type) {
  fun <- switch(type,
    Sturges = nclass.Sturges,
    scott = nclass.scott,
    FD = nclass.FD,
    stop("Unknown type", call. = FALSE)
  )
  
  function(x) {
    (max(x) - min(x)) / fun(x)
  }
}
