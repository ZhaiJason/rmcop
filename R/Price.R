# Top Level Pricing Functions for Options ======================================

# Following functions are exported:
# ALL FUNCTIONS

#' Pricing `option` Class Object
#'
#' @param obj
#' @param env The predefined `option.env` object, which contains specified arguments
#' @param method
#' @param ...
#' @param all A boolean specifying whether the pricing function should return only the result price (if `FALSE`) or other (maybe useful) data during computation (if `TRUE`). The default value for this argument is `FALSE`.
#'
#' @return
#' @export
#'
#' @examples
#' op <- option("european", "vanilla", "call", 20, 0.75)
#' op.env <- option.env(S = 20, r = 0.03, q = 0.01, sigma = 0.05)
#' price.option(op, op.env, n = 10, steps = 1)
price.option <- function(obj, env, method = env$method, n = env$n, steps = env$steps, ... , all = FALSE) {
    if (is.null(method)) stop("Pricing method not specified")
    res <- get(paste0("price.option.", method))(obj, env, n, steps, ..., all)
    res
}

#' Option Pricing via Monte Carlo Method
#'
#' @param obj
#' @param env
#' @param ...
#' @param all
#'
#' @return
#' @export
#'
#' @examples
price.option.mc <- function(obj, env, n = env$n, steps = env$steps, all = FALSE) {
    if (is.null(n)) stop("n is not specified")
    if (is.null(steps)) {
        steps <- 1
        warning("steps is not specified, evaluated to default value 1")
    }
    res <- get(paste0(class(obj)[1], ".mc"))(obj, env, n, steps, all)
    res
}

#' Title
#'
#' @param obj
#' @param env
#' @param ...
#' @param all
#'
#' @return
#' @export
#'
#' @examples
price.option.bs <- function(obj, env, all = FALSE) {
    res <- get(paste0(class(obj)[1], ".bs"))(obj, env, all)
    res
}

#' Option Pricing via Binomial Lattice Model
#'
#' @param obj
#' @param env
#' @param ...
#' @param all
#'
#' @return
#' @export
#'
#' @examples
price.option.binomial <- function(obj, env, all = FALSE) {
    res <- get(paste0(class(obj)[1], ".binomial"))(obj, env, all)
    res
}

#' Option Pricing via Trinomial Lattice Model
#'
#' @param obj
#' @param env
#' @param ...
#' @param all
#'
#' @return
#' @export
#'
#' @examples
price.option.trinomial <- function(obj, env, all = FALSE) {
    res <- get(paste0(class(obj)[1], ".trinomial"))(obj, env, all)
    res
}
