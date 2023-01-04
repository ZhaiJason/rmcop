# Creating new option object ===================================================

# Following functions are exported:
# option
# option.env

## Option ======================================================================
#' Initialise new `option` class object
#'
#' @param style The style of the option.
#' @param ... Arguments required are determined by specified option style.
#'
#' @return An object of class `option`.
#' @export
#'
#' @examples
#' # Create an European call option with strike price 10 and mature in 2 years
#' option("european", is.vanilla = TRUE, type = "call", K = 10, t = 2)
#'
#' # Create an Binary option with payout 10 and mature
#' option("european", option = "binary")
option <- function(style, option = "vanilla", ...) {

    obj <- list(
        "style" = style
    )

    # Distribute to corresponding initialisation function according to specified arguments
    if (option == "vanilla") {
        obj <- option.vanilla(obj, ...)
    } else {
        obj <- option.exotic(obj, option, ...)
    }
    obj
}

## Vanilla =====================================================================
#' Initialise vanilla option
#'
#' @param obj
#' @param type
#' @param K
#' @param t
#'
#' @return
#'
#' @keywords internal
option.vanilla <- function(obj, type, K, t) {
    obj <- c(
        obj,
        "type" = type,
        "K" = K,
        "t" = t
    )
    class(obj) <- c("vanilla", "option")
    obj
}

## Exotic ======================================================================
#' Initialise exotic option
#'
#' @param obj
#' @param option
#' @param ...
#'
#' @return
#'
#' @keywords internal
option.exotic <- function(obj, option, ...) {
    obj <- get(paste0("option.exotic.", option))(obj, ...) # Direct to corresponding initialisation function
    class(obj) <- append(class(obj), "exotic", after = 1)
    obj
}

#' Initialise Asian option
#'
#' @param obj
#' @param type
#' @param K
#' @param t
#'
#' @return
#'
#' @keywords internal
option.exotic.asian <- function(obj, type, K, t) {
    obj <- c(
        obj,
        "type" = type,
        "K" = K,
        "t" = t
    )
    class(obj) <- c("asian", "option")
    obj
}

#' Initialise barrier option
#'
#' @param obj
#' @param barrier
#' @param is.knockout
#'
#' @return
#'
#' @keywords internal
option.exotic.barrier <- function(obj, barrier, is.knockout = TRUE, t) {
    obj <- c(
        obj,
        "barrier" = barrier,
        "is.knockout" = is.knockout,
        "t" = t
    )
    class(obj) <- c("barrier", "option")
    obj
}

#' Initialise binary option
#'
#' @param obj
#' @param payout
#' @param t
#'
#' @return
#'
#' @keywords internal
option.exotic.binary <- function(obj, payout, t) {
    obj <- c(
        obj,
        "payout" = payout,
        "t" = t
    )
    class(obj) <- c("binary", "option")
    obj
}

#' Initialise lookback option
#'
#' @param obj
#' @param type
#' @param K
#' @param t
#'
#' @return
#'
#' @keywords internal
option.exotic.lookback <- function(obj, type, K, t) {
    obj <- c(
        obj,
        "type" = type,
        "K" = K,
        "t" = t
    )
    class(obj) <- c("lookback", "option")
    obj
}

# Creating new option environment ==============================================

#' Initialise new `option.env` class object
#'
#' @param method A character specifying the method that will be used for option pricing. The default value for this argument is `"mc"`, which represents to use Monte Carlo approach.
#' @param S A number specifying the current price of the underlying asset.
#' @param r A number specifying the (assumed fixed) continuously compounded annual interest rate.
#' @param q A number specifying the (assumed fixed) continuously compounded annual dividend yield rate.
#' @param sigma A number specifying the (assumed fixed) annual volatility measure. Notice when pricing with `mc` method, it can be also evaluated to a corresponding `n` by `m` matrix, which allows variation of volatility through the option(s)'s life; this should, however, be exercised with caution, as this is not the intended application of the algorithm.
#' @param ... Other arguments required by specified `method`.
#' @param all A boolean specifying whether the pricing function should return only the result price (if `FALSE`) or other (maybe useful) data during computation (if `TRUE`). The default value for this argument is `FALSE`.
#'
#' @return
#' @export
#'
#' @examples
#'
option.env <- function(method = "mc", S, r, q = 0, sigma, n = NULL, steps = NULL) {
    env <- list(
        "method" = method,
        "S" = S,
        "r" = r,
        "q" = q,
        "sigma" = sigma,
        "n" = n,
        "steps" = steps
    )
    class(env) <- "option.env"
    env
}
