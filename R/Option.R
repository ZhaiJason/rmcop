# Creating new option object ===================================================

# Following functions are exported:
# option
# option.env

## Option ======================================================================
#' Initialise new `option` class object
#'
#' @param style A string specifying the style of the option of interest. Possible values are `"european"` and `"american"`, corresponding to European options and American options, respectively.
#' @param option A string specifying the specific family of the option of interest. Supported values are: `"vanilla"` (default, for Vanilla options), `"asian"` (for Asian options), `"barrier"` (for Barrier options), `"binary"` (for Binary/Cash-or-nothing options), and `"lookback"` (for Lookback options).
#' @param type A string specifying the type of the option of interest. Possible values are either `"call"` or `"put"`, corresponding to call option (right to buy) and pull option (right to sale), respectively.
#' @param K A number specifying the strike price of the option.
#' @param t A number specifying the maturity/expiry of the option in years.
#' @param ... Arguments required by corresponding specified option families.
#'
#' @return An S3 object with `class` attribute evaluated to `"option"`.
#' @export
#'
#' @examples
#' # Create an European vanilla option with strike price 20, expiry in 1 year.
#' vanilla1 <- option("european", "vanilla", "call", K = 20, t = 1)
#'
#' # Create European  average strike Asian option with strike price 20, expiry in 1 year.
#' asian1 <- option("european", "asian", "call", K = 20, t = 1, is.avg_price = FALSE)
#'
#' # Create European knock-out barrier option with strike price 20, barrier price 21, expiry in 1 year.
#' barrier1 <- option("european", "barrier", "call", K = 20, t = 1, barrier = 21, is.knockout = TRUE)
#'
#' # Create European binary option with strike price 20, fixed payout 5, expiry in 1 year.
#' binary1 <- option("european", "binary", "call", K = 20, t = 1, payout = 5)
#'
#' # Create European fixed strike lookback option with strike price 20, expiry in 1 year.
#' lookback1 <- option("european", "lookback", "call", K = 20, t = 1, is.fixed = TRUE)
option <- function(style, option = "vanilla", type, K, t, ...) {

    obj <- list(
        "style" = style,
        "type" = type,
        "K" = K,
        "t" = t
    )

    # Distribute to corresponding initialisation function according to specified arguments
    if (option == "vanilla") {
        obj <- option.vanilla(obj)
    } else {
        obj <- option.exotic(obj, option, ...)
    }
    obj
}

## Vanilla =====================================================================
#' Initialise vanilla option
#'
#' @param obj The specified `"option"` class object which encapsulate some properties of an option of interest.
#'
#' @keywords internal
option.vanilla <- function(obj) {
    class(obj) <- c("vanilla", "option")
    obj
}

## Exotic ======================================================================
#' Initialise exotic option
#'
#' @param obj The specified `"option"` class object which encapsulate some properties of an option of interest.
#' @param option A string specifying the specific family of the option of interest. Supported values are: `"vanilla"` (default, for Vanilla options), `"asian"` (for Asian options), `"barrier"` (for Barrier options), `"binary"` (for Binary/Cash-or-nothing options), and `"lookback"` (for Lookback options).
#' @param ... Arguments required by corresponding specified option families.
#'
#' @keywords internal
option.exotic <- function(obj, option, ...) {
    obj <- get(paste0("option.exotic.", option))(obj, ...) # Direct to corresponding initialisation function
    obj
}

#' Initialise Asian option
#'
#' @param obj The specified `"option"` class object which encapsulate some properties of an option of interest.
#' @param is.avg_price A logical value for `"asian" "option"` class objects, specifying whether the option is an "average price Asian option" or "average strike Asian option". The default value for this argument is `TRUE`, indicating the option being an "average price Asian option".
#'
#' @keywords internal
option.exotic.asian <- function(obj, is.avg_price = TRUE) {
    obj <- c(
        obj,
        "is.avg_price" = is.avg_price
    )
    class(obj) <- c("asian", "option")
    obj
}

#' Initialise barrier option
#' @param obj The specified `"option"` class object which encapsulate some properties of an option of interest.
#' @param barrier A number for `"barrier" "option"` class objects, specifying the barrier price of the barrier option.
#' @param is.knockout A logical value for `"barrier" "option"` class objects, specifying whether the option is an "knock-out barrier option" (`is.knockout = TRUE`) or "knock-in barrier option" (`is.knockout = FALSE`).
#'
#' @keywords internal
option.exotic.barrier <- function(obj, barrier, is.knockout = TRUE) {
    obj <- c(
        obj,
        "barrier" = barrier,
        "is.knockout" = is.knockout
    )
    class(obj) <- c("barrier", "option")
    obj
}

#' Initialise binary option
#'
#' @param obj The specified `"option"` class object which encapsulate some properties of an option of interest.
#' @param payout A number for `"binary" "option"` class objects, specifying the payout of a binary option.
#'
#' @keywords internal
option.exotic.binary <- function(obj, payout) {
    obj <- c(
        obj,
        "payout" = payout
    )
    class(obj) <- c("binary", "option")
    obj
}

#' Initialise lookback option
#'
#' @param obj The specified `"option"` class object which encapsulate some properties of an option of interest.
#' @param is.fixed A logical value for `"lookback" "option"` class objects. specifying whether the option is an "fixed strike look-back option" (`is.fixed = TRUE`) or "floating strike look-back option" (`is.fixed = FALSE`).
#'
#' @keywords internal
option.exotic.lookback <- function(obj, is.fixed = TRUE) {
    obj <- c(
        obj,
        "is.fixed" = is.fixed
    )
    class(obj) <- c("lookback", "option")
    obj
}

# Creating new option environment ==============================================

#' Initialise new `env` class object
#'
#' @param method A string specifying the method used to price the option. Supported values are `"mc"` (Monte Carlo Method), `"bs"` (for Black-Scholes formula), `"binomial"` (Binomial Lattice Tree).
#' @param S A number specifying the current price of the underlying asset.
#' @param r A number specifying the (fixed) annual interest rate.
#' @param q A number specifying the (fixed) annual dividend yield rate of the option.
#' @param sigma A number specifying the annual volatility measure.
#' @param n A number speciying the number of simulations to make (for `method = "mc"`), or the number of time steps the life of the option will be broken into (for `method = "binomial"` and `method = "trinomial"`).
#' @param steps A number specifying the number of steps each asset price trajectory will contain, used only for `method = "mc"`.
#'
#' @return An S3 object with `class` attribute evaluated to `"env"`.
#' @export
#'
#' @examples
#' # Create an environment with current price 20, annual interest rate 2%,
#' # annual dividend yield rate 1%, annual volatility measure 0.05, and number
#' # of simulations set to 100
#' env1 <- option.env(S = 20, r = 0.02, q = 0.01, sigma = 0.05, n = 100)
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
    class(env) <- "env"
    env
}
