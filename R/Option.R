# Creating new option object ===================================================

# Following functions are exported:
# option
# option.env

## Option ======================================================================
#' Initialise new `option` class object
#'
#' Create an S3 class object which characterised an option of interest. The returned object is then used together with a `"env"` class object together as inputs of the `price.option` function for further option pricing.
#'
#' @param style A string specifying the style of the option of interest. Possible values are `"european"` and `"american"`, corresponding to European options and American options, respectively.
#' @param option A string specifying the specific family of the option of interest. Supported values are: `"vanilla"` (default, for Vanilla options), `"asian"` (for Asian options), `"barrier"` (for Barrier options), `"binary"` (for Binary/Cash-or-nothing options), and `"lookback"` (for Lookback options). This argument is a string that specifies the subclass of the S3 class `"option"` for the defined object.
#' @param type A string specifying the type of the option of interest. Possible values are either `"call"` or `"put"`, corresponding to call option (right to buy) and pull option (right to sale), respectively.
#' @param K A number specifying the strike price of the option.
#' @param t A number specifying the maturity/expiry of the option in years.
#' @param ... Arguments required by corresponding specified option families.
#' \itemize{
#'      \item \code{is.avg_price}: A logical value for `option = "asian"` case, specifying whether the option is an "average price Asian option" or "average strike Asian option". The default value for this argument is `TRUE`, indicating the option being an "average price Asian option".
#'      \item \code{barrier}: A number for `option = "barrier` case, specifying the barrier price of the barrier option.
#'      \item \code{is.knockout}: A logical value for `option = "barrier"` class objects, specifying whether the option is an "knock-out barrier option" (`is.knockout = TRUE`) or "knock-in barrier option" (`is.knockout = FALSE`).
#'      \item \code{payout}: A number for `option = "binary"` class objects, specifying the payout of a binary option.
#'      \item \code{is.fixed}: A logical value for `option = "lookback"` class objects. specifying whether the option is an "fixed strike look-back option" (`is.fixed = TRUE`) or "floating strike look-back option" (`is.fixed = FALSE`).
#' }
#'
#' @return An S3 object with `class` attribute evaluated to `"option"`. This object can then be used for pricing together with an `"env"` class object through `price.option()` function.
#' @export
#'
#' @examples
#' # Create an European vanilla option with strike price 20, expiry in 1 year.
#' option("european", "vanilla", "call", K = 20, t = 1)
#'
#' # Create European average strike Asian option with strike price 20, expiry in 1 year.
#' option("european", "asian", "call", K = 20, t = 1, is.avg_price = FALSE)
#'
#' # Create European knock-out barrier option with strike price 20, barrier price 21, expiry in 1 year.
#' option("european", "barrier", "call", K = 20, t = 1, barrier = 21, is.knockout = TRUE)
#'
#' # Create European binary option with strike price 20, fixed payout 5, expiry in 1 year.
#' option("european", "binary", "call", K = 20, t = 1, payout = 5)
#'
#' # Create European fixed strike lookback option with strike price 20, expiry in 1 year.
#' option("european", "lookback", "call", K = 20, t = 1, is.fixed = TRUE)
option <- function(style, option = "vanilla", type, K, t, ...) {

    # Encapsulate basic option properties
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
    obj <- get(paste0("option.exotic.", option))(obj, ...) # Direct to corresponding exotic option's initialisation function
    obj
}

#' Initialise Asian option
#'
#' @param obj The specified `"option"` class object which encapsulate some properties of an option of interest.
#' @param is.avg_price A logical value for `"asian" "option"` class objects, specifying whether the option is an "average price Asian option" or "average strike Asian option". The default value for this argument is `TRUE`, indicating the option being an "average price Asian option".
#'
#' @keywords internal
option.exotic.asian <- function(obj, is.avg_price = TRUE) {
    obj <- c( # Add additional exotic option properties
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
    obj <- c( # Add additional exotic option properties
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
    obj <- c( # Add additional exotic option properties
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
    obj <- c( # Add additional exotic option properties
        obj,
        "is.fixed" = is.fixed
    )
    class(obj) <- c("lookback", "option")
    obj
}

# Creating new option environment ==============================================

#' Initialise new `env` class object
#'
#' Create an S3 class object which characterised the market environment used when pricing the option of interest. The returned object is used together with an `"option"` class object as inputs of the pricing function `price.option`.
#'
#' @param method A string specifying the method used to price the option. Supported values are `"mc"` (Monte Carlo Method), `"bs"` (for Black-Scholes formula), `"binomial"` (Binomial Lattice Tree).
#' @param S A number specifying the current price of the underlying asset.
#' @param r A number specifying the (fixed) annual interest rate.
#' @param q A number specifying the (fixed) annual dividend yield rate of the option.
#' @param sigma A number specifying the annual volatility measure.
#' @param n A number speciying the number of simulations to make (for `method = "mc"`), or the number of time steps the life of the option will be broken into (for `method = "binomial"` and `method = "trinomial"`).
#' @param steps A number specifying the number of steps each asset price trajectory will contain, used only for `method = "mc"`.
#'
#' @return An S3 object with `class` attribute evaluated to `"env"`. It represents the market environment or the properties of the underlying asset for the option pricing setting.
#' @export
#'
#' @examples
#' # Create an environment with current price 20, annual interest rate 2%, annual
#' # dividend yield rate 1%, annual volatility measure 0.05, and number of simulations
#' # set to 100, with each trajectory breaking into 100 steps.
#' option.env(S = 20, r = 0.02, q = 0.01, sigma = 0.05, n = 100, steps = 100)
#'
#' # Alternatively, one can leave arguments `n` and `steps` unspecified or as NULL,
#' # and specify the two parameters when calling the `price.option` function.
#' option.env(S = 20, r = 0.02, q = 0.01, sigma = 0.05)
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
