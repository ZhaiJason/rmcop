# Top Level Pricing Functions for Options ======================================

# Following functions are exported:
# ALL FUNCTIONS

#' Pricing `option` Class Object
#'
#' The all-in-one pricing function for `rmcop`. User should predefine an `"option"` class object and an `"env"` class object as inputs of `price.option`. According to the pricing method selected, the user shall provide additional inputs as instructions for the pricing function. Depending
#'
#' @param obj The specified `"option"` class object which encapsulate some properties of an option of interest.
#' @param env The predefined `option.env` object, which contains specified arguments
#' @param method A string specifying the method used to price the option. Supported values are `"mc"` (Monte Carlo Method), `"bs"` (for Black-Scholes formula), `"binomial"` (Binomial Lattice Tree), and `"trinomial"` (Trinomial Lattice Tree).
#' @param n A number speciying the number of simulations to make (for `method = "mc"`), or the number of time steps the life of the option will be broken into (for `method = "binomial"` and `method = "trinomial"`).
#' @param ... Arguments required by corresponding specified option pricing methods.
#' \itemize{
#'      \item \code{steps}:
#'      \item \code{u}:
#'      \item \code{d}: Used when `method = "binomial"`
#' }
#' @param all A boolean specifying whether the pricing function should return only the result price (if `FALSE`) or other (maybe useful) data during computation (if `TRUE`). The default value for this argument is `FALSE`.
#'
#' @return The result estimate/solution of the option price.
#' @export
#'
#' @examples
#' # To use price.option, we first define the "option" and "env" objects
#' op <- option("european", "vanilla", "call", 20, 0.75)
#' op.env <- option.env(S = 20, r = 0.03, q = 0.01, sigma = 0.05)
#'
#' # The below codes
#' # Monte Carlo
#' price.option(op, op.env, n = 100, steps = 10, all = FALSE, plot = FALSE)
#'
#' # Black-Scholes
#' price.option(op, op.env, method = "bs", all = FALSE)
#'
#' # Binomial Lattice Tree
#' price.option(op, op.env, n = 10, u = 1.2, d = 0.8, method = "binomial", all = FALSE)
price.option <- function(obj, env, method = env$method, n = env$n, ... , all = FALSE) {
    check.method(method) # Checking if the method specified is supported, function is under Tools.R
    res <- get(paste0("price.option.", method))(obj = obj, env = env, n = n, ..., all = all) # Distribute to corresponding pricing methods
    res
}

#' Option Pricing via Monte Carlo Method
#'
#' @param obj The specified `"option"` class object which encapsulate some properties of an option of interest.
#' @param env The specified `"env"` class object which encapsulate some market variables required by corresponding pricing methods.
#' @param n A number speciying the number of simulations to make (for `method = "mc"`), or the number of time steps the life of the option will be broken into (for `method = "binomial"` and `method = "trinomial"`).
#' @param steps A number specifying the number of steps each asset price trajectory will contain, used only for `method = "mc"`.
#' @param all A logical value specifying whether the pricing function should return only the result price (`all = FALSE`) or other data during computation (`all = TRUE`). The default value for this argument is `FALSE`.
#'
#' @keywords internal
price.option.mc <- function(obj, env, n, steps = env$steps, all, ...) {
    # Checking if parameters n and steps are specified
    if (is.null(n)) stop("n is not specified")
    if (is.null(steps)) {
        steps <- 1
        warning("steps is not specified, evaluated to default value 1")
    }
    res <- get(paste0(class(obj)[1], ".mc"))(obj = obj, env = env, n = n, steps = steps, all = all, ...) # Trigger pricing engine
    res
}

#' Option Pricing via Black-Scholes Model
#'
#' @param obj The specified `"option"` class object which encapsulate some properties of an option of interest.
#' @param env The specified `"env"` class object which encapsulate some market variables required by corresponding pricing methods.
#' @param n A number speciying the number of simulations to make (for `method = "mc"`), or the number of time steps the life of the option will be broken into (for `method = "binomial"` and `method = "trinomial"`).
#' @param all A logical value specifying whether the pricing function should return only the result price (`all = FALSE`) or other data during computation (`all = TRUE`). The default value for this argument is `FALSE`.
#'
#' @keywords internal
price.option.bs <- function(obj, env, n, all) {
    if (obj$style == "american") { # American option pricing using bs is limited, check if the condition is satisfied
        if (env$q != 0 | obj$type != "call") {
            warning("Black Scholes valuation should only apply on European european options or American call option with non-dividend paying asset, the result here will be based on European options pricing and may not be accurate.")
        }
    }
    res <- get(paste0(class(obj)[1], ".bs"))(obj, env, all) # Trigger pricing engine
    res
}

#' Option Pricing via Binomial Lattice Model
#'
#' @param obj The specified `"option"` class object which encapsulate some properties of an option of interest.
#' @param env The specified `"env"` class object which encapsulate some market variables required by corresponding pricing methods.
#' @param n A number speciying the number of simulations to make (for `method = "mc"`), or the number of time steps the life of the option will be broken into (for `method = "binomial"` and `method = "trinomial"`).
#' @param u A number representing the ratio of an upward price movement.
#' @param d A number representing the ratio of an downward price movement.
#' @param all A logical value specifying whether the pricing function should return only the result price (`all = FALSE`) or other data during computation (`all = TRUE`). The default value for this argument is `FALSE`.
#'
#' @keywords internal
price.option.binomial <- function(obj, env, n, u, d, all) {
    if (is.null(n)) stop("n is not specified") # Check if number of binomial tree steps is specified
    res <- get(paste0(class(obj)[1], ".binomial"))(obj = obj, env = env, n = n, u = u, d = d, all = all) # Trigger pricing engine
    res
}
