# The Base function contains internal functions which perform basic level operations.

#' Compute d2 in Black-Scholes Formulas
#'
#' @param t A number representing time to expiration (in years).
#' @param sigma A number representing volatility per annual.
#' @param S A number representing the price of the underlying asset.
#' @param K A number representing the strike price.
#' @param r A number representing the continuously compounded risk-free interest rate per annual.
#' @param q A number representing the continuously compounded dividend yield per annual.
#'
#' @return
#' @export
#'
#' @examples
d1 <- function(t, sigma, S, K, r, q = 0) {
    (log(S / K) + t * (r - q + sigma^2 / 2)) / (sigma * sqrt(t))
}

#' Compute d2 in Black-Scholes Formulas
#'
#' @param t A number representing time to expiration (in years).
#' @param sigma A number representing volatility per annual.
#' @param d1 A number representing the d1 value, if this argument is specified, it will directly used to calculate the value of d2, and the following arguments will not be used.
#' @param S A number representing the price of the underlying asset.
#' @param K A number representing the strike price.
#' @param r A number representing the continuously compounded risk-free interest rate per annual.
#' @param q A number representing the continuously compounded dividend yield per annual.
#'
#' @return
#' @export
#'
#' @examples
d2 <- function(t, sigma, d1 = NA, S = NULL, K = NULL, r = NULL, q = 0) {
    if (is.na(d1)) {
        (log(S / K) + t * (r - q + sigma^2 / 2)) / (sigma * sqrt(t)) - sigma * sqrt(t)
    } else {
        d1 - sigma * sqrt(t)
    }
}
