# Engine =======================================================================

#' Compute d1 in Black-Scholes Formula
#'
#' @param S A number representing the current price of the underlying asset.
#' @param K A number representing the strike price of the option.
#' @param t A number representing the time to maturity in years.
#' @param r A number representing the continuously compounded risk-free interest rate per annum.
#' @param q A number representing the continuously compounded dividend yield per annum.
#' @param sigma A number representing the volatility per annum.
#'
#' @return A number representing the value of d1 used in Black-Scholes formula.
#'
#' @keywords internal
BlackScholes.d1 <- function(S, K, t, r, q, sigma) {
    (log(S / K) + (r - q + sigma^2 / 2) * t) / (sigma * sqrt(t))
}

# Model ========================================================================

#' Black-Scholes Model for Option Pricing
#'
#' @importFrom stats pnorm
#'
#' @param S A number representing the current price of the underlying asset.
#' @param K A number representing the strike price of the option.
#' @param t A number representing the time to maturity in years.
#' @param r A number representing the continuously compounded risk-free interest rate per annum.
#' @param q A number representing the continuously compounded dividend yield per annum.
#' @param sigma A number representing the volatility per annum.
#' @param type A character evaluating to either `"call"` or `"put"`, representing the type of the option; the default value is set to be `"call"`.
#'
#' @return A number representing the current price of the option.
#'
#' @keywords internal
BlackScholes <- function(S, K, t, r, q, sigma, type = "call") {
    d1 <- BlackScholes.d1(S, K, t, r, q, sigma)
    d2 <- d1 - sigma * sqrt(t)

    if (type == "call") {
        exp(-q * t) * S * pnorm(d1) - exp(-r * t) * K * pnorm(d2)
    } else if (type == "put") {
        exp(-r * t) * K * pnorm(-d2) - exp(-q * t) * S * pnorm(-d1)
    } else {
        stop("Invalid option type. `type` must be specified to either 'call' or 'put'.")
    }
}

vanilla.bs <- function(obj, env, all) {
    NA
}
