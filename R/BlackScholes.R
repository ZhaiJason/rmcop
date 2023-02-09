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
bs.d1 <- function(S, K, t, r, q, sigma) {
    (log(S / K) + (r - q + sigma^2 / 2) * t) / (sigma * sqrt(t))
}

# Model ========================================================================

#' Pricing vanilla option using Black-Scholes model
#'
#' @param obj The specified `"option"` class object which encapsulate some properties of an option of interest.
#' @param env The specified `"env"` class object which encapsulate some market variables required by corresponding pricing methods.
#' @param all A logical value specifying whether the pricing function should return only the result price (`all = FALSE`) or other data during computation (`all = TRUE`). The default value for this argument is `FALSE`.
#'
#' @importFrom stats pnorm
#'
#' @keywords internal
vanilla.bs <- function(obj, env, all = FALSE) {
    type <- obj$type
    K <- obj$K
    t <- obj$t
    S <- env$S
    r <- env$r
    q <- env$q
    sigma <- env$sigma

    d1 <- bs.d1(S, K, t, r, q, sigma)
    d2 <- d1 - sigma * sqrt(t)

    if (type == "call") {
        price <- exp(-q * t) * S * pnorm(d1) - exp(-r * t) * K * pnorm(d2)
    } else if (type == "put") {
        price <- exp(-r * t) * K * pnorm(-d2) - exp(-q * t) * S * pnorm(-d1)
    }

    if (all) {
        list(
            obj,
            env,
            "price" = price
        )
    } else {
        price
    }
}
