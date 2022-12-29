# Trinomial Model ==============================================================

## [Internal] Generate Trinomial Tree ==========================================

#' Generating Trinomial Tree Matrix
#'
#' @param S0 A number representing the current price of the underlying asset.
#' @param u A number representing the ratio of an up jump in the asset value. Notice here a ratio for down jump is not specified, this is because under the Trinomial model we assume that d = 1/u.
#' @param n An integer representing the number of time steps into which the interval of length t is divided.
#'
#' @return A matrix representing the Trinomial lattice tree.
#'
#' @keywords internal
Trinomial.tree <- function(S0, u, n) {
    d <- 1 / u
    dim <- c(n + 1, 1 + 2 * n)
    S <- matrix(nrow = dim[1], ncol = dim[2])
    temp <- c(S0)
    for (i in 1:dim[1]) {
        S[i, ] <- c(temp, rep(NA, 2 * (dim[1] - i)))
        temp <- c(temp[1] * d, temp, temp[length(temp)] * u)
    }
    S
}

## [Internal] Backward Recursive Risk-neutral Valuation for Trinomial Model ====

#' Backward Recursive Risk-neutral Valuation for Trinomial Model
#'
#' @param p A number representing the risk-neutral probability.
#'
#' @param fn A numeric vector containing all possible stock prices at the final step/state of the binomial tree.
#' @param p1 A number representing the risk-neutral probability of an up jump of the asset price.
#' @param p2 A number representing the risk-neutral probability of an horizontal jump of the asset price.
#' @param p3 A number representing the risk-neutral probability of an down jump of the asset price.
#' @param r A number representing the continuously compounded yearly interest rate.
#' @param dt A number representing the length in time of each step.
#' @param K A number representing the exercise price of option, needed only when calculating an American option.
#' @param S A number representing the current price of option, needed only when calculating an American option.
#' @param type A character evaluating to either `"call"` or `"put"`, representing the type of the option; the default value is set to be `"call"`.
#' @param style A character evaluating to either `"European"` or `"American"`, representing the style of the option; the default value is set to be `"American"`.
#'
#' @return A number representing the result option price.
#'
#' @keywords internal
Trinomial.rnv <- function(fn, p1, p2, p3, r, n, dt, K = NULL, S = NULL,
                          type = "call",
                          style = "European") {

    if (style == "European") {
        rnv.European <- function(fn, p1, p2, p3, r, dt, state = n) {
            if (state <= 0) {
                fn
            } else {
                l <- 1 + 2 * (state - 1)
                fm <- sapply(1:l, function(i) {exp(-r * dt) * (p1 * fn[i + 2] + p2 * fn[i + 1] + p3 * fn[i])})
                rnv.European(fm, p1, p2, p3, r, dt, state = state - 1)
            }
        }
        rnv.European(fn, p1, p2, p3, r, dt, state = n)
    } else if (style == "American") {
        rnv.American <- function(fn, p1, p2, p3, r, dt, K, S, type = type, state = n) {
            if (state <= 0) {
                fn
            } else {
                l <- 1 + 2 * (state - 1)
                fm <- sapply(1:l, function(i) {
                    if (type == "call") {
                        max(max(S[state, i] - K, 0), exp(-r * dt) * (p1 * fn[i + 2] + p2 * fn[i + 1] + p3 * fn[i]))
                    } else if (type == "put") {
                        max(max(K - S[state, i], 0), exp(-r * dt) * (p1 * fn[i + 2] + p2 * fn[i + 1] + p3 * fn[i]))
                    }
                })
                rnv.American(fm, p1, p2, p3, r, dt, K, S, type = type, state = state - 1)
            }
        }
        rnv.American(fn, p1, p2, p3, r, dt, K, S, type = type)
    } else {
        stop("Invalid option style. `style` must be specified to either 'European' or 'American'.")
    }
}

## Phelim Boyle Trinomial Model ================================================

#' Phelim Boyle Trinomial Lattice Model
#'
#' @param K A number representing the exercise price of option.
#' @param S A number representing the current price of option, this is also the asset value after a horizontal jump.
#' @param u A number representing the ratio of an up jump in the asset value. Notice here a ratio for down jump is not specified, this is because under the Trinomial model we assume that d = 1/u.
#' @param r A number representing the continuously compounded yearly interest rate.
#' @param t A number representing the time to option maturity (in years).
#' @param n An integer representing the number of time steps into which the interval of length t is divided.
#' @param sigma A number representing the volatility (in standard deviation) of the rate of return on the underlying asset (yearly).
#' @param type A character evaluating to either `"call"` or `"put"`, representing the type of the option; the default value is set to be `"call"`.
#' @param style A character evaluating to either `"European"` or `"American"`, representing the style of the option; the default value is set to be `"American"`.
#' @param all A logical evaluating to `TRUE` or `FALSE` indicating whether the output should contain all option price, risk-neutral probability, and stock price tree, or option price only; the default value is set to be `FALSE`.
#' @param plot A logical evaluating to `TRUE` or `FALSE` indicating whether the binomial tree should be plotted; the default value is set to be `FALSE`.
#'
#' @return If `all = FALSE`, return only a number representing the result option price, where as `all = TRUE` return a list containing the result options price, risk-neutral probability, and the stock price tree (matrix). If `plot = TRUE`, the function will also produce a plot of the Binomial tree.
#' @export
#'
#' @examples Trinomial(50, 8, 2, 0.05, 2, 3, type = "put", all = TRUE)
Trinomial <- function(K, S, u, r, t, n, sigma = 0,
                      type = "call",
                      style = "European",
                      all = FALSE,
                      plot = FALSE) {

    # Restrict u != 1

    dt <- T/n
    M <- exp(r * dt)
    V <- M^2 * (exp(sigma^2 * dt) - 1)
    p1 <- ((V + M^2 - M) * u - (M - 1)) / ((u - 1) * (u^2 - 1)) # Compute riskless probability for an upward price movement
    p3 <- (u^2 * (V + M^2 - M) - u^3 * (M - 1)) / ((u - 1) * (u^2 - 1)) # Compute riskless probability for a downward price movement
    p2 <- 1 - p1 - p3 # Compute riskless probability for a horizontal price movement

    price_tree <- Trinomial.tree(S, u, n)
    Sn <- price_tree[n + 1, ]

    if (type == "call") {
        fn <- sapply(Sn, function(x) {max(x - K, 0)})
    } else if (type == "put") {
        fn <- sapply(Sn, function(x) {max(K - x, 0)})
    } else {
        stop("Invalid option type. `type` must be specified to either 'call' or 'put'.")
    }

    price <- Trinomial.rnv(fn, p1, p2, p3, r, n, dt, K, S = price_tree, type = type, style = style)

    if (all) {
        list(
            "price" = price,
            "p" = c(p1, p2, p3),
            "price_tree" = price_tree
        )
    } else {
        price
    }
}
