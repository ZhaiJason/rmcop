## Binomial ====================================================================

## [Internal] Geometric Sequence for Vectors ===================================

#' Geometric Sequence for Vectors using Recursion Method
#'
#' @param x An vector used as the base of calculation.
#' @param q An fixed ratio.
#' @param n A number specifying the number of times we wish to advanced for the geometric sequence.
#'
#' @return A vector concatenating the original vector and the resulting geometric sequences.
#'
#' @keywords internal
seq_geom <- function(x, q, n) {
    seq_geom.component <- function(x, q, n, iter = 0) {
        if (iter >= n) {
            x
        } else {
            c(x, seq_geom.component(x * q, q, n, iter = iter + 1))
        }
    }
    seq_geom.component(x, q, n)
}

## [Internal] Backward Recursive Risk-neutral Valuation for Binomial Model =====

#' Backward Recursive Risk-neutral Valuation for Binomial Model
#'
#' @param fn A numeric vector containing all possible stock prices at the final step/state of the binomial tree.
#' @param p A number representing the risk-neutral probability.
#' @param r A number representing the continuously compounded yearly interest rate.
#' @param dt A number representing the length in time of each step.
#' @param K A number representing the exercise price of option, needed only when calculating an American option.
#' @param S A number representing the current price of option, needed only when calculating an American option.
#' @param type A character evaluating to either `"call"` or `"put"`, representing the type of the option; the default value is set to be `"call"`.
#' @param style A character evaluating to either `"European"` or `"American"`, representing the style of the option; the default value is set to be `"American"`.
#'
#' @return
#'
#' @keywords internal
Binomial.rnv <- function(fn, p, r, n, dt, K = NULL, S = NULL,
                         type = "call",
                         style = "European") {
    q <- 1 - p
    if (style == "European") {
        rnv.European <- function(fn, p, r, dt, state = n) {
            if (length(fn) <= 0) {
                fn
            } else {
                l <- state # Specify the length l of the vector containing prices of next state looking back by 1 step
                fm <- sapply(1:l, function(i) {exp(-r * dt) * (p * fn[i + 1] + q * fn[i])})
                rnv.European(fm, p, r, dt, state = state - 1)
            }
        }
        rnv.European(fn, p, r, dt)
    } else if (style == "American") {
        rnv.American <- function(fn, p, r, dt, K, S, type = type, state = n) {
            if (state <= 0) {
                fn
            } else {
                l <- state
                fm <- sapply(1:l, function(i) {
                    if (type == "call") {
                        max(max(S[state, i] - K, 0), exp(-r * dt) * (p * fn[i + 1] + q * fn[i]))
                    } else if (type == "put") {
                        max(max(K - S[state, i], 0), exp(-r * dt) * (p * fn[i + 1] + q * fn[i]))
                    }
                })
                rnv.American(fm, p, r, dt, K, S, type = type, state = state - 1)
            }
        }
        rnv.American(fn, p, r, dt, K, S, type = type)
    }
}

## Cox, Ross & Rubinstein Binomial Model =======================================

#' Cox, Ross & Rubinstein Lattice Binomial Options Pricing Model
#'
#' @param K A number representing the exercise price of option.
#' @param S A number representing the current price of option, this is also the asset value after a horizontal jump.
#' @param u A number representing the ratio of an up jump in the asset value.
#' @param d A number representing the ratio of an down jump in the asset value.
#' @param r A number representing the continuously compounded yearly interest rate.
#' @param t A number representing the time to option maturity (in years).
#' @param n An integer representing the number of time steps into which the interval of length t is divided.
#' @param σ A number representing the volatility (in standard deviation) of the rate of return on the underlying asset (yearly).
#' @param type A character evaluating to either `"call"` or `"put"`, representing the type of the option; the default value is set to be `"call"`.
#' @param style A character evaluating to either `"European"` or `"American"`, representing the style of the option; the default value is set to be `"American"`.
#' @param all A logical evaluating to `TRUE` or `FALSE` indicating whether the output should contain all option price, risk-neutral probability, and stock price tree, or option price only; the default value is set to be `FALSE`.
#' @param plot A logical evaluating to `TRUE` or `FALSE` indicating whether the binomial tree should be plotted; the default value is set to be `FALSE`.
#'
#' @return If `all = FALSE`, return only a number representing the result option price, where as `all = TRUE` return a list containing the result options price, risk-neutral probability, and the stock price tree (matrix). If `plot = TRUE`, the function will also produce a plot of the Binomial tree.
#' @export
#'
#' @examples Binomial(K = 14, S = 8, u = 2, d = 0.5, r = 0.25, t = 0.75, n = 3, all = TRUE, style = "American")
Binomial <- function(K, S, u = exp(σ * sqrt(t / n)), d = exp(-σ * sqrt(t / n)), r, t, n, σ = 0,
                         type = "call",
                         style = "European",
                         all = FALSE,
                         plot = FALSE) {

    dt <- t / n # Compute delta t
    p <- (exp(r * dt) - d) / (u - d) # Compute risk-neutral probability
    S0 <- S * d ^ (0:n) # Stores the route of stock prices which falls every time.

    # Store the stock prices of the tree for each state in a matrix `price_tree`
    price_tree <- matrix(seq_geom(S0, u/d, n), nrow = n + 1) # Consider remove the excessive calculation by replacing the geom_seq method

    # Alternative method for generating the matrix, a bit slower than using seq_geom method.
    # price_tree <- t(sapply(S0, function(x) {x * (u / d) ^ (0:n)}))
    price_tree[upper.tri(price_tree)] <- NA

    Sn <- price_tree[n + 1, ] # Stores the possible stock prices at the terminal steps.

    # Calculate the option value at the final step, i.e. maturity.
    if (type == "call") {
        fn <- sapply(Sn, function(x) {max(x - K, 0)})
    } else if (type == "put") {
        fn <- sapply(Sn, function(x) {max(K - x, 0)})
    }

    # Calculate the option value at current time.
    price <- Binomial.rnv(fn, p, r, n, dt, K, S = price_tree, type = type, style = style)

    # PLOTTING FUNCTION NOT ADDED

    # Return output
    if (all) {
        list(
            "price" = price,
            "p" = p,
            "price_tree" = price_tree
        )
    } else {
        price
    }
}

# Trinomial ====================================================================

## Generate Trinomial Tree =====================================================

#' Generating Trinomial Tree Matrix
#'
#' @param S0
#' @param u
#' @param n An integer representing the number of time steps into which the interval of length t is divided.
#'
#' @return A matrix representing the Trinomial lattice tree.
#'
#' @keywords internal
Trinomial.tree <- function(S0, u, n) {
    u <- q
    d <- 1 / u
    dim <- c(n + 1, 1 + 2 * n)
    S <- matrix(nrow = dim[1], ncol = dim[2])
    temp <- c(S0)
    for (i in 1:dim[1]) {
        S[i, ] <- c(temp, rep(NA, 2 * (n + 1 - i)))
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
#' @return
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
    }
}

## Phelim Boyle Trinomial Model ================================================

#' Phelim Boyle Trinomial Lattice Model for Option Pricing
#'
#' @param K A number representing the exercise price of option.
#' @param S A number representing the current price of option, this is also the asset value after a horizontal jump.
#' @param u A number representing the ratio of an up jump in the asset value. Notice here a ratio for down jump is not specified, this is because under the Trinomial model we assume that d = 1/u.
#' @param r A number representing the continuously compounded yearly interest rate.
#' @param t A number representing the time to option maturity (in years).
#' @param n An integer representing the number of time steps into which the interval of length t is divided.
#' @param σ A number representing the volatility (in standard deviation) of the rate of return on the underlying asset (yearly).
#' @param type A character evaluating to either `"call"` or `"put"`, representing the type of the option; the default value is set to be `"call"`.
#' @param style A character evaluating to either `"European"` or `"American"`, representing the style of the option; the default value is set to be `"American"`.
#' @param all A logical evaluating to `TRUE` or `FALSE` indicating whether the output should contain all option price, risk-neutral probability, and stock price tree, or option price only; the default value is set to be `FALSE`.
#' @param plot A logical evaluating to `TRUE` or `FALSE` indicating whether the binomial tree should be plotted; the default value is set to be `FALSE`.
#'
#' @return If `all = FALSE`, return only a number representing the result option price, where as `all = TRUE` return a list containing the result options price, risk-neutral probability, and the stock price tree (matrix). If `plot = TRUE`, the function will also produce a plot of the Binomial tree.
#' @export
#'
#' @examples Trinomial(K = 50, S = 8, u = 2, r = 0.25, t = 0.75, n = 3, all = TRUE, type = "put", style = "American")
Trinomial <- function(K, S, u = exp(σ * sqrt(t / n)), r, t, n, σ = 0,
                      type = "call",
                      style = "european",
                      all = FALSE,
                      plot = FALSE) {

    # Restrictions:
    # u != 1, d = 1/u

    dt <- T/n
    M <- exp(r * dt)
    V <- M^2 * (exp(σ^2 * dt) - 1)
    p1 <- ((V + M^2 - M) * u - (M - 1)) / ((u - 1) * (u^2 - 1))
    p3 <- (u^2 * (V + M^2 - M) - u^3 * (M - 1)) / ((u - 1) * (u^2 - 1))
    p2 <- 1 - p1 - p3

    price_tree <- Trinomial.tree(S, u, n)
    Sn <- price_tree[n + 1, ]

    if (type == "call") {
        fn <- sapply(Sn, function(x) {max(x - K, 0)})
    } else if (type == "put") {
        fn <- sapply(Sn, function(x) {max(K - x, 0)})
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
