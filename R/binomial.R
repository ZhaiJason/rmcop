# Binomial Model ===============================================================

## [Internal] Binomial Tree ====================================================

#' Generating Binomial Tree Matrix
#'
#' @param S0 A number representing the current price of the underlying asset.
#' @param u A number representing the ratio of an upward price movement.
#' @param d A number representing the ratio of an downward price movement.
#' @param n An integer representing the number of time steps into which the interval of length t is divided.
#'
#' @return A matrix representing the Binomial lattice tree.
#'
#' @keywords internal
Binomial.tree <- function(S0, u, d, n) {
    dim <- n + 1
    S <- matrix(nrow = dim, ncol = dim) # Predefine the size
    temp <- c(S0)
    for (i in 1:dim) {
        S[i, ] <- c(temp, rep(NA, dim - i))
        temp <- c(temp[1] * d, temp * u)
    }
    S
}

## [Internal] Backward Recursive Risk-neutral Valuation for Binomial Model =====

#' Backward Recursive Risk-neutral Valuation for Binomial Model
#'
#' @param fn A numeric vector containing all possible stock prices at the final step/state of the binomial tree.
#' @param p A number representing the risk-neutral probability for an upward price movement.
#' @param q A number representing the risk-neutral probability for a downward price movement.
#' @param r A number representing the continuously compounded yearly interest rate.
#' @param dt A number representing the length in time of each step.
#' @param K A number representing the exercise price of option, needed only when calculating an American option.
#' @param S A number representing the current price of option, needed only when calculating an American option.
#' @param type A character evaluating to either `"call"` or `"put"`, representing the type of the option; the default value is set to be `"call"`.
#' @param style A character evaluating to either `"European"` or `"American"`, representing the style of the option; the default value is set to be `"American"`.
#' @param n
#'
#' @return A number representing the result options price.
#'
#' @keywords internal
Binomial.rnv <- function(fn, p, q, r, n, dt, K = NULL, S = NULL,
                         type = "call",
                         style = "European") {
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
    } else {
        stop("Invalid option style. `style` must be specified to either 'European' or 'American'.")
    }
}

## Cox, Ross & Rubinstein Binomial Model =======================================

#' Cox, Ross & Rubinstein Binomial Lattice Model
#'
#' @param K A number representing the exercise price of option.
#' @param S A number representing the current price of option, this is also the asset value after a horizontal jump.
#' @param u A number representing the ratio of an up jump in the asset value.
#' @param d A number representing the ratio of an down jump in the asset value.
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
#' @examples Binomial(14, 8, 2, 0.5, 0.25, 0.75, 3, all = TRUE, type = "put", style = "American")
Binomial <- function(K, S, u = exp(sigma * sqrt(t / n)), d = exp(-sigma * sqrt(t / n)), r, t, n, sigma = 0,
                         type = "call",
                         style = "European",
                         all = FALSE,
                         plot = FALSE) {

    dt <- t / n # Compute delta t
    p <- (exp(r * dt) - d) / (u - d) # Compute riskless probability for an upward price movement
    q <- 1 - p # Compute riskless probability for a downward price movement

    # Store the stock prices of the tree for each state in a matrix `price_tree`
    price_tree <- Binomial.tree(S, u, d, n)

    # Alternative method for generating the matrix, a bit slower than using seq_geom method.
    # price_tree <- t(sapply(S0, function(x) {x * (u / d) ^ (0:n)}))
    price_tree[upper.tri(price_tree)] <- NA

    Sn <- price_tree[n + 1, ] # Stores the possible stock prices at the terminal steps.

    # Calculate the option value at the final step, i.e. maturity.
    if (type == "call") {
        fn <- sapply(Sn, function(x) {max(x - K, 0)})
    } else if (type == "put") {
        fn <- sapply(Sn, function(x) {max(K - x, 0)})
    } else {
        stop("Invalid option type. `type` must be specified to either 'call' or 'put'.")
    }

    # Calculate the option value at current time.
    price <- Binomial.rnv(fn, p, q, r, n, dt, K, S = price_tree, type = type, style = style)

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

