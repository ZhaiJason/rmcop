# Engine =======================================================================

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

#' Backward Recursive Risk-neutral Valuation for Binomial Model
#'
#' @param fn A numeric vector containing all possible stock prices at the final step/state of the binomial tree.
#' @param p A number representing the risk-neutral probability for an upward price movement.
#' @param q A number representing the risk-neutral probability for a downward price movement.
#' @param r A number representing the continuously compounded yearly interest rate.
#' @param dt A number representing the length in time of each step.
#' @param K A number representing the exercise price of option, needed only when calculating an american option.
#' @param S A number representing the current price of option, needed only when calculating an american option.
#' @param type A character evaluating to either `"call"` or `"put"`, representing the type of the option; the default value is set to be `"call"`.
#' @param style A character evaluating to either `"european"` or `"american"`, representing the style of the option; the default value is set to be `"american"`.
#' @param n
#'
#' @return A number representing the result options price.
#'
#' @keywords internal
Binomial.rnv <- function(fn, p, q, r, n, dt, K = NULL, S = NULL,
                         type = "call",
                         style = "european") {
    if (style == "european") {
        rnv.european <- function(fn, p, r, dt, state = n) {
            if (state <= 0) {
                fn
            } else {
                l <- state # Specify the length l of the vector containing prices of next state looking back by 1 step
                fm <- sapply(1:l, function(i) {exp(-r * dt) * (p * fn[i + 1] + q * fn[i])})
                rnv.european(fm, p, r, dt, state = state - 1)
            }
        }
        rnv.european(fn, p, r, dt)
    } else if (style == "american") {
        rnv.american <- function(fn, p, r, dt, K, S, type = type, state = n) {
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
                rnv.american(fm, p, r, dt, K, S, type = type, state = state - 1)
            }
        }
        rnv.american(fn, p, r, dt, K, S, type = type)
    } else {
        stop("Invalid option style. `style` must be specified to either 'european' or 'american'.")
    }
}

# Model ========================================================================

#' Binomial model for pricing vanilla options
#'
#' @param obj The specified `"option"` class object which encapsulate some properties of an option of interest.
#' @param env The specified `"env"` class object which encapsulate some market variables required by corresponding pricing methods.
#' @param n A number speciying the number of simulations to make (for `method = "mc"`), or the number of time steps the life of the option will be broken into (for `method = "binomial"` and `method = "trinomial"`).
#' @param u A number representing the ratio of an upward price movement.
#' @param d A number representing the ratio of an downward price movement.
#' @param all A logical value specifying whether the pricing function should return only the result price (`all = FALSE`) or other data during computation (`all = TRUE`). The default value for this argument is `FALSE`.
#'
#' @keywords internal
vanilla.binomial <- function(obj, env, n, u, d, all = FALSE) {

    style = obj$style
    type = obj$type
    K = obj$K
    t = obj$t
    S = env$S
    r = env$r

    dt <- t / n # Compute delta t
    p <- (exp(r * dt) - d) / (u - d) # Compute riskless probability for an upward price movement
    q <- 1 - p # Compute riskless probability for a downward price movement

    # Store the stock prices of the tree for each state in a matrix `price_tree`
    price_tree <- Binomial.tree(S, u, d, n)
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



# Binomial(20, 8, u = 2, d = 0.5, r = 0.1, n = 3, t = 1, sigma = 0, all = TRUE, type = "call", style = "european")

