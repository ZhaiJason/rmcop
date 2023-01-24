# Engine =======================================================================

#' Generating Trinomial Tree Matrix
#'
#' @param S0 A number representing the current price of the underlying asset.
#' @param u A number representing the ratio of an upward price movement. Notice here a ratio for down jump is not specified, this is because under the Trinomial model we assume that d = 1/u.
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

#' Backward Recursive Risk-neutral Valuation for Trinomial Model
#'
#' @param fn A numeric vector containing all possible stock prices at the final step/state of the binomial tree.
#' @param p1 A number representing the risk-neutral probability of an upward movement of the asset price.
#' @param p2 A number representing the risk-neutral probability of an horizontal movement of the asset price.
#' @param p3 A number representing the risk-neutral probability of an downward movement of the asset price.
#' @param r A number representing the (assumed fixed) continuously compounded yearly interest rate.
#' @param n A number speciying the number of simulations to make (for `method = "mc"`), or the number of time steps the life of the option will be broken into (for `method = "binomial"` and `method = "trinomial"`).
#' @param dt A number representing the length in time of each step.
#' @param K A number representing the exercise price of option, needed only when calculating an american option.
#' @param S A number representing the current price of option, needed only when calculating an american option.
#' @param type A character evaluating to either `"call"` or `"put"`, representing the type of the option; the default value is set to be `"call"`.
#' @param style A character evaluating to either `"european"` or `"american"`, representing the style of the option; the default value is set to be `"american"`.
#'
#' @return A number representing the result options price.
#'
#' @keywords internal
Trinomial.rnv <- function(fn, p1, p2, p3, r, n, dt, K = NULL, S = NULL,
                          type = "call",
                          style = "european") {

    if (style == "european") {
        rnv.european <- function(fn, p1, p2, p3, r, dt, state = n) {
            if (state <= 0) {
                fn
            } else {
                l <- 1 + 2 * (state - 1)
                fm <- sapply(1:l, function(i) {exp(-r * dt) * (p1 * fn[i + 2] + p2 * fn[i + 1] + p3 * fn[i])})
                rnv.european(fm, p1, p2, p3, r, dt, state = state - 1)
            }
        }
        rnv.european(fn, p1, p2, p3, r, dt, state = n)
    } else if (style == "american") {
        rnv.american <- function(fn, p1, p2, p3, r, dt, K, S, type = type, state = n) {
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
                rnv.american(fm, p1, p2, p3, r, dt, K, S, type = type, state = state - 1)
            }
        }
        rnv.american(fn, p1, p2, p3, r, dt, K, S, type = type)
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
#' @param u A number representing the ratio of an upward price movement. Notice here a ratio for down jump is not specified, this is because under the Trinomial model we assume that d = 1/u.
#' @param all A logical value specifying whether the pricing function should return only the result price (`all = FALSE`) or other data during computation (`all = TRUE`). The default value for this argument is `FALSE`.
#'
#' @keywords internal
vanilla.trinomial <- function(obj, env, n, u, all = FALSE) {

    style = obj$style
    type = obj$type
    K = obj$K
    t = obj$t
    S = env$S
    r = env$r
    sigma = env$sigma

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
