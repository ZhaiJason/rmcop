
# Geometric Sequence for Vectors ===============================================

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

# Risk-neutral valuation for European options ==================================

#' Risk-neutral Valuation for European Options with Recursion Method
#'
#' @param fn A vector containing the final states value of the Binomial model.
#' @param p A number representing the risk-neutral probability for the stock price to rise.
#' @param r A number representing the fixed annual interest rate.
#' @param dt A number representing the time lapse between each Binomial sub-tree.
#'
#' @return A number representing the computed current value of the option.
#'
#' @keywords internal
rnv.european <- function(fn, p, r, dt) {
    q <- 1 - p
    if (length(fn) <= 1) {
        fn
    } else {
        m <- length(fn) - 1 # Specify the target state/step
        fm <- sapply(1:m, function(i) {exp(-r * dt) * (p * fn[i + 1] + q * fn[i])})
        rnv.european(fm, p, r, dt)
    }
}

# Risk-neutral valuation for American options ==================================

#' Risk-neutral Valuation for American Options with Recursion Method
#'
#' @param fn A vector containing the final states value of the Binomial model.
#' @param p A number representing the risk-neutral probability for the stock price to rise.
#' @param r A number representing the fixed annual interest rate.
#' @param dt A number representing the time lapse between each Binomial sub-tree.
#' @param K A number representing the strike price
#' @param S A matrix representing the stock price tree.
#' @param type A string specifying the type of option, either "call" or "put", with "put" being the default option.
#'
#' @return A number representing the computed current value of the option.
#' @export
#'
#' @keywords internal
rnv.american <- function(fn, p, r, dt, K, S, type = "call") {
    q <- 1 - p
    if (length(fn) <= 1) {
        fn
    } else {
        m <- length(fn) - 1 # Specify the target state/step
        fm <- sapply(1:m, function(i) {
            if (type == "call") {
                max(max(S[m, i] - K, 0), exp(-r * dt) * (p * fn[i + 1] + q * fn[i]))
            } else if (type == "put") {
                max(max(K - S[m, i], 0), exp(-r * dt) * (p * fn[i + 1] + q * fn[i]))
            }
        })
        rnv.american(fm, p, r, dt, K, S, type = type)
    }
}

# n-step Binomial Model ========================================================

#' Basic n-step Binomial Options Pricing Model
#'
#' @param K A number representing the exercise price of the option.
#' @param S A number representing the current price of the stock.
#' @param u A number representing the ratio of rise in price in the binomial tree.
#' @param d A number representing the ratio of fall in price in the binomial tree.
#' @param r A number representing the risk-free annual interest rate.
#' @param t A number representing the number of years reaching maturity.
#' @param n An integer representing the number of steps of the binomial model.
#' @param type A string specifying the type of options, takes values "call" or "put, default takes value "call".
#' @param style A string specifying the style of options, takes values "European" or "American", default takes value "European".
#' @param bystep A logical evaluating to `TRUE` or `FALSE` indicating whether the pay-off of each step should be recorded during calculation.
#' @param plot A logical evaluating to `TRUE` or `FALSE` indicating whether the binomial tree should be plotted.
#'
#' @return A number representing the computed current price of the option.
#' @export
#'
#' @examples Binomial.Basic(K = 14, S = 8, u = 2, d = 0.5, r = 0.25, t = 0.75, n = 3)
Binomial.Basic <- function(K, S, u, d, r, t, n,
                           type = "call",
                           style = "European",
                           all = FALSE,
                           plot = FALSE) {

    dt <- t / n # Compute delta t
    p <- (exp(r * dt) - d) / (u - d) # Compute risk-neutral probability
    S0 <- S * d ^ (0:n) # Stores the route of stock prices which falls every time.

    # Store the stock prices of the tree for each state in a matrix `price_tree`
    price_tree <- matrix(seq_geom(S0, u/d, n), nrow = n + 1)

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
    if (style == "European") {
        price <- rnv.european(fn, p, r, dt)
    } else if (style == "American") {
        price <- rnv.american(fn, p, r, dt, K, S = price_tree, type = type)
    }

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

