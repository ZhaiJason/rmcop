# Monte Carlo option pricing procedure =========================================



#' Pricing vanilla option using Monte Carlo method
#'
#' @importFrom stats rnorm
#'
#' @param obj The predefined `option` class object.
#' @param S A number of the current price of the underlying asset.
#' @param r A number representing the continuously compounded yearly interest rate.
#' @param t A number representing the time to option maturity (in years).
#' @param sigma A number representing the (assumed fixed) continuously compounded yearly interest rate.
#' @param n An integer representing the number of Monte Carlo replications of asset price to take.
#'
#' @return
#' @export
#'
#' @examples
#' euro1 <- option("european", type = "call", K = 20)
#' price.option(euro1, S = 20, t = 0.5, r = 0.01, sigma = 0.1, n = 100, all = FALSE)
vanilla.mc <- function(obj, S, r, t = obj$t, sigma, n, all = FALSE) {

    # Extract object information
    if (is.na(t)) {stop("Maturity t not specified")}
    style <- obj$style
    type <- obj$type
    K <- obj$K

    # Apply MC
    if (style == "european") {
        # Generate n random samples from N(0,1)
        Z_i <- matrix(rnorm(n), nrow = n)
        # Compute corresponding stock prices at maturity
        S_i <- S * exp((r - sigma^2 / 2) * t + sigma * sqrt(t) * Z_i)
        # Compute corresponding discounted price at current time
        if (type == "call") {
            C_i <- exp(-r * t) * pmax(S_i - K, 0)
        } else if (type == "put") {
            C_i <- exp(-r * t) * pmax(K - S_i, 0)
        }
        # Obtain the estimate of option price by taking average of C_i
        C.estimate <- mean(C_i)
    } else if (style == "american") {
        stop("American option pricing via MC not supported")
    }

    # Compute standard error of the estimate
    s_c <- sqrt(sum(C_i - C.estimate)^2 / (n - 1))
    SE <- s_c / sqrt(n)

    # Return results
    if (all) {
        list(
            "C.estimate" = C.estimate,
            "SE" = SE,
            "rv" = Z_i,
            "path" = S_i
        )
    } else {
        C.estimate
    }
}

#' Pricing Asian option using Monte Carlo method
#'
#' @importFrom stats rnorm
#'
#' @param obj
#' @param S
#' @param r
#' @param t
#' @param sigma
#' @param n
#' @param m
#' @param all
#'
#' @return
#' @export
#'
#' @examples
asian.mc <- function(obj, S, r, t = obj$t, sigma, n, m, all = FALSE) {

    # Extract object information
    if (is.na(t)) {stop("Maturity t not specified")}
    dt <- t / m # Set time change for each set
    style <- obj$style
    type <- obj$type
    K <- obj$K

    # Apply MC
    if (style == "european") {
        # Generate n random samples from N(0,1)
        Z_i <- matrix(rnorm(n), nrow = n, ncol = m)

        # # Compute corresponding stock price movements
        # dS <- S * exp((r - sigma^2 / 2) * dt + sigma * sqrt(dt) * Z_i)
        # # Compute corresponding discounted price at current time
        # if (type == "call") {
        #     C_i <- exp(-r * t) * pmax(S_i - K, 0)
        # } else if (type == "put") {
        #     C_i <- exp(-r * t) * pmax(K - S_i, 0)
        # }
        # # Obtain the estimate of option price by taking average of C_i
        # C.estimate <- mean(C_i)
    } else if (style == "american") {
        stop("American option pricing via MC not supported")
    }

    # Compute standard error of the estimate
    s_c <- sqrt(sum(C_i - C.estimate)^2 / (n - 1))
    SE <- s_c / sqrt(n)

    # Return results
    if (all) {
        list(
            "C.estimate" = C.estimate,
            "SE" = SE,
            "rv" = Z_i,
            "path" = S_i
        )
    } else {
        C.estimate
    }
}

# price.option(euro1, S = 20, t = 1, r = 0.01, sigma = 0.03, n = 10)
