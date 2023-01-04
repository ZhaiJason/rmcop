# Engine =======================================================================

#' Engine for Monte Carlo Simulation on Asset Price Trajectory
#'
#' @importFrom stats rnorm
#'
#' @param type
#' @param K
#' @param S
#' @param r
#' @param q
#' @param t
#' @param sigma
#' @param n
#' @param m
#'
#' @return
#'
#' @keywords internal
mc.engine <- function(type, K, t, S, r, q, sigma, n, steps) {

    # Generate n random samples from N(0,1)
    Z_i <- matrix(rnorm(n * steps), nrow = n)

    if (steps == 1) { # Path independent simulation

        # Compute corresponding stock prices at maturity
        S_i <- S * exp((r - q - sigma^2 / 2) * t + sigma * sqrt(t) * Z_i)
        S_i <- S_i * exp(-q * t) # Adjust the asset price for dividends

        # Compute corresponding discounted price at current time
        if (type == "call") {
            C_i <- exp(-r * t) * pmax(S_i - K, 0)
        } else if (type == "put") {
            C_i <- exp(-r * t) * pmax(K - S_i, 0)
        }

    } else { # Path dependent simulation

        dt <- t / steps

        # Get logarithm of price change per step
        log_dS <- (r - q - sigma^2 / 2) * dt + sigma * sqrt(dt) * Z_i

        # Use vectorised MC method to compute logarithm of S(t) at each time step
        log_S_i <- log(S) + apply(log_dS, 1, cumsum)
        S_i <- exp(log_S_i) # Obtain the simulated stock price by taking exponential
        S_i <- S_i * exp(-q * t) # Adjust the asset price for dividends

        # Compute corresponding discounted price at current time
        if (type == "call") {
            C_i <- exp(-r * t) * pmax(S_i - K, 0)
        } else if (type == "put") {
            C_i <- exp(-r * t) * pmax(K - S_i, 0)
        }
    }
    C_i
}

#' Compute Monte Carlo Model's Standard Error
#'
#' @param C_i
#' @param C.estimate
#' @param n
#'
#' @return
#'
#' @keywords internal
mc.SE <- function(C_i, C.estimate, n) {
    s_c <- sqrt(sum(C_i - C.estimate)^2 / (n - 1))
    SE <- s_c / sqrt(n)
    SE
}

# Monte Carlo option pricing procedure =========================================

#' Pricing vanilla option using Monte Carlo method
#'
#' @param obj The predefined `option` class object.
#' @param env The predefined `option.env` class object, or a list specifying method's corresponding arguments.
#' @param n An integer specifying the number of Monte Carlo replications of asset price to take.
#' @param m An integer specifying the number of time steps the option(s)'s life will be divided into.
#' @param all A boolean specifying whether the pricing function should return only the result price (if `FALSE`) or other (maybe useful) data during computation (if `TRUE`). The default value for this argument is `FALSE`.
#'
#' @return
#'
#' @keywords internal
vanilla.mc <- function(obj, env, n, steps, all = FALSE) {

    # Check object integrity, CAN BE OMITTED
    if (any(class(obj) != c("vanilla", "option"))) stop("Invalid obj class")
    if (class(env) != "option.env") stop("Invalid env class")

    # Extract objects properties
    style <- obj$style
    type <- obj$type
    K <- obj$K
    t <- obj$t
    S <- env$S
    r <- env$r
    q <- env$q
    sigma <- env$sigma

    # Apply MC
    if (style == "european") {
        C_i <- mc.engine(type, K, t, S, r, q, sigma, n, steps)
    } else if (style == "american") {
        stop("American option pricing via MC not supported")
    }

    # Obtain the estimate of option price by taking average of C_i
    C.estimate <- mean(C_i)

    # Compute standard error of the estimate
    SE <- mc.SE(C_i, C.estimate, n)

    # Return result
    list(
        "C.estimate" = C.estimate,
        "SE" = SE,
        "C_i" = C_i
    )
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
#'
#' @keywords internal
asian.mc <- function(obj, env = NULL, S, r, t = obj$t, sigma, n, m, all = FALSE) {


}

