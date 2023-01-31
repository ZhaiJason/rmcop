# Engine =======================================================================

#' Engine for Monte Carlo Simulation on Asset Price Trajectory
#'
#' @param type A string specifying the type of the option of interest. Possible values are either `"call"` or `"put"`, corresponding to call option (right to buy) and pull option (right to sale), respectively.
#' @param K A number specifying the strike price of the option.
#' @param t A number specifying the maturity/expiry of the option in years.
#' @param S A number specifying the current price of the underlying asset.
#' @param r A number specifying the (fixed) annual interest rate.
#' @param q A number specifying the (fixed) annual dividend yield rate of the option.
#' @param sigma A number specifying the annual volatility measure.
#' @param n A number speciying the number of simulations to make (for `method = "mc"`), or the number of time steps the life of the option will be broken into (for `method = "binomial"` and `method = "trinomial"`).
#' @param steps A number specifying the number of steps each asset price trajectory will contain, used only for `method = "mc"`.
#'
#' @importFrom stats rnorm
#' @return A matrix recording the simulated price trajectories.
#'
#' @keywords internal
mc.engine <- function(type, K, t, S, r, q, sigma, n, steps) {

    # Generate n random samples from N(0,1)
    Z_i <- matrix(rnorm(n * steps), nrow = n)

    if (steps == 1) { # Path independent simulation

        # Compute corresponding stock prices at maturity
        S_i <- S * exp((r - q - sigma^2 / 2) * t + sigma * sqrt(t) * Z_i)
        S_i <- S_i * exp(-q * t) # Adjust the asset price for dividends

    } else { # Path dependent simulation

        dt <- t / steps

        # Get logarithm of price change per step
        log_dS <- (r - q - sigma^2 / 2) * dt + sigma * sqrt(dt) * Z_i

        # Use vectorised MC method to compute logarithm of S(t) at each time step
        log_S_i <- log(S) + t(apply(log_dS, 1, cumsum))
        S_i <- exp(log_S_i) # Obtain the simulated stock price by taking exponential
        S_i <- S_i * exp(-q * t) # Adjust the asset price for dividends

    }
    S_i <- cbind(S, S_i) # Add column of current price
}

#' Compute Monte Carlo Model's Standard Error
#'
#' @param C_i A matrix recording the resulted option pay-offs from Monte Carlo simulations.
#' @param C.estimate A number recording the resulted estimate of the price of the option.
#' @param n A number speciying the number of simulations to make (for `method = "mc"`), or the number of time steps the life of the option will be broken into (for `method = "binomial"` and `method = "trinomial"`).
#'
#' @return A number recording the standard error of the corresponding Monte Carlo estimate of the option price.
#'
#' @keywords internal
mc.SE <- function(C_i, C.estimate, n) {
    s_c <- sqrt(sum(C_i - C.estimate)^2 / (n - 1))
    SE <- s_c / sqrt(n)
    SE
}

# Model ========================================================================

#' Pricing vanilla option using Monte Carlo method
#'
#' @param obj The specified `"option"` class object which encapsulate some properties of an option of interest.
#' @param env The specified `"env"` class object which encapsulate some market variables required by corresponding pricing methods.
#' @param n A number speciying the number of simulations to make (for `method = "mc"`), or the number of time steps the life of the option will be broken into (for `method = "binomial"` and `method = "trinomial"`).
#' @param steps A number specifying the number of steps each asset price trajectory will contain, used only for `method = "mc"`.
#' @param all A logical value specifying whether the pricing function should return only the result price (`all = FALSE`) or other data during computation (`all = TRUE`). The default value for this argument is `FALSE`.
#' @param plot A logical value specifying whether the simulated price trajectories should be plotted. The default value for this argument is `FALSE`.
#' @param ... Graphical arguments used for plotting.
#'
#' @keywords internal
vanilla.mc <- function(obj, env, n, steps, all = FALSE, plot = FALSE, ...) {

    # Check object integrity, CAN BE OMITTED
    check.args(obj, env, option = "vanilla")

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
        S_i <- mc.engine(type, K, t, S, r, q, sigma, n, steps)
    } else if (style == "american") {
        stop("American option pricing via MC not supported")
    }

    # Compute corresponding discounted price at current time
    # Vanilla option only consider option price at maturity, so only take the last column of the price matrix S_i
    if (type == "call") {
        C_i <- exp(-r * t) * pmax(S_i[ , steps + 1] - K, 0)
    } else if (type == "put") {
        C_i <- exp(-r * t) * pmax(K - S_i[ , steps + 1], 0)
    }

    # Obtain the estimate of option price by taking average of C_i
    C.estimate <- mean(C_i, ...)

    # Compute standard error of the estimate
    SE <- mc.SE(C_i, C.estimate, n)

    # Plot function
    if (plot) {mc.plot(S_i, ...)}

    # Return result
    if (all) {
        # Return actual object environment used
        env <- option.env(method = "mc", S = S, r = r, q = q, sigma = sigma, n = n, steps = steps)
        list(
            "env" = env,
            "C.estimate" = C.estimate,
            "SE" = SE,
            "S_i" = S_i,
            "C_i" = C_i
        )
    } else {
        C.estimate
    }
}

#' Pricing Asian option using Monte Carlo method
#'
#' @param obj The specified `"option"` class object which encapsulate some properties of an option of interest.
#' @param env The specified `"env"` class object which encapsulate some market variables required by corresponding pricing methods.
#' @param n A number speciying the number of simulations to make (for `method = "mc"`), or the number of time steps the life of the option will be broken into (for `method = "binomial"` and `method = "trinomial"`).
#' @param steps A number specifying the number of steps each asset price trajectory will contain, used only for `method = "mc"`.
#' @param all A logical value specifying whether the pricing function should return only the result price (`all = FALSE`) or other data during computation (`all = TRUE`). The default value for this argument is `FALSE`.
#' @param plot A logical value specifying whether the simulated price trajectories should be plotted. The default value for this argument is `FALSE`.
#' @param ... Graphical arguments used for plotting.
#'
#' @keywords internal
asian.mc <- function(obj, env, n, steps, all = FALSE, plot = FALSE, ...) {
    # Check object integrity, CAN BE OMITTED
    check.args(obj, env, option = "asian")

    # Extract objects properties
    style <- obj$style
    type <- obj$type
    K <- obj$K
    t <- obj$t
    is.avg_price <- obj$is.avg_price
    S <- env$S
    r <- env$r
    q <- env$q
    sigma <- env$sigma

    # Apply MC
    if (style == "european") {
        S_i <- mc.engine(type, K, t, S, r, q, sigma, n, steps)
    } else if (style == "american") {
        stop("American option pricing via MC not supported")
    }

    # Compute the mean of each replication's price movement, as required to compute Asian option pay-off
    S.mean <- apply(S_i, 1, mean)

    # Compute corresponding discounted price at current time
    if (is.avg_price) { # Compute payoff for average price Asian option
        if (type == "call") {
            C_i <- exp(-r * t) * pmax(S.mean - K, 0)
        } else if (type == "put") {
            C_i <- exp(-r * t) * pmax(K - S.mean, 0)
        }
    } else { # Compute payoff for average strike Asian option
        if (type == "call") {
            C_i <- exp(-r * t) * pmax(S_i[ , steps + 1] - S.mean, 0)
        } else if (type == "put") {
            C_i <- exp(-r * t) * pmax(S.mean - S_i[ , steps + 1], 0)
        }
    }


    # Obtain the estimate of option price by taking average of C_i
    C.estimate <- mean(C_i)

    # Compute standard error of the estimate
    SE <- mc.SE(C_i, C.estimate, n)

    # Return result
    if (all) {
        # Return actual object environment used
        env <- option.env(method = "mc", S = S, r = r, q = q, sigma = sigma, n = n, steps = steps)
        list(
            "env" = env,
            "C.estimate" = C.estimate,
            "SE" = SE,
            "S_i" = S_i,
            "C_i" = C_i
        )
    } else {
        C.estimate
    }
}

#' Pricing Barrier option using Monte Carlo method
#'
#' @param obj The specified `"option"` class object which encapsulate some properties of an option of interest.
#' @param env The specified `"env"` class object which encapsulate some market variables required by corresponding pricing methods.
#' @param n A number speciying the number of simulations to make (for `method = "mc"`), or the number of time steps the life of the option will be broken into (for `method = "binomial"` and `method = "trinomial"`).
#' @param steps A number specifying the number of steps each asset price trajectory will contain, used only for `method = "mc"`.
#' @param all A logical value specifying whether the pricing function should return only the result price (`all = FALSE`) or other data during computation (`all = TRUE`). The default value for this argument is `FALSE`.
#' @param plot A logical value specifying whether the simulated price trajectories should be plotted. The default value for this argument is `FALSE`.
#' @param ... Graphical arguments used for plotting.
#'
#' @keywords internal
barrier.mc <- function(obj, env, n, steps, all = FALSE, plot = FALSE, ...) {
    # Check object integrity, CAN BE OMITTED
    check.args(obj, env, option = "barrier")

    # Extract objects properties
    style <- obj$style
    type <- obj$type
    K <- obj$K
    t <- obj$t
    barrier <- obj$barrier
    is.knockout <- obj$is.knockout
    S <- env$S
    r <- env$r
    q <- env$q
    sigma <- env$sigma

    # Apply MC
    if (style == "european") {
        S_i <- mc.engine(type, K, t, S, r, q, sigma, n, steps)
    } else if (style == "american") {
        stop("American option pricing via MC not supported")
    }

    # Check if price movements corssed the specified barrier
    S.min <- apply(S_i, 1, min)
    S.max <- apply(S_i, 1, max)
    is.cross <- (S.min < barrier & S.max > barrier)

    # Compute corresponding discounted price at current time
    if (type == "call") {
        C_i <- exp(-r * t) * pmax(S_i[ , steps] - K, 0)
    } else if (type == "put") {
        C_i <- exp(-r * t) * pmax(K - S_i[ , steps], 0)
    }

    # Adjust for knock-out & knock-in situations for a barrier option
    if (is.knockout) {
        C_i <- ifelse(is.cross, 0, C_i)
    } else {
        C_i <- ifelse(is.cross, C_i, 0)
    }

    # Obtain the estimate of option price by taking average of C_i
    C.estimate <- mean(C_i)

    # Compute standard error of the estimate
    SE <- mc.SE(C_i, C.estimate, n)

    # Return result
    if (all) {
        # Return actual object environment used
        env <- option.env(method = "mc", S = S, r = r, q = q, sigma = sigma, n = n, steps = steps)
        list(
            "env" = env,
            "C.estimate" = C.estimate,
            "SE" = SE,
            "S_i" = S_i,
            "C_i" = C_i
        )
    } else {
        C.estimate
    }
}

#' Pricing Barrier option using Monte Carlo method
#'
#' @param obj The specified `"option"` class object which encapsulate some properties of an option of interest.
#' @param env The specified `"env"` class object which encapsulate some market variables required by corresponding pricing methods.
#' @param n A number speciying the number of simulations to make (for `method = "mc"`), or the number of time steps the life of the option will be broken into (for `method = "binomial"` and `method = "trinomial"`).
#' @param steps A number specifying the number of steps each asset price trajectory will contain, used only for `method = "mc"`.
#' @param all A logical value specifying whether the pricing function should return only the result price (`all = FALSE`) or other data during computation (`all = TRUE`). The default value for this argument is `FALSE`.
#' @param plot A logical value specifying whether the simulated price trajectories should be plotted. The default value for this argument is `FALSE`.
#' @param ... Graphical arguments used for plotting.
#'
#' @keywords internal
binary.mc <- function(obj, env, n, steps, all = FALSE, plot = FALSE, ...) {

    # Check object integrity, CAN BE OMITTED
    check.args(obj, env, option = "binary")

    # Extract objects properties
    style <- obj$style
    type <- obj$type
    K <- obj$K
    t <- obj$t
    payout <- obj$payout
    S <- env$S
    r <- env$r
    q <- env$q
    sigma <- env$sigma

    # Apply MC
    if (style == "european") {
        S_i <- mc.engine(type, K, t, S, r, q, sigma, n, steps)
    } else if (style == "american") {
        stop("American option pricing via MC not supported")
    }

    # Compute corresponding discounted price at current time
    # Compute uniform payout for each MC replication of a binary option
    C_i <- matrix(exp(-r * t) * payout, nrow = n, ncol = 1)
    if (type == "call") {
        C_i <- ifelse(S_i[, steps + 1] > K, C_i, 0)
    } else if (type == "put") {
        C_i <- ifelse(S_i[, steps + 1] < K, C_i, 0)
    }

    # Obtain the estimate of option price by taking average of C_i
    C.estimate <- mean(C_i)

    # Compute standard error of the estimate
    SE <- mc.SE(C_i, C.estimate, n)

    # Return result
    if (all) {
        # Return actual object environment used
        env <- option.env(method = "mc", S = S, r = r, q = q, sigma = sigma, n = n, steps = steps)
        list(
            "env" = env,
            "C.estimate" = C.estimate,
            "SE" = SE,
            "S_i" = S_i,
            "C_i" = C_i
        )
    } else {
        C.estimate
    }
}

#' Pricing Lookback option using Monte Carlo method
#'
#' @param obj The specified `"option"` class object which encapsulate some properties of an option of interest.
#' @param env The specified `"env"` class object which encapsulate some market variables required by corresponding pricing methods.
#' @param n A number speciying the number of simulations to make (for `method = "mc"`), or the number of time steps the life of the option will be broken into (for `method = "binomial"` and `method = "trinomial"`).
#' @param steps A number specifying the number of steps each asset price trajectory will contain, used only for `method = "mc"`.
#' @param all A logical value specifying whether the pricing function should return only the result price (`all = FALSE`) or other data during computation (`all = TRUE`). The default value for this argument is `FALSE`.
#' @param plot A logical value specifying whether the simulated price trajectories should be plotted. The default value for this argument is `FALSE`.
#' @param ... Graphical arguments used for plotting.
#'
#' @keywords internal
lookback.mc <- function(obj, env, n, steps, all = FALSE, plot = FALSE, ...) {
    # Check object integrity, CAN BE OMITTED
    check.args(obj, env, option = "lookback")

    # Extract objects properties
    style <- obj$style
    type <- obj$type
    K <- obj$K
    t <- obj$t
    is.fixed <- obj$is.fixed
    S <- env$S
    r <- env$r
    q <- env$q
    sigma <- env$sigma

    # Apply MC
    if (style == "european") {
        S_i <- mc.engine(type, K, t, S, r, q, sigma, n, steps)
    } else if (style == "american") {
        stop("American option pricing via MC not supported")
    }

    # Check if price movements corssed the specified barrier
    S.min <- apply(S_i, 1, min)
    S.max <- apply(S_i, 1, max)

    # Compute corresponding discounted price at current time
    if (type == "call") {
        if (is.fixed) {
            C_i <- exp(-r * t) * pmax(S.max - K, 0)
        } else {
            C_i <- S_i[ , steps + 1] - S.min
        }
    } else if (type == "put") {
        if (is.fixed) {
            C_i <- exp(-r * t) * pmax(K - S.min, 0)
        } else {
            C_i <- S.max - S_i[ , steps + 1]
        }
    }

    # Obtain the estimate of option price by taking average of C_i
    C.estimate <- mean(C_i)

    # Compute standard error of the estimate
    SE <- mc.SE(C_i, C.estimate, n)

    # Return result
    if (all) {
        # Return actual object environment used
        obj <- option(style = style, option = "lookback", type = type, K = K, t = t, is.fixed = is.fixed)
        env <- option.env(method = "mc", S = S, r = r, q = q, sigma = sigma, n = n, steps = steps)
        list(
            "env" = env,
            "C.estimate" = C.estimate,
            "SE" = SE,
            "S_i" = S_i,
            "C_i" = C_i
        )
    } else {
        C.estimate
    }
}
