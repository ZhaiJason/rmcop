# Engine =======================================================================

#' Engine for Monte Carlo Simulation on Asset Price Trajectory
#'
#' @param t A number specifying the maturity/expiry of the option in years.
#' @param S0 A number specifying the current price of the underlying asset.
#' @param r A number specifying the (fixed) annual interest rate.
#' @param q A number specifying the (fixed) annual dividend yield rate of the option.
#' @param sigma A number specifying the annual volatility measure.
#' @param n A number speciying the number of simulations to make (for `method = "mc"`), or the number of time steps the life of the option will be broken into (for `method = "binomial"` and `method = "trinomial"`).
#' @param steps A number specifying the number of steps each asset price trajectory will contain, used only for `method = "mc"`.
#'
#' @importFrom stats rnorm
#' @return A matrix recording the simulated price trajectories. Noticing that if dividend rate q is not zero, the resulted asset price trajectories are ex-dividend. When a stock goes ex-dividend, its price typically drops by the amount of the dividend. This is because once the stock goes ex-dividend, new buyers of the stock are no longer entitled to receive the dividend payment.
#'
#' @keywords internal
#' @examples
#' # Example simulating price trajectories, noticing that if dividend rate q is not zero,
#' # the resulted asset price trajectories are ex-dividend.
#' with_dividend <- rmcop:::mc.engine(t = 2, S0 = 20, r = 0.5, q = 0.1, sigma = 0.1,
#' n = 100, steps = 30)
#' no_dividend <- rmcop:::mc.engine(t = 2, S0 = 20, r = 0.5, q = 0, sigma = 0.1,
#' n = 100, steps = 30)
#'
#' # Plot trajectories
#' rmcop:::mc.plot(with_dividend)
#' rmcop:::mc.plot(no_dividend)
mc.engine <- function(t, S0, r, q, sigma, n, steps) {

    # Calculate delta_t, length of each step in time
    dt <- t / steps

    # Generate n random samples from N(0,1)
    Z <- matrix(rnorm(n * steps), ncol = n)

    # Get logarithm of price change per step
    increment <- (r - q - sigma^2 / 2) * dt + sigma * sqrt(dt) * Z

    # Use vectorised MC method to compute logarithm of S(t) at each time step
    logS <- log(S0) + apply(increment, 2, cumsum)
    S <- exp(logS) # Obtain the simulated stock price by taking exponential
    S <- S * exp(-q * t) # Adjust the asset price for dividends
    S <- rbind(S0, S) # Add column of current price
    S
}

#' Compute Monte Carlo Model's Standard Error
#'
#' @param C_i A matrix recording the resulted option pay-offs from Monte Carlo simulations.
#' @param C.mean A number recording the resulted estimate of the price of the option.
#' @param n A number speciying the number of simulations to make (for `method = "mc"`), or the number of time steps the life of the option will be broken into (for `method = "binomial"` and `method = "trinomial"`).
#'
#' @return A number recording the standard error of the corresponding Monte Carlo estimate of the option price.
#'
#' @keywords internal
mc.SE <- function(C_i, C.mean, n) {
    s_c <- sqrt(sum(C_i - C.mean)^2 / (n - 1))
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
    S0 <- env$S
    r <- env$r
    q <- env$q
    sigma <- env$sigma

    # Apply MC
    if (style == "european") {
        S <- mc.engine(t, S0, r, q, sigma, n, steps)
    } else if (style == "american") {
        stop("American option pricing via MC not supported")
    }

    # Compute corresponding discounted price at current time
    # Vanilla option only consider option price at maturity, so only take the last column of the price matrix S_i
    if (type == "call") {
        C <- exp(-r * t) * pmax(S[steps + 1, ] - K, 0)
    } else if (type == "put") {
        C <- exp(-r * t) * pmax(K - S[steps + 1, ], 0)
    }

    # Obtain the estimate of option price by taking average of C
    C.mean <- mean(C, ...)

    # Compute standard error of the estimate
    SE <- mc.SE(C, C.mean, n)

    # Plot function
    if (plot) {mc.plot(S, ...)}

    # Return result
    if (all) {
        # Return actual object environment used
        env <- option.env(method = "mc", S = S, r = r, q = q, sigma = sigma, n = n, steps = steps)
        list(
            "env" = env,
            "price" = C.mean,
            "SE" = SE,
            "S" = S,
            "C" = C
        )
    } else {
        C.mean
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
    S0 <- env$S
    r <- env$r
    q <- env$q
    sigma <- env$sigma

    # Apply MC
    if (style == "european") {
        S <- mc.engine(t, S0, r, q, sigma, n, steps)
    } else if (style == "american") {
        stop("American option pricing via MC not supported")
    }

    # Compute the mean of each replication's price movement, as required to compute Asian option pay-off
    S.mean <- apply(S, 2, mean)

    # Compute corresponding discounted price at current time
    if (is.avg_price) { # Compute payoff for average price Asian option
        if (type == "call") {
            C <- exp(-r * t) * pmax(S.mean - K, 0)
        } else if (type == "put") {
            C <- exp(-r * t) * pmax(K - S.mean, 0)
        }
    } else { # Compute payoff for average strike Asian option
        if (type == "call") {
            C <- exp(-r * t) * pmax(S[steps + 1, ] - S.mean, 0)
        } else if (type == "put") {
            C <- exp(-r * t) * pmax(S.mean - S[steps + 1, ], 0)
        }
    }

    # Obtain the estimate of option price by taking average of C
    C.mean <- mean(C)

    # Compute standard error of the estimate
    SE <- mc.SE(C, C.mean, n)

    # Plot function
    if (plot) {mc.plot(S, ...)}

    # Return result
    if (all) {
        # Return actual object environment used
        env <- option.env(method = "mc", S = S, r = r, q = q, sigma = sigma, n = n, steps = steps)
        list(
            "env" = env,
            "price" = C.mean,
            "SE" = SE,
            "S" = S,
            "C" = C
        )
    } else {
        C.mean
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
    S0 <- env$S
    r <- env$r
    q <- env$q
    sigma <- env$sigma

    # Apply MC
    if (style == "european") {
        S <- mc.engine(t, S0, r, q, sigma, n, steps)
    } else if (style == "american") {
        stop("American option pricing via MC not supported")
    }

    # Check if price movements corssed the specified barrier
    S.min <- apply(S, 2, min)
    S.max <- apply(S, 2, max)
    is.cross <- (S.min < barrier & S.max > barrier)

    # Compute corresponding discounted price at current time
    if (type == "call") {
        C <- exp(-r * t) * pmax(S[steps + 1, ] - K, 0)
    } else if (type == "put") {
        C <- exp(-r * t) * pmax(K - S[steps + 1, ], 0)
    }

    # Adjust for knock-out & knock-in situations for a barrier option
    if (is.knockout) {
        C <- ifelse(is.cross, 0, C)
    } else {
        C <- ifelse(is.cross, C, 0)
    }

    # Obtain the estimate of option price by taking average of C
    C.mean <- mean(C)

    # Compute standard error of the estimate
    SE <- mc.SE(C, C.mean, n)

    # Plot function
    if (plot) {mc.plot(S, ...)}

    # Return result
    if (all) {
        # Return actual object environment used
        env <- option.env(method = "mc", S = S, r = r, q = q, sigma = sigma, n = n, steps = steps)
        list(
            "env" = env,
            "price" = C.mean,
            "SE" = SE,
            "S" = S,
            "C" = C
        )
    } else {
        C.mean
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
    S0 <- env$S
    r <- env$r
    q <- env$q
    sigma <- env$sigma

    # Apply MC
    if (style == "european") {
        S <- mc.engine(t, S0, r, q, sigma, n, steps)
    } else if (style == "american") {
        stop("American option pricing via MC not supported")
    }

    # Compute corresponding discounted price at current time
    # Compute uniform payout for each MC replication of a binary option
    C <- matrix(exp(-r * t) * payout, nrow = n, ncol = 1)
    if (type == "call") {
        C <- ifelse(S[steps + 1, ] > K, C, 0)
    } else if (type == "put") {
        C <- ifelse(S[steps + 1, ] < K, C, 0)
    }

    # Obtain the estimate of option price by taking average of C
    C.mean <- mean(C)

    # Compute standard error of the estimate
    SE <- mc.SE(C, C.mean, n)

    # Plot function
    if (plot) {mc.plot(S, ...)}

    # Return result
    if (all) {
        # Return actual object environment used
        env <- option.env(method = "mc", S = S, r = r, q = q, sigma = sigma, n = n, steps = steps)
        list(
            "env" = env,
            "price" = C.mean,
            "SE" = SE,
            "S" = S,
            "C" = C
        )
    } else {
        C.mean
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
    S0 <- env$S
    r <- env$r
    q <- env$q
    sigma <- env$sigma

    # Apply MC
    if (style == "european") {
        S <- mc.engine(t, S0, r, q, sigma, n, steps)
    } else if (style == "american") {
        stop("American option pricing via MC not supported")
    }

    # Check if price movements crossed the specified barrier
    S.min <- apply(S, 2, min)
    S.max <- apply(S, 2, max)

    # Compute corresponding discounted price at current time
    if (type == "call") {
        if (is.fixed) {
            C <- exp(-r * t) * pmax(S.max - K, 0)
        } else {
            C <- exp(-r * t) * (S[steps + 1, ] - S.min)
        }
    } else if (type == "put") {
        if (is.fixed) {
            C <- exp(-r * t) * pmax(K - S.min, 0)
        } else {
            C <- exp(-r * t) * (S.max - S[steps + 1, ])
        }
    }

    # Obtain the estimate of option price by taking average of C
    C.mean <- mean(C)

    # Compute standard error of the estimate
    SE <- mc.SE(C, C.mean, n)

    # Plot function
    if (plot) {mc.plot(S, ...)}

    # Return result
    if (all) {
        # Return actual object environment used
        obj <- option(style = style, option = "lookback", type = type, K = K, t = t, is.fixed = is.fixed)
        env <- option.env(method = "mc", S = S, r = r, q = q, sigma = sigma, n = n, steps = steps)
        list(
            "env" = env,
            "C.price" = C.mean,
            "SE" = SE,
            "S" = S,
            "C" = C
        )
    } else {
        C.mean
    }
}
