# Check whether option object is well-defined ==================================

#' Check option object integrity
#'
#' @param obj The defined option class object.
#'
#' @keywords internal
check.option <- function(obj) {
    NA
}

# Check whether env object is well-defined =====================================

#' Check env object integrity
#'
#' @param env The defined env class object
#'
#' @keywords internal
check.env <- function(env) {
    NA
}

# Check pricing function input argument integrity ==============================

#' Check argument inputs integrity for price.option function
#'
#' @param obj The specified `"option"` class object which encapsulate some properties of an option of interest.
#' @param env The specified `"env"` class object which encapsulate some market variables required by corresponding pricing methods.
#'
#' @keywords internal
check.args <- function(obj, env, option) {
    if (!(inherits(obj, "option"))) stop("Invalid obj class")
    if (!(inherits(env, "env"))) stop("Invalid env class")
}

# Monte Carlo Price Trajectory Plot ============================================

#' Monte Carlo Price Trajectory Plot
#'
#' @param S_i Price trajectory matrix.
#' @param ... Other parameters used for plotting
#'
#' @importFrom graphics matplot
#' @importFrom grDevices rgb
#'
#' @keywords internal
mc.plot <- function(S, threshold, ...) {
    alpha <- 1.5^-log(ncol(S))
    matplot(S, type = "l", col = rgb(0, 0, 0, alpha), lty = 1,
            main = "Simulated Price Trajectories", xlab = "time", ylab = "price",
            ...)
}
