# Check pricing function input argument integrity ==============================

#' Object integrity check for price.option functions
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
#'
#' @keywords internal
mc.plot <- function(S_i, ...) {
    if (prod(dim(S_i)) > 10000) {
        warning("Trajectory points with number greater than 10000, plotting may take time and the result may look fairly ugly")
    }
    matplot(t(S_i), type = "l",
            main = "Simulated Price Trajectories", xlab = "time", ylab = "price",
            ...)
}
