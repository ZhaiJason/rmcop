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

# Format Monte Carlo Output ====================================================

# Not yet implemented
