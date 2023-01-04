# Check pricing function input argument integrity ==============================

#' Title
#'
#' @param obj
#' @param env
#'
#' @return
#' @export
#'
#' @examples
check.args <- function(obj, env, option) {
    if (any(class(obj) != c(option, "option"))) stop("Invalid obj class")
    if (class(env) != "option.env") stop("Invalid env class")
}

# Format Monte Carlo Output ====================================================

# print.res <- function(res) {
#     print("test print res")
# }
