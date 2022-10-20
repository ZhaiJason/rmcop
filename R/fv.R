#' Future Value of A Plain Vanilla Bond
#'
#' @description `fv` computes the future value of a plain vanilla bond given its present value and assumes a fixed interest rate.
#'
#' @param pv a number representing the present value of an asset at a specific point in time in the future.
#' @param r a number representing the fixed annual interest rate.
#' @param n a number representing the number of years forward.
#' @param m an optional number specifying the number of interest payments within a year.
#'
#' @return If `m` is not specified, the future value is computed via continuous compounding using natural logarithm.
#' @return If `m` is specified as a number, the future value is computed via discrete compounding.
#' @export
#'
#' @examples
#' fv(pv = 100, r = 0.05, m = 1) # = 105
fv <- function(pv, r, m, n = 1) {

    if (!is.numeric(pv)) stop("pv must be a number")
    if (!is.numeric(r)) stop("r must be a number")
    if (!is.numeric(m)) stop("m must be a number")
    if (!is.numeric(n)) stop("n must be a number")

    pv * ifelse(missing(m), exp(r * n), (1 + r / m)^(n * m))

}
