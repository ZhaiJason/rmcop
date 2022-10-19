#' Present Value of A Plain Vanilla Bond
#'
#' @description `pv` computes the present value of a plain vanilla bond given its future value and assumes a fixed interest rate.
#'
#' @param fv a number representing the future value of an asset at a specific point in time in the future.
#' @param r a number representing the fixed annual interest rate.
#' @param n a number representing the number of years reaching the specified future value `fv`.
#' @param m an optional number specifying the number of interest payments within a year.
#'
#' @return If `m` is not specified, the present value is computed via continuous compounding using natural logarithm.
#' @return If `m` is specified as a number, the present value is computed via discrete compounding.
#' @export
#'
#' @examples
#' pv(fv = 105, r = 0.05, m = 1) # = 1
pv <- function(fv, r, m, n = 1) {

    if(!is.numeric(fv)) stop("fv must be a number")
    if(!is.numeric(r)) stop("r must be a number")
    if(!is.numeric(m)) stop("m must be a number")
    if(!is.numeric(n)) stop("n must be a number")

    fv * ifelse(missing(m), exp(-r * n), (1 + r / m)^(-n * m))

}
