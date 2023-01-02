# Top Level Pricing Functions for Options ======================================

#' Pricing `option` Class Object
#'
#' @param obj
#' @param method
#'
#' @return
#' @export
#'
#' @examples
price.option <- function(obj, method = "mc", ...) {
    eval(parse(text = paste0( "price.option.", method, "(obj, ...)" )))
}

#' Option Pricing via Monte Carlo Method
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
price.option.mc <- function(obj, ...) {
    if (class(obj)[-2] == "vanilla") {
        res <- vanilla.mc(obj, ...)
    } else if (class(obj)[-2] == "exotic") {
        res <- exotic.mc(obj, ...)
    }
    res
}

#' Option Pricing via Binomial Model
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
price.option.binomial <- function(obj, ...) {
    if (class(obj)[-2] == "vanilla") {
        res <- vanilla.binomial(obj, ...)
    } else if (class(obj)[-2] == "exotic") {
        res <- exotic.binomial(obj, ...)
    }
    res
}

price.option.trinomial <- function(obj, ...) {
    if (class(obj)[-2] == "vanilla") {
        res <- NA
    } else if (class(obj)[-2] == "exotic") {
        res <- NA
    }
    res
}
