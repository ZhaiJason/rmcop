# Creating new option object ===================================================

## Option ======================================================================
#' Create new `option` class object
#'
#' @param style The style of the option.
#' @param ... Arguments required are determined by specified option style.
#'
#' @return An object of class `option`.
#' @export
#'
#' @examples
#' # Create an European call option with strike price 10 and mature in 2 years
#' option("european", is.vanilla = TRUE, type = "call", K = 10, t = 2)
#'
#' # Create an Binary option with payout 10 and mature
#' option("european", option = "binary")
option <- function(style, option = "vanilla", ...) {

    # List of supported styles: american, european, asian

    obj <- list(
        "style" = style
    )

    # class(obj) <- "option"

    # Distribute to corresponding initialisation function according to specified arguments
    if (option == "vanilla") {
        obj <- option.vanilla(obj, ...)
    } else {
        obj <- option.exotic(obj, option, ...)
    }

    obj
}

## Vanilla =====================================================================
#' Initialise vanilla option
#'
#' @param obj
#' @param type
#' @param K
#' @param t
#'
#' @return
#'
#' @keywords internal
option.vanilla <- function(obj, type, K, t = NA) {
    obj <- c(
        obj,
        "type" = type,
        "K" = K,
        "t" = t
    )
    class(obj) <- c("vanilla", "option")
    obj
}

## Exotic ======================================================================
#' Initialise exotic option
#'
#' @param obj
#' @param option
#' @param ...
#'
#' @return
#'
#' @keywords internal
option.exotic <- function(obj, option, ...) {
    obj <- eval(parse(text = paste0( "option.exotic.", tolower(option), "(obj, ...)" ))) # Direct to corresponding initialisation function
    class(obj) <- append(class(obj), "exotic", after = 1)
    obj
}

#' Initialise Asian option
#'
#' @param obj
#' @param type
#' @param K
#' @param t
#'
#' @return
#'
#' @keywords internal
option.exotic.asian <- function(obj, type, K, t = NA) {
    obj <- c(
        obj,
        "type" = type,
        "K" = K,
        "t" = t
    )
    class(obj) <- c("asian", "option")
    obj
}

#' Initialise barrier option
#'
#' @param obj
#' @param barrier
#' @param is.knockout
#'
#' @return
#'
#' @keywords internal
option.exotic.barrier <- function(obj, barrier, is.knockout = TRUE, t = NA) {
    obj <- c(
        obj,
        "barrier" = barrier,
        "is.knockout" = is.knockout,
        "t" = t
    )
    class(obj) <- c("barrier", "option")
    obj
}

#' Initialise binary option
#'
#' @param obj
#' @param payout
#' @param t
#'
#' @return
#'
#' @keywords internal
option.exotic.binary <- function(obj, payout, t = NA) {
    obj <- c(
        obj,
        "payout" = payout,
        "t" = t
    )
    class(obj) <- c("binary", "option")
    obj
}

#' Initialise lookback option
#'
#' @param obj
#' @param type
#' @param K
#' @param t
#'
#' @return
#'
#' @keywords internal
option.exotic.lookback <- function(obj, type, K, t = NA) {
    obj <- c(
        obj,
        "type" = type,
        "K" = K,
        "t" = t
    )
    class(obj) <- c("lookback", "option")
    obj
}
