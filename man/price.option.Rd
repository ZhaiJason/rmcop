% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Price.R
\name{price.option}
\alias{price.option}
\title{Pricing \code{option} Class Object}
\usage{
price.option(obj, env, method = env$method, n = env$n, ..., all = FALSE)
}
\arguments{
\item{obj}{The specified \code{"option"} class object which encapsulate some properties of an option of interest.}

\item{env}{The predefined \code{option.env} object, which contains specified arguments}

\item{method}{A string specifying the method used to price the option. Supported values are \code{"mc"} (Monte Carlo Method), \code{"bs"} (for Black-Scholes formula), \code{"binomial"} (Binomial Lattice Tree), and \code{"trinomial"} (Trinomial Lattice Tree).}

\item{n}{A number speciying the number of simulations to make (for \code{method = "mc"}), or the number of time steps the life of the option will be broken into (for \code{method = "binomial"} and \code{method = "trinomial"}).}

\item{...}{Arguments required by corresponding specified option pricing methods.
\itemize{
\item \code{steps}:
\item \code{u}:
\item \code{d}: Used when \code{method = "binomial"}
}}

\item{all}{A boolean specifying whether the pricing function should return only the result price (if \code{FALSE}) or other (maybe useful) data during computation (if \code{TRUE}). The default value for this argument is \code{FALSE}.}
}
\value{
The result estimate/solution of the option price.
}
\description{
The all-in-one pricing function for \code{rmcop}. User should predefine an \code{"option"} class object and an \code{"env"} class object as inputs of \code{price.option}. According to the pricing method selected, the user shall provide additional inputs as instructions for the pricing function. Depending
}
\examples{
# To use price.option, we first define the "option" and "env" objects
op <- option("european", "vanilla", "call", 20, 0.75)
op.env <- option.env(S = 20, r = 0.03, q = 0.01, sigma = 0.05)

# The below codes
# Monte Carlo
price.option(op, op.env, n = 100, steps = 10, all = FALSE, plot = FALSE)

# Black-Scholes
price.option(op, op.env, method = "bs", all = FALSE)

# Binomial Lattice Tree
price.option(op, op.env, n = 10, u = 1.2, d = 0.8, method = "binomial", all = FALSE)
}
