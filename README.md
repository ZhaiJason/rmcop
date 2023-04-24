# rmcop

## R Monte Carlo Option Pricing

`rmcop` is an R package that contains functions for pricing financial options. It supports Monte Carlo option pricing on several types of European options, including *vanilla option*, *Asian option*, **barrier option*, *binary option*, and *lookback option*. It also supports pricing via Black-Scholes formula and Binomial tree method for vanilla European and American options.

There are three functions within `rmcop`:

- `option` allows users to define an S3 `“option”` class object in R, which encapsulates characteristics of an financial option of interest.
- `option.env` allows users to define an S3 `“env”` class object in R, which encapsulates the characteristics of the market environment that the pricing will perform on.
- `price.option` perform options pricing via methods that users may specify, takes an `“option”` class object and an `“env”` class object as input, together with additional arguments required by the specific method.

Check package manual for arguments for each function. Practical illustrations are presented in the Example section below.

---

## Installation

The development version can be installed using the following code. You will also need to install package `remotes` ahead before running the line below.

```r
remotes::install_github("ZhaiJason/rmcop")
```

Alternatively, it can be installed using package `devtools` using the line:

```r
devtools::install_github("ZhaiJason/rmcop")
```

---

## Examples

Pricing an financial option using `rmcop` typically follows the procedure shown below:

1. Call `option` function to define option(s) of interest.
2. Call `option.env` function to define a suitable market environment.
3. Call `price.option` and pass in the defined objects from step 1&2 as arguments, together with other necessary arguments, after which an estimate of the option price will be returned.

Prior following any of the examples below, users shall have `rmcop` loaded using the `library` command.

```r
library(rmcop)
```

---

### Defining Objects

Following steps 1&2, we first define

```r
# Define the "option" and "env" objects
op <- option(
    style = "european", option = "vanilla", type = "call",
    K = 20, # Strike price
    t = 0.75 # Maturity
)
op.env <- option.env(
    S = 20, # Current price
    r = 0.03, # Fixed annual interest rate
    q = 0.01, # Fixed annual dividend yield rate
    sigma = 0.05 # Fixed annual volatility measure
)
```

---

### Monte Carlo Method

Monte Carlo method is the default pricing method for `price.option` function, which can be also specified using argument `method = "mc"`. Using the objects defined in the section above, one can 

```r
# Monte Carlo
price.option(obj = op, env = op.env,
             n = 100, # Number of price trajectories to simulate
             steps = 10, # Number of steps for each trajectory to simulate, for path-independent options it's reasonable to set to 1
             all = FALSE,
             plot = FALSE)
```

### Black-Scholes Method

To use Black-Scholes formula instead of Monte Carlo method, simply specify in `price.option` function using argument `method = "bs"`.

```r
# Black-Scholes
price.option(obj = op, env = op.env, method = "bs", all = FALSE)
```

### Binomial Tree Method

To use Binomial Lattice Tree pricing method, use argument `method = "binomial"`.

```r
# Binomial Lattice Tree
price.option(op, op.env, method = "binomial",
             n = 10, # Number of binomial-tree steps
             u = 1.2, # Ratio of an upward jump
             d = 0.8, # Ratio of an downward jump
             all = FALSE)
```

---

## Support

This package is for the UCL STAT0035 Project topic “R Package Development”. The development of this pacakge is supported by UCL Department of Statistical Science faculties.
