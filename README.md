
# fishEVA

<!-- badges: start -->

<!-- badges: end -->

The goal of fishEVA is to use Extreme Value Analysis (EVA) to estimate
the maximum body length of fishes. The packages uses two approaches, the
first is from Extreme Value Theory (EVT), which shows that the maxima of
a set of samples follows a specific distribution - the Generalised
Extreme Value (GEV) distribution. The second approach uses knowledge on
the underlying body size distribution to estimate the likely parameters
of the underlying distribution that would give rise to the observed
sample maxima. The two approaches are implemented using a Bayesian
Framework.

## Installation

You can install the development version of fishEVA from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("FreddieJH/fishEVA")
#> ✔ Updated metadata database: 5.39 MB in 4 files.
#> ℹ Updating metadata database✔ Updating metadata database ... done
#>  
#> ℹ No downloads are needed
#> ✔ 1 pkg + 49 deps: kept 37 [30.9s]
```

## Example

This is a basic example which shows you how to fit an EVT model to a set
of maxima values:

``` r
library(fishEVA)
## basic example code
length_maxima <- c(40, 41, 35, 42, 31) #cm

# model_fit <- fit_mod(length_maxima, model_type = "evt")
```
