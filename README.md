
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
## ℹ Loading metadata database✔ Loading metadata database ... done
##  
## → Will update 1 package.
## → Will download 1 package with unknown size.
## + fishEVA 0.0.0.9000 → 0.0.0.9000 [bld][cmp][dl] (GitHub: ac83420)
## ℹ Getting 1 pkg with unknown size
## ✔ Got fishEVA 0.0.0.9000 (source) (3.36 MB)
## ℹ Packaging fishEVA 0.0.0.9000
## ✔ Packaged fishEVA 0.0.0.9000 (7.8s)
## ℹ Building fishEVA 0.0.0.9000
## ✔ Built fishEVA 0.0.0.9000 (24.3s)
## ✔ Installed fishEVA 0.0.0.9000 (github::FreddieJH/fishEVA@ac83420) (660ms)
## ✔ 1 pkg + 44 deps: kept 32, upd 1, dld 1 (NA B) [58.4s]
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
