
# fishmax

<!-- badges: start -->

<!-- badges: end -->

The goal of fishmax provides a robust method to estimate the maximum
body length of fishes, with uncertanty. The packages uses two
approaches, the first is from Extreme Value Theory (EVT), which shows
that the maxima of a set of samples follows a specific distribution -
the Generalised Extreme Value (GEV) distribution. The second approach
uses knowledge on the underlying body size distribution to estimate the
likely parameters of the underlying distribution that would give rise to
the observed sample maxima. The two approaches are implemented using a
Bayesian Framework.

## Installation

You can install the development version of fishmax from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("FreddieJH/fishmax")
```

## Example

This is a basic example which shows you how to fit an EVT model to a set
of maxima values:

``` r
library(fishmax)

# five example sample maxima (e.g., max from five fishing competitions)
length_maxima <- c(40, 41, 35, 42, 31) #cm

evt_fit <- fit_mod(length_maxima, model_type = "evt")
efs_fit <- fit_mod(length_maxima, model_type = "efs")
```

### Get $L_{nax}$ estimates

``` r
# estimate LMAX based on 20 samples, showing 80% credible intervals
est_max(evt_fit, ci = 0.8, k = 20)
est_max(efs_fit, ci = 0.8, k = 20)
```

### Visualise the $L_{nax}$ estimates

``` r
plot_model_comparison(
  evt_fit = evt_fit,
  efs_fit = efs_fit,
  data_vector = length_maxima
)
```

### Check the model fit with traceplots

``` r
traceplot(evt_fit)
traceplot(efs_fit)
```
