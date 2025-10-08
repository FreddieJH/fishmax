
# fishmax

[![](https://img.shields.io/badge/devel%20version-0.0.0.9000-green.svg)](https://github.com/FreddieJH/fishmax)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://cran.r-project.org/web/licenses/MIT)

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

You can install the development version of fishmax with:

``` r
# install.packages("pak")
pak::pak("FreddieJH/fishmax")
```

## Example

This is a basic example which shows you how to fit an EVT and EFS model
to a set of maxima values. Length maxima can either be in the from of a
vector, or a list of vectors (multiple maxima per sample)/

``` r
library(fishmax)

# five example sample maxima (e.g., max from five fishing competitions)
maxima_vector <- c(40, 41, 35, 42, 31) #cm

# the same five example samples, but where more information is known for each sample
maxima_list <- list(c(40, 39), 41, c(33, 34, 35), c(42, 40, 39), 31) #cm
```

### Model fitting

First step is to fit the maxima models.

``` r
# By default, when fitting to a vector of maxima (only maximum known per sample), it will fit the EVT (GEV), EVT (Gumbel), and EFS models.
fit_single <- fit_max_model(maxima_vector)

# By default, when fitting to a list of maxima (largest m known per sample), it will fit all models: EVT (GEV), EVT (Gumbel), EFS, and EFSMM models.
# Note that when fiting the models that can only take the maxima (EVT-GEV, EVT-Gumbel, and EFS), the models will only use the maximum frpm each sample
fit_mult <- fit_max_model(maxima_list)
```

### Get $L_{max}$ estimates

You can then obtain the estimates of $L_{max}$ from the models, chosing
the credible interval of choice (here we use 80% credible intervals),
and the 20-sample maxima. Here, we set $k$ to 20 (default value), to
estimate $L_{max}$ if we had 20 samples, this is an arbitrary number but
we recommend this value for consistency. Note that this is **not** the
number of sample maxima used to fit the model.

``` r
# estimate the 20-sample LMAX , showing 80% credible intervals
get_max(fit = fit_single, ci = 0.8, k = 20)
```

### Visualise the $L_{max}$ estimates

``` r
# visualise the PDF of the maximum for each sample
plot_pdf(fit_single)
```

### Check the model fit with traceplots

``` r
# check to make sure there is convergence of the model parameters
plot_traceplot(fit_single)
```
