<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

<a href="https://github.com/GMACS-project/gmr/releases" class="gmr-devel"><img src="https://img.shields.io/github/release/GMACS-project/gmr" alt="GitHub release (latest by date)" /></a>
<a href="https://github.com/GMACS-project/gmr/actions" class="gmr-devel"><img src="https://github.com/GMACS-project/gmr/workflows/R-CMD-check/badge.svg" alt="R-CMD-check" /></a>
<!-- badges: end -->

**An R package to work with the Generalized Model for Assessing
Crustacean Stocks (GMACS)**

`gmr` is an R package providing tools to work with
*[GMACS](https://github.com/GMACS-project/GMACS_Assessment_code/tree/main/GMACS_versions/Latest_Version)*,
a generalized size-structured assessment model for Crustaceans.
Specifically, it includes an extensive library of functions developed to
build and run GMACS for one or several crustacean stock(s) and to plot
outputs of the model.

## Installation

To install the `gmr` package, the
[`devtools`](https://cran.r-project.org/web/packages/devtools/index.html)
and [`gdata`](https://cran.r-project.org/web/packages/gdata/index.html)
packages are required.

``` r
    if(!require("devtools"))
        install.packages("devtools")
    if(!require("devtools"))
        install.packages("gdata")

    # Install development version from Github
    .DirSrc <- "GMACS-project/gmr"
    devtools::install_github(.DirSrc)
```

Now you can load the package and inspect the functions:

``` r
    # Load the gmr package
    library(gmr)
    # Explore function
    plot_growth
```

Using the functions requires output from the General Model for Assessing
Crustacean Stocks (GMACS) which is described in the
[GMACS](https://github.com/GMACS-project/GMACS_Assessment_code/tree/main/GMACS_versions/Latest_Version)
repo.
