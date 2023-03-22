<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![GitHub release (latest by
date)](https://img.shields.io/github/release/GMACS-project/gmr)](https://github.com/GMACS-project/gmr/releases)
[![R-CMD-check](https://github.com/GMACS-project/gmr/workflows/R-CMD-check/badge.svg)](https://github.com/GMACS-project/gmr/actions)
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


## NOAA Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105). The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States.

****************************

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" height="75" alt="NOAA Fisheries">

[U.S. Department of Commerce](https://www.commerce.gov/) | [National Oceanographic and Atmospheric Administration](https://www.noaa.gov) | [NOAA Fisheries](https://www.fisheries.noaa.gov/)
