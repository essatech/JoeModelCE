
# JoeModelCE <img src="man/figures/JoeModelCE_small.png" align="right" style="max-width: 120px;"/>

<!-- badges: start -->
<!-- badges: end -->

The Joe Model Cumulative Effects package (JoeModelCE) is a collection of functions to support the application of the ‘Alberta Environmental Parks Cumulative Effects Assessment Joe Model’ (add citation) coupled with a flexible population modelling framework. This package is accompanied by an interactive web-based R Shiny dashboard (add link). The intent of the package.

#### Package Contributors:
The JoeModelCE package is being developed as part of a larger initiative between groups [A], [B] & [C]. 
Contributors include:
-   Jordan Rosenfled (TODO: add description) ...
-   Eva Enders (TODO: add description) ...
-   [Andrew Paul](https://github.com/andrewpaul68) (TODO: add description) ...
-   [Kyle Wilson](https://github.com/klwilson23) population model development.
-   Isuru Dharmasena core Shiny application development.
-   [Matthew Bayly](https://github.com/mattjbayly), Marc Porter and Alejandra Urcelay from [ESSA Technologies Ltd](https://essa.com/). Support R package and Shiny App development.


## Features
-   Run custom implementations of the Joe Model on non-standard data formats.
-   Batch-run the integrated Joe Model/Population model across large datasets.
-   Run sensitivity tests.
-   Explore model extensions.


## Installation

The easiest way to install the `JoeModelCE` package is from within the [RStudio IDE](https://www.rstudio.com/products/rstudio/download/) using `remotes::install_github()`. At this time the package has not been published to CRAN so the default `install.packages()` will not work. Instead use remotes (or devtools):
``` r
# You may need to install remotes
library(remotes)
remotes::install_github("mattjbayly/JoeModelCE")
```

## Usage
There are several vignettes available that provide detailed guidance for `JoeModelCE` usage and common workflows:
-   [placeholder for links to vignette].

## Code of Conduct

Please note that the `JoeModelCE` package is released with a [Contributor Code of Conduct](https://pkgs.rstudio.com/rmarkdown/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

