
<!-- README.md is generated from README.Rmd. Please edit that file -->

# repvisforODK

<!-- badges: start -->
<!-- badges: end -->

**This package is currently under active development**

The idea of **repvisforODK** was to develop a quick way to visualize data which is stored on ODK Central by creating **custom reports**. To do so, the package leverages on a variety of other external R packages. Of paramount importance are [ruODK](https://docs.ropensci.org/ruODK/reference/ruODK-package.html) for the interaction with the ODK Central API and both [ggplot2](https://ggplot2.tidyverse.org/index.html) & [plotly](https://plotly.com/r/) for plotting.\
\
repvisforODK was developed to be accessible not only for people with but also for people without coding skills. Thus, repvisforODK can be used in **two ways**:

1. **Code solution**: repvisforODK contains several functions that create plots for different question types. These functions can be used individually which enables the user to use them in whatever way fits best for their use case -  visualize data on the fly, integrate in already existing code, create a new personalized report etc.
2. **No Code solution**: All functions of repvisforODK are united in an integrated shiny app. The app provides an interface where the user can connect to ODK Central, select the visualizations they desire, set the required parameters and ultimately generate an html report. More details to come below...

## Installation

The development version from [GitHub](https://github.com/) can be installed using:

``` r
require("devtools")
devtools::install_github("lucidviews/repvisforODK")
```

At the moment you **cannot install** repvisforODK from
[CRAN](https://CRAN.R-project.org) because it has not been published yet.
When published installing will be possible through:

``` r
install.packages("repvisforODK")
```

## Example

COMING SOON

``` r
library(repvisforODK)
## basic example code
```
