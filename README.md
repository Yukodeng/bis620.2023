<!-- README.md is generated from README.Rmd. Please edit that file -->

# bis620.2023

<!-- badges: start -->

[![R-CMD-check](https://github.com/Yukodeng/bis620.2023/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Yukodeng/bis620.2023/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/Yukodeng/bis620.2023/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/Yukodeng/bis620.2023/actions/workflows/test-coverage.yaml)

<!-- badges: end -->

The *bis620.2023* project integrates the Clinical Trials Query Shiny app we built in the last homework assignment into an R package, which allows easier management of the app features and testing of its functionality.

### Package features

Users can filter the dataset by specific fields, including:

1.  Title keywords search

2.  Define study date ranges

3.  Define sponsor types

4.  Histogram visualization customization

Displaying Visualization includes:

1.  Phase Histogram

2.  Condition Histogram

3.  Primary Purpose Pie Chart

4.  Study Type Histogram

5.  Intervention Type Pie Chart

6.  Intervention Histogran Specified by Intervention Type

## Installation

You can install the development version of bis620.2023 from [GitHub](https://github.com/) with:

``` r
install.packages("devtools")
install_github("Yukodeng/bis620.2023")

# Or alternatively:
devtools::install_github("Yukodeng/bis620.2023")
```

***Important note:** This package assumes there is a `ctrialsgov.duckdb` file under the `/data-raw` folder. Due to file size restriction of GitHub, this file is removed from the package directory. To use the shiny app function of this package, make sure to manually add this file to the designated location.*

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(bis620.2023)
accel |> 
  head(100) |> 
  plot_accel()
```

<img src="man/figures/README-example-1.png" width="100%"/>

To launch the Clinical Trials Query application, use `run_application()`. Query the clinical trials data by typing in or selecting values in the search fields:

``` r
library(bis620.2023)
run_application()
```

<img src="man/figures/condition_hist.png" width="100%"/>

<img src="man/figures/inter_pie.png" width="100%"/>
