Konstantin Mingoulin

<!-- README.md is generated from README.Rmd. Please edit that file -->

# kmfars2

<!-- badges: start -->

<!-- badges: end -->

## The goal of *kmfars2* is to:

  - import year specific data on accidents in a specific format
  - summarize the data
  - create some basic maps

## Installation

You CANNOT install the released version of kmfars2 from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("kmfars2")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(kmfars2)
df2<-fars_summarize_years(c(2014,2015,2016))
```

## External data

The package contains external data that can be used for testing. The
data can be accessed by using the following code:

``` r
system.file("extdata", "accident_2013.csv.bz2", package = "kmfars2")
system.file("extdata", "accident_2014.csv.bz2", package = "kmfars2")
system.file("extdata", "accident_2015.csv.bz2", package = "kmfars2")
```
