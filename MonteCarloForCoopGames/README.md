
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MonteCarloForCoopGames

<!-- badges: start -->
<!-- badges: end -->

The goal of MonteCarloForCoopGames is to …

## Installation

You can install the development version of MonteCarloForCoopGames like
so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(MonteCarloForCoopGames)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

## Getting started

Install devtools

``` r
package.install ("devtools")
```

## While working

Run the following command in each new R session to load the devtools

``` r
library(devtools)
```

Run the following command periodically to check the state of the package
Address any issues as soon as possible

``` r
check()
```

Do not run the code directly. Use the following command to load the code

``` r
load_all()
```

Generate the documentation files (man/\*\*.Rd) from the special comments

``` r
document()
```

Install the package

``` r
install()
```

## Testing

Create new or open test

``` r
use_test("FUNCTION_NAME_TO_TEST")
```

Run tests

``` r
test()
```

Build the readme file

``` r
build_readme()
```
