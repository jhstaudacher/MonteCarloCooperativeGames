
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Monte Carlo For Coop Games

Monte Carlo For Coop Games is a R package for simulating arbitrary
cooperative games and approximating the Shapley value of them.

## Getting started with development

Install the devtools and other neccessary packages

``` r
install.packages(c("devtools", "roxygen2", "testthat", "knitr", "styler"))
```

R Studio: Tools/Install Packages or Build Pane: Install

### Rtools

Install Rtools (required to build packages with C++ code) Windows:
Download it from [here](https://cran.r-project.org/bin/windows/Rtools/)
and run the installer. MacOs: Run xcode-select –install Ubuntu/Debian:
sudo apt install r-base-dev

Verify installation with:

``` r
devtools::dev_sitrep()
```

## Development workflow

- Run check often and fix any errors, warnings and notes
- Be extremly careful with top level code in a package. It is ONLY run
  when the package is build, NEVER again. Mainly use them for static
  variables. Any R code outside of a function is suspicious and should
  be carefully reviewed.
- Using library, source and require in a package is forbidden
- Subfolders in the R directory are not supported.

### General workflow

![General dev
workflow](https://r-pkgs.org/diagrams/workflow.png "General development workflow")

Run the following command in each new R session to load the devtools.

``` r
library(devtools)
```

Alternativly a .Rprofile file can be used to automaticly load the
library. Create the file manually at \~/.RProfile or use use_devtools()
to create the file.

``` r
if (interactive()) {
  suppressMessages(require(devtools))
}
```

### Check

Run the following command periodically to check the state of the package
Address any issues as soon as possible.

Console:

``` r
check()
```

R Studio: Build/Check Package or Build Pane: Check

### Loading the code

Running code directly can cause problems, because R Studio just sends
the code to the console and defines it in the global environment. This
is not well suited for package development, because as the package gets
more and more complicated, manually defining all required functions
becomes unsustainable. Instead, use the following command to load the
code.:

Use this command to reload package Console:

``` r
load_all()
```

R Studio: Build/Load All

If you want to install the package like normal use the following
command. For development it is better to use load_all() for faster
iteration.

Console:

``` r
install()
```

R Studio: Build/Install Package

## Testing

Unit testing is handled by [testthat](https://testthat.r-lib.org/)

Create a new test or open an existing one.

``` r
# For the current active file
use_test()
# Or explicitly defined name
use_test("FUNCTION_NAME_TO_TEST")
```

Execute all tests:

Console:

``` r
test()
```

R Studio: Build/Test Package or Build Pane: Test

## Documentation

Documentation for individual files is handled by
[roxygen2](https://cran.r-project.org/package=roxygen2)

### man

Generate the documentation files (man/\*\*.Rd) from roxygen comments.
Also regenerates the NAMESPACE file.

``` r
document()
```

Preview the documentation of any function by running:

``` r
?FUNCTION_NAME
# For the documentation of function "take" run:
?take
```

### Readme

Build the readme file

``` r
build_readme()
```

## Style

This project follows the tidyverse style guide.

Use Linter from the Console:

``` r
styler:::style_active_pkg()
```

R Studio: Menu bar/Addins/Style active file or Style active package

After using the linter, inspect the code again to avoid any unwanted
changes.