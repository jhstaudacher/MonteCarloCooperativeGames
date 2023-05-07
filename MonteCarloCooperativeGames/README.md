
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Monte Carlo For Coop Games

Monte Carlo For Coop Games is a R package for simulating arbitrary
cooperative games and approximating the Shapley value of them.

## Getting started with development

Install the devtools and other necessary packages

``` r
install.packages(c("devtools", "roxygen2", "testthat", "knitr", "styler"))
```

R Studio: Tools/Install Packages or Build Pane: Install

### Rtools

Install Rtools (required to build packages with C++ code) - Windows:
Download it from [here](https://cran.r-project.org/bin/windows/Rtools/)
and run the installer. - MacOs: Run `xcode-select --install` -
Ubuntu/Debian: Run `sudo apt install r-base-dev`

Verify installation with:

``` r
devtools::dev_sitrep()
```

## Development workflow

- Run `check()` often and fix any errors, warnings and notes.
- Be extremely careful with top level code in a package. It is ONLY run
  when the package is build, NEVER again. Mainly use them for static
  variables. Any R code outside of a function is suspicious and should
  be carefully reviewed.
- Using `library`, `source` and `require` in a package is **forbidden**.
- Sub folders in the R directory are not supported.

### General workflow

![General dev
workflow](https://r-pkgs.org/diagrams/workflow.png "General development workflow")

Run the following command in each new R session to load the devtools.

``` r
library(devtools)
```

Alternatively a `.Rprofile` file can be used to automatically load the
library. Create the file manually at `~/.RProfile` or run
`use_devtools()` to create the file.

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

### Writing tests

- Each test file should be located in the `tests/testthat/`folder and
  should be named `test-FILENAME_OF_FUNCIONS_TO_TEST`.
- The actual result can be compared with the expected result with the
  functions that start with `expect_`.
- Write tests for input that is expected in typical use and for input
  that represend edge cases and may throw errors.
  - You can test for errors with \`\`\`expect_error\`\`\`\`
  - You can test for equality, with some reasonable amount of numeric
    tolerance with `expect_equal()`. Use `expect_identical()` for no
    tolerance.

### Test converage

To see the test converage of the package run:

    test_coverage()

## Documentation

Documentation for individual files is handled by
[roxygen2](https://cran.r-project.org/package=roxygen2)

### man Generation

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

### Roxygen comments

- Start any roxygen comment with `#'` (hash and single quote)  
- Use `\code{CODE_HERE}` for inline code
- Use `\link{function_name}` to link to other function documentation.
- Required and Optional tags in this order: (These are very opinionated
  rules to keep the documentation tidy and consistent):
  - `@name` (Required): The function name
  - `@title` (Required): Explain the function in as few words as
    possible. Avoid any non necessary words. If sufficiend use: Function
    name with all abbreviations expanded and with spaces.
  - `@description` (Required): Explain what the function does from a
    user perspective.
  - `@details` (Optional): Explain how the algorithm works internally
    and any other useful information. Anything that is not required to
    use the function but still useful. If required, this can be
    organized with the `@section Section_Name:` tag
  - Arguments (Required): Document **all** arguments the function uses.
    Look through the `man-roxygen/params` folder to check if there is
    already a param in the package with the same usage. If so add
    `@template param/ARGUMENT_NAME`. If there is none, create a new file
    in the `man-roxygen/params` folder. Explain thoroughly what the
    argument does and what it is used for. Use the following syntax in
    the file:
    `#' @params PARAMETER_NAME Here is a very detailed explanation of what this argument does.`
  - Returns (Required): Look through the `man-roxygen/returns` folder to
    check if a return type is already documented there. Because the
    return type can vary wildly, it isn’t required to use roxygen
    templates, if you expect no other function will return data with the
    same shape and meaning.
    - If you decided to use a roxygen template return documentation use
      `#' @template return/RETURN_FILE_NAME` as the comment.
    - If you decide to use a standalone comment, describe the shape
      (scaler, vector, matrix, …), size and meaning of the output.
    - If the functions returns nothing, use `@returns None`.
  - `@export`: Add if the functions should be used from outside.
  - `@importFrom` (Required): If you use functionality from another
    package (even the base package) declare them with `@importFrom`.
    Also do not forget to add the package in the `DESCRIPTION` file
    under the `Imports` (or for development dependencies `Suggests`)
    section. `check()` will also complain if you forget this.
  - References (Required): If the algorithm is based on a paper, you
    **must** cite all sources. To cite, check the
    `man-roxygen/cites`folder is the paper is already present there. If
    not create a new file.
    - **For new files**:
    - Use the last name of the author in upper case, `ET_AL` if there
      are more than one contributor and the year it was published as the
      file name.
    - Use the following syntax in the file:
      `#' @references <Authors> <Published Year>, <Paper Title>, <Published in> <%=FILE_NAME_P%>,`.
      The last part is a placeholder for the page number.
    - **For the roxygen comment**:
    - Use: `#' @template cites/FILE_NAME` and
      `#' @templateVar FILE_NAME_P pp. PAGE_NUMBER_OR_RANGE`
  - `@examples` (Required): Provide one or multiple examples of how the
    function can be used.
    - They should be as short as possible, while still using the
      function in a realistic and authentic way. They must run
      **without** errors and sideeffects (change the environment,
      working directory, write files, …). You should just be able to
      copy and run them.
    - Add comments to the examples as well.
    - If you provide multiple examples, break them up with a comment
      line (e.g. `# --------------`).
    - Do not use edge cases as examples. Make tests out of them instead.
    - Do not use excessive sample sizes. Running the examples should be
      almost instantaneous and can not take more than 3 seconds. If this
      is not possible add the `\dontrun` attribute.
    - `check()` also runs these examples.

### Vignette - NOT DONE YET. DO NOT USE YET.

The vignette should a provide a higher level overview of how the package
should be used, compared to the function docuementation.

#### Build the vignette

The vignette is build from the installed version of the package. Not the
loaded version. Therefor `load_all()` can not be used. It needs to be
installed.

The easiest way is to use the following command. This both installs the
package and builds the vignette:

    install(build_vignettes = TRUE)

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
