# crunchtabs

![CI](https://github.com/Crunch-io/crunchtabs/workflows/CI/badge.svg?branch=master) [![Build Status](https://travis-ci.org/Crunch-io/crunchtabs.png?branch=master)](https://travis-ci.org/Crunch-io/crunchtabs)  [![codecov](https://codecov.io/gh/Crunch-io/crunchtabs/branch/master/graph/badge.svg)](https://codecov.io/gh/Crunch-io/crunchtabs)


## Installing

<!-- If you're putting `crunchtabs` on CRAN, it can be installed with

    install.packages("crunchtabs") -->

`crunchtabs` is not yet on CRAN, but you can install it from GitHub using the [remotes](https://github.com/r-lib/remotes) package (part of `devtools`):

    # install.packages("remotes")
    remotes::install_github("Crunch-io/crunchtabs")

To make PDF reports, you'll need a working LaTeX installation. One way to get this is with the [tinytex](https://yihui.name/tinytex/) package. Or, see https://www.latex-project.org/get/ to install everything.

If you find that you need additional TeX packages to get the PDF to build, try running the `tlmgr install` command found in [.travis.yml](https://github.com/Crunch-io/crunchtabs/blob/master/.travis.yml).

## Getting started

    library(crunchtabs)

    login()
    ds <- loadDataset("My survey")
    crossbreaks <- banner(ds, list(
        Demographics=c("gender", "age4", "education"),
        Results=c("q4", "segmentation")
    ))
    crosstabs <- crosstabs(ds, banner=crossbreaks)
    writeExcel(crosstabs, filename="my-tab-book.xlsx")

## For developers

The repository includes a Makefile to facilitate some common tasks, if you're into that kind of thing.

### Running tests

`$ make test`. Requires the [httptest](https://github.com/nealrichardson/httptest) package. You can also specify a specific test file or files to run by adding a "file=" argument, like `$ make test file=latex`. `test_package` will do a regular-expression pattern match within the file names. See its documentation in the `testthat` package.

### Updating documentation

`$ make doc`. Requires the [roxygen2](https://github.com/klutometis/roxygen) package.
