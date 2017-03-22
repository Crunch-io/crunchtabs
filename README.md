# crunchtabs

[![Build Status](https://travis-ci.org/Crunch-io/crunchtabs.png?branch=master)](https://travis-ci.org/Crunch-io/crunchtabs)  [![codecov](https://codecov.io/gh/Crunch-io/crunchtabs/branch/master/graph/badge.svg)](https://codecov.io/gh/Crunch-io/crunchtabs) [![Build status](https://ci.appveyor.com/api/projects/status/5x52wy88wnvdky3n?svg=true)](https://ci.appveyor.com/project/nealrichardson/crunchtabs)


## Installing

<!-- If you're putting `crunchtabs` on CRAN, it can be installed with

    install.packages("crunchtabs") -->

The pre-release version of the package can be pulled from GitHub using the [devtools](https://github.com/hadley/devtools) package:

    # install.packages("devtools")
    devtools::install_github("Crunch-io/crunchtabs")

## For developers

The repository includes a Makefile to facilitate some common tasks.

### Running tests

`$ make test`. Requires the [httptest](https://github.com/nealrichardson/httptest) package. You can also specify a specific test file or files to run by adding a "file=" argument, like `$ make test file=logging`. `test_package` will do a regular-expression pattern match within the file names. See its documentation in the `testthat` package.

### Updating documentation

`$ make doc`. Requires the [roxygen2](https://github.com/klutometis/roxygen) package.
