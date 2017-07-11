Sys.setlocale("LC_COLLATE", "C") ## What CRAN does; affects sort order
set.seed(999) ## To ensure that tests that involve randomness are reproducible
options(warn=1)

unserializeJSON <- jsonlite::unserializeJSON

with_mock_crunchtabs <- function (expr) {
  env <- parent.frame()
  with(temp.options(crunch.api="https://app.crunch.io/api/",
                    httptest.mock.paths=c(".", system.file(package="crunchtabs"))), {
                      with_mock_API({
                        try(crunch:::warmSessionCache())
                        eval(expr, envir=env)
                      })
                    })
}

fixtures_dir <- "fixtures"
with_mock_tabs <- function(book_file, mt_file, expr) {
    with_mock(
        `crunch::tabBook`=function (...) {
            crunch:::TabBookResult(jsonlite::fromJSON(file.path(fixtures_dir, book_file), simplifyVector=FALSE))
        },
        `crunchtabs:::getMultitable`=function (...) {
          crunch:::Multitable(jsonlite::fromJSON(file.path(fixtures_dir, mt_file), simplifyVector=FALSE))
        },
        eval.parent(expr)
    )
}

## Load the crunch package test setup
source(system.file("crunch-test.R", package="crunch"))
