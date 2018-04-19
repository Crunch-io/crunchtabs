library(openxlsx)
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
with_mock_tabs <- function(book_file, mt_file, path = fixtures_dir, expr) {
    with_mock(
        `crunch::tabBook`=function (...) {
            crunch:::TabBookResult(jsonlite::fromJSON(file.path(path, book_file), simplifyVector=FALSE))
        },
        `crunchtabs:::getMultitable`=function (...) {
          crunch:::Multitable(jsonlite::fromJSON(file.path(path, mt_file), simplifyVector=FALSE))
        },
        eval.parent(expr)
    )
}

with_temp_dir <- function(expr) {
    wd <- getwd()
    d <- tempfile()
    dir.create(d)
    setwd(d)
    on.exit(setwd(wd))

    eval.parent(expr)
}

# Source crunch-test.R when: R CMD check, devtools::test(), make test,
# crunchdev::test_crunch()
# Don't source crunch-test.R when: devtools::load_all() (interactively)
# https://github.com/hadley/devtools/issues/1202
source_if <- !interactive() || identical(Sys.getenv("NOT_CRAN"), "true")
# And don't source it when running pkgdown
source_if <- source_if && !identical(Sys.getenv("DEVTOOLS_LOAD"), "true")
if (source_if) {
    source(system.file("crunch-test.R", package="crunch"))
}
