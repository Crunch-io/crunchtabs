Sys.setlocale("LC_COLLATE", "C") ## What CRAN does; affects sort order
set.seed(999) ## To ensure that tests that involve randomness are reproducible
options(warn=1)

with_mock_tabs <- function (file, expr) {
    with_mock(
        `crunch::tabBook`=function (...) {
            crunch:::TabBookResult(jsonlite::fromJSON(file, simplifyVector=FALSE))
        },
        eval.parent(expr)
    )
}
