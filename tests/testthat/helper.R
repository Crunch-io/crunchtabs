Sys.setlocale("LC_COLLATE", "C") ## What CRAN does; affects sort order
set.seed(999) ## To ensure that tests that involve randomness are reproducible
options(warn=1)

## Load the crunch package test setup
source(system.file("crunch-test.R", package="crunch"))

fromJSON <- jsonlite::fromJSON
unserializeJSON <- jsonlite::unserializeJSON
unserializeJSON <- jsonlite::unserializeJSON

fixtures_dir <- "fixtures"


# with_mock_crunch <- function (expr) {
#   env <- parent.frame()
#   print(getwd())
#   with(temp.options(crunch.api="https://app.crunch.io/api/"), {
#                       with_mock_API({
#                         try(crunch:::warmSessionCache())
#                         eval(expr, envir=env)
#                       })
#                     })
# }

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

newDatasetFromFixture <- function (filename) {
  m <- fromJSON(file.path(fixtures_dir, paste0(filename, ".json")), simplifyVector=FALSE)
  return(suppressMessages(createWithMetadataAndFile(m, file.path(fixtures_dir, paste0(filename, ".csv")))))
}

