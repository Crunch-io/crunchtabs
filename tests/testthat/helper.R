library(openxlsx)
Sys.setlocale("LC_COLLATE", "C") ## What CRAN does; affects sort order
set.seed(999) ## To ensure that tests that involve randomness are reproducible
options(warn = 1)

unserializeJSON <- jsonlite::unserializeJSON

# with_mock_crunchtabs <- function (expr) {
#   env <- parent.frame()
#   with(temp.options(crunch.api="https://app.crunch.io/api/",
#                     httptest.mock.paths=c(".", system.file(package="crunchtabs"))), {
#                       with_mock_API({
#                         try(crunch:::warmSessionCache())
#                         eval(expr, envir=env)
#                       })
#                     })
# }

fixtures_dir <- "fixtures"
with_mock_tabs <- function(book_file, mt_file, path = fixtures_dir, expr) {
  mock_tabBook <- function(...) {
    crunch:::TabBookResult(jsonlite::fromJSON(file.path(path, book_file), simplifyVector = FALSE))
  }
  mock_getMultitable <- function(...) {
    crunch:::Multitable(jsonlite::fromJSON(file.path(path, mt_file), simplifyVector = FALSE))
  }

  crunch_ns <- asNamespace("crunch")
  crunchtabs_ns <- asNamespace("crunchtabs")
  crunchtabs_imports <- parent.env(crunchtabs_ns)

  orig_tabBook <- crunch_ns$tabBook
  orig_getMultitable <- crunchtabs_ns$getMultitable

  swap_binding <- function(name, value, envir) {
    if (bindingIsLocked(name, envir)) unlockBinding(name, envir)
    assign(name, value, envir = envir)
    lockBinding(name, envir)
  }

  swap_binding("tabBook", mock_tabBook, crunch_ns)

  if (exists("tabBook", envir = crunchtabs_imports, inherits = FALSE)) {
    swap_binding("tabBook", mock_tabBook, crunchtabs_imports)
  }

  for (env_name in c("package:crunchtabs", "package:crunch")) {
    if (env_name %in% search()) {
      env <- as.environment(env_name)
      if (exists("tabBook", envir = env, inherits = FALSE)) {
        swap_binding("tabBook", mock_tabBook, env)
      }
    }
  }

  swap_binding("getMultitable", mock_getMultitable, crunchtabs_ns)

  on.exit({
    swap_binding("tabBook", orig_tabBook, crunch_ns)

    if (exists("tabBook", envir = crunchtabs_imports, inherits = FALSE)) {
      swap_binding("tabBook", orig_tabBook, crunchtabs_imports)
    }

    for (env_name in c("package:crunchtabs", "package:crunch")) {
      if (env_name %in% search()) {
        env <- as.environment(env_name)
        if (exists("tabBook", envir = env, inherits = FALSE)) {
          swap_binding("tabBook", orig_tabBook, env)
        }
      }
    }

    swap_binding("getMultitable", orig_getMultitable, crunchtabs_ns)
  })

  eval.parent(expr)
}

with_temp_dir <- function(expr) {
  wd <- getwd()
  d <- tempfile()
  dir.create(d)
  setwd(d)
  on.exit(setwd(wd))

  eval.parent(expr)
}

# # Source crunch-test.R when: R CMD check, devtools::test(), make test,
# # crunchdev::test_crunch()
# # Don't source crunch-test.R when: devtools::load_all() (interactively)
# # https://github.com/hadley/devtools/issues/1202
# source_if <- !interactive() || identical(Sys.getenv("NOT_CRAN"), "true")
# # And don't source it when running pkgdown
# source_if <- source_if && !identical(Sys.getenv("DEVTOOLS_LOAD"), "true")
# if (source_if) {
#     source(system.file("crunch-test.R", package="crunch"))
# }
