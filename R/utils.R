#' pdflatex
#'
#' A utility function that runs pdflatex
#'
#' @param texfile The path to the texfile
#' @param open Logical, should the result be opened?
#' @param verbose Logical, should the function be verbose?
#' @param cleanup Logical, should tex/log files be cleaned?
#' @param options Additional options passed to pdflatex
#' @param path.to.pdflatex The system path to pdflatex
#' @param ... Further arguments, unused.
pdflatex <- function(texfile, open = interactive(), verbose = FALSE, cleanup = TRUE, options = "-halt-on-error",
                     path.to.pdflatex = Sys.which("pdflatex"), ...) {
  # nocov start
  filepath <- dirname(texfile)
  texfile <- basename(texfile)
  pdffile <- sub("\\.tex$", "\\.pdf", texfile, ignore.case = TRUE)
  if (grepl(" ", texfile)) {
    texfile <- paste("\"", texfile, "\"", sep = "")
    pdffile <- paste("\"", pdffile, "\"", sep = "")
  }
  if (grepl(" ", filepath)) {
    filepath <- paste("\"", filepath, "\"", sep = "")
  }

  if (grepl("unix", .Platform$OS.type)) {
    if (nchar(path.to.pdflatex) == 0) {
      path.to.pdflatex <- "/usr/texbin/pdflatex"
    }
  } else {
    if (nchar(path.to.pdflatex) == 0) {
      path.to.pdflatex <- "pdflatex"
    }
    texfile <- paste(texfile, pdffile)
  }

  texcommand <- paste(path.to.pdflatex, options, "-output-directory", filepath,
                      texfile)
  # In case it needs packages/styles you don't have, just keep hitting enter
  input <- paste0(rep("\n", 100))
  system(texcommand, input = input, ignore.stdout = !verbose)
  system(texcommand, input = input, ignore.stdout = !verbose)

  pdffile <- sub("^\"", "", sub("\"$", "", pdffile))
  filepath <- sub("^\"", "", sub("\"$", "", filepath))
  returnfile <- file.path(filepath, pdffile)

  if (cleanup) {
    files <- dir(path = filepath, pattern = sub("\\.pdf$", "", pdffile))
    files <- grep("out$|log$|aux$", files, value = TRUE)
    if (length(files)) {
      file.remove(file.path(filepath, files))
    }
  }

  if (!file.exists(returnfile)) {
    stop("PDF file does not exist. Check that there are no errors in the LaTeX file.")
  } else if (open) {
    file.open(returnfile)
  }
  # nocov end
  return(returnfile)
}

#' Open
#'
#' Open a document
#'
#' @param x A vector of filenames
file.open <- function(x) {
  # start nocov
  if (Sys.info()['sysname'] == "Linux") {
    for (i in x) system(paste("xdg-open", shQuote(i)))
  } else if (Sys.info()['sysname'] == "Windows") {
    for (i in x) system(paste("start", shQuote(i)))
  } else {
    for (i in x) system(paste("open", shQuote(i)))
  }
  # end nocov
}

#' error if items
#'
#' A function assertion used to
#' create an error condition on the existence
#' of some items
#'
#' @param items A character vector of items
#' @param text A character vector that items may be within
#' @param error Logical, should a stop be called?
#' @param and Logical, passed to \link{collapse_items}
#' @param or Logical, passed to \link{collapse_items}
#' @param quotes Logical, passed to \link{collapse_items}
error_if_items <- function(items, text, error = TRUE, and = FALSE, or = FALSE, quotes = FALSE){
  if (length(items) != 0 && !all(items %in% "")) {
    message <- gsub("\\{items\\}", collapse_items(items, and, or, quotes), text)
    if (error) { stop(message, call. = FALSE) }
    warning(message, call. = FALSE)
  }
}

#' Assert class
#'
#' Function assertion that verifies the
#' class of an object
#'
#' @param value The object
#' @param expected_class A character vector of potential classes
#' @param name The name of the object
#' @param null Logical, identifying if the object can be null
wrong_class_error <- function(value, expected_class, name, null = FALSE){
  if (length(intersect(class(value), expected_class)) != length(expected_class)) {
    stop("The expected class for `", name, "`", if (null) ", if provided, ",
         " is ", collapse_items(expected_class), ", not ", collapse_items(class(value)),
         ".", call. = FALSE)
  }
}

#' Paste Around
#'
#' Embeds a string in given before and after
#'
#' @param str A character string
#' @param before A character string to go before str
#' @param after A character string to go after str
paste_around <- function(str, before, after) paste0(before, str, after)


#' Collapse items
#'
#' Rule based collapse
#'
#' @param x A character string
#' @param and Logical, collapse using "and"
#' @param or Logical, collapse using "or"
#' @param quotes Logical, should it be quoted?
collapse_items <- function(x, and = FALSE, or = FALSE, quotes = FALSE){
  if (quotes) {
    x <- paste0("'", x, "'")
  }
  if (length(x) > 2) {
    x <- c(paste0(x[1:(length(x) - 1)], ",", collapse = " "), x[length(x)])
  }
  return(paste0(x, collapse = if (and) { " and " } else if (or) {" or " } else {" " }))
}

#' Operator overload
#'
#' Creates an ifelse function that handles null
#'
#' @param a An R object
#' @param b An R object
#' @rdname ifelseOverload
#' @name ifelseOverload
"%||%" <- function(a, b) if (!is.null(a)) a else b

#' crunch test wrapper
#'
#' Use this to wrap tests that require access to
#' the crunch api
#'
#' @param fixture_path A full path to fixtures
#' @param expr An expression to be run within the api fixture
#' @export
with_api_fixture <- function(fixture_path, expr) {
  with(
    crunch::temp.options(
      crunch.api = "https://app.crunch.io/api/",
      httptest.mock.paths = fixture_path,
      crunch.show.progress = FALSE
    ),
    httptest::with_mock_api(
      # Also need to redact UUIDs as is done in POSTs
      with_mock(
        `crunch::crPOST` = function(...) {
          args <- list(...)
          # Necessary for post
          args$body <- gsub("([0-9a-f]{6})[0-9a-f]{26}", "\\1", args$body)
          args[[1]] <- gsub("([0-9a-f]{6})[0-9a-f]{26}", "\\1", args[[1]])
          do.call(
            function(...) crunch:::crunchAPI("POST", ...),
            args
          )
        },
        expr
      )
    )
  )
}
