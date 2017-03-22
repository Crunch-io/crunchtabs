directory <- function() ifelse(grepl("unix", .Platform$OS.type), "/", "\\")

#' @importFrom utils tail
splitFilePath <- function(filename) {
    dirchar <- directory()
    filename <- unlist(strsplit(filename, dirchar, fixed = TRUE))
    pathfile <- c(ifelse(length(filename) > 1, paste(filename[-length(filename)],
        collapse = dirchar), "."), tail(filename, 1))
    return(pathfile)
}

pdflatex <- function(texfile, open = TRUE, verbose = FALSE, cleanup = TRUE, options = "-halt-on-error",
    path.to.pdflatex = Sys.which("pdflatex"), ...) {
    filepath <- splitFilePath(texfile)
    texfile <- filepath[2]
    filepath <- filepath[1]
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
        nullout <- ">/dev/null"
    } else {
        if (nchar(path.to.pdflatex) == 0) {
            path.to.pdflatex <- "pdflatex"
        }
        texfile <- paste(texfile, pdffile)
        nullout <- ">NUL"
        open <- FALSE
    }
    if (verbose)
        nullout <- ""

    texcommand <- paste(path.to.pdflatex, options, "-output-directory", filepath,
        texfile, nullout)
    system(texcommand)
    system(texcommand)

    pdffile <- sub("^\"", "", sub("\"$", "", pdffile))
    filepath <- sub("^\"", "", sub("\"$", "", filepath))
    returnfile <- paste(filepath, pdffile, sep = directory())

    if (cleanup) {
        files <- dir(path = filepath, pattern = sub("\\.pdf$", "", pdffile))
        files <- sgrep(c("out$", "log$", "aux$"), files, value = TRUE)
        if (length(files) > 0) {
            file.remove(paste(filepath, files, sep = directory()))
        }
    }

    if (!file.exists(returnfile)) {
        stop("PDF file does not exist. Check that there are no errors ", "in the LaTeX file.")
    } else if (open) {
        file.open(returnfile)
    }

    return(returnfile)
}

file.open <- function(x) {
    if (grepl("mac", .Platform$pkgType)) {
        for (i in x) system(paste("open", shQuote(i)))
    }
}

sgrep <- function(strs, ..., simplify = TRUE) {
    out <- sapply(strs, function(x) grep(x, ...), simplify = FALSE)
    if (simplify)
        out <- unique(unlist(out))
    return(out)
}