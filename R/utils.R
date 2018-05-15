pdflatex <- function(texfile, open = TRUE, verbose = FALSE, cleanup = TRUE, options = "-halt-on-error",
    path.to.pdflatex = Sys.which("pdflatex"), ...) {

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
    returnfile <- file.path(filepath, pdffile)

    if (cleanup) {
        files <- dir(path = filepath, pattern = sub("\\.pdf$", "", pdffile))
        files <- sgrep(c("out$", "log$", "aux$"), files, value = TRUE)
        if (length(files) > 0) {
            file.remove(file.path(filepath, files))
        }
    }

    if (!file.exists(returnfile)) {
        stop("PDF file does not exist. Check that there are no errors in the LaTeX file.")
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

error_if_items <- function(items, text, error = TRUE){
    if (length(items) != 0 && !all(items %in% "")){
        if (error) { stop(gsub("\\{items\\}", collapse_items(items), text), call. = FALSE) }
        warning(gsub("\\{items\\}", collapse_items(items), text), call. = FALSE)
    }
}

wrong_class_error <- function(value, expected_class, name, null = FALSE){
    if (length(intersect(class(value), expected_class)) != length(expected_class)){
        stop("The expected class for `", name, "`", if (null) ", if provided, ", " is ", collapse_items(expected_class), ", not ", collapse_items(class(value)), ".", call. = FALSE)
    }
}

paste_around <- function(str, before, after) { paste0(before, str, after) }

collapse_items <- function(x){
    paste(x, collapse = ", ")
}
