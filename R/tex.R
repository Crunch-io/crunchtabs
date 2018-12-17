multicolumn <- function(width=1, ..., align="c") {
    paste0("\\multicolumn{", width, "}{", align, "}{", ..., "}")
}

newline <- "\\\\"

cmidrule <- function(from, to, size="lr{.75em}") {
    paste0("\\cmidrule(", size, "){", from, "-", to, "}")
}

texEscape <- function(str) {
    if (is.null(str)) {
        return("")
    }
    gsub("^ *(\\[)", "\\\\hspace\\*\\{0in\\}\\1", # IDK what this does
        gsub("([#$%&_])", "\\\\\\1", # Escape special characters
            gsub("[\u00A3\uFFE1]", "\\\\pounds", # Handle GBP currency
                gsub("\n", " \\\\newline ", # Turn newlines into \newlines
                    str
                )
            )
        )
    )
}

fontsize <- function(size) paste0("\\fontsize{", size, "}{", size * 1.5, "}")

center <- function (...) {
    paste("\\begin{center}", ..., "\\end{center}", sep="\n")
}

document <- function (...) {
    c("\\begin{document}", ..., "\\end{document}\n")
}

italics <- function (...) paste0("\\textit{", ..., "}")

bold <- function (...) paste0("\\textbf{", ..., "}")

underline <- function (...) paste0("\\underline{", ..., "}")

# Some TeX styles work within {}, like { \bf ...}
in_brackets <- function (...) c("{", ..., "}")

newcommand <- function (command, ..., args=0) {
    command <- paste0("\\", command)
    start <- c("\\newcommand", in_brackets(command))
    if (args) {
        # TODO: infer number of args from ..., the highest #N referenced
        start <- c(start, "[", args, "]")
    }
    paste0(c(start, in_brackets(...)), collapse="")
}

usepackage <- function (package, ...) {
    args <- paste(..., sep=", ")
    if (length(args)) {
        return(paste0("\\usepackage[", args, "]{", package, "}"))
    } else {
        return(paste0("\\usepackage{", paste(package, sep=", "), "}"))
    }
}

vspace <- function(space) paste0("\\vspace{", space, "}")

## Some functions that are more specific to this package

applyLatexStyle <- function(item, item_theme) {
    if (is.null(item) || identical(item, "")) {
        # Nothing to style
        return("")
    }
    if (!is.null(item_theme$decoration)) {
        if (any(c("underline", "underline2") %in% item_theme$decoration)) {
            item <- underline(item)
        }
        if ("italic" %in% item_theme$decoration) {
            item <- italics(item)
        }
        if ("bold" %in% item_theme$decoration) {
            item <- bold(item)
        }
    }
    if (!is.null(item_theme$font_size)) {
        item <- paste0(fontsize(item_theme$font_size), item)
    }
    if (!is.null(item_theme$font_color)) {
        if (grepl("^#[A-z0-9]{6}", item_theme$font_color)) {
            warning("In Latex, colors must be color names not hex codes. ", item_theme$font_color,
                " will be ignored.", call. = FALSE)
        } else {
            item <- paste0("\\color{", item_theme$font_color, "}", item)
        }
    }
    return(item)
}

validLatexFont <- function (theme_font) {
    # Make sure the theme font is valid; provide a fallback rather than erroring
    poss_fonts <- c("bookman","charter","courier","fourier","helvet","lmodern",
        "lmr","palatino","tgadventor","tgbonum","tgcursor","tgheros","tgpagella",
        "tgschola","tgtermes","times","utopia")
    if (is.null(theme_font) || !tolower(theme_font) %in% poss_fonts) {
        theme_font <- "helvet"
        warning("theme$font must be in ", paste0(poss_fonts, collapse = ", "),
            ". It has been set to `helvet`.", call. = FALSE)
    }
    return(theme_font)
}
