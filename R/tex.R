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
                gsub("\n", "\\\\", # Turn newlines into \newlines
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

italics <- function (...) paste0("\\textit{", ..., "}")

bold <- function (...) paste0("\\textbf{", ..., "}")

underline <- function (...) paste0("\\underline{", ..., "}")

texTable <- function (df, sep=" & ", collapse=paste0(newline, "\n")) {
    do.call(paste, c(df, sep=sep, collapse=collapse))
}
