#' multicolumn tex
#'
#' Creates a programmable multicolumn with width and alignment
#'
#' @param width The number of columns
#' @param ... Further arguments passed to multicolumn
#' @param align One of l, c or r.
multicolumn <- function(width=1, ..., align="c") {
  paste0("\\multicolumn{", width, "}{", align, "}{", ..., "}")
}

# TODO: Convert to function. No globals!
newline <- "\\\\"

#' cmidrule tex
#'
#' Used for creating rule (lines) underneath a sub-header in
#' tables
#' @param from The column number
#' @param to The column number
#' @param size The size of the rule (line)
cmidrule <- function(from, to, size="lr{.75em}") {
  paste0("\\cmidrule(", size, "){", from, "-", to, "}")
}

#' texEscape
#'
#' A utility function for handling a number of unique
#' situations that can crop up in tex. (special characters,
#' British currency, character new line as tex newline)
#'
#' @param string A string
texEscape <- function(string) {
  if (is.null(string)) {
    return("")
  }
  # TODO: Change to one line rather than nested gsubs(), yuck.
  gsub("^ *(\\[)", "\\\\hspace\\*\\{0in\\}\\1", # Trim leading whitespace
       gsub("([#$%&_])", "\\\\\\1", # Escape special characters
            gsub("[\u00A3\uFFE1]", "\\\\pounds", # Handle GBP currency
                 gsub("\n", " \\\\newline ", # Turn newlines into \newlines
                      string
                 )
            )
       )
  )
}

#' Font Size
#'
#' Specify and scale fontsize in tex
#' @param size A number specifying the size. No default.
fontsize <- function(size) paste0("\\fontsize{", size, "}{", size * 1.5, "}")

#' Wrap center tex
#'
#' Center tex
#'
#' @param ... A list of other tex to be wrapped in center
center <- function(...) {
  paste("\\begin{center}", ..., "\\end{center}", sep = "\n")
}

#' Wrap document tex
#'
#' An outer level wrapper for a document
#'
#' @param ... A list of other tex to be wrapped in document
document <- function(...) {
  c("\\begin{document}", ..., "\\end{document}\n")
}

#' Apply italics tex
#'
#' Appliy italics to all tex within
#'
#' @param ... A list of other tex to be wrapped
italics <- function(...) paste0("\\textit{", ..., "}")

#' Apply bold tex
#'
#' Appliy Bold to all tex within
#'
#' @param ... A list of other tex to be wrapped
bold <- function(...) paste0("\\textbf{", ..., "}")

#' Apply underline tex
#'
#' Appliy underline to all tex within
#'
#' @param ... A list of other tex to be wrapped
underline <- function(...) paste0("\\underline{", ..., "}")


#' Bracket styling
#'
#' Some tex styles only work within {}, like { \\bf ...}
#'
#' @param ... Tex to be wrapped
in_brackets <- function(...) c("{", ..., "}")

#' newcommand tex
#'
#' Creates a newcommand tex string
#'
#' @param command Numeric value in first brackets.
#' @param ... Numeric value in second brackets.
#' @param args Numeric value in square brackets.
newcommand <- function(command, ..., args=0) {
  command <- paste0("\\", command)
  start <- c("\\newcommand", in_brackets(command))
  if (args) {
    # TODO: infer number of args from ..., the highest #N referenced
    start <- c(start, "[", args, "]")
  }
  paste0(c(start, in_brackets(...)), collapse = "")
}

#' usepackage tex
#'
#' Creates usepackage tex that accepts pacakge
#' argumentsd
#'
#' @param package A string naming the ctan package to include
#' @param ... Further arguments passed to the ctan package
usepackage <- function(package, ...) {
  args <- paste(..., sep = ", ")
  if (length(args)) {
    return(paste0("\\usepackage[", args, "]{", package, "}"))
  } else {
    return(paste0("\\usepackage{", paste(package, sep = ", "), "}"))
  }
}

#' vspace
#'
#' Create dynamic vspace tex
#'
#' @param space An integer identifying the number of spaces to print
vspace <- function(space) paste0("\\vspace{", space, "}")


#' Font decoration
#'
#' Apply font decorations like:
#' * underline / underline2 \link{underline}
#' * italics from \link{italics}
#' * bold from \link{bold}
#' * fontsize \link{fontsize}
#' * color defined inside this function
#'
#' @param item The text to wrap in font decoration
#' @param item_theme The font decorations to apply
#' @md
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
      # TODO: Define fontcolor as function for consistency
      item <- paste0("\\color{", item_theme$font_color, "}", item)
    }
  }
  return(item)
}


#' Validate fonts
#'
#' Validate the font passed to a theme object.
#'
#' @param theme_font A string naming the font specified in the crunchtab theme. Can be one of:
#' * bookman
#' * charter
#' * courier
#' * fourier
#' * helvet
#' * lmodern
#' * lmr
#' * palatino
#' * tgadventor
#' * tgbonum
#' * tgcursor
#' * tgheros
#' * tgpagella
#' * tgschola
#' * tgtermes
#' * times
#' * utopia
#'
#' @md
#'
validLatexFont <- function(theme_font) {
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
