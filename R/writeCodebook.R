#' Generate Codebook
#'
#' \code{writeCodebook} produces codebooks (as LaTeX files) containing statistics
#' about variables in a Crunch dataset.
#'
#' @param data_summary An object of class \code{Codebook}.
#' @param filename character. The name of the output file (without extension).
#' @param digits integer. Number of decimal digits to use for rounding.
#' Defaults to 0.
#' @param title An optional title. Defaults to the data summary title.
#' @param description A character string describing the dataset.
#' @param field_period A character string describing the field period.
#' @param sample_desc A character string describing the sample.
#' @param notes A character string with additional information.
#' @param moe An optional numeric margin of error.
#' @param append_text An optional character string that, if supplied, will be appended after
#' the final table. Useful for adding in disclosure information.
#' @param table_of_contents logical. Should a list of tables be included at the start
#' of the report Defaults to \code{FALSE}.
#' @param returndata logical. If \code{TRUE}, a processed data that was used to produce
#' the report is returned.
#' @param pdf logical. Compile LaTeX using pdflatex? Implemented only on MacOS/Linux.
#' @param path.to.pdflatex Character path to pdflatex.
#' @param open logical. If PDF document was produced, open it with
#' the default application? Only implemented for MacOS.
#' @param headtext An optional character string indicating what text should be
#' placed at the top of continuation tables. 'tbc' is a shortcut for 'to be
#' continued.'
#' @param foottext An optional character string indicating what text should be
#' placed at the bottom of continuation tables. 'tbc' is a shortcut for
#' 'continued from previous page.'
#' @param graphicspath character. The path to the folder with graphics files, e.g. logo.
#' Defaults to \code{NULL} - LaTeX output directory is used.
#' @param logo character. The name of the logo file.
#' Defaults to \code{NULL} - no logo is used.
#' @param landscape logical. If \code{TRUE}, generate pages in the landscape
#' mode. Defaults to \code{FALSE}.
#' @param font A character string specifying the font package to use in LaTeX.
#' @param font_size Font size points. Defaults to '12' pt font.
#' @param page_width Page width. Defaults to 9in for landscape mode and 6.5in otherwise.
#' @param row_label_width Width of the row label comuln. Defaults to 1.5in.
#' @param margin An optional argument to pass to the LaTeX package
#' \code{geometry}. Default is 'top=.6in, bottom=.6in, left=1in,
#' right=1in, includeheadfoot'.
#' @param clearpage logical. Should every variable be on a separete page?
#' @param round_to_100 logical. Should percentages be rounded to sum up to 100?
#' Defaults to \code{FALSE}.
#' @return If \code{returndata} is set to \code{TRUE}, a processed data that was used to produce
#' the report is returned. Otherwise \code{NULL} is returned.
#' @examples
#' \dontrun{
#' codebook_summary <- crosstabs(crunch_dataset, weight = 'weight', codebook = TRUE)
#' writeCodebook(codebook_summary, 'filename')
#' }
#' @export
writeCodebook <- function(data_summary, filename = NULL, digits = 0, title = getName(data_summary),
                          description = NULL, field_period = NULL, sample_desc = NULL, notes = NULL,
                          moe = NULL, append_text = NULL, table_of_contents = FALSE, returndata = FALSE,
                          pdf = FALSE, path.to.pdflatex = Sys.which("pdflatex"), open = FALSE,
                          headtext = "", foottext = "", graphicspath = NULL, logo = NULL,
                          landscape = FALSE, font = "helvet", font_size = 12,
                          page_width = ifelse(landscape, 9, 6.5), row_label_width = 1.5,
                          margin = list(top = 0.6, bottom = 0.6, left = 1, right = 1),
                          clearpage = TRUE, round_to_100 = FALSE) {

  data_summary$results <- lapply(data_summary$results, function(x) {
    reformatCodebookResults(x, digits = digits, reformat = TRUE, round_to_100 = round_to_100)
  })

  if (!is.null(description)) data_summary$metadata$description <- description
  if (!is.null(field_period)) data_summary$metadata$field_period <- field_period
  if (!is.null(sample_desc)) data_summary$metadata$sample_desc <- sample_desc
  if (!is.null(notes)) data_summary$metadata$notes <- notes

    out <- c(latexHeadT(surveyhead = title, font = font, font_size = font_size, margin = margin),
           startCodebookDoc(data_summary, table_of_contents = table_of_contents, clearpage = clearpage),
           sapply(data_summary$results,
                  function(x) paste(c(generateCodebookVarHeader(x, clearpage = clearpage),
                                    generateCodebookVarResults(x),
                                    if (!clearpage) "\n\\noindent\\rule{\\textwidth}{1pt}\n"),
                                    collapse = "\n")),
           latexFootT()
  )

  if (!is.null(filename)) {
    filename <- paste0(filename, ".tex")
    cat(out, sep = "\n", file = filename)
    if (pdf) {
      pdflatex(filename, open, path.to.pdflatex = path.to.pdflatex)
    }
  }
  invisible(data_summary)
}


addCodebookLatexPackages <- function() {
  paste(c("\\usepackage{caption}"), collapse = "\n")
}


startCodebookDoc <- function(data_summary, table_of_contents = FALSE, clearpage = TRUE) {
  paste(c("\\begin{document}",
          # "\\captionsetup[longtable]{labelfont=bf,textfont=it,labelsep=newline}",
          "\\begin{hyphenrules}{nohyphenation}",
          generateTitlePage(data_summary),
          if (table_of_contents) {"\\clearpage\\tableofcontents \n"},
          "\n",
          if (!clearpage) "\\clearpage\n"),
          collapse = "\n")
}

generateTitlePage <- function(data_summary) {
  paste0(c("\\begin{tabular}{ll}\n",
         if (!is.null(data_summary$metadata$description)) c("Description: & ", escM(data_summary$metadata$description), "\\\\\n"),
         if (!is.null(data_summary$metadata$field_period)) c("Field period: & ", escM(data_summary$metadata$field_period), "\\\\\n"),
         if (!is.null(data_summary$metadata$weight)) c("Weight: & ", escM(data_summary$metadata$weight), "\\\\\n"),
         if (!is.null(data_summary$metadata$sample_desc)) c("Sample: & ", escM(data_summary$metadata$sample_desc), "\\\\\n"),
         if (!is.null(data_summary$metadata$notes)) c("Notes: & ", escM(data_summary$metadata$notes), "\\\\\n"),
         "\\end{tabular}\n\n"), collapse = "")
}

generateCodebookVarHeader <- function(x, clearpage = TRUE) {
  filterText <- getNotes(x)
  desc <- getDescription(x)
  paste0(if (clearpage) "\\clearpage\n",
    "\\section{", escM(getAlias(x)), "}\n",
    "\\begin{tabular}{ll}\n",
    "Name: & ", escM(getName(x)), "\\\\\n",
    "Type: & ", escM(getType(x)), "\\\\\n",
    if (!is.null(desc) && desc != "") {paste0("Description: & ", escM(desc), "\\\\\n")},
    if (!is.null(filterText) && filterText != "") {paste0("Logic: & ", escM(filterText), "\\\\\n")},
    "\\end{tabular}\n\n"
  )
}


generateCodebookVarTable <- function(x, proportions = FALSE, weight = NULL, caption = "Results",
                                     details = TRUE) {
  res <- getResults(x, proportions = proportions, details = details)
  res <- if (is.matrix(res)) res else as.matrix(res)
  cols <- dim(res)[2] + 1
  col_names <- colnames(res)
  col_names <- if (!is.null(col_names)) c("", col_names)
  paste0(c("\\begin{center}\n",
        "\\begin{longtable}{", rep("r", cols), "}\n",
        "\\caption*{", escM(caption), "} \\\\\n",
        "\\hline\n",
        if (!is.null(col_names)) paste0(c(
        paste("\\multicolumn{1}{r}{", col_names, "}", collapse = " & "), "\\\\ \\hline\n",
        "\\endfirsthead\n",
        "\\multicolumn{", cols, "}{r}",
        "{{\\bfseries \\tablename\\ \\thetable{} -- continued from previous page}} \\\\\n",
        "\\hline ", paste("\\multicolumn{1}{r}{", col_names, "}", collapse = " & "), "\\\\ \\hline\n",
        "\\endhead\n",
        "\\hline \\multicolumn{", cols, "}{r}{{Continued on next page}} \\\\ \\hline\n",
        "\\endfoot\n",
        "\\hline\n",
        "\\endlastfoot\n"), collapse = ""),
        latexTable.body(res, autorownames = TRUE),
        "\\hline\n",
        "\\end{longtable}\n",
        "\\end{center}\n"
        ), collapse = "")
}


generateCodebookVarResults <- function(x, weight = NULL) {
  UseMethod("generateCodebookVarResults", x)
}

#' @export
generateCodebookVarResults.default <- function(x, weight = NULL) {
  warning(paste("generateCodebookVarResults doesn't support objects of class", class(x)))
  NULL
}

#' @export
generateCodebookVarResults.ToplineNumeric <- function(x, weight = NULL) {
  generateCodebookVarTable(x, proportions = FALSE, weight = weight, caption = "Summary")
}

#' @export
generateCodebookVarResults.ToplineMultipleResponse <- function(x, weight = NULL) {
  paste0(
      generateCodebookVarTable(x, proportions = FALSE, weight = weight, caption = "Counts"),
      generateCodebookVarTable(x, proportions = TRUE, weight = weight, caption = "Percentages",
                               details = FALSE),
      collapse = "\n\n")
}

#' @export
generateCodebookVarResults.ToplineCategoricalGeneral <- function(x, weight = NULL) {
  paste0(
    generateCodebookVarTable(x, proportions = FALSE, weight = weight, caption = "Counts"),
    generateCodebookVarTable(x, proportions = TRUE, weight = weight, caption = "Percentages",
                             details = FALSE),
    collapse = "\n\n")
}

#' @export
generateCodebookVarResults.ToplineBase <- function(x, weight = NULL) {
  "\\vspace{1cm}"
}
