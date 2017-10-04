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
#' @param table_of_contents logical. Should a list of tables be included at the start
#' of the report Defaults to \code{FALSE}.
#' @param pdf logical. Compile LaTeX using pdflatex? Implemented only on MacOS/Linux.
#' @param path.to.pdflatex Character path to pdflatex.
#' @param open logical. If PDF document was produced, open it with
#' the default application? Only implemented for MacOS.
#' @param graphicspath character. The path to the folder with graphics files, e.g. logo.
#' Defaults to \code{NULL} - LaTeX output directory is used.
#' @param logo character. The name of the logo file.
#' Defaults to \code{NULL} - no logo is used.
#' @param landscape logical. If \code{TRUE}, generate pages in the landscape
#' mode. Defaults to \code{FALSE}.
#' @param font A character string specifying the font package to use in LaTeX.
#' @param font_size Font size points. Defaults to '12' pt font.
#' @param margin An optional argument to pass to the LaTeX package
#' \code{geometry}. Default is 'top=.6in, bottom=.6in, left=1in,
#' right=1in, includeheadfoot'.
#' @param clearpage logical. Should every variable be on a separete page?
#' @param round_to_100 logical. Should percentages be rounded to sum up to 100?
#' @param concatenate_categories logical. Should category' ID be concatenated
#' with category's name?
#' Defaults to \code{FALSE}.
#' @return Processed data (invisibly).
#' @examples
#' \dontrun{
#' codebook_summary <- crosstabs(crunch_dataset, weight = 'weight', codebook = TRUE)
#' writeCodebook(codebook_summary, 'filename')
#' }
#' @export
writeCodebook <- function(data_summary, filename = NULL, digits = 0, title = getName(data_summary),
                          description = NULL, field_period = NULL, sample_desc = NULL, notes = NULL,
                          table_of_contents = FALSE,
                          pdf = FALSE, path.to.pdflatex = Sys.which("pdflatex"), open = FALSE,
                          graphicspath = NULL, logo = NULL,
                          landscape = FALSE, font = "helvet", font_size = 12,
                          margin = list(top = 0.6, bottom = 0.6, left = 1, right = 1),
                          clearpage = TRUE, round_to_100 = FALSE, concatenate_categories = FALSE) {

  if (!is(data_summary, "Codebook")) {
    stop(paste0("writeCodebook doesn't support objects of class '",
                paste0(class(data_summary), collapse = " "), "'"))
  }

  data_summary$results <- lapply(data_summary$results, function(x) {
    reformatCodebookResults(x, digits = if (!is.null(x$settings) && !is.null(x$settings$digits)) x$settings$digits else digits,
                            reformat = TRUE, round_to_100 = round_to_100)
  })

  data_summary <- addMetadata(data_summary, description = description, field_period = field_period,
                              sample_desc = sample_desc, notes = notes)

  out <- c(latexHeadCb(surveyhead = title, font = font, font_size = font_size, margin = margin),
         startCodebookDoc(data_summary, table_of_contents = table_of_contents, clearpage = clearpage),
         sapply(data_summary$results,
                function(x) paste(c(generateCodebookVarHeader(x, clearpage = clearpage),
                                  generateCodebookVarResults(x, concatenate_categories = concatenate_categories),
                                  if (!clearpage) "\n\\noindent\\rule{\\textwidth}{1pt}"),
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


addMetadata <- function(data_summary, description = NULL, field_period = NULL, sample_desc = NULL, notes = NULL) {
  if (!is.null(description)) data_summary$metadata$description <- description
  if (!is.null(field_period)) data_summary$metadata$field_period <- field_period
  if (!is.null(sample_desc)) data_summary$metadata$sample_desc <- sample_desc
  if (!is.null(notes)) data_summary$metadata$notes <- notes
  return(data_summary)
}

addCodebookLatexPackages <- function() {
  paste(c("\\usepackage{caption}"), collapse = "\n")
}


startCodebookDoc <- function(data_summary, table_of_contents = FALSE, clearpage = TRUE) {
  paste(c("\\setcounter{secnumdepth}{0}\n",
          "\\begin{document}",
          "\\tabulinesep=1.75mm",
          "\\begin{hyphenrules}{nohyphenation}",
          generateTitlePage(data_summary),
          # if (table_of_contents) {"\\clearpage\\tableofcontents \n"},
          if (table_of_contents) {c("\\clearpage\n", generateTableOfContents(data_summary$results))},
          "\n",
          if (!clearpage) "\\clearpage\n"),
          collapse = "\n")
}

generateTableOfContents <- function(results, title = "Contents") {
  paste0("{\\Large ", title, "\\\\}\n",
         "{\\vskip 0.2cm}\n",
         "\\begin{longtabu} to \\textwidth {@{}X[5,L]X[10,L]X[3,L]X[1,L]@{}}\n",
         "{\\bfseries Alias} & {\\bfseries Name} & {\\bfseries Type} & {\\bfseries Page} \\\\ \\noalign{\\vskip 0.125cm}\n",
         "\\endhead\n",
         paste(unlist(sapply(results, function(x) c(generateTocRow(getAlias(x), getName(x), getType(x)),
                                             if (getType(x) %in% c("categorical_array", "multiple_response"))
                                               sapply(seq_len(length(x$subvariables$aliases)),
                                                      function(i) generateTocRow(x$subvariables$aliases[i],
                                                                                 x$subvariables$names[i],
                                                                                 ref_alias = getAlias(x),
                                                                                 indent = TRUE))))),
               collapse = " \\\\\n"),
         "\\end{longtabu}\n\n"
         )
}


generateTocRow <- function(alias, name, type = NULL, ref_alias = alias, indent = FALSE) {
        paste(paste(if (indent) "\\hspace{3mm}", escM(alias)),
              paste(if (indent) "\\hspace{3mm}", escM(name)),
              if (!is.null(type)) escM(gsub("_", " ", type)) else "",
              paste0("\\pageref{", gsub("_", "", ref_alias), "}"),
              sep = " & ")
}

generateTitlePage <- function(data_summary) {
  paste0(c("\\begin{longtabu} to \\textwidth {@{}X[1,L]X[5,L]@{}}\n",
         if (!is.null(data_summary$metadata$description)) c("{\\bfseries Description} & ", escM(data_summary$metadata$description), "\\\\\n"),
         if (!is.null(data_summary$metadata$field_period)) c("{\\bfseries Field period} & ", escM(data_summary$metadata$field_period), "\\\\\n"),
         # if (!is.null(data_summary$metadata$weight)) c("{\\bfseries Weight} & ", escM(data_summary$metadata$weight), "\\\\\n"),
         if (!is.null(data_summary$metadata$sample_desc)) c("{\\bfseries Sample} & ", escM(data_summary$metadata$sample_desc), "\\\\\n"),
         if (!is.null(data_summary$metadata$notes)) c("{\\bfseries Notes} & ", escM(data_summary$metadata$notes), "\\\\\n"),
         "\\end{longtabu}\n"), collapse = "")
}

generateCodebookVarHeader <- function(x, clearpage = TRUE) {
  filterText <- getNotes(x)
  desc <- getDescription(x)
  paste0(if (clearpage) "\\clearpage\n",
    "\\section{", escM(getAlias(x)), "} \\label{", gsub("_", "", getAlias(x)), "}\n",
    "\\begin{longtabu} to \\textwidth {@{}X[1,L]X[6,L]@{}}\n",
    "{\\bfseries Name} & ", escM(getName(x)), "\\\\\n",
    "{\\bfseries Type} & ", escM(gsub("_", " ", getType(x))), "\\\\\n",
    if (!is.null(desc) && desc != "") {paste0("{\\bfseries Description} & ", escM(desc), "\\\\\n")},
    if (!is.null(filterText) && filterText != "") {paste0("{\\bfseries Logic} & ", escM(filterText), "\\\\\n")},
    "\\end{longtabu}\n"
  )
}


generateCodebookVarTable <- function(x, proportions = FALSE, weight = NULL, caption = "Results",
                                     details = TRUE, longtable = FALSE, transpose = FALSE,
                                     autorownames = TRUE, concatenate_categories = FALSE,
                                     adjust = "l") {
  res <- getResults(x, proportions = proportions, details = details)
  res <- if (is.matrix(res)) res else as.matrix(res)
  if (transpose) {
    res <- t(res)
  }
  categories_row <- NULL
  categories_col <- NULL
  if (concatenate_categories) {
    res <- appendCategoryId(x, res)
  } else {
    if (x$type %in% 'categorical') {
      categories_row <- matchCategoriesConcatenate(x$categories, rownames(res), "(", ")", concatenate = FALSE, else_val = "-")
      if (!is.null(categories_row)) {
        res <- data.frame(categories_row, if (autorownames) rownames(res), res, stringsAsFactors = FALSE)
        colnames(res) <- c("{\\bfseries Id}", "{\\bfseries Name}", "{\\bfseries Value}")
        autorownames <- FALSE
      }
    }
    if (x$type %in% 'categorical_array') {
      categories_col <- matchCategoriesConcatenate(x$categories, colnames(res), "(", ")", concatenate = FALSE, else_val = "")
      if (!(is.null(categories_col)) && autorownames) categories_col <- c("", categories_col)
    }
  }
  cols <- dim(res)[2] + autorownames
  col_names <- colnames(res)
  if (!is.null(col_names)) {
    if (autorownames) {
      col_names <- c("", col_names)
      if (!is.null(categories_row)) {
        col_names <- c("", col_names)
        categories_col <- c("", categories_col)
      }
    }
  }

  wordLen <- function(vals) {
    sapply(strsplit(vals, " "), function(x) max(nchar(x)))
  }

  col_def <-
    if (x$type %in% 'categorical') "X[1,l]X[8,L]X[1,l]"
    else if (x$type %in% c('categorical_array')) {
      label_words_len <- wordLen(rownames(res))
      max_label_words_len <- max(label_words_len)
      if (mean(label_words_len) > 5) max_label_words_len <- max_label_words_len * 1.2
      words_len <- c(max_label_words_len, wordLen(col_names[2:length(col_names)]))
      words_len[words_len < 5] <- 5
      max_word_len <- max(words_len)
      paste0("X[", ceiling((words_len/max_word_len)*10), ",L,m]", collapse = "")
    } else if (x$type %in% c('multiple_response')) {
      "X[3,L]X[1,l]X[1,l]X[1,l]"
    }
    else paste0(rep("X[1,L,m]", cols), collapse = "")

  tab_def <- if (!is.null(col_names)) c(paste0(col_names, collapse = " & "), "\\\\\n")
  if (!is.null(tab_def) && !is.null(categories_col)) tab_def <- c(paste0(categories_col, collapse = " & "), "\\\\\n", tab_def)

  caption_def <- paste0(c(), collapse = "")
  paste0(c("\\begin{longtabu} to ", if (x$type %in% 'categorical') "0.65", "\\textwidth {", col_def, "}\n",
        "\\caption*{", escM(caption), "} \\\\\n",
        "\\hline\n",
        if (!is.null(col_names)) c(
          tab_def,
          "\\endfirsthead\n"),
        if (!is.null(col_names)) c(
          tab_def,
          "\\endhead\n"),
        latexTable.body(res, autorownames = autorownames),
        "\\hline\n",
        "\\end{longtabu}\n"), collapse = "")
}


generateCodebookVarResults <- function(x, weight = NULL, concatenate_categories = FALSE) {
  UseMethod("generateCodebookVarResults", x)
}

#' @export
generateCodebookVarResults.default <- function(x, weight = NULL, concatenate_categories = FALSE) {
  warning(paste("generateCodebookVarResults doesn't support objects of class", class(x)))
  NULL
}

#' @export
generateCodebookVarResults.ToplineNumeric <- function(x, weight = NULL, concatenate_categories = FALSE) {
  generateCodebookVarTable(x, proportions = FALSE, weight = weight, caption = "Summary",
                           transpose = TRUE, autorownames = FALSE)
}

#' @export
generateCodebookVarResults.ToplineMultipleResponse <- function(x, weight = NULL, concatenate_categories = FALSE) {
  paste0(
      generateCodebookVarTable(x, proportions = FALSE, weight = weight, caption = "Counts",
                               concatenate_categories = concatenate_categories),
      generateCodebookVarTable(x, proportions = TRUE, weight = weight, caption = "Percentages",
                               concatenate_categories = concatenate_categories),
      collapse = "\n\n")
}

#' @export
generateCodebookVarResults.ToplineCategoricalGeneral <- function(x, weight = NULL, concatenate_categories = FALSE) {
  paste0(
    generateCodebookVarTable(x, proportions = FALSE, weight = weight, caption = "Counts",
                             concatenate_categories = concatenate_categories),
    generateCodebookVarTable(x, proportions = TRUE, weight = weight, caption = "Percentages",
                             concatenate_categories = concatenate_categories),
    collapse = "\n\n")
}


#' @export
generateCodebookVarResults.ToplineBase <- function(x, weight = NULL, concatenate_categories = FALSE) {
  "\\vspace{1cm}"
}


appendCategoryId <- function(x, data) {
  UseMethod("appendCategoryId", x)
}


#' @export
appendCategoryId.default <- function(x, data) {
  data
}


#' @export
appendCategoryId.ToplineCategoricalArray <- function(x, data) {
  data_names <- colnames(data)
  matched <- matchCategoriesConcatenate(x$categories, data_names,
                                        prefix = "(", suffix = ") ")
  if (!is.null(matched)) {
    colnames(data) <- matched
  }
  data
}


#' @export
appendCategoryId.ToplineCategorical <- function(x, data) {
  data_names <- rownames(data)
  matched <- matchCategoriesConcatenate(x$categories, data_names,
                             prefix = "(", suffix = ") ")
  if (!is.null(matched)) {
    rownames(data) <- matched
  }
  data
}


matchCategories <- function(categories, data_names, else_val = "") {
  sapply(data_names, function (cn) {
    nval <- categories$numeric_value[categories$name == cn]
    if (length(nval) > 0 && !is.na(nval)) nval else else_val
  })
}


matchCategoriesConcatenate <- function(categories, data_names, prefix = NULL, suffix = NULL, concatenate = TRUE, else_val = NULL) {
  matched <- matchCategories(categories, data_names, else_val = NA)
  sapply(seq_len(length(matched)), function(i) {
    if (!is.na(matched[i])) paste0(prefix, matched[i], suffix, if (concatenate) data_names[i])
    else if (!is.null(else_val)) else_val
    else data_names[i]
  })
}

latexHeadCb <- function(surveyhead, font_size, margin, font, landscape=FALSE, subhead=NULL, graphicspath = NULL, logo = NULL){
  paste("\\documentclass[", font_size, "pt", ifelse(landscape, ', landscape', ''), "]",
        "{article}\n",
        "\\usepackage[pdftex]{graphicx}\n",
        if (!is.null(graphicspath)) paste0("\\graphicspath{ {", graphicspath,"/} }"),
        "\\usepackage[utf8]{inputenc}\n",
        "\\usepackage{fancyhdr}\n",
        "\\usepackage{sfmath}\n",
        "\\usepackage[T1]{fontenc}\n",
        "\\usepackage{longtable,tabu}\n",
        "\\usepackage{tabulary}\n",
        "\\usepackage[scaled]{", font, "}\n",
        '\\renewcommand*\\familydefault{\\sfdefault}
        \\usepackage{booktabs}
        \\usepackage{float}
        \\usepackage[pdftex=true,
        pdftoolbar=true,
        pdfmenubar=true,
        pdfauthor = {},
        pdfcreator = {PDFLaTeX},
        pdftitle = {},
        colorlinks=true,
        urlcolor=blue,
        linkcolor=blue,
        citecolor=blue,
        implicit=true,
        hypertexnames=false]{hyperref}\n',
        "\\usepackage{marginnote}\n",
        "\\usepackage[",
        ifelse(!is.null(margin),
               paste("top=", margin$t, "in, bottom=", margin$b, "in, left=", margin$l, "in, right=", margin$r, "in, includeheadfoot", sep=""),
               "top=.6in,bottom=.6in,left=1in,right=1in,includeheadfoot"),
        "]{geometry}\n",
        "\\usepackage{array}\n",
        "\\setlength\\extrarowheight{2pt}\n",
        "\\newlength\\mywidth\n",
        "\\setlength\\mywidth{", ifelse(landscape, 6, 3.5), "in}\n",
        "\\newcolumntype{B}[2]{>{#1\\hspace{0pt}\\arraybackslash}b{#2}}\n",
        "\\pagestyle{fancy}\n",
        "\\renewcommand{\\headrulewidth}{0pt}\n",
        "\\renewcommand{\\footrulewidth}{0pt}\n",
        "\\fancyhead{}\n",
        "\\fancyhead[L]{{\\Large {\\bf ",
        ifelse(is.null(surveyhead),"",escM(surveyhead)), "}}",
        ifelse(is.null(subhead), "", paste(" \\\\", escM(subhead))), "}\n",
        if (!is.null(logo)) paste0("\\fancyhead[R]{\\includegraphics[scale=.4]{", logo, "}}\n"),
        "\\fancyfoot{}\n",
        "\\fancyfoot[R]{\\thepage}\n",
        "\\setlength{\\parindent}{0pt}",
        "\\usepackage[english]{babel}\n",
        "\\usepackage{caption}\n",
        "\\captionsetup[table]{labelformat=empty}\n")
}
