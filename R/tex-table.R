#' Create Tex of Table Body
#'
#' Prepare the TeX for the table body given initial
#' content and apply theme elements.
#'
#' @param df An object from \link{reformatLatexResults}
#' @param theme A theme object from \link{themeNew}
#' @param question_alias The variable's alias
latexTableBody <- function(df, theme, question_alias = NULL) {
  # The input "df" object is shaped like this:
  # List of 9
  #  $ top            : NULL
  #  $ bottom         : Named chr "unweighted_n"
  #   ..- attr(*, "names")= chr "unweighted_n"
  #  $ data_order     : Named chr [1:2] "body" "unweighted_n"
  #   ..- attr(*, "names")= chr [1:2] "" "unweighted_n"
  #  $ inserts        : NULL
  #  $ data_list      :List of 2
  #   ..$ body        :'data.frame':	3 obs. of  5 variables:
  #   .. ..$ Total   : num [1:3] 25 47 38
  #   .. ..$ 16 to 34: num [1:3] 0 31 44
  #   .. ..$ 35+     : num [1:3] 53 65 33
  #   .. ..$ Male    : num [1:3] 46 28 28
  #   .. ..$ Female  : num [1:3] 0 71 51
  #   ..$ unweighted_n:'data.frame':	1 obs. of  5 variables:
  #   .. ..$ Total   : num 17
  #   .. ..$ 16 to 34: num 6
  #   .. ..$ 35+     : num 11
  #   .. ..$ Male    : num 9
  #   .. ..$ Female  : num 8
  #  $ min_cell_top   : NULL
  #  $ min_cell_body  : logi [1:3, 1:5] NA NA NA NA NA NA ...
  #  $ min_cell_bottom: NULL
  #  $ min_cell       : logi [1:5] FALSE FALSE FALSE FALSE FALSE
  #  - attr(*, "class")= chr [1:2] "MultipleResponseCrossTabVar" "CrossTabVar"

  data <- df$data_list

  # So `data` is a list of data frames

  topline <- theme$topline
  topline_catarray <- inherits(df, "ToplineCategoricalArray")

  dfapply <- function(df, FUN, ..., Names = FALSE) {
    # lapply over columns to alter them and return the data.frame
    # yes yes, just use purrr instead.
    df[] <- lapply(df, FUN, ...)
    if (Names) {
      # Do it to the row and col names too
      names(df) <- FUN(names(df), ...)
      rownames(df) <- FUN(rownames(df), ...)
    }
    df
  }

  for (nm in intersect(c("body", "totals_row"), names(data))) {
    # For each column in these data.frames, round and treat as percentages
    if (!is.null(df$type)) {
      if (df$type == "NumericVariable") {
        data[[nm]] <- dfapply(data[[nm]], formatNum, digits = theme$digits_numeric)
      }
    } else {
      data[[nm]] <- dfapply(data[[nm]], formatNum, digits = theme$digits)
    }

    if (theme$proportions) {
      # Add a percent sign
      if (!is.null(df$type)) {
        # No action becasue it is one of: Numeric, Datetime, or Text
      } else {
        data[[nm]] <- dfapply(data[[nm]], paste0, "%")
        if(any(data[[nm]] == "NA%"))
          data$body[which(data$body == "NA%", arr.ind = TRUE)] <- "-"
      }
    }
  }
  # NPR: this one is doing some wacky things currently
  if (is.null(df$type)) {
    for (nm in intersect(c("unweighted_n", "weighted_n"), names(data))) {
      this_theme <- theme[[paste0("format_", nm)]]

      data[[nm]] <- dfapply(data[[nm]], formatNum)

      if (this_theme$latex_add_parenthesis) {
        data[[nm]] <- dfapply(data[[nm]], paste_around, "(", ")")
      }
      alignment <- this_theme$latex_adjust
      if (!is.null(alignment) && !topline) {
        data[[nm]] <- dfapply(data[[nm]], function(x) {
          # Align these cells
          multicolumn(1, x, align = alignment)
        })
      }
    }
  }

  mask_vars <- c("totals_row", "means", "medians")
  if (!is.null(theme$format_min_base$min_base) && any(df$min_cell_body)) {
    # NPR: I'm not clear on what this does; something about hiding cells
    # with a base size below some threshold?
    if (!is.null(theme$format_min_base$mask)) {
      data$body[df$min_cell_body] <- theme$format_min_base$mask
      for (nm in intersect(mask_vars, names(data))) {
        data[[nm]][, df$min_cell] <- theme$format_min_base$mask
      }
    }
    for (i in which(colSums(df$min_cell_body) != 0)) {
      data$body[df$min_cell_body[, i], i] <- applyLatexStyle(
          data$body[df$min_cell_body[, i], i], theme$format_min_base)
      # nocov start
      for (nm in intersect(mask_vars, names(data))) {
        data[[nm]][, df$min_cell] <- applyLatexStyle(
          data[[nm]][, df$min_cell], theme$format_min_base)
      }
      # nocov end
    }
  }

  # Add more formatting
  headers <- df$inserts %in% "Heading"
  subtotals <- df$inserts %in% "Subtotal"
  for (i in which(headers)) {
    # Apply style to the heading, then blank out the rest of the row
    data$body[i, ] <- ""
    rownames(data$body)[i] <- applyLatexStyle(rownames(data$body)[i], theme$format_headers)
  }
  for (i in which(subtotals)) {
    # Apply subtotal style to the whole row
    data$body[i, ] <- applyLatexStyle(data$body[i, ], theme$format_subtotals)
    rownames(data$body)[i] <- applyLatexStyle(rownames(data$body)[i], theme$format_subtotals)
  }

  # Escape everything
  data <- lapply(data, function(dt) dfapply(dt, texEscape, Names = TRUE))

  # Apply additional styles to whole table sections
  # TODO: add tests for this
  for (nm in intersect(gsub("format_", "", names(theme)), names(data))) {
    data[[nm]] <- dfapply(
      data[[nm]],
      applyLatexStyle,
      theme[[paste0("format_", nm)]],
      Names = TRUE
    )
  }

  # After that formatting has been applied, `data` looks like this:
  # List of 2
  #  $ body        :'data.frame':	3 obs. of  5 variables:
  #   ..$ Total   : chr [1:3] "25%" "47%" "38%"
  #   ..$ 16 to 34: chr [1:3] "0%" "31%" "44%"
  #   ..$ 35+     : chr [1:3] "53%" "65%" "33%"
  #   ..$ Male    : chr [1:3] "46%" "28%" "28%"
  #   ..$ Female  : chr [1:3] "0%" "71%" "51%"
  #  $ unweighted_n:'data.frame':	1 obs. of  5 variables:
  #   ..$ Total   : chr "\\multicolumn{1}{c}{17}"
  #   ..$ 16 to 34: chr "\\multicolumn{1}{c}{6}"
  #   ..$ 35+     : chr "\\multicolumn{1}{c}{11}"
  #   ..$ Male    : chr "\\multicolumn{1}{c}{9}"
  #   ..$ Female  : chr "\\multicolumn{1}{c}{8}"

  # Turn each table in `data` into a LaTeX table string
  if (topline_catarray) {
    if (theme$latex_flip_grids | question_alias %in% theme$latex_flip_specific_grids) {
      data$body <- as.data.frame(t(data$body), check.names = FALSE, stringsAsFactors = FALSE)
    }
    # Apparently you can't have any extra table members for these, only "body"

    # Also, IMPORTANT: cat arrays get displayed transposed. First, paste
    # by column to generate the table rows
    rows <- lapply(data$body, paste, collapse = " & ")
    # Add to each row an empty cell (idk why), the row name (from the df
    # column names), and a newline, and collapse them into a single string
    return(paste(
      "&",
      names(rows),
      "&",
      rows,
      newline,
      collapse = "\n"
    ))
  }

  data <- lapply(data, function(dt) {
    if (topline) {
      # tables have a single column, and we use a dotfill to connect them
      # to the label. Plus there's a leading empty cell, for some reason
      rows <- paste(
        "&",
        rownames(dt),
        "\\hspace*{0.15em} \\dotfill",
        dt[[1]]
      )
    } else {
      rows <- apply(cbind(rownames(dt), dt), 1, paste, collapse = " & ")
    }
    # Add a newline to each row, then join in a single string
    return(paste(rows, newline, collapse = "\n"))
  })

  # Assemble the components of the table, based on "data_order"
  main_table <- data[intersect(c("body", "medians", "means"), df$data_order)]
  footer <- data[intersect(c("totals_row", "weighted_n", "unweighted_n"), df$data_order)]
  if (topline) {
    # Just join them
    out <- paste(c(main_table, footer), collapse = "\n")
  } else {
    # For crosstabs, there should be a separator between the table and the N rows
    out <- paste(c(main_table, "\\midrule", footer), collapse = "\n")
  }

  return(out)
}

#' Format Number
#'
#' Round and format a number, removing what space.
#'
#' @param x A numeric vector
#' @param digits The number of digits
#' @param ... Furth arguments, unused.
formatNum <- function(x, digits = 0, ...) {
  trimws(
    format(
      round(x, digits),
      nsmall = digits,
      big.mark = ","
    )
  )
}

#' Generate a tableHeader
#'
#' A passthrough function that creates a table header appropriate to the class
#' of the data object being passed. Expected classes are:
#' * CrossTabVar
#' * ToplineVar
#' * ToplineCategoricalArray
#'
#' Importantly, this also controls the relative widths of the columns.
#'
#' @md
#' @param var An object of one of the types listed
#' @param theme A theme object from \link{themeNew}
tableHeader <- function(var, theme) {
  UseMethod("tableHeader", var)
}

#' @rdname tableHeader
#' @export
tableHeader.default <- function(var, theme) {
  wrong_class_error(var, c("CrossTabVar", "ToplineVar", "ToplineCategoricalArray"), "tableHeader")
}

#' Header for LongTable with Banner.
#'
#' Title indicates whether the title should be displayed, or not (as in the
#' case of multiple banners displayed underneath each other, the title only
#' appears on the top one).
#' Assumes that `\\banner[a-z]{}` macros are defined in the preamble
#' @param var An object of class CrossTabVar
#' @param theme A theme object from \link{themeNew}
#' @rdname tableHeader
#' @export
tableHeader.CrossTabVar <- function(var, theme) {
  label_width <- ifelse(
    is.na(theme$format_label_column$col_width), "1.5in",
    paste0(theme$format_label_column$col_width, "in"))

  check <- theme$format_label_column_exceptions[var$alias]
  check <- ifelse(is.null(check), NA_real_, check)

  if (!is.na(check)) {
    label_width <- paste0(theme$format_label_column_exceptions[var$alias], "in")
  }

  nopagebreak <- NULL
  if (!theme$pagebreak_in_banner) {
    nopagebreak <- "\\begin{absolutelynopagebreak}"
  }

  header <- paste(
    nopagebreak,
    paste0("\\tbltopa[", label_width, "]"),
    latexTableName(var, theme),
    "\\addlinespace",
    "\\bannera{}",
    "",
    "",
    sep = "\n"
  )
  ntabs <- length(var$crosstabs)
  if (ntabs > 1) {
    # Subsequent banners don't get the same table name at the top, and the
    # negative vspace squeezes them closer to the one above
    next_headers <- paste(
      nopagebreak,
      vspace("-.25in"),
      paste0("\\tbltop", letters[2:ntabs], "[", label_width, "]"),
      "\\addlinespace",
      paste0("\\banner", letters[2:ntabs], "{}"),
      "",
      "",
      sep = "\n"
    )
    header <- c(header, next_headers)
  }
  return(header)
}

#' @rdname tableHeader
#' @param var An object of class ToplineVar
#' @param theme A theme object from \link{themeNew}
#' @export
tableHeader.ToplineVar <- function(var, theme) {
  toplineTableDef(
    var,
    paste0("\\begin{", ifelse(var$longtable, "longtable", "tabular"), "}{p{0.3in}p{5.5in}}"),
    header_row = if (var$longtable) "\\longtablesep",
    theme = theme
  )
}

#' @rdname tableHeader
#' @param var An object of class ToplineCategoricalArray
#' @param theme A theme object from \link{themeNew}
#' @export
tableHeader.ToplineCategoricalArray <- function(var, theme) { # nolint
  header_row <- newline

  if (theme$latex_flip_grids | var$alias %in% theme$latex_flip_specific_grids) {
    col_names <- var$subnames
  } else {
    col_names <- sapply(var$inserts_obj, name)
    if (length(col_names) == 0) {
      col_names <- var$rownames
    }
  }

  col_names_len <- length(col_names)
  col_width <- paste(round(1 / col_names_len, digits = 2), "\\mywidth", sep = "")

  # use heuristic for scale questions
  if (col_names_len >= 10) { # nocov start
    # NPR: Trying to interpret what this code does, it's looking for
    # something like a 0-10 scale where the endpoints are labeled like
    # "0 - Not at all likely" and "10 - Extremely likely" and the middle
    # labels are just the integer scale values. The goal is to create a
    # two-row table header: top row has the prose labels, second row has
    # just the numbers.
    which.split <- grep("^[0-9]+ - ", col_names)
    if (length(which.split) == 2) {
      # Extract the two string labels
      labs <- texEscape(sub("^[0-9]+ - (.*)$", "\\1", col_names[which.split]))
      # Note that this allows for values outside the scale, such as
      # "Don't know". We'll label the scale range within just the part
      # that is the scale, and the DK or other categories on the end will
      # be formatted as appropriate.
      scale_range <- max(which.split) - min(which.split)
      # We're going to divide this range into two multicolumns
      mcwidth <- (scale_range + 1) / 2
      # If there is an odd number, we may have to have a gap cell between
      # the multicolumns
      midgaps <- 1 + ceiling(mcwidth)
      mcwidth <- floor(mcwidth)
      midgaps <- midgaps - mcwidth
      # Left align the the low end label and right align the high end
      labs[1] <- multicolumn(mcwidth, labs[1], align = "l")
      labs[2] <- multicolumn(mcwidth, labs[2], align = "r")
      # Construct the table row for these labels
      scalestart <- paste(rep(" &", min(which.split)), collapse = "")
      scalemid <- paste(rep(" &", midgaps), collapse = "")
      scaleend <- paste(rep(" &", length(col_names) - max(which.split)), collapse = "")
      thisrow <- paste(
        scalestart,
        labs[1],
        scalemid,
        labs[2],
        scaleend,
        newline
      )
      # Append that to the "header_row" we've started collecting
      header_row <- paste(header_row, thisrow, sep = "\n")
      # Now strip those string labels from the column names, and we'll
      # pass those along to the "normal" code path
      col_names <- sub("^([0-9]+) - .*$", "\\1", col_names)
      # I have no idea why col_width is changed here, what this math
      # entails, or why theme$format_label_column$col_width is invoked
      # here but not earlier where col_width is previously defined
      col_width <- paste(round((5.5 - theme$format_label_column$col_width) /
        col_names_len - 0.11, 3), "in", sep = "")

      # nocov end
    }
  }

  if (var$longtable) {
    header_row <- paste(
      header_row,
      # Make a table row of column names (hence joined by &), with some empty
      # cells at the beginning (for row labels?) and end (why?)
      # PT: end has extra column so it remains centered under the question wording.
      paste(c("", "", texEscape(col_names), ""), collapse = " & "),
      "\\endfirsthead",
      paste(multicolumn(col_names_len + 2, italics(theme$latex_headtext)), newline),
      header_row,
      "\\endhead",
      paste(multicolumn(col_names_len + 2, italics(theme$latex_foottext)), newline),
      "\\endfoot",
      "\\endlastfoot",
      sep = "\n"
    )
  } else {
    header_row <- paste0(
      header_row,
      paste(c("", "", texEscape(col_names), ""), collapse = " & "),
      "\\\\\n"
    )
  }

  # Issue # 67: Add smart widths
  if (is.na(theme$format_label_column$col_width)) {
    # \\mywidth == 3.5in
    # page width topline = 6.5in
    # spacing between response column 0.1in
    # minimum width of stub = 1.5in, leaving 5.5in to work with
    # 10 or more response category case, dealt with above

    col_names_adj <- seq(0, 1, length.out = 9)[col_names_len]

    col_width_factor <- seq(0.75, 0.55, length.out = 9)[col_names_len]
    col_width_perc <- round(col_width_factor / 3.5, 2)
    first_col_width <- 6.5 - # page width
      (col_width_factor * col_names_len) - # subtract real col widths
      (0.1 * (col_names_len + col_names_adj)) - # spacing 0.1in per name
      col_width_factor / (9 - col_names_len) - # scaled column_width_factor
      0.275 # match left indent (~1em)

    # Never go below 1.5in
    first_col_width <- ifelse(first_col_width < 1.5, 1.5, first_col_width)

    col_width <- paste(round(col_width_perc, digits = 2), "\\mywidth", sep = "")

    col.header <- paste0("B{\\centering}{", col_width, "}")
    col.header <- paste0(
      paste(rep(col.header, col_names_len + 1), collapse = "")
    )

    tab_definition <- paste0(
      "\\begin{", ifelse(var$longtable, "longtable", "tabular"), "}",
      "{",
      "@{\\extracolsep{\\fill}}",
      "p{0.1in}",
      "B{\\raggedright}{", first_col_width, "in}",
      col.header,
      "}"
    )
  } else {
    if (is.na(theme$format_label_column$col_width)) {
      label_width <- 1.5
    } else {
      # Global override, exception overrules
      label_width <- theme$format_label_column$col_width
    }

    check <- theme$format_label_column_exceptions[var$alias]

    if (!is.na(check) & !is.null(check)) {
      label_width <- theme$format_label_column_exceptions[var$alias]
    }

    col_width <- paste(round(1 / col_names_len, digits = 2), "\\mywidth", sep = "")
    col.header <- paste0("B{\\centering}{", col_width, "}")
    col.header <- paste(rep(col.header, col_names_len + 1), collapse = "")

    tab_definition <- paste0(
      "\\begin{", ifelse(var$longtable, "longtable", "tabular"), "}",
      "{",
      "@{\\extracolsep{\\fill}}",
      "p{0.1in}",
      "B{\\raggedright}{", label_width, "in}",
      col.header,
      "}"
    )
  }

  toplineTableDef(
    var,
    tab_definition,
    header_row,
    theme
  )
}

#' Combine topline data
#'
#' Combines topline data to create the Tex
#' @param var TODO: Identify where this comes from?
#' @param tab_definition Defines a longtable or tabular
#' @param header_row A different header row depending on if its longtable or tabular
#' @param theme A theme object from \link{themeNew}
toplineTableDef <- function(var, tab_definition, header_row, theme) {
  paste(tab_definition, latexTableName(var, theme), header_row, "", sep = "\n")
}

#' Question Text
#'
#' Creates the question text that leads a topline or crosstab
#'
#' @param var A crunch variable
#' @param theme An object created by \link{themeNew}
latexTableName <- function(var, theme) {
  var_info <- getVarInfo(var, theme)
  if (length(var_info) > 0) {
    bg_color <- theme[[names(var_info)[1]]]$background_color
  } else {
    bg_color <- NULL
  }

  if (inherits(var, "ToplineVar")) {
    page_width <- 6.5
  } else {
    page_width <- 9
  }

  # Munge var_info names to match the macros defined in the .tex file
  names(var_info) <- gsub("_", "", names(var_info))
  if (!is.null(var_info$formatvarsubname) && names(var_info)[1] != "formatvarsubname") {
    # That's an em-dash
    var_info[[1]] <- paste0(var_info[[1]], " \u2014 ", var_info$formatvarsubname)
    var_info$formatvarsubname <- NULL
  }
  # if (length(var_info) == 0) {
  #   # TODO: This shouldn't ever happen. User should be warned
  #   warning("Missing variable: ", var$alias)
  #   if(!is.null(bg_color)) {
  #     var_info <- list(formatvarname = paste0("\\color{", bg_color, "}{404}"))
  #   }
  # }
  out <- paste0(
    "\\addcontentsline{lot}{table}{ ", texEscape(var_info[[1]]), "}\n",
    "\\hangindent=0em \\parbox{", page_width, "in}{\n",
    paste0(
      "\\", names(var_info), "{", texEscape(var_info), "}",
      collapse = "\\\\ \n"
    ),
    "}"
  )
  if (!is.null(bg_color)) {
    # Wrap it in a colorbox
    out <- paste0(
      "\\colorbox{", bg_color, "}{\n",
      out,
      "\\hspace*{1ex}}" # Adding horizontal space to match left padding
    )
  }
  return(paste(out, newline))
}

# " Long table header and footer creation.
#'
#'  Generates two macros for the preamble
#'  \\bannera{} that takes one argument (first column label)
#'  \\tbltopa that takes no arguments
#'  If given multiple banners, \\bannerb \\tbltopb, etc are created
#'
#' @param banner A banner object from \link{banner}
#' @param num An integer that later specifies a letter based on the integers intrinsic position
#' @param page_width The width of the page in inches (typically 9 landscape or 6.5 portrait)
#' @param theme A theme object from \link{themeNew}
longtableHeadFootMacros <- function(banner, num, page_width = 9, theme) {
  binfo <- getBannerInfo(banner, theme)
  col_num_sum <- length(unlist(binfo$multicols))

  if (is.na(theme$format_label_column$col_width)) {
    default_width <- 1.5
  } else {
    default_width <- theme$format_label_column$col_width
  }
  banner_width <- round((page_width - default_width) / col_num_sum - .1, 2)
  banner_def_body1 <- makeLatexBanner(binfo, width = banner_width)
  banner_def_body2 <- paste(
    "&",
    multicolumn(col_num_sum, theme$latex_headtext),
    newline,
    banner_def_body1
  )

  return(c(
    # Here is \bannera{}
    # Note that \bannera is never called with an argument, even though it
    # takes one (see below for what would happen if you did give an arg)
    newcommand(
      paste0("banner", letters[num]),
      args = 1, paste(
        "\\toprule",
        banner_def_body1,
        "\\midrule",
        "\\endfirsthead",
        "\\toprule",
        banner_def_body2,
        "\\midrule",
        "\\endhead",
        "\\midrule",
        paste("&", multicolumn(col_num_sum, theme$latex_foottext), newline, ""),
        "\\bottomrule",
        "\\endfoot",
        "\\bottomrule",
        "\\endlastfoot",
        "",
        sep = "\n"
      )
    ),
    # Here is \tbltopa
    newcommand(
      paste0("tbltop", letters[num], "[1]"), # Issue 77
      paste0(
        "\n",
        "\\begin{longtable}",
        "{",
        "@{\\extracolsep{\\fill}}",
        ">{\\hangindent=1em \\PBS \\raggedright \\hspace{0pt}}",
        "b{#1}", # Issue #77
        "*{", col_num_sum, "}",
        "{", theme$latex_table_align, "}",
        "}"
      )
    ),
    ""
  ))
}


#' Make Banner
#'
#' Provides spacing and additional formatting for
#' LaTeX banners.
#'
#' @param binfo banner info from \link{getBannerInfo}
#' @param width Unused argument
makeLatexBanner <- function(binfo, width = NULL) {
  # NPR: This is not used in the function, but maybe it should be, given
  # vague bug reports. Keep it here until we sort that out.
  # m_split <- paste0("}{m{", width, "in}}{\\centering ")

  # The top row are the variable names
  first_row <- paste(
    " &",
    multicolumn(binfo$len, "\\bf ", texEscape(binfo$names)),
    collapse = ""
  )
  # Add a newline
  first_row <- paste(first_row, newline)
  # Now add a bunch of horizontal rules underneath the headers, grouping the
  # category names in the second row under the variable names in the first
  start_cols <- binfo$multicols_csum[2:(length(binfo$multicols_csum) - 1)]
  end_cols <- binfo$multicols_csum[3:length(binfo$multicols_csum)] - 1
  # cmidrule() is vectorized over those integers, so we need to paste(collapse)
  first_row <- paste(
    c(first_row, cmidrule(start_cols, end_cols)),
    collapse = " "
  )

  # The second row has the category names
  second_row <- paste(
    " &",
    multicolumn(1, texEscape(unlist(binfo$multicols))),
    collapse = ""
  )
  # Add a variable anchor to the beginning of the second row (this is where
  # the argument to \bannera{} would go, if anyone supplied it)
  # and end with a newline
  second_row <- paste("{\\bf #1}", second_row, newline)

  # Assemble the full "banner"
  return(paste(
    first_row,
    second_row,
    sep = "\n"
  ))
}


#' Calculate number of lines
#'
#' Calculates the length of the table depending on
#' the type of data provided as well as some theme
#' information
#'
#' @param x An object of type: CrossTabVar, ToplineVar, or ToplineCategoricalArray
#' @param theme An object created by \link{themeNew}
calculateIfLongtable <- function(x, theme) {
  UseMethod("calculateIfLongtable", x)
}

#' @rdname calculateIfLongtable
#' @export
calculateIfLongtable.default <- function(x, theme) {
  wrong_class_error(
    x,
    c("CrossTabVar", "ToplineVar", "ToplineCategoricalArray"), "calculateIfLongtable")
}

#' @rdname calculateIfLongtable
#' @export
calculateIfLongtable.CrossTabVar <- function(x, theme) { # nolint
  return(sum(ceiling(nchar(x$rownames) / 25)) >
    theme$latex_max_lines_for_tabular)
}

#' @rdname calculateIfLongtable
#' @export
calculateIfLongtable.ToplineVar <- function(x, theme) { # nolint
  return(sum(ceiling(nchar(x$rownames) / 90)) >
    theme$latex_max_lines_for_tabular)
}

#' @rdname calculateIfLongtable
#' @export
calculateIfLongtable.ToplineCategoricalArray <- function(x, theme) { # nolint
  return(sum(ceiling(nchar(x$rownames) / 25)) >
    theme$latex_max_lines_for_tabular)
}
