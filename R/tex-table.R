latexTableBody <- function(df, theme, topline) {
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
    for (nm in intersect(c("body", "totals_row"), names(data))) {
        # For each column in these data.frames, round and treat as percentages
        data[[nm]] <- format(
            round(data[[nm]], theme$digits),
            nsmall=theme$digits,
            big.mark=","
        )
        if (theme$proportions) {
            data[[nm]][] <- lapply(data[[nm]], function (x) {
                paste0(trimws(x), "%")
            })
        }
    }
    # NPR: this one is doing some wacky things currently
    for (nm in intersect(c("unweighted_n", "weighted_n"), names(data))) {
        nm2 <- paste0("format_", nm)
        data[[nm]][] <- trimws(format(data[[nm]], big.mark=","))
        if (theme[[nm2]]$latex_add_parenthesis) {
            data[[nm]][] <- apply(data[[nm]], 2, paste_around, "(", ")")
        }
        if (!is.null(theme[[nm2]]$latex_adjust) && !topline) {
            # NPR: this is the part that is printing weird stuff:
            # \midrule Unweighted N: Min & \multicolumn{1}{c}{c(" 8", "11")} & \multicolumn{1}{c}{c("1", "2")} & \multicolumn{1}{c}{c(" 8", "11")} & \multicolumn{1}{c}{c("1", "2")}\\
            # Unweighted N: Max & \multicolumn{1}{c}{c("2", "4")} & \multicolumn{1}{c}{c("4", "7")} & \multicolumn{1}{c}{c("2", "4")} & \multicolumn{1}{c}{c("4", "7")} \\
            data[[nm]][] <- apply(data[[nm]], 2, paste_around,
                paste0("\\multicolumn{1}{", theme[[nm2]]$latex_adjust, "}{"), "}")
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
            data$body[df$min_cell_body[,i], i] <- applyLatexStyle(data$body[df$min_cell_body[,i], i], theme$format_min_base)
            for (nm in intersect(mask_vars, names(data))) {
                data[[nm]][, df$min_cell] <- applyLatexStyle(data[[nm]][, df$min_cell], theme$format_min_base)
            }
        }
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
    data <- lapply(data, function(dt) {
        matrix(apply(data.frame(rownames(dt), dt, stringsAsFactors = FALSE), 2, texEscape),
            nrow = nrow(dt))
    })
    # That step turns everything into a character matrix, including with rownames
    # List of 2
    #  $ body        : chr [1:3, 1:6] "Cat" "Dog" "Bird" "25\\%" ...
    #  $ unweighted_n: chr [1, 1:6] "Unweighted N" "\\multicolumn{1}{c}{17}" "\\multicolumn{1}{c}{6}" "\\multicolumn{1}{c}{11}" ...

    # Apply additional styles to the whole table
    # TODO: this can probably be simplified greatly, need tests first
    for (nm in intersect(gsub("format_", "", names(theme)), names(data))) {
        data[[nm]][] <- apply(data[[nm]], 2, applyLatexStyle, theme[[paste0("format_", nm)]])
    }

    # TODO: this code assumes that categories are along the rows, but for a
    # topline categorical array with subtotals, they're across the columns
    # $inserts
    # [1] "Category" "Category" "Category" "Subtotal"
    #
    # $data_list
    # $data_list$body
    #      Cat Dog Bird Net: Cat/Dog
    # Home  49  43    9           92
    # Work  42  37   21           79
    for (i in which(df$inserts %in% c("Heading"))) {
        # Apply style to the heading (col 1), then blank out the rest of the row
        data$body[i, 2:ncol(data$body)] <- ""
        # TODO: this should be [i, 1]
        data$body[i, ] <- applyLatexStyle(data$body[i, ], theme$format_headers)
    }
    for (i in which(df$inserts %in% c("Subtotal"))) {
        # Apply subtotal style to the whole row
        data$body[i, ] <- applyLatexStyle(data$body[i, ], theme$format_subtotals)
    }

    # Turn each table in `data` into a LaTeX table string
    if (topline && ncol(data$body) == 2) {
        sepstring <- " \\hspace*{0.15em} \\dotfill "
    } else {
        sepstring <- " & "
    }
    data <- lapply(data, function(dt) {
        rows <- apply(dt, 1, paste, collapse = sepstring)
        if (topline) {
            rows <- paste0(" & ", rows)
        }
        # Add a newline and a carriage return to each row, then join in a single string
        return(paste0(rows, " ", newline, "\n", collapse=""))
    })

    # Assemble the components of the table, based on "data_order"
    if (is(df, "ToplineCategoricalArray")) {
        # Apparently you can't have any extra table members for these
        df$data_order <- "body"
    }
    main_table <- paste(
        data[intersect(c("body", "medians", "means"), df$data_order)],
        collapse=""
    )
    footer <- paste(
        data[intersect(c("totals_row", "weighted_n", "unweighted_n"), df$data_order)],
        collapse=""
    )
    if (nchar(footer)) {
        if (topline) {
            # Just join them
            out <- paste(main_table, footer)
        } else {
            # For crosstabs, there should be a separator between the table and the N rows
            out <- paste(main_table, "\\midrule", footer)
        }
    } else {
        # The main table is all there is
        out <- main_table
    }

    # This produces a single string that looks like:
    #
    # Cat & 25\% & 0\% & 53\% & 46\% & 0\%\\
    # Dog & 47\% & 31\% & 65\% & 28\% & 71\%\\
    # Bird & 38\% & 44\% & 33\% & 28\% & 51\% \\
    #  \midrule Unweighted N & \multicolumn{1}{c}{17} & \multicolumn{1}{c}{6} & \multicolumn{1}{c}{11} & \multicolumn{1}{c}{9} & \multicolumn{1}{c}{8} \\
    return(out)
}

texTable <- function (df, sep=" & ", collapse=paste0(newline, "\n")) {
    # TODO: use this
    do.call(paste, c(df, sep=sep, collapse=collapse))
}


tableHeader <- function(x, theme) {
    UseMethod("tableHeader", x)
}

#' @export
tableHeader.default <- function(x) {
    wrong_class_error(x, c("CrossTabVar", "ToplineVar", "ToplineCategoricalArray"), "getName")
}

# Header for LongTable with Banner.
# Title indicates whether the title should be displayed, or not (as in the
# case of multiple banners displayed underneath each other, the title only
# appears on the top one).
# Assumes that \banner[a-z]{} macros are defined in the preamble
#' @export
tableHeader.CrossTabVar <- function(var, theme) {
    sapply(seq_along(var$crosstabs), function(num) {
        paste(
            if (num != 1) "\\vspace{-.25in}",
            "\\tbltop", letters[num], "\n",
            if (num == 1) latexTableName(var, theme),
            "\\addlinespace \n",
            "\\banner", letters[num],"{} \n\n",
            sep=""
        )
    })
}

#' @export
tableHeader.ToplineVar <- function(var, theme) {
    toplineTableDef(
        var,
        "\\begin{longtable}{p{0.3in}p{5.5in}}",
        header_row = "\\longtablesep\n",
        theme = theme
    )
}

#' @export
tableHeader.ToplineCategoricalArray <- function(var, theme) {
    header_row <- "\n"
    col_names <- sapply(var$inserts_obj, name)
    col_names_len <- length(col_names)
    col_width <- paste(round(1/col_names_len, digits = 2), "\\mywidth", sep = "")
    # use heuristic for scale questions
    if (col_names_len >= 10) {
        which.split <- grep("^[0-9]+ - ", col_names)
        if (length(which.split) == 2) {
            labs <- texEscape(sub("^[0-9]+ - (.*)$", "\\1", col_names[which.split]))
            mcwidth <- (max(which.split) - min(which.split) + 1)/2
            midgaps <- 1 + ceiling(mcwidth)  ## in case it's an odd number
            mcwidth <- floor(mcwidth)
            midgaps <- midgaps - mcwidth
            labs[1] <- multicolumn(mcwidth, labs[1], align="l")
            labs[2] <- multicolumn(mcwidth, labs[2], align="r")
            scalestart <- paste(rep(" &", min(which.split)), collapse = "")
            scalemid <- paste(rep(" &", midgaps), collapse = "")
            scaleend <- paste(rep(" &", length(col_names) - max(which.split)), collapse = "")
            thisrow <- paste(scalestart, labs[1], scalemid, labs[2], scaleend, "\\\\")
            header_row <- paste(header_row, thisrow, "\n")
            col_names <- sub("^([0-9]+) - .*$", "\\1", col_names)
            col_width <- paste(round((5.5 - theme$format_label_column$col_width)/
                    col_names_len - 0.11, 3), "in", sep = "")
        }
    }
    header_row <- paste(newline, header_row, "& &", paste(texEscape(col_names), collapse = " & "), " & \\\n")
    header_row <- paste(
        header_row,
        "\\endfirsthead",
        paste(multicolumn(col_names_len + 2, italics(theme$latex_headtext)), newline),
        header_row,
        "\\endhead",
        paste(multicolumn(col_names_len + 2, italics(theme$latex_foottext)), newline),
        "\\endfoot",
        "\\endlastfoot",
        "",
        sep = "\n"
    )
    col.header <- paste0("B{\\centering}{", col_width, "}")
    col.header <- paste(rep(col.header, col_names_len + 1), collapse = "")
    tab_definition <- paste0("\\begin{longtable}{@{\\extracolsep{\\fill}}p{0.1in}B{\\raggedright}{",
        theme$format_label_column$col_width, "in}", col.header, "}")

    toplineTableDef(
        var,
        tab_definition,
        header_row,
        theme
    )
}


latexTableName <- function(var, theme) {
    var_info <- getVarInfo(var, theme)
    bg_color <- theme[[names(var_info)[1]]]$background_color
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
    if (length(var_info) == 0) {
        # NPR: I guess this is a fallback to print 404 if there's no variable
        # metadata? Is this likely even valid TeX?
        var_info <- list(formatvarname = paste0("\\color{", bg_color, "}{404}"))
    }
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
            "}" # Should put a \n before this
        )
    }
    return(paste(out, newline))
}

# Crosstabs ---------------------------------------------------------------


# Long table header and footer creation.
# Generates two macros for the preamble
# \bannera{} that takes one argument (first column label)
# \tbltopa that takes no arguments
# If given multiple banners, \bannerb \tbltopb, etc are created
longtableHeadFootB <- function (banner, num, page_width = 9, theme) {
    binfo <- getBannerInfo(banner, theme)
    col_num_sum <- length(unlist(binfo$multicols))

    banner_width <- round((page_width - theme$format_label_column$col_width)/col_num_sum-.1, 2)
    banner_def_body1 <- makeLatexBanner(binfo, width = banner_width)
    banner_def_body2 <- paste(
        "&",
        multicolumn(col_num_sum, theme$latex_headtext),
        newline,
        banner_def_body1
    )

    return(paste(
        # Here is \bannera
        paste0(
            "\\newcommand{\\banner", letters[num],"}[1]",
            "{",
            "\\toprule"
        ),
            banner_def_body1,
            "\\midrule ",
            "\\endfirsthead ",
            "\\toprule",
            banner_def_body2,
            "\\midrule ",
            "\\endhead ",
            "\\midrule ",
            paste("&", multicolumn(col_num_sum, theme$latex_foottext), newline, ""),
            "\\bottomrule ",
            "\\endfoot ",
            "\\bottomrule ",
            "\\endlastfoot ",
        "}",
        # Here is \tbltopa
        paste0(
            "\\newcommand{\\tbltop", letters[num], "}",
            "{"
        ),
            # Indendation here just to show what is inside the {} of newcommand
            paste0(
                "\\begin{longtable}{@{\\extracolsep{\\fill}}>{\\hangindent=1em \\PBS ",
                "\\raggedright \\hspace{0pt}}b{", theme$format_label_column$col_width,
                "in}*{", col_num_sum, "}{", theme$latex_table_align, "}}",
            "}"),
        "",
        sep="\n"
    ))
}

makeLatexBanner <- function (binfo, width=NULL) {
    # NPR: This is not used in the function, but maybe it should be, given
    # vague bug reports. Keep it here until we sort that out.
    # m_split <- paste0("}{m{", width, "in}}{\\centering ")

    # The top row are the variable names
    first_row <- paste(
        " &",
        multicolumn(binfo$len, "\\bf ", texEscape(binfo$names)),
        collapse=""
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
        collapse=""
    )
    # Add a variable anchor to the beginning of the second row (this is for the
    # table of contents?) and end with a newline
    second_row <- paste("{\\bf #1}", second_row, newline)

    # Assemble the full "banner"
    return(paste(
        first_row,
        second_row,
        sep = "\n"
    ))
}


# Toplines ----------------------------------------------------------------


toplineTableDef <- function(var, tab_definition, header_row, theme) {
    paste0(
        tab_definition, "\n",
        latexTableName(var, theme),
        header_row
    )
}