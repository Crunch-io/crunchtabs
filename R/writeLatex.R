#' Generate LaTeX Reports: Toplines and Banners
#'
#' \code{writeLatex} produces publication-quality LaTeX reports:
#' Toplines (one-way frequency tables) or Banners (cross tabulations).
#'
#' @param data_summary An object of class \code{Toplines} or \code{Crosstabs}.
#' @param filename character. The name of the output file (without extension).
#' @param title An optional title. Defaults to the data summary title.
#' @param subtitle An optional character subtitle. Defaults to an empty string.
#' @param pdf logical. Compile LaTeX using pdflatex? Implemented only on MacOS/Linux.
#' @param open logical. If PDF document was produced, open it with
#' the default application? Only implemented for MacOS.
#' @param proportions logical. If \code{TRUE} the output report shows proportions,
#' if \code{FALSE} the report shows counts.
#' @param table_of_contents logical. Should a list of tables be included at the start
#' of the report Defaults to \code{FALSE}.
#' @param sample_desc A character string describing the sample.
#' @param field_period A character string describing the field period.
#' @param moe An optional numeric margin of error.
#' @param append_text An optional character string that, if supplied, will be appended after
#' the final table. Useful for adding in disclosure information. Defaults to an empty string.
#'
#' @param theme
#'
#' @return If \code{returndata} is set to \code{TRUE}, a processed data that was used to produce
#' the report is returned. Otherwise \code{NULL} is returned.
#' @examples
#' \dontrun{
#' # toplines report
#' toplines_summary <- crosstabs(crunch_dataset, weight = 'weight')
#' writeLatex(toplines_summary, 'filename')
#' # crosstabs report
#' crosstabs_summary <- crosstabs(crunch_dataset, banner = banner_object)
#' writeLatex(crosstabs_summary, 'filename')
#' }
#' @export
writeLatex <- function(data_summary, theme = themeDefaultLatex(),
    filename = getName(data_summary), title = getName(data_summary),
    subtitle = NULL, table_of_contents = FALSE, sample_desc = NULL,
    field_period = NULL, moe = NULL, append_text = NULL, proportions = TRUE,
    pdf = FALSE, open = FALSE, logging = FALSE) {

    if (pdf && is.null(filename)) {
        stop("Please provide a file name to generate PDF output.", call.=FALSE)
    }
    theme_validator(theme)

    wrong_class_error(data_summary, "CrunchTabs", "data_summary")
    if (!any(c("Toplines", "Crosstabs") %in% class(data_summary))) {
        stop("The expected class for `data_summary` is either Toplines, CrunchTabs or Crosstabs CrunchTabs, not ", collapse_items(class(data_summary)))
    }

    topline <- is(data_summary, "Toplines")
    if (is.null(theme$font_size)) {
        theme$font_size <- 12
    }
    theme$proportions <- proportions

    # Build the tables
    headers <- lapply(data_summary$results, tableHeader, theme = theme)

    data_summary$results <- lapply(data_summary$results, removeInserts, theme)
    results <- reformatLatexResults(data_summary, proportions = proportions, theme = theme)
    bodies <- lapply(results, function (x)
        sapply(x, latexTableBody, theme = theme, topline = topline))
    table_footer <- "\n\\end{longtable}\n\n"
    table_bodies <- sapply(seq_along(data_summary$results), function(i) {
        paste(
            headers[[i]],
            bodies[[i]],
            table_footer,
            sep="\n",
            collapse="\n"
        )
    })
    if (topline) {
        # Topline tables are centered (probably should be a theme option?)
        # Also munge newline whitespace to match status quo output
        table_bodies <- sub("\\\n\\\n$", "", table_bodies)
        table_bodies <- center(table_bodies)
        table_bodies <- paste0(table_bodies, "\n\n")
    }
    if (theme$one_per_sheet) {
        table_bodies <- paste0(table_bodies, "\n\\clearpage")
    }

    out <- latexDocHead(
        theme = theme,
        title = title,
        subtitle = subtitle,
        topline = topline
    )
    if (!topline) {
        out <- c(out, sapply(seq_along(data_summary$banner), function (j) {
            longtableHeadFootB(data_summary$banner[[j]], num = j, page_width = 9,
                theme = theme)
        }))
    }
    out <- c(
        out,
        latexStart(table_of_contents = table_of_contents, sample_desc = sample_desc,
            field_period = field_period, moe = moe, font_size = theme$font_size),
        table_bodies,
        append_text,
        latexDocFoot()
    )
    if (!is.null(filename)) {
        filename <- paste0(filename, ".tex")
        cat(out, sep = "\n", file = filename)
        if (pdf) {
            if (logging) {
                print("PDF-ing")
            }
            pdflatex(filename, open)
        }
    }
    return(invisible(data_summary))
}

latexTableBody <- function(df, theme, topline) {
    data <- df$data_list
    for (nm in intersect(c("body", "totals_row"), names(data))) {
        data[[nm]][] <- round(data[[nm]], theme$digits)
        data[[nm]][] <- format(data[[nm]], nsmall=theme$digits, big.mark=",")
        data[[nm]][] <- apply(data[[nm]], 2, trimws)
        if (theme$proportions) {
            data[[nm]][] <- apply(data[[nm]], 2, paste0, "%")
        }
    }
    for (nm in intersect(c("unweighted_n", "weighted_n"), names(data))) {
        nm2 <- paste0("format_", nm)
        data[[nm]][] <- trimws(format(data[[nm]], big.mark=","))
        if (theme[[nm2]]$latex_add_parenthesis) {
            data[[nm]][] <- apply(data[[nm]], 2, paste_around, "(", ")")
        }
        if (!is.null(theme[[nm2]]$latex_adjust) && !topline) {
            data[[nm]][] <- apply(data[[nm]], 2, paste_around,
                paste0("\\multicolumn{1}{", theme[[nm2]]$latex_adjust, "}{"), "}")
        }
    }

    mask_vars <- c("totals_row", "means", "medians")
    if (!is.null(theme$format_min_base$min_base) && any(df$min_cell_body)) {
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

    data <- lapply(data, function(dt) {
        matrix(apply(data.frame(rownames(dt), dt, stringsAsFactors = FALSE), 2, texEscape),
            nrow = nrow(dt))
    })

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
        data$body[i, 2:ncol(data$body)] <- ""
        data$body[i, ] <- applyLatexStyle(data$body[i, ], theme$format_headers)
    }
    for (i in which(df$inserts %in% c("Subtotal"))) {
        data$body[i, ] <- applyLatexStyle(data$body[i, ], theme$format_subtotals)
    }

    collapsestring <- "\\\\\n"
    if (topline && ncol(data$body) == 2) {
        sepstring <- " \\hspace*{0.15em} \\dotfill "
    } else {
        sepstring <- " & "
    }
    data <- lapply(data, function(dt) {
        paste(
            paste(
                paste0(
                    if (topline) " & ",
                    apply(dt, 1, paste, collapse = sepstring)
                ),
                collapse = collapsestring),
            collapsestring
        )
    })

    if (is(df, "ToplineCategoricalArray")) {
        df$data_order <- "body"
    }
    paste(
        paste0(
            data[intersect(c("body", "medians", "means"), df$data_order)],
            collapse = ""
        ),
        if (!topline) "\\midrule",
        paste0(
            data[intersect(c("totals_row", "weighted_n", "unweighted_n"), df$data_order)],
            collapse = ""
        )
    )
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

latexDocHead <- function (theme, title, subtitle, topline) {
    title <- texEscape(title)
    subtitle <- texEscape(subtitle)
    if (nchar(subtitle)) {
        # If there is one, precede it by a newline
        subtitle <- paste("", newline, subtitle)
    }

    bdr <- ifelse(topline, 1, 0.5)
    logo <- theme$logo$file
    if (!is.null(logo)) {
        logo <- paste0("\\fancyhead[R]{\\includegraphics[scale=.4]{", logo, "}}\n")
    }

    paste0("\\documentclass", if (!topline) { "[landscape]" }, "{article}\n",
        "\\usepackage[pdftex]{graphicx}\n",
        "\\usepackage[utf8]{inputenc}\n",
        "\\usepackage{fancyhdr}\n",
        "\\usepackage{sfmath}\n",
        "\\usepackage{comment}\n",
        "\\usepackage[T1]{fontenc}\n",
        "\\usepackage[pdftex=true, pdftoolbar=true, pdfmenubar=true, pdfauthor = {},",
            "pdfcreator = {PDFLaTeX}, pdftitle = {}, colorlinks=true, urlcolor=blue,",
            "linkcolor=blue, citecolor=blue, implicit=true, hypertexnames=false]{hyperref}\n",
        "\\usepackage[scaled]{", validLatexFont(theme$font), "}\n",
        "\\renewcommand*\\familydefault{\\sfdefault}\n",
        "\\usepackage{booktabs, dcolumn, longtable}\n",
        "\\usepackage[top=0.6in, bottom=0.6in, left=", bdr,
            "in, right=", bdr, "in, includeheadfoot]{geometry}\n",
        "\\usepackage{array}\n",
        "\\usepackage[english]{babel}\n",
        "\\newcolumntype{B}[2]{>{#1\\hspace{0pt}\\arraybackslash}b{#2}}\n",
        "\\setlength{\\parindent}{0pt}\n",
        "\\usepackage[dvipsnames]{color}\n",
        "\\definecolor{gray}{gray}{0.85}\n",
        "\\pagestyle{fancy}\n",
        "\\renewcommand{\\headrulewidth}{0pt}\n",
        "\\renewcommand{\\footrulewidth}{0pt}\n",
        "\\fancyhead{}\n",
        "\\fancyhead[L]{{", applyLatexStyle(title, theme$format_title), "}",
            applyLatexStyle(subtitle, theme$format_subtitle), "}\n",
        logo,
        "\\newcolumntype{d}{D{.}{.}{3.2}}\n", #!topline
        "\\newcolumntype{g}{D{\\%}{\\%}{5.0}}\n", #!topline
        "\\usepackage{float}\n", #topline
        "\\usepackage{marginnote}\n", #topline
        "\\setlength\\extrarowheight{2pt}\n", #topline
        "\\newlength\\mywidth\n", #topline
        "\\setlength\\mywidth{3.5in}\n", #topline
        "\\usepackage{caption}\n", #topline
        "\\captionsetup[table]{labelformat=empty}\n", #topline
        "\\renewcommand*{\\marginfont}{\\scriptsize\\itshape}\n", #topline
        "\\fancyfoot{}\n",
        "\\fancyfoot[R]{\\thepage}\n",
        "\\newcommand{\\PreserveBackslash}[1]{\\let\\temp=\\\\#1\\let\\\\=\\temp}\n",
        "\\let\\PBS=\\PreserveBackslash\n",
        "\\newcommand{\\longtablesep}{\\endfirsthead ",
            multicolumn(2, italics(texEscape(theme$latex_headtext))), " \\\\ \\endhead ",
            multicolumn(2, italics(texEscape(theme$latex_foottext))), " \\\\ \\endfoot \\endlastfoot}\n",
        "\\usepackage[titles]{tocloft}\n",
        "\\newcommand{\\cftchapfont}{", fontsize(theme$font_size), "}\n",
        "\\newcommand{\\formatvardescription}[1]{", applyLatexStyle("#1", theme$format_var_description), "}\n",
        "\\newcommand{\\formatvarname}[1]{", applyLatexStyle("#1", theme$format_var_name), "}\n",
        "\\newcommand{\\formatvaralias}[1]{", applyLatexStyle("#1", theme$format_var_alias), "}\n",
        "\\newcommand{\\formatvarfiltertext}[1]{", applyLatexStyle("#1", theme$format_var_filtertext), "}\n",
        "\\newcommand{\\formatvarsubname}[1]{", applyLatexStyle("#1", theme$format_var_subname), "}\n",
        "\n\n\n\n\n")
}


latexStart <- function(table_of_contents, sample_desc, field_period, moe, font_size) {
    if (!is.null(sample_desc)) {
        sample_desc <- paste("Sample  & ", sample_desc, "\\\\ \n ")
    }
    if (!is.null(moe)) {
        moe <- paste("Margin of Error &  $\\pm ", round(100 * moe, digits = 1), "\\%$ \\\\ \n")
    }
    if (!is.null(field_period)) {
        field_period <- paste("Conducted  & ", field_period, "\\\\ \n")
    }
    return(paste0(
        "\\begin{document}\n",
        "\\begin{tabular}{ll}\n",
        sample_desc,
        field_period,
        moe,
        "\\end{tabular}\n",
        ifelse(table_of_contents, "\\listoftables\n\\newpage", ""),
        "\n\n{",
        "\\setlength{\\LTleft}{0pt}\n",
        "\\setlength{\\LTright}{\\fill}\n",
        "\\setlength{\\LTcapwidth}{\\textwidth}\n\n\n",
        "%% here's where individual input starts %%\n\n\n \\vspace{.25in} \n\n"
    ))
}

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
    tab_definition <- "\\begin{longtable}{p{0.3in}p{5.5in}}"
    toplineTableDef(var, tab_definition, header_row = "\\longtablesep\n", theme = theme)
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
            labs[1] <- paste("\\multicolumn{", mcwidth, "}{l}{", labs[1], "}", collapse = "")
            labs[2] <- paste("\\multicolumn{", mcwidth, "}{r}{", labs[2], "}", collapse = "")
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
    header_row <- paste(header_row, "\n\\endfirsthead\n\\multicolumn{",
        col_names_len + 2, "}{c}{", italics(theme$latex_headtext), "} \\\\",
        header_row, "\\endhead\n\\multicolumn{", col_names_len + 2,
        "}{c}{", italics(theme$latex_foottext), "} \\\\ \n\\endfoot\n\\endlastfoot\n",
        sep = "")
    col.header <- paste("B{\\centering}{", col_width, "}", sep = "")
    col.header <- paste(rep(col.header, col_names_len+1), collapse = "")
    tab_definition <- paste0("\\begin{longtable}{@{\\extracolsep{\\fill}}p{0.1in}B{\\raggedright}{",
        theme$format_label_column$col_width, "in}", col.header, "}")

    toplineTableDef(var, tab_definition, header_row, theme)
}


latexTableName <- function(var, theme) {
    var_info <- getVarInfo(var, theme)
    bg_color <- theme[[names(var_info)[[1]]]]$background_color
    col <- if (!is.null(bg_color)) {
        paste0("\\colorbox{", bg_color, "}{\n")
    } else {
        NULL
    }
    if (!is.null(var_info$format_var_subname) && names(var_info)[1] != "format_var_subname") {
        # NPR: Is this dash character valid?
        var_info[[1]] <- paste0(var_info[[1]], " \u2014 ", var_info$format_var_subname)
        var_info$format_var_subname <- NULL
    }
    if (length(var_info) == 0) {
        # NPR: I don't see how this makes sense (or valid TeX)
        var_info <- list(format_var_name = paste0("\\color{", col, "}{404}"))
    }
    out <- paste0(
        "\\addcontentsline{lot}{table}{ ", texEscape(var_info[[1]]), "}\n",
        "\\hangindent=0em \\parbox{", ifelse(inherits(var, "ToplineVar"), "6.5", "9"), "in}{\n",
        paste0("\\", gsub("_", "", names(var_info)), "{", texEscape(var_info), "}", collapse = "\\\\ \n"),
        "}"
    )
    if (!is.null(col)) {
        # Wrap it in color
        out <- paste0(col, out, "}")
    }
    return(paste0(out, " \\\\"))
}

latexDocFoot <- function() "\n}\n\\end{document}\n"

# Crosstabs ---------------------------------------------------------------


# Long table header and footer creation.
# Generates two macros for the preamble
# \bannera{} that takes one argument (first column label)
# \tbltopa that takes no arguments
# If given multiple banners, \bannerb \tbltopb, etc are created
longtableHeadFootB <- function (banner, num, page_width = 9, theme) {
    binfo <- getBannerInfo(banner, theme)
    col_num_sum <- length(unlist(binfo$multicols))

    banner_def_head <- paste0("\\newcommand{\\banner", letters[num],"}[1]{")

    banner_def_body <- rep(makeLatexBanner(binfo,
        width = round((page_width - theme$format_label_column$col_width)/col_num_sum-.1, 2),
        theme = theme), 2)
    banner_def_body[2] <- paste(
        "&",
        multicolumn(col_num_sum, theme$latex_headtext),
        newline,
        banner_def_body[2]
    )
    banner_def_body <- paste("\\toprule", banner_def_body, sep="\n",
        collapse="\\endfirsthead \n")

    banner_def_foot <- paste0("\\endhead \n\\midrule \n& \\multicolumn{",
        col_num_sum, "}{c}{", theme$latex_foottext, "} \\\\ \n",
        "\\bottomrule \n\\endfoot \n\\bottomrule \n\\endlastfoot \n}\n")

    table_def <- paste0("\\newcommand{\\tbltop", letters[num], "}{\n",
        "\\begin{longtable}{@{\\extracolsep{\\fill}}>{\\hangindent=1em \\PBS ",
        "\\raggedright \\hspace{0pt}}b{", theme$format_label_column$col_width,
        "in}*{", col_num_sum, "}{", theme$latex_table_align, "}}}\n")
    return(paste0(banner_def_head, banner_def_body, banner_def_foot, table_def))
}

makeLatexBanner <- function (binfo, width=NULL, theme) {
    # NPR: This is not used in the function, but maybe it should be, given
    # vague bug reports. Keep it here until we sort that out.
    # m_split <- paste0("}{m{", width,"in}}{\\centering ")

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

    # Assemble the full "banner", including a full horizontal rule underneath it
    # (and an extra whitespace newline, because)
    ban <- c(first_row, second_row, "\\midrule ", "")
    return(paste(ban, collapse = "\n"))
}


# Toplines ----------------------------------------------------------------


toplineTableDef <- function(var, tab_definition, header_row, theme) {
    paste0(
        tab_definition, "\n",
        latexTableName(var, theme),
        header_row
    )
}
