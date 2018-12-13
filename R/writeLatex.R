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

    theme$topline <- topline <- is(data_summary, "Toplines")
    if (is.null(theme$font_size)) {
        theme$font_size <- 12
    }
    theme$proportions <- proportions

    # Build the tables
    # NPR: `data_summary$results` is a list of lists of objects. Each element
    # will be a table thing, but each of those tables may contain more than one
    # table body when it's a crosstab with multiple banners.
    table_bodies <- lapply(data_summary$results, function (x) {
        headers <- tableHeader(x, theme)
        # Do some munging and generate the table bodies to match those headers
        x <- removeInserts(x, theme)
        content <- reformatLatexResults(x, data_summary$banner, theme)
        bodies <- sapply(content, latexTableBody, theme = theme)
        # This paste will collapse the perhaps multiple banner tables into a
        # single string of LaTeX
        paste(
            headers,
            bodies,
            "",
            "\\end{longtable}", # Table footer
            sep="\n",
            collapse="\n\n\n"
        )
    })
    if (topline) {
        # Topline tables are centered (probably should be a theme option?)
        table_bodies <- center(table_bodies)
    }
    if (theme$one_per_sheet) {
        table_bodies <- paste0(table_bodies, "\n\\clearpage")
    }
    # Put some space between each table
    table_bodies <- paste0(table_bodies, "\n\n")

    # Now assemble the .tex document
    head <- latexDocHead(
        theme = theme,
        title = title,
        subtitle = subtitle,
        topline = topline
    )
    if (!topline) {
        # Generate the banner definition macros in the header so they can
        # be reused on each page
        head <- c(
            head,
            sapply(seq_along(data_summary$banner), function (j) {
                longtableHeadFootMacros(
                    data_summary$banner[[j]],
                    num = j,
                    page_width = 9,
                    theme = theme
                )
            })
        )
    }

    out <- c(
        head,
        document(
            latexStart(
                table_of_contents = table_of_contents,
                sample_desc = sample_desc,
                field_period = field_period,
                moe = moe,
                font_size = theme$font_size
            ),
            table_bodies,
            append_text,
            "",
            "}"
        )
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

latexDocHead <- function (theme, title, subtitle, topline) {
    title <- texEscape(title)
    subtitle <- texEscape(subtitle)
    if (nchar(subtitle)) {
        # If there is one, precede it by a newline
        subtitle <- paste("", newline, subtitle)
    }

    bdr <- ifelse(topline, "1in", "0.5in")
    logo <- theme$logo$file
    if (!is.null(logo)) {
        logo <- paste0("\\fancyhead[R]{\\includegraphics[scale=.4]{", logo, "}}")
    }
    if (topline) {
        doc_class <- "\\documentclass{article}"
    } else {
        doc_class <- "\\documentclass[landscape]{article}"
    }

    c(
        doc_class,
        "\\usepackage[pdftex]{graphicx}",
        "\\usepackage[utf8]{inputenc}",
        "\\usepackage{fancyhdr}",
        "\\usepackage{sfmath}",
        "\\usepackage{comment}",
        "\\usepackage[T1]{fontenc}",
        paste0(
            "\\usepackage[pdftex=true, pdftoolbar=true, pdfmenubar=true, pdfauthor = {},",
            "pdfcreator = {PDFLaTeX}, pdftitle = {}, colorlinks=true, urlcolor=blue,",
            "linkcolor=blue, citecolor=blue, implicit=true, hypertexnames=false]{hyperref}"
        ),
        paste0("\\usepackage[scaled]{", validLatexFont(theme$font), "}"),
        "\\renewcommand*\\familydefault{\\sfdefault}",
        "\\usepackage{booktabs, dcolumn, longtable}",
        paste0("\\usepackage[top=0.6in, bottom=0.6in, left=", bdr,
            ", right=", bdr, ", includeheadfoot]{geometry}"),
        "\\usepackage{array}",
        "\\usepackage[english]{babel}",
        "\\newcolumntype{B}[2]{>{#1\\hspace{0pt}\\arraybackslash}b{#2}}",
        "\\setlength{\\parindent}{0pt}",
        "\\usepackage[dvipsnames]{color}",
        "\\definecolor{gray}{gray}{0.85}",
        "\\pagestyle{fancy}",
        "\\renewcommand{\\headrulewidth}{0pt}",
        "\\renewcommand{\\footrulewidth}{0pt}",
        "\\fancyhead{}",
        paste0("\\fancyhead[L]{{", applyLatexStyle(title, theme$format_title), "}",
            applyLatexStyle(subtitle, theme$format_subtitle), "}"),
        logo,
        "\\newcolumntype{d}{D{.}{.}{3.2}}", #!topline
        "\\newcolumntype{g}{D{\\%}{\\%}{5.0}}", #!topline
        "\\usepackage{float}", #topline
        "\\usepackage{marginnote}", #topline
        "\\setlength\\extrarowheight{2pt}", #topline
        "\\newlength\\mywidth", #topline
        "\\setlength\\mywidth{3.5in}", #topline
        "\\usepackage{caption}", #topline
        "\\captionsetup[table]{labelformat=empty}", #topline
        "\\renewcommand*{\\marginfont}{\\scriptsize\\itshape}", #topline
        "\\fancyfoot{}",
        "\\fancyfoot[R]{\\thepage}",
        "\\newcommand{\\PreserveBackslash}[1]{\\let\\temp=\\\\#1\\let\\\\=\\temp}",
        "\\let\\PBS=\\PreserveBackslash",
        paste0(
            "\\newcommand{\\longtablesep}{\\endfirsthead ",
            multicolumn(2, italics(texEscape(theme$latex_headtext))), " \\\\ \\endhead ",
            multicolumn(2, italics(texEscape(theme$latex_foottext))), " \\\\ \\endfoot \\endlastfoot}"
        ),
        "\\usepackage[titles]{tocloft}",
        paste0("\\newcommand{\\cftchapfont}{", fontsize(theme$font_size), "}"),
        paste0("\\newcommand{\\formatvardescription}[1]{", applyLatexStyle("#1", theme$format_var_description), "}"),
        paste0("\\newcommand{\\formatvarname}[1]{", applyLatexStyle("#1", theme$format_var_name), "}"),
        paste0("\\newcommand{\\formatvaralias}[1]{", applyLatexStyle("#1", theme$format_var_alias), "}"),
        paste0("\\newcommand{\\formatvarfiltertext}[1]{", applyLatexStyle("#1", theme$format_var_filtertext), "}"),
        paste0("\\newcommand{\\formatvarsubname}[1]{", applyLatexStyle("#1", theme$format_var_subname), "}"),
        "",
        "",
        "",
        "",
        "",
        ""
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

latexStart <- function(table_of_contents, sample_desc, field_period, moe, font_size) {
    # More preamble before the tables
    if (!is.null(sample_desc)) {
        sample_desc <- paste("Sample  & ", sample_desc, newline, "")
    }
    if (!is.null(moe)) {
        moe <- paste("Margin of Error &  $\\pm ", round(100 * moe, digits = 1), "\\%$", newline, "")
    }
    if (!is.null(field_period)) {
        field_period <- paste("Conducted  & ", field_period, newline, "")
    }
    if (table_of_contents) {
        toc <- c("\\listoftables", "\\newpage")
    } else {
        toc <- NULL
    }

    c(
        "\\begin{tabular}{ll}", # TODO: don't include this tabular at all if empty
        sample_desc,
        field_period,
        moe,
        "\\end{tabular}",
        toc,
        "",
        "",
        paste0(
            "{", # Is this the random { that gets closed at end(document)?
            "\\setlength{\\LTleft}{0pt}"
        ),
        "\\setlength{\\LTright}{\\fill}",
        "\\setlength{\\LTcapwidth}{\\textwidth}",
        "",
        "",
        "%% here's where individual input starts %%",
        "",
        "",
        " \\vspace{.25in} ",
        "",
        ""
    )
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
