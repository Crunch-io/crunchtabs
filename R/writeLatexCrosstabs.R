
#' @export
writeLatex.Crosstabs <- function(data_summary, filename = NULL, proportions = TRUE, digits = 0,
    title = getName(data_summary), subtitle = NULL, sample_desc = "", field_period = "", moe = NULL,
    table_of_contents = FALSE, returndata = FALSE, append_text = "",
    pdf = FALSE, path.to.pdflatex = Sys.which("pdflatex"), open = FALSE,
    headtext = "", foottext = "", graphicspath = NULL, logo = NULL, longtablewrap = TRUE,
    tableonly = FALSE, landscape = TRUE, font = "helvet", font_size = "small",
    page_width = ifelse(landscape, 9, 6.5), row_label_width = 1.5,
    margin = list(top = .6, bottom = .6, left = .5, right = .5),
    min_cell_size = NULL, min_cell_label = NULL,
    show_totals = TRUE, weighted_n = FALSE, add_parenthesis = TRUE,
    dc = c(3.2, 4.1), multirowheaderlines = FALSE,
    latex_adjust = 'c', clearpage = TRUE, grid_num_letters = TRUE, custom_numbering = NULL,
    round_percentages = FALSE) {
    
    # reformat results for LaTeX output
    banner <- data_summary$banner
    print(data_summary$results$shut_blame$crosstabs$banner1)
    data_summary$results <- reformatCrosstabsResults(data_summary$results, banner, proportions = proportions, digits = digits, add_parenthesis = add_parenthesis,
        show_totals = show_totals, weighted_n = weighted_n, latex_adjust = latex_adjust,
        min_cell_size = min_cell_size, min_cell_label = min_cell_label, reformat = TRUE,
        round_percentages = round_percentages)
    
    parbox_width <- ifelse(landscape,"8.5in","6in")
    hinfo <- lapply(data_summary$results, function (x) lapply(getTableHeader(x), escM))
    headers <- lapply(seq_along(hinfo), function(i) tabreportHeader(hinfo[[i]], length(banner), parbox_width,
        if (!is.null(custom_numbering)) custom_numbering[i]
        else if (grid_num_letters) hinfo[[i]]$number
        else i))
    
    bodies <- lapply(data_summary$results, function (x) {
        sapply(x$crosstabs, function (y) {
            latexTable.body(y, longtablewrap = longtablewrap, autorownames = TRUE, summary.midrule = TRUE,
                show_totals = !x$settings$no_totals & show_totals)
        })
    })
    
    out <- sapply(seq_along(data_summary$results), function(i) {
        c(paste(headers[[i]], bodies[[i]], tableFootLT(),
            sep="\n", collapse="\n"),
            ifelse(clearpage, "\\clearpage", ""))
    })
    out <- c(out, append_text)
    
    if (!tableonly) {
        out <- c(
            latexHeadLT(title = title, landscape = landscape, margin = margin, dc = dc, subtitle = subtitle, font = font, graphicspath = graphicspath, logo = logo),
            sapply(seq_along(banner), function (j) {
                longtableHeadFootB(banner[[j]], headtext = headtext, foottext = foottext, num = j, coltype=ifelse(digits==0, "g", "d"),
                    multirow = multirowheaderlines, page_width = page_width, row_label_width = row_label_width)
            }),
            latexStartLT(table_of_contents = table_of_contents, font_size = font_size),
            out,
            latexFootLT()
        )
    }
    
    if (!is.null(filename)) {
        filename <- paste0(filename, ".tex")
        cat(out, sep = "\n", file = filename)
        if (pdf) {
            pdflatex(filename, open, path.to.pdflatex = path.to.pdflatex)
        }
    }
    if (returndata) {
        return(data_summary)
    }
}

latexHeadLT <- function (title="", landscape=FALSE, margin=NULL, dc=c("3.2", "5.0"), subtitle=NULL, font="helvet", graphicspath = NULL, logo = NULL) {
    dc <- rep(dc, length=2)
    paste("\\documentclass[", ifelse(landscape, "landscape", ""),"]",
        "{article}\n",
        "\\usepackage[pdftex]{graphicx}\n",
        if (!is.null(graphicspath)) paste0("\\graphicspath{ {", graphicspath,"/} }\n"),
        "\\usepackage[utf8]{inputenc}\n",
        "\\usepackage{fancyhdr}\n",
        "\\usepackage{sfmath}\n",
        "\\usepackage[T1]{fontenc}\n",
        '\\usepackage[pdftex=true,
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
        "\\usepackage[scaled]{", font, "}\n",
        "\\renewcommand*\\familydefault{\\sfdefault}\n",
        "\\usepackage{booktabs, dcolumn, longtable}\n",
        "\\usepackage[",
        ifelse(!is.null(margin),
            paste("top=", margin$t, "in, bottom=", margin$b, "in, left=", margin$l, "in, right=", margin$r, "in, includeheadfoot", sep=""),
            "top=.6in,bottom=.6in,left=.5in,right=.5in,includeheadfoot"),
        "]{geometry}\n",
        "\\usepackage{array}\n",
        "\\usepackage[english]{babel}\n",
        "\\setlength{\\parindent}{0pt}",
        "\\usepackage[dvipsnames]{color}\n",
        "\\definecolor{gray}{gray}{0.85}\n\n",
        "\\pagestyle{fancy}\n",
        "\\renewcommand{\\headrulewidth}{0pt}\n",
        "\\renewcommand{\\footrulewidth}{0pt}\n",
        "\\fancyhead{}\n",
        "\\fancyhead[L]{{\\Large {\\bf ",
        ifelse(is.null(title), "", escM(title)), "}}",
        ifelse(is.null(subtitle), "", paste(" \\\\", escM(subtitle))),
        "}\n",
        if (!is.null(logo)) paste0("\\fancyhead[R]{\\includegraphics[scale=.4]{", logo, "}}\n"),
        "\\fancyfoot{}\n",
        "\\fancyfoot[R]{\\thepage}\n\n",
        "\\newcolumntype{d}{D{.}{.}{", dc[1],"}}\n\n",
        "\\newcolumntype{g}{D{\\%}{\\%}{", dc[2], "}}\n\n",
        "\\newcolumntype{B}[2]{>{#1\\hspace{0pt}\\arraybackslash}b{#2}}\n",
        "\\newcommand{\\PreserveBackslash}[1]{\\let\\temp=\\",
        "\\#1\\let\\",
        "\\=\\temp}\n",
        "\\let\\PBS=\\PreserveBackslash\n\n", sep="")
}


# Long table header and footer creation.
# Generates two macros for the preamble
# \bannera{} that takes one argument (first column label)
# \tbltopa that takes no arguments
# If given multiple banners, \bannerb \tbltopb, etc are created
longtableHeadFootB <- function (banner, headtext = "tbc", foottext = "tbc", num = 1, coltype = "d",
    multirow = FALSE, page_width = 9, row_label_width = 2) {
    if (headtext == "tbc") headtext="continued from previous page"
    if (foottext == "tbc") foottext="continued on the next page \\dots"
    
    binfo <- lapply(banner, function(x) {list(getName(x), getNames(x))})
    col_num_sum <- sum(sapply(binfo, function (binfo.i) length(binfo.i[[2]]))) # excludes rownames column, as before
    
    banner_def_head <- paste0("\\newcommand{\\banner", letters[num],"}[1]{")
    
    banner_def_body <- rep(makeLatexBanner(binfo, multirow, width=round((page_width - row_label_width)/col_num_sum-.1,2)), 2)
    banner_def_body[2] <- paste0("& \\multicolumn{", col_num_sum, "}{c}{", headtext, "} \\\\ ", banner_def_body[2])
    banner_def_body <- paste("\\toprule", banner_def_body, sep="\n", collapse="\\endfirsthead \n")
    
    banner_def_foot <- paste0("\\endhead \n\\midrule \n& \\multicolumn{", col_num_sum, "}{c}{", foottext, "} \\\\ \n",
        "\\bottomrule \n\\endfoot \n\\bottomrule \n\\endlastfoot \n}\n")
    
    table_def <- paste0("\\newcommand{\\tbltop",letters[num],"}{\n",
        "\\begin{longtable}{@{\\extracolsep{\\fill}}>{\\hangindent=1em \\PBS \\raggedright \\hspace{0pt}}b{", row_label_width, "in}*{", col_num_sum,
        "}{", coltype, "}}}\n")
    
    return(paste0(banner_def_head, banner_def_body, banner_def_foot, table_def))
}

getMulticolumnWidth <- function(binfo.i) {
    return(c(length(binfo.i[[2]]), length(binfo.i[[1]])))
}

latexStartLT <- function(table_of_contents, font_size){
    paste0("\\begin{document}\n",
        ifelse(table_of_contents, "\\listoftables\n\\newpage\n\n", "\n\n"),
        "{\\", font_size, "\n",
        "\\setlength{\\LTleft}{0pt}\n",
        "\\setlength{\\LTright}{\\fill}\n",
        "\\setlength{\\LTcapwidth}{\\textwidth}\n\n\n",
        "%% here's where individual input starts %%\n\n\n")
}


tabreportHeader <- function(hinfo, nbanners, parbox_width, table_num) {
    return(sapply(1:nbanners, function (k) {
        longtableHeader(num=k, hinfo, table_num = table_num, parbox_width = parbox_width,
            title=I(k==1))
    }))
}

# Header for LongTable with Banner.
# Title indicates whether the title should be displayed, or not (as in the
# case of multiple banners displayed underneath each other, the title only
# appears on the top one).
# Assumes that \banner[a-z]{} macros are defined in the preamble
longtableHeader <- function (num, hinfo, table_num, parbox_width, title=TRUE) {
    paste(ifelse(title, "", "\\vspace{-.25in}"),
        "\\tbltop", letters[num], "\n",
        ifelse(title, latexTableHeadTitle(hinfo, table_num, parbox_width), ""),
        "\\addlinespace \n",
        "\\banner", letters[num],"{} \n\n", sep="")
}

latexTableHeadTitle <- function (hinfo, table_num, parbox_width) {
    paste("\\addcontentsline{lot}{table}{ ", table_num, ". ", hinfo$label, "}\n",
        "\\hangindent=0em \\parbox{", parbox_width, "}{{\\bf ", table_num, ". ", hinfo$label, "} \\\\ \n",
        hinfo$wording, "} \\\\", sep="")
}

makeLatexBanner.internal <- function (binfo.i, multirow=FALSE, width=NULL) {
    cps <- getMulticolumnWidth(binfo.i)
    if (nchar(binfo.i[1]) > 0) {
        binfo.i[[1]] <- paste0("\\bf ",binfo.i[[1]])
    }
    binfo.i <- lapply(seq_along(binfo.i), function(i) {paste0("\\multicolumn{", cps[i],
        ifelse(!multirow | cps[i]>1,
            "}{c}{",
            paste0("}{m{", width,"in}}{\\centering ")),
        escM(binfo.i[[i]]),
        "}",
        collapse=" & ")})
    
    return(unlist(binfo.i))
}

makeLatexBanner <- function (binfo, multirow=FALSE, width=NULL,
    tabreport=TRUE) {
    ban <- paste("&",apply(as.data.frame(lapply(binfo, makeLatexBanner.internal, multirow, width), stringsAsFactors=FALSE), 1, paste, collapse= " & "),"\\\\", sep=" ")
    ban[2] <- paste("{\\bf #1}", ban[2])
    
    multicols <- c(1, sapply(binfo, function(x) length(x[[2]])))
    notempty <- c(FALSE, sapply(binfo, function(x) nchar(x[[1]]) > 0))
    multicols_csum <- cumsum(multicols)
    for (j in seq_along(multicols))
        if (multicols[j] > 1 && notempty[j]) {
            ban[1] <- paste0(ban[1], " \\cmidrule(lr{.75em}){", 1+multicols_csum[j-1], "-", multicols_csum[j], "}")
        }
    
    ban <- c(ban, "\\midrule \n")
    return(paste(ban, collapse="\n"))
}

getBannerInfo <- function(banner) lapply(banner, function(x) {list(label = getName(x), wording = getNames(x))})

getTableHeader <- function (ds_var) {
    description <- paste(getDescription(ds_var), getFilterText(ds_var))
    list(label=getName(ds_var), wording=description, number=ds_var$settings$number)
}

tableFootLT <- function() "\n \\end{longtable}\n\n"

latexFootLT <- function() "}\\end{document}\n"

