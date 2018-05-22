#' @export
writeLatex.Crosstabs <- function(data_summary, theme = themeDefaultLatex(), 
    filename = getName(data_summary), title = getName(data_summary), 
    subtitle = NULL, table_of_contents = FALSE, sample_desc = NULL, 
    field_period = NULL, moe = NULL, append_text = NULL, proportions = TRUE, 
    pdf = FALSE, open = FALSE, multirowheaderlines = FALSE, 
    grid_num_letters = TRUE, custom_numbering = NULL, logging = FALSE) {
    
    banner <- data_summary$banner

    data_summary$results <- lapply(data_summary$results, rm_inserts, theme)
    
    # data_summary$results <- reformatCrosstabsResults(data_summary$results, banner, proportions = proportions, theme = theme)
    results <- reformatLatexResults(data_summary, proportions = proportions, theme = theme)
    
    headers <- lapply(data_summary$results, longtableHeader, theme)
    
    # hinfo <- lapply(data_summary$results, function (x) lapply(getTableHeader_old(x, theme), escM))
    # headers2 <- lapply(seq_along(hinfo), function(i) tabreportHeader_old(hinfo[[i]], length(banner), parbox_width = "8.5in",
    #     if (!is.null(custom_numbering)) custom_numbering[i]
    #     else if (grid_num_letters) hinfo[[i]]$number
    #     else i))

    bodies <- lapply(results, function (x) {
        sapply(x, function (y) {
            latexTable.body(y, autorownames = TRUE, crosstabs = TRUE)
        })
    })
    out <- sapply(seq_along(data_summary$results), function(i) {
        c(paste(headers[[i]], bodies[[i]], tableFootLT(),
            sep="\n", collapse="\n"),
            if (theme$one_per_sheet) { "\\clearpage" })
    })
    out <- c(out, append_text)
    out <- c(
        latexHead(theme = theme, title = title, subtitle = subtitle, crosstabs = TRUE),
        # latexHeadLT(title = title, subtitle = subtitle, theme = theme),
        sapply(seq_along(banner), function (j) {
            longtableHeadFootB(banner[[j]], num = j, coltype= if (theme$digits == 0) { "g" } else { "d" },
                multirow = multirowheaderlines, page_width = 9, row_label_width = theme$format_label_column$col_width, theme = theme)
        }),
        latexStart(table_of_contents = table_of_contents, sample_desc = sample_desc, field_period = field_period, moe = moe,
            font_size = if (theme$font_size < 16) { "small" } else { "large" }, crosstabs = TRUE),
        out,
        latexFootLT()
    )
    if (!is.null(filename)) {
        filename <- paste0(filename, ".tex")
        cat(out, sep = "\n", file = filename)
        if (pdf) {
            if (logging) { print("PDF-ing") }
            pdflatex(filename, open, path.to.pdflatex = Sys.which("pdflatex"))
        }
    }
    return(invisible(data_summary))
}

# Long table header and footer creation.
# Generates two macros for the preamble
# \bannera{} that takes one argument (first column label)
# \tbltopa that takes no arguments
# If given multiple banners, \bannerb \tbltopb, etc are created
longtableHeadFootB <- function (banner, num = 1, coltype = "d",
    multirow = FALSE, page_width = 9, row_label_width, theme) {
    headtext <- if (is.null(theme$latex_headtext)) "" else theme$latex_headtext
    foottext <- if (is.null(theme$latex_foottext)) "" else theme$latex_foottext
    if (headtext == "tbc") headtext <- "continued from previous page"
    if (foottext == "tbc") foottext <- "continued on the next page \\dots"
    
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

# latexStartLT <- function(table_of_contents, font_size){
#     paste0("\\begin{document}\n",
#         ifelse(table_of_contents, "\\listoftables\n\\newpage\n\n", "\n\n"),
#         "{\\", font_size, "\n",
#         "\\setlength{\\LTleft}{0pt}\n",
#         "\\setlength{\\LTright}{\\fill}\n",
#         "\\setlength{\\LTcapwidth}{\\textwidth}\n\n\n",
#         "%% here's where individual input starts %%\n\n\n")
# }


# tabreportHeader_old <- function(hinfo, nbanners, parbox_width, table_num) {
#     return(sapply(1:nbanners, function (k) {
#         longtableHeader_old(num=k, hinfo, table_num = table_num, parbox_width = parbox_width,
#             title=I(k==1))
#     }))
# }

longtableHeader <- function(var, theme) {
    sapply(seq_along(var$crosstabs), function(bi){
        paste(if (bi != 1) "\\vspace{-.25in}" ,
            "\\tbltop", letters[bi], "\n",
            if (bi == 1) latexTableHeadTitle(var, theme),
            "\\addlinespace \n",
            "\\banner", letters[bi],"{} \n\n", sep="")
        
    })
}

latexTableHeadTitle <- function (var, theme) {
    var_info <- var_header(var, theme)
    if (!is.null(var_info$format_var_subname) && names(var_info)[1] != "format_var_subname") {
        var_info[[1]] <- paste0(var_info[[1]], if (!is.null(var_info$format_var_subname))
            paste0(" — ", var_info$format_var_subname))
        var_info$format_var_subname <- NULL
    }
    paste("\\addcontentsline{lot}{table}{ ", escM(var_info[[1]]), "}\n",
        "\\hangindent=0em \\parbox{8.5in}{", " \n",
        paste(sapply(names(var_info), function(info_name)
            latexDecoration(escM(var_info[[info_name]]), theme[[info_name]],
                scriptsize = FALSE)), collapse = "\\\\ \n"), "} \\\\", sep="")
}

# Header for LongTable with Banner.
# Title indicates whether the title should be displayed, or not (as in the
# case of multiple banners displayed underneath each other, the title only
# appears on the top one).
# Assumes that \banner[a-z]{} macros are defined in the preamble
# longtableHeader_old <- function (num, hinfo, table_num, parbox_width, title=TRUE) {
#     paste(if (title) { "" } else { "\\vspace{-.25in}" },
#         "\\tbltop", letters[num], "\n",
#         if (title) { latexTableHeadTitle_old(hinfo, table_num, parbox_width) } else { "" },
#         "\\addlinespace \n",
#         "\\banner", letters[num],"{} \n\n", sep="")
# }

# latexTableHeadTitle_old <- function (hinfo, table_num, parbox_width) {
#     paste("\\addcontentsline{lot}{table}{ ", table_num, ". ", hinfo$label, "}\n",
#         "\\hangindent=0em \\parbox{", parbox_width, "}{\n{\\bf ", table_num, ". ", hinfo$label, "} \\\\ \n",
#         hinfo$wording, "} \\\\", sep="")
# }

makeLatexBanner.internal <- function (binfo.i, multirow=FALSE, width=NULL) {
    cps <- getMulticolumnWidth(binfo.i)
    if (nchar(binfo.i[1]) > 0) {
        binfo.i[[1]] <- paste0("\\bf ",binfo.i[[1]])
    }
    binfo.i <- lapply(seq_along(binfo.i), function(i) {paste0("\\multicolumn{", cps[i],
        if (!multirow | cps[i]>1) { "}{c}{" } else { paste0("}{m{", width,"in}}{\\centering ") },
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

# getBannerInfo <- function(banner) lapply(banner, function(x) {list(label = getName(x), wording = getNames(x))})

# getTableHeader_old <- function (ds_var, theme) {
#     description <- paste(getDescription(ds_var), getFilterText(ds_var))
#     var_info <- var_header(ds_var, theme)
#     lab <- paste0(getName(ds_var), if (!is.null(var_info$format_var_subname))
#         paste0(" — ", var_info$format_var_subname))
#     list(label=lab, wording=description, number=ds_var$number)
# }

tableFootLT <- function() "\n \\end{longtable}\n\n"

latexFootLT <- function() "}\\end{document}\n"