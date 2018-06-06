#' @export
writeLatex.Crosstabs <- function(data_summary, theme = themeDefaultLatex(), 
    filename = getName(data_summary), title = getName(data_summary), 
    subtitle = NULL, table_of_contents = FALSE, sample_desc = NULL, 
    field_period = NULL, moe = NULL, append_text = NULL, proportions = TRUE, 
    pdf = FALSE, open = FALSE, logging = FALSE) {
    
    topline <- is(data_summary, "Toplines")
    if (is.null(theme$font_size)) { theme$font_size <- 12 }
    
    headers <- lapply(data_summary$results, longtableHeader, theme) # dif
    
    data_summary$results <- lapply(data_summary$results, rm_inserts, theme)
    results <- reformatLatexResults(data_summary, proportions = proportions, theme = theme)
    bodies <- lapply(results, function (x) 
        sapply(x, latexTable.body, topline = topline))
    
    out <- c(
        latexDocHead(theme = theme, title = title, subtitle = subtitle, topline = topline),
        sapply(seq_along(data_summary$banner), function (j) {
            longtableHeadFootB(data_summary$banner[[j]], num = j, page_width = 9, 
                theme = theme)
        }), # dif
        latexStart(table_of_contents = table_of_contents, sample_desc = sample_desc, 
            field_period = field_period, moe = moe, font_size = theme$font_size),
        sapply(seq_along(data_summary$results), function(i) {
            c(paste(headers[[i]], bodies[[i]], latexTableFoot(topline = topline), # dif
                sep="\n", collapse="\n"),
                if (theme$one_per_sheet) { "\\clearpage" })
        }),        
        append_text,
        latexDocFoot()
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
longtableHeadFootB <- function (banner, num, page_width = 9, theme) {

    binfo <- get_banner_info(banner, theme)
    col_num_sum <- length(unlist(binfo$multicols))
    
    banner_def_head <- paste0("\\newcommand{\\banner", letters[num],"}[1]{")
    
    banner_def_body <- rep(makeLatexBanner(binfo, 
        width = round((page_width - theme$format_label_column$col_width)/col_num_sum-.1, 2), 
        theme = theme), 2)
    banner_def_body[2] <- paste0("& \\multicolumn{", col_num_sum, "}{c}{", 
        theme$latex_headtext, "} \\\\ ", banner_def_body[2])
    banner_def_body <- paste("\\toprule", banner_def_body, sep="\n", 
        collapse="\\endfirsthead \n")
    
    banner_def_foot <- paste0("\\endhead \n\\midrule \n& \\multicolumn{", 
        col_num_sum, "}{c}{", theme$latex_foottext, "} \\\\ \n",
        "\\bottomrule \n\\endfoot \n\\bottomrule \n\\endlastfoot \n}\n")
    
    table_def <- paste0("\\newcommand{\\tbltop",letters[num],"}{\n",
        "\\begin{longtable}{@{\\extracolsep{\\fill}}>{\\hangindent=1em \\PBS ",
        "\\raggedright \\hspace{0pt}}b{", theme$format_label_column$col_width, 
        "in}*{", col_num_sum, "}{", if (theme$digits == 0) { "g" } else { "d" }, "}}}\n")
    return(paste0(banner_def_head, banner_def_body, banner_def_foot, table_def))
}

makeLatexBanner <- function (binfo, width=NULL, theme) {
    m_split <- paste0("}{m{", width,"in}}{\\centering ")
    br <- ifelse(!theme$latex_multirowheaderlines | binfo$len > 1, "}{c}{", m_split)
    ban <- paste0(c(paste(" & \\multicolumn{", binfo$len, br, "\\bf ",
        escM(binfo$names), "}", collapse = "", sep = ""),
        paste(" & \\multicolumn{1", ifelse(!theme$latex_multirowheaderlines, "}{c}{", m_split),
            escM(unlist(binfo$multicols)), "}", collapse = "", sep = "")),
        " \\\\")
    ban[2] <- paste("{\\bf #1}", ban[2])
    
    ban[1] <- paste0(ban[1], paste0(" \\cmidrule(lr{.75em}){",
        binfo$multicols_csum[2:(length(binfo$multicols_csum)-1)], "-",
        binfo$multicols_csum[3:length(binfo$multicols_csum)] - 1, "}", collapse = ""))
    return(paste(c(ban, "\\midrule \n"), collapse = "\n"))
}

# getMulticolumnWidth <- function(binfo.i) {
#     return(c(length(binfo.i[[2]]), length(binfo.i[[1]])))
# }

# Header for LongTable with Banner.
# Title indicates whether the title should be displayed, or not (as in the
# case of multiple banners displayed underneath each other, the title only
# appears on the top one).
# Assumes that \banner[a-z]{} macros are defined in the preamble
longtableHeader <- function(var, theme) {
    sapply(seq_along(var$crosstabs), function(num){
        paste(if (num != 1) "\\vspace{-.25in}" ,
            "\\tbltop", letters[num], "\n",
            if (num == 1) latexTableHeadTitle(var, theme),
            "\\addlinespace \n",
            "\\banner", letters[num],"{} \n\n", sep="")
        
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
            paste0("\\", gsub("_", "", info_name), "{", escM(var_info[[info_name]]), "}")), 
            collapse = "\\\\ \n"), "} \\\\", sep="")
}

# makeLatexBanner.internal <- function (binfo.i, multirow=FALSE, width=NULL) {
#     cps <- getMulticolumnWidth(binfo.i)
#     if (nchar(binfo.i[1]) > 0) {
#         binfo.i[[1]] <- paste0("\\bf ",binfo.i[[1]])
#     }
#     binfo.i <- lapply(seq_along(binfo.i), function(i) {paste0("\\multicolumn{", cps[i],
#         if (!multirow | cps[i]>1) { "}{c}{" } else { paste0("}{m{", width,"in}}{\\centering ") },
#         escM(binfo.i[[i]]),
#         "}",
#         collapse=" & ")})
#     
#     return(unlist(binfo.i))
# }

# makeLatexBanner <- function (banner, binfo, multirow=FALSE, width=NULL,
#     tabreport=TRUE) {
#     binfo2 <- get_banner_info(banner, theme)
#     ban <- paste("&",apply(as.data.frame(lapply(binfo, makeLatexBanner.internal,
#         multirow, width), stringsAsFactors=FALSE), 1, paste, collapse= " & "),"\\\\", sep=" ")
#     ban[2] <- paste("{\\bf #1}", ban[2])
#     
#     multicols <- c(1, sapply(binfo, function(x) length(x[[2]])))
#     notempty <- c(FALSE, sapply(binfo, function(x) nchar(x[[1]]) > 0))
#     multicols_csum <- cumsum(multicols)
#     for (j in seq_along(multicols))
#         if (multicols[j] > 1 && notempty[j]) {
#             ban[1] <- paste0(ban[1], " \\cmidrule(lr{.75em}){", 1+multicols_csum[j-1], "-", multicols_csum[j], "}")
#         }
#     
#     ban <- c(ban, "\\midrule \n")
#     browser()
#     return(paste(ban, collapse="\n"))
# }

# tableFootLT <- function() "\n \\end{longtable}\n\n"

# latexFootLT <- function() "}\\end{document}\n"


