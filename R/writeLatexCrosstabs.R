
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
        "in}*{", col_num_sum, "}{r}}}\n")
    return(paste0(banner_def_head, banner_def_body, banner_def_foot, table_def))
}

makeLatexBanner <- function (binfo, width=NULL, theme) {
    m_split <- paste0("}{m{", width,"in}}{\\centering ")
    ban <- paste0(c(paste(" & \\multicolumn{", binfo$len, "}{c}{\\bf ",
            escM(binfo$names), "}", collapse = "", sep = ""),
        paste(" & \\multicolumn{1}{c}{", escM(unlist(binfo$multicols)), "}", 
            collapse = "", sep = "")),
        " \\\\")
    ban[2] <- paste("{\\bf #1}", ban[2])
    
    ban[1] <- paste0(ban[1], paste0(" \\cmidrule(lr{.75em}){",
        binfo$multicols_csum[2:(length(binfo$multicols_csum)-1)], "-",
        binfo$multicols_csum[3:length(binfo$multicols_csum)] - 1, "}", collapse = ""))
    return(paste(c(ban, "\\midrule \n"), collapse = "\n"))
}

# Header for LongTable with Banner.
# Title indicates whether the title should be displayed, or not (as in the
# case of multiple banners displayed underneath each other, the title only
# appears on the top one).
# Assumes that \banner[a-z]{} macros are defined in the preamble
tableHeader.CrossTabVar <- function(var, theme) {
    sapply(seq_along(var$crosstabs), function(num){
        paste(if (num != 1) "\\vspace{-.25in}" ,
            "\\tbltop", letters[num], "\n",
            if (num == 1) latexTableName(var, theme),
            "\\addlinespace \n",
            "\\banner", letters[num],"{} \n\n", sep="")
    })
}



