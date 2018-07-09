
#' @export
tableHeader.ToplineVar <- function(var, theme) {
    tab_definition <- paste0("\\begin{longtable}{p{0.3in}p{5.5in}}")
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
            labs <- escM(sub("^[0-9]+ - (.*)$", "\\1", col_names[which.split]))
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
    header_row <- paste("\\\\", header_row, "& &", paste(escM(col_names), collapse = " & "), " & \\\n")
    header_row <- paste(header_row, "\n\\endfirsthead\n\\multicolumn{", 
        col_names_len + 2, "}{c}{\\textit{", theme$latex_headtext, "}} \\\\",
        header_row, "\\endhead\n\\multicolumn{", col_names_len + 2, 
        "}{c}{\\textit{", theme$latex_foottext, "}} \\\\ \n\\endfoot\n\\endlastfoot\n", 
        sep = "")
    col.header <- paste("B{\\centering}{", col_width, "}", sep = "")
    col.header <- paste(rep(col.header, col_names_len+1), collapse = "")
    tab_definition <- paste0("\\begin{longtable}{@{\\extracolsep{\\fill}}p{0.1in}B{\\raggedright}{", 
        theme$format_label_column$col_width, "in}", col.header, "}")
    
    toplineTableDef(var, tab_definition, header_row, theme)
}

toplineTableDef <- function(var, tab_definition, header_row, theme) {
    return(paste("\\begin{center}\n",
        tab_definition, "\n",
        latexTableName(var, theme),
        header_row,
        sep = ""))
}



