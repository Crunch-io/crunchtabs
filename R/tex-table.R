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
