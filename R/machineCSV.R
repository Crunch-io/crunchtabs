


writeMachineCSV <- function(
    data_summary, filename = getName(data_summary)) {
    
    if (is.null(filename)) {
        stop("No filename provided.")
    }
    
    wrong_class_error(data_summary, "CrunchTabs", "data_summary")
    if (!any(c("Toplines", "Crosstabs") %in% class(data_summary))) {
        stop(
            paste(
                "The expected class for `data_summary` is either Toplines, CrunchTabs",
                "or Crosstabs CrunchTabs, not "), collapse_items(class(data_summary)))
    }
    
    if (!endsWith(filename, ".csv")) {
        filename <- paste0(filename, ".csv")
    }
    
    df <- lapply(data_summary$results, function(var) {
        lapply(names(data_summary$banner), function(banner_name) {
            banner_info <- getBannerInfo(data_summary$banner[[banner_name]], 
                theme=themeDefaultExcel())
            reformatCSVVar(var = var, banner_name = banner_name, 
                banner_info = banner_info)
        }) %>% bind_rows()
    }) %>%
        bind_rows() %>%
    write.csv(file=filename, row.names=FALSE)
    
}


# round stuff?
# multiply percents?
# % sign?
# ___total___ not margin
# multiple banners. what to do with repeats?
# add unweighted counts? how?
# add weighted bases?
# make it work with other variables?
# ignores inserts atm
reformatCSVVar <- function(var, banner_name, banner_info) {
    var_data <- var$crosstabs[[banner_name]]
    lapply(names(var_data), function(bvn) {
        perc <- reshape2::melt(var_data[[bvn]]$proportions)
        count <- reshape2::melt(var_data[[bvn]]$counts)
        base <- reshape2::melt(var_data[[bvn]]$base)
        data.frame("Row variable" = var$name, 
            "Row label" = perc$Var1,
            "Column variable" = ifelse(bvn %in% "___total___", bvn, 
                banner_info$names[[bvn]]),
            "Column label" = perc$Var2,
            "Percent" = perc$value,
            "Count (weighted)" = count$value,
            "Unweighted N" = base$value,
            check.names = FALSE
        )
    }) %>%
        bind_rows()
    
}
