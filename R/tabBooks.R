
#' @importFrom crunch multitables newMultitable tabBook allVariables aliases types type crtabs
#' @importFrom digest digest
tabBooks <- function(dataset, vars = names(dataset), banner = NULL, weight = NULL) {
  tabs_data <- list()

  # NPR: how do I supply a multitable definition that I've already made?
  mtvars <- setdiff(sapply(flattenBanner(banner), getAlias), "___total___")
  mt_name <- digest(sort(mtvars), "md5")
  m <- multitables(dataset)[[mt_name]]
  if (is.null(m)) {
    m <- newMultitable(paste("~", paste(mtvars, collapse = " + ")), data = dataset, name = mt_name)
  }
  book <- tabBook(m, dataset=dataset, weight = weight, format="json")

  banner_map <- lapply(seq_along(banner), function(bx) sapply(banner[[bx]], function(bv) bv$alias))
  banner_flatten <- flattenBanner(banner)
  # var_names <- names(allVariables(dataset))
  # var_aliases <- aliases(allVariables(dataset))
  # var_types <- types(allVariables(dataset))
  # banner_var_aliases <- c("___total___", mtvars)

  # NPR: can you add some inline comments so that I can follow along what you're doing?
  for (vi in seq_along(book)) {
    crunch_cube <- book[[vi]][[1]]
    if (!(getAlias(crunch_cube)) %in% vars) next

    array_type <- type(dataset[[getAlias(crunch_cube)]]) == 'categorical_array'
    vnames <- if(array_type) paste(getName(crunch_cube), getSubNames(crunch_cube), sep = " - ") else getName(crunch_cube)
    valiases <- if(array_type) paste(getAlias(crunch_cube), getSubAliases(crunch_cube), sep = ".") else getAlias(crunch_cube)

    for (vai in seq_along(valiases)) {
      tab_book_var <- list(alias = valiases[vai]
                           , name = vnames[vai]
                           , description = getDescription(crunch_cube)
                           , notes = getNotes(crunch_cube)
                           , crosstabs = sapply(names(banner), function(x) list(), simplify = FALSE, USE.NAMES = TRUE)
      )
      class(tab_book_var) <- "CrossTabVar"
      tabs_data[[valiases[vai]]] <- tab_book_var
    }

    for (vbi in seq_along(book[[vi]])) {
      crunch_cube <- book[[vi]][[vbi]]
      margin <- if (array_type) c(1, 3) else 2

      prop_banner_counts <- as.array(crunch_cube)
      prop_banner_proportions <- crunch::prop.table(crunch_cube, margin = margin)
      prop_banner_counts_unweighted <- if (!is.null(weight)) bases(crunch_cube, margin = 0) else prop_banner_counts

      banner_var_alias <- if (vbi == 1) "___total___" else crunch_cube@.Data[[1]]$dimensions[[length(crunch_cube@.Data[[1]]$dimensions)]]$references$alias

      for (ri in seq_along(valiases)) {
        if (array_type) {
          counts_out = prop_banner_counts[ri, ,]
          proportions_out = prop_banner_proportions[ri, ,]
          prop_banner_counts_unweighted_out = prop_banner_counts_unweighted[ri, ,]
        } else {
            counts_out = prop_banner_counts
            proportions_out = prop_banner_proportions
            prop_banner_counts_unweighted_out = prop_banner_counts_unweighted
        }

        if (banner_var_alias != "___total___") {
          banner_var <- banner_flatten[[banner_var_alias]]
          counts_out <- bannerDataRecode(counts_out, banner_var)
          prop_banner_counts_unweighted_out <- bannerDataRecode(prop_banner_counts_unweighted_out, banner_var)
          proportions_out <- if (ncol(counts_out) == ncol(proportions_out)) {
            bannerDataRecode(proportions_out, banner_var)
          } else {
            prop.table(counts_out, 2)
          }
        }

        totals_counts_out = colSums(counts_out)
        totals_proportions_out = colSums(proportions_out)
        unweighted_n_out = colSums(prop_banner_counts_unweighted_out)

        if (vbi == 1) {
          colnames(counts_out) <- "Total"
          colnames(proportions_out) <- "Total"
          names(totals_counts_out) <- "Total"
          names(totals_proportions_out) <- "Total"
          names(unweighted_n_out) <- "Total"
        }

        banner_var_cross <- list(
          counts = counts_out,
          proportions = proportions_out,
          totals_counts = totals_counts_out,
          totals_proportions = totals_proportions_out,
          unweighted_n = unweighted_n_out
        )

        class(banner_var_cross) <- c("CrossTabBannerVar", class(banner_var_cross))
        for (bi in seq_along(banner_map)) {
          if (banner_var_alias %in% banner_map[[bi]]) {
            tabs_data[[valiases[ri]]][['crosstabs']][[bi]][[if (banner_var_alias == "___total___") "Total" else banner_var_alias]] <- banner_var_cross
          }
        }
      }
    }
  }
  return(tabs_data)
}
