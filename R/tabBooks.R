
#' @importFrom crunch multitables newMultitable tabBook allVariables aliases types type crtabs
#' @importFrom digest digest
tabBooks <- function(dataset, vars = names(dataset), banner = NULL, weight = NULL) {
  tabs_data <- list()

  # NPR: how do I supply a multitable definition that I've already made?
  # KS: you can create an object of the Banner class and pass it as a parameter (banner);
  #   the code below "digest" varialbes' names and generates a new multitable
  #   (if the name of the multitable that you created is the same as the "digest" output
  #   than that multitable is used)
  ## NPR: what does Banner do that Multitable does not?
  ## KS: it's a slightly different concept. A banner can contain 'subbanners' that are displayed
  # one above another in the PDF (only one multitable is used in such case).
  # It also enables changing labels and hiding/renaming
  # columns of "banner" variables (without modyfing the dataset).
  mtvars <- setdiff(sapply(flattenBanner(banner), getAlias), "___total___")
  mt_name <- digest(sort(mtvars), "md5")
  m <- multitables(dataset)[[mt_name]]
  if (is.null(m)) {
    m <- newMultitable(paste("~", paste(mtvars, collapse = " + ")), data = dataset, name = mt_name)
  }
  book <- tabBook(m, dataset=dataset, weight = weight, format="json")

  banner_map <- lapply(seq_along(banner), function(bx) sapply(banner[[bx]], function(bv) bv$alias))
  banner_flatten <- flattenBanner(banner)
  ## KS: I can get types/names/aliases using:
  # var_names <- names(allVariables(dataset))
  # var_aliases <- aliases(allVariables(dataset))
  # var_types <- types(allVariables(dataset))
  # banner_var_aliases <- c("___total___", mtvars)
  ## or the book (tabBook output). I found the second solution more flexible, but that way
  ## I relay on implementation details (structure of the tabBook output).

  ## NPR: oh, it's all implementation details :)
  ## TabBookResult, the return from tabBook, is an object with methods, including "names",
  ## and it can grow more methods, so you're not merely relying on internal data structures.
  ## Likewise, CrunchCube can grow more methods so you don't have to do evil things like
  ## x@.Data[[1]]$dimensions[[1]]$references$name.
  ## Feel free to make an issue for whatever methods you need at https://github.com/Crunch-io/rcrunch/issues.
  ## Can also help you make a pull request to implement them.
  ## KS: OK, that's a good idea.

  # NPR: can you add some inline comments so that I can follow along what you're doing?
  # for every variable in the book
  for (vi in seq_along(book)) {
    ## NPR: you can do `for (alias in names(book))` and save the lookup later
    ## KS:
    ## - the last time I tried that, names(book) and book[[alias]] gave me NULL
    ## - in the loop later, I'm iterating not over names(book) but I'm combining that with subvariables' names/aliases
    crunch_cube <- book[[vi]][[1]]
    # if it's not a "main" variable (so it's a "banner" variable) than skip it
    ## NPR: subset `dataset` when you pass it to tabBook so you only compute what you want
    ## Or, IMO even better, have the caller subset dataset, so that this function doesn't
    ## have to care about that.
    ## KS: I tried that before but got some errors, it seems to work now
    # if (!(getAlias(crunch_cube)) %in% vars) next

    # get type of the variable
    array_type <- type(dataset[[getAlias(crunch_cube)]]) == 'categorical_array'
    ## NPR: seems like most logic below switches on `array_type` so I'd recommend
    ## factoring that out into functions. More readable IMO, more testable, etc.
    ## And, once you slice the array crosstabs into the various subtables, the
    ## following logic should be the same.
    # generate new names and aliases for categorical_array variables by combining variable names/aliases
    # with subvariables' names/aliases
    # KS: that might be a good idea
    vnames <- if(array_type) paste(getName(crunch_cube), getSubNames(crunch_cube), sep = " - ") else getName(crunch_cube)
    valiases <- if(array_type) paste(getAlias(crunch_cube), getSubAliases(crunch_cube), sep = ".") else getAlias(crunch_cube)

    # for every 2d variable (categorical_array variables are sliced) prepare a data structure that will contain data
    for (vai in seq_along(valiases)) {
      tabs_data[[valiases[vai]]] <- structure(list(alias = valiases[vai]
                                                   , name = vnames[vai]
                                                   , description = getDescription(crunch_cube)
                                                   , notes = getNotes(crunch_cube)
                                                   , crosstabs = sapply(names(banner), function(x) list(), simplify = FALSE, USE.NAMES = TRUE)
                                                   ), class = "CrossTabVar")
    }

    # for every "banner" variable
    for (vbi in seq_along(book[[vi]])) {
      crunch_cube <- book[[vi]][[vbi]]
      margin <- if (array_type) c(1, 3) else 2

      banner_counts <- as.array(crunch_cube)
      banner_proportions <- crunch::prop.table(crunch_cube, margin = margin)
      banner_counts_unweighted <- if (!is.null(weight)) bases(crunch_cube, margin = 0) else banner_counts

      ## NPR: this is evil, let's stop @.Data-ing. There is a CubeDims class in 'crunch'
      ## that probably just needs a few more getter methods.
      # KS: right
      banner_var_alias <- if (vbi == 1) "___total___" else crunch_cube@.Data[[1]]$dimensions[[length(crunch_cube@.Data[[1]]$dimensions)]]$references$alias

      for (ri in seq_along(valiases)) {
        if (array_type) {
            counts_out = as.matrix(banner_counts[ri, ,])
            proportions_out = as.matrix(banner_proportions[ri, ,])
            banner_counts_unweighted_out = as.matrix(banner_counts_unweighted[ri, ,])
        } else {
            counts_out = banner_counts
            proportions_out = banner_proportions
            banner_counts_unweighted_out = banner_counts_unweighted
        }

        # KS: the initial idea was that the banner function should enable merging columns of "banner" variables
        # using "recode" syntax. the code below is responsible for doing that, but it incorectly computes
        # proportions for multiple_array variables now. we may abandon this idea as this can be done
        # using the "combine" function
        if (banner_var_alias != "___total___") {
          banner_var <- banner_flatten[[banner_var_alias]]
          counts_out <- bannerDataRecode(counts_out, banner_var)
          banner_counts_unweighted_out <- bannerDataRecode(banner_counts_unweighted_out, banner_var)
          proportions_out <- if (ncol(counts_out) == ncol(proportions_out)) {
            bannerDataRecode(proportions_out, banner_var)
          } else {
            prop.table(counts_out, 2)
          }
        }

        totals_counts_out = colSums(counts_out)
        totals_proportions_out = colSums(proportions_out)
        unweighted_n_out = colSums(banner_counts_unweighted_out)

        if (vbi == 1) {
          colnames(counts_out) <- "Total"
          colnames(proportions_out) <- "Total"
          names(totals_counts_out) <- "Total"
          names(totals_proportions_out) <- "Total"
          names(unweighted_n_out) <- "Total"
        }

        banner_var_cross <- structure(list(
          counts = counts_out,
          proportions = proportions_out,
          totals_counts = totals_counts_out,
          totals_proportions = totals_proportions_out,
          unweighted_n = unweighted_n_out
        ), class = c("CrossTabBannerVar", "list"))

        # this could be probably implemented in a more readable and efficient way
        # there may be a few subbanners and each of them should have a 'total' column
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
