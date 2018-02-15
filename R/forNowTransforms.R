
# collate insertions and categories together
# given a set of insertions and categories, collate together into a single set
# of AbstractCategories which includes both `Category`s and `Insertion`s
collateCats <- function (inserts, var_cats) {
    # setup an empty AbstractCategories object to collate into
    cats_out <- AbstractCategories()
    cats_out@.Data <- var_cats
    
    # for each insert, find the position for its anchor, and add the insertion
    # at that position we use a for loop, because as we insert, the positions of
    # categories (which may serve as anchors) will change.
    for (insert in inserts) {
        pos <- findInsertPosition(insert, cats_out)
        cats_out@.Data <- append(cats_out, list(insert), pos)
    }
    return(cats_out)
}

# for a single Insertion, and a set of categories (or collated categories and
# insertions) find the position to insert to
findInsertPosition <- function (insert, cats) {
    anchr <- anchor(insert)
    # if the anchor is 0, put at the beginning
    if (anchr == 0 | anchr == "top") {
        return(0)
    }
    
    # if the anchor is the id of a non-missing category put it after that cat
    if (anchr %in% ids(cats)) {
        which_cat <- which(anchr == ids(cats))
        if (!is.na(cats[[which_cat]])) {
            return(which_cat)
        }
    }
    
    # all other situations, put at the end
    return(Inf)
}

#' Given a vector of values and elements, calculate the insertions
#'
#' @param vec values to transform (a single dimension of an array)
#' @param elements AbstractCategories of both `Category`s and `Insertion`s to
#' calculate. Generally derived from `mapInsertions()`
#' @param var_cats the `Categories` object tat corresponds to the vector in
#' `vec` of the transform
#'
#' @return the values given in `vec`, with any insertions specified in
#' `trans` calculated and inserted
#' @keywords internal
calcInsertions <- function (vec, elements, var_cats) {

    # make the actual calculations and insertions
    vec_out <- data.frame(do.call(rbind, lapply(elements, function (element) {
        # if element is a category, simply return the value
        if (inherits(element, "Category")) {
            return(vec[name(element),])
        }
        
        # if element is a heading return NA (since there is no value to be
        # calculated but we need a placeholder non-number)
        if (inherits(element, "Heading")) {
            return(NA)
        }
        
        # if element is a subtotal, sum the things it corresponds to which are
        # found with arguments()
        if (inherits(element, "Subtotal")) {
            # grab category combinations, and then sum those categories.
            combos <- element$categories
            which.cats <- names(var_cats[ids(var_cats) %in% combos])
            if (dim(vec)[2] == 1) return(sum(vec[which.cats,]))
            return(colSums(vec[which.cats,]))
        }
        
    })))
    
    colnames(vec_out) <- colnames(vec)
    
    # make sure that the vector is named appropriately
    rownames(vec_out) <- names(elements)
    
    return(vec_out)
}
