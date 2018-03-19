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
calcTabInsertions <- function (vec, elements, var_cats) {

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


#' @importFrom stats weighted.mean
# a list of possible summary statistics to use as an insertion
calcTabMeanInsert <- function (vec, var_cats) {
    ok <- !is.na(vec) & !is.na(values(var_cats))
    return(weighted.mean(values(var_cats)[ok], vec[ok]))
}

