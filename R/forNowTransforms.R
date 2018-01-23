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
    return(sapply(cats_out, class))
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
