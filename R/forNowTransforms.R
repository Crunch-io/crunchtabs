# collate insertions and categories together
collateCats <- function (inserts, var_cats) {
    # setup abstract categories to collate into
    cats_out <- AbsCats(data=lapply(var_cats, function(x) {
        new_abscat <- as(x, "AbsCat")
        # save the original class for easy of iding former categories later.
        new_abscat$class <- class(x)
        return(new_abscat)
    }))
    
    # for each insert, find the position for its anchor, and add the insertion
    # at that position we use a for loop, because as we insert, the positions of
    # categories (which may serve as anchors) will change.
    for (insert in inserts) {
        pos <- findInsertPosition(insert, cats_out)
        new_abscat <- as(insert, "AbsCat")
        new_abscat$class <- class(insert)
        cats_out <- AbsCats(data = append(cats_out, list(new_abscat), pos))
    }
    return(cats_out)
}
# for a single Insertion, and a set of categories (or merged categories and
# insertions) find the position to insert to
findInsertPosition <- function (insert, cats) {
    anchr <- anchor(insert)
    if (anchr == 0) {
        return(0)
    }
    # if the anchor is the id of a non-missing category place after
    if (anchr %in% ids(cats)) {
        which_cat <- which(anchr == ids(cats))
        if (!is.na(cats[[which_cat]])) {
            return(which_cat)
        }
    }
    
    # all other situations, put at the end
    return(Inf)
}

# make styles based on transforms and categories
transformStyles <- function (trans, cats) {
    # collate categories and instertions
    all_labs <- collateCats(trans$insertions, cats)
    
    # make a list of styles to apply
    styles <- sapply(all_labs, function (lab) {
        if (is.abscat.subtotal(lab)) {
            return('subtotal')
        } else if (is.abscat.heading(lab)) {
            return('heading')
        } else {
            return(NA)
        }
    })
    return(styles)
}

