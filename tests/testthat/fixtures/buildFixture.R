# To create a new example dataset, go to https://g4-emea.yougov.net/#crunchtabs_example
# Run a monkey test with your desired number of rows.

# Change user to your first.last name
user <- "persephone.tsebelis"
# Go to this URL. It will upload and show you the progress each time yu reload the page.
# The reason for Sys.Date() is so you don't accidentally delete someone elses example
# dataset.
url <- paste0("https://crunch-upload.uat.yougov.net/upload/crunchtabs_example/?share_with=", 
    user, "@yougov.com&segment=monkeys&starttime_from=", Sys.Date())

# Once the dataset has been uploaded, load it here:
ds <- loadDataset(paste0("YouGov crunchtabs_example monkeys (starttime: from ", Sys.Date(), ")"))

values(categories(ds$books2)) <- c(1:5, NA, NA, NA)
subtotals(ds$books2) <- list(Subtotal(name = "3 or more", categories = 3:5, position = "bottom"))
values(categories(ds$books3_book)) <- c(0.5, 2.5, 4.5, 6, NA, NA, NA)
values(categories(ds$books3_television)) <- c(2.5, 7.5, 12.5, 17.5, 20, NA, NA, NA)
values(categories(ds$books5)) <- c(2, 1, 0, -1, -2, NA, NA, NA)
subtotals(ds$books5) <- list(Subtotal(name = "Net: like", categories = 1:2, after = 2),
    Subtotal(name = "Neutral", categories = 3, after = 3),
    Subtotal(name = "Net: Dislike", categories = 4:5, after = 5))
names(subvariables(ds$books7)) <- gsub(".*-- ", "", names(subvariables(ds$books7)))
subtotals(ds$books7) <- list(Subtotal(name = "Net: Not Other", categories = 1:3, position = "top"))
subtotals(ds$books8) <- list(Heading(name = "Half way through", after = 498))
is.na(categories(ds$books8))[ids(categories(ds$books8)) == 498] <- TRUE

values(categories(ds$movies1)) <- c(-2, -1, 0, 1, 2, NA, NA, NA)

subtotals(ds$tv3) <- list(Heading(name = "Brooklyn 99", after = 1),
    Subtotal(name = "Net: Brooklyn 99", categories = 1, position = "bottom"))
values(categories(ds$tv4)) <- c(2, 1, 0, -1, -2, NA, NA, NA)
subtotals(ds$tv4) <- list(Subtotal(name = "Net: like", categories = 1:2, after = 2),
    Subtotal(name = "Neutral", categories = 3, after = 3),
    Subtotal(name = "Net: Dislike", categories = 4:5, after = 5))

values(categories(ds$art1_a)) <- c(1:5, NA, NA, NA, NA)
is.na(categories(ds$art1_a))[3] <- TRUE
subtotals(ds$art2) <- list(Subtotal(name = "Net: Not Poetry", categories = 1:4, position = "bottom"))
is.na(categories(ds$art2))[ids(categories(ds$art2)) == 3] <- TRUE
values(categories(ds$misc2)) <- c(1:3, NA, NA, NA, NA)
ds$misc2_dk <- copyVariable(ds$misc2, name = "Importance Rank (with not ranked)")
is.na(categories(ds$misc2_dk))[names(categories(ds$misc2_dk)) %in% c("don't know", "skipped")] <- FALSE
ds$art5_nonUniform <- copyVariable(ds$art5, name = "Art Talked About (Non Uniform Basis)")
uniformBasis(ds$art5_nonUniform) <- FALSE
notes(ds$art5_nonUniform) <- "Asked of those who like art form. Non uniform basis."
mv(ds, ds["art5_nonUniform"], "/Art")

values(categories(ds$political_attention)) <- c(0:10, NA, NA, NA)
values(categories(ds$polAttentionpdl)) <- c(0:10, NA, NA, NA)
subtotals(ds$profile_partyid) <- list(Subtotal(name = "Yes", categories = 1:5, after = -1))

for (var in c("books1", "books3", "books7", "books8", "movies4", "tv2", "tv3", "tv5", 
    "art2", "art4", "profile_ethnicity", "profile_gender", "profile_GOR", "profile_govregnew", 
    "xprofile_gross_household", "profile_marital_stat", "profile_newstype", 
    "profile_region", "profile_socialgrade_cie", "profile_work_industry", 
    "profile_education_level_recode", "xprofile_religion", "profile_partyid", 
    "profile_past_vote_2005", "profile_past_vote_2010", "pastvote_2015_by_region", 
    "political_attention", "pastvote_2015_recode", "polAttentionpdl", "exit_status", 
    "disposition", "has_profanity", "has_media_error", "has_test")) {
    values(categories(ds[[var]])) <- NA
}




