# To create a new example dataset, go to https://g4-emea.yougov.net/#crunchtabs_example
# Run a monkey test with your desired number of rows.

# Change user to your first.last name
user <- "persephone.tsebelis"
# Go to this URL. It will upload and show you the progress each time yu reload the page.
# The reason for Sys.Date() is so you don't accidentally delete someone elses example
# dataset.
url <- paste0(
  "https://crunch-upload.uat.yougov.net/upload/crunchtabs_example/?share_with=",
  user, "@yougov.com&segment=monkeys&starttime_from=", Sys.Date()
)
url

# Once the dataset has been uploaded, load it here:
ds <- unlock(loadDataset(paste0("YouGov crunchtabs_example monkeys (starttime: from ", Sys.Date(), ")")))
# Original one is:
# ds <- loadDataset("YouGov crunchtabs_example monkeys (starttime: from 2018-06-19)", project="crunchtabs")
# ds <- loadDataset("https://app.crunch.io/api/datasets/868e8b3e01834c45b73e56e80160d3c3/")
# Move it to the crunchtabs project so that you have it for the future
proj <- projects()$crunchtabs
datasets(proj) <- ds

ds <- deleteVariable(ds, "polAttentionpdl")

values(categories(ds$books2)) <- c(1:5, NA, NA, NA)
subtotals(ds$books2) <- list(Subtotal(name = "3 or more", categories = 3:5, position = "bottom"))
values(categories(ds$books3_book)) <- c(0.5, 2.5, 4.5, 6, NA, NA, NA)
values(categories(ds$books3_television)) <- c(2.5, 7.5, 12.5, 17.5, 20, NA, NA, NA)
values(categories(ds$books5)) <- c(2, 1, 0, -1, -2, NA, NA, NA)
subtotals(ds$books5) <- list(
  Subtotal(name = "Net: like", categories = 1:2, after = 2),
  Subtotal(name = "Neutral", categories = 3, after = 3),
  Subtotal(name = "Net: Dislike", categories = 4:5, after = 5)
)
names(subvariables(ds$books7)) <- gsub(".*-- ", "", names(subvariables(ds$books7)))
subtotals(ds$books7) <- list(Subtotal(name = "Net: Not Other", categories = 1:3, position = "top"))
subtotals(ds$books8) <- list(Heading(name = "Half way through", after = 498))
is.na(categories(ds$books8))[ids(categories(ds$books8)) == 498] <- TRUE

values(categories(ds$movies1)) <- c(-2, -1, 0, 1, 2, NA, NA, NA)

subtotals(ds$tv3) <- list(
  Heading(name = "Brooklyn 99", after = 1),
  Subtotal(name = "Net: Brooklyn 99", categories = 1, position = "bottom")
)
values(categories(ds$tv4)) <- c(2, 1, 0, -1, -2, NA, NA, NA)
subtotals(ds$tv4) <- list(
  Subtotal(name = "Net: like", categories = 1:2, after = 2),
  Subtotal(name = "Neutral", categories = 3, after = 3),
  Subtotal(name = "Net: Dislike", categories = 4:5, after = 5)
)

values(categories(ds$art1_a)) <- c(1:5, NA, NA, NA, NA)
is.na(categories(ds$art1_a))[3] <- TRUE
subtotals(ds$art2) <- list(Subtotal(name = "Net: Not Poetry", categories = 1:4, position = "bottom"))
is.na(categories(ds$art2))[ids(categories(ds$art2)) == 3] <- TRUE
values(categories(ds$misc2)) <- c(1:3, NA, NA, NA, NA)
ds$misc2_dk <- copyVariable(ds$misc2, name = "Importance Rank (with not ranked)")
is.na(categories(ds$misc2_dk))[names(categories(ds$misc2_dk)) %in% c("don't know", "skipped")] <- FALSE
mv(ds, ds["misc2_dk"], "/Miscellaneous")
if (aliases(cd(ds, "/Miscellaneous"))[5] %in% "misc2_dk") {
  setOrder(
    cd(ds, "/Miscellaneous"),
    c(1:3, 5, 4)
  )
}
ds$art5_nonUniform <- copyVariable(ds$art5, name = "Art Talked About (Non Uniform Basis)")
uniformBasis(ds$art5_nonUniform) <- FALSE
notes(ds$art5_nonUniform) <- "Asked of those who like art form. Non uniform basis."
mv(ds, ds["art5_nonUniform"], "/Art")

values(categories(ds$political_attention)) <- c(0:10, NA, NA, NA)
subtotals(ds$profile_partyid) <- list(Subtotal(name = "Yes", categories = 1:5, after = -1))

for (var in c(
  "books1", "books3", "books7", "books8", "movies4", "tv2", "tv3", "tv5",
  "art2", "art4", "age4", "profile_gender", "age4_gender", "profile_socialgrade_cie",
  "socialgrade_weight", "profile_ethnicity", "profile_GOR", "profile_govregnew",
  "profile_region", "profile_education_level_recode", "xprofile_gross_household",
  "profile_bpcnews", "profile_newstype", "profile_marital_stat", "profile_work_industry",
  "xprofile_religion", "profile_partyid", "pastvote_2015_recode", "pastvote_2015_by_region",
  "profile_past_vote_2010", "profile_past_vote_2005", "exit_status", "disposition",
  "has_profanity", "has_media_error", "has_test"
)) {
  values(categories(ds[[var]])) <- NA
}

# # If the derived variables are not filled in correctly, use these lines.
# # Also age was all > 35 for some reason so I needed to lower the min age.
# ds$age <- sample(16:80, nrow(ds), replace = TRUE)
# ds$age4[ds$age >= 16 & ds$age < 25] <- 1
# ds$age4[ds$age >= 25 & ds$age < 40] <- 2
# ds$age4[ds$age >= 40 & ds$age < 55] <- 3
# ds$age4[ds$age >= 55] <- 4

# ds$age5[ds$age >= 16 & ds$age < 25] <- 1
# ds$age5[ds$age >= 25 & ds$age < 35] <- 2
# ds$age5[ds$age >= 35 & ds$age < 45] <- 3
# ds$age5[ds$age >= 45 & ds$age < 55] <- 4
# ds$age5[ds$age >= 55] <- 5

# ds$age4_gender[ds$age4 %in% 1 & ds$profile_gender %in% 1] <- 1
# ds$age4_gender[ds$age4 %in% 2 & ds$profile_gender %in% 1] <- 2
# ds$age4_gender[ds$age4 %in% 3 & ds$profile_gender %in% 1] <- 3
# ds$age4_gender[ds$age4 %in% 4 & ds$profile_gender %in% 1] <- 4
# ds$age4_gender[ds$age4 %in% 1 & ds$profile_gender %in% 2] <- 5
# ds$age4_gender[ds$age4 %in% 2 & ds$profile_gender %in% 2] <- 6
# ds$age4_gender[ds$age4 %in% 3 & ds$profile_gender %in% 2] <- 7
# ds$age4_gender[ds$age4 %in% 4 & ds$profile_gender %in% 2] <- 8

# ds$socialgrade_weight[ds$profile_socialgrade_cie %in% c(1,2)] <- 1
# ds$socialgrade_weight[ds$profile_socialgrade_cie %in% 3] <- 2
# ds$socialgrade_weight[ds$profile_socialgrade_cie %in% 4] <- 3
# ds$socialgrade_weight[ds$profile_socialgrade_cie %in% 5] <- 4
# ds$socialgrade_weight[ds$profile_socialgrade_cie %in% 6] <- 5

# ds$profile_bpcnews <- sample(1:7, nrow(ds), replace = TRUE)

weight(ds) <- makeWeight(ds$age4_gender ~ c(7.6, 12.1, 12.9, 16, 7.2, 12.2, 13.2, 18.8),
  ds$socialgrade_weight ~ c(25.9, 29.1, 20.7, 16.2, 8.1),
  ds$profile_bpcnews ~ c(16, 22, 16, 4, 9.5, 12.5, 20),
  ds$profile_GOR ~ c(4.2, 11.2, 8.5, 7.3, 8.8, 9.3, 12.4, 13.6, 8.5, 4.9, 8.4, 2.9, 0),
  name = "weight", alias = "weight", description = "weight"
)
