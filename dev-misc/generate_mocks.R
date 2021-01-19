library(crunchtabs)
login()

# Generate tabBooks-tabBook_crunchtabs.rds ----
## tabBooks arguments ----
ds <- newExampleDataset()
ds <- loadDataset("Example dataset")
ds$weight1 <- makeWeight(ds$q1 ~ c(0.3,0.3,0.4,0), name = 'weight1')
ds$weight2 <- makeWeight(ds$q1 ~ c(0.4,0.4,0.1,0.1), name = 'weight2')
weight(ds) <- ds$weight1
weight(ds) <- ds$weight2
weight(ds) <- ds$weight1
ds$q1_pre <- copyVariable(ds$q1, deep = TRUE, name = "Pet name pre")
ds$q1_post <- copyVariable(ds$q1, deep = TRUE, name = "Pet name post")
ds$country_pre <- copyVariable(ds$country, deep = TRUE, name = "Country pre")
ds$country_post <- copyVariable(ds$country, deep = TRUE, name = "Country post")

dataset <- ds
vars <- c("allpets", "q1")
banner <- structure(list(Results = list(`___total___` = structure(list(
    alias = "___total___", name = "", type = "Total", old_categories = "Total",
    categories_out = "Total", categories = "Total"), class = "BannerVar"),
    allpets = structure(list(alias = "allpets", name = "All pets owned",
     type = "multiple_response", old_categories = c("Cat",
    "Dog", "Bird"), categories_out = c("Cat", "Dog", "Bird"
    ), categories = c("Cat", "Dog", "Bird")), class = "BannerVar"))), class = "Banner")
weight <- NULL
topline <- TRUE
include_original_weighted <- TRUE

## tabBooks body ----
banner_flatten <- unique(unlist(banner, recursive = FALSE))
names(banner_flatten) <- sapply(banner_flatten, function(v) v$alias)
banner_use <- banner
if (topline) { banner_use$Results[[2]] <- NULL }

multitable <- crunchtabs:::getMultitable(banner_flatten, dataset)

if (is.null(weight) | is.null(weight(dataset))) {
    default_weight <- NULL
} else {
    default_weight <- alias(weight(dataset))
}


if (is.list(weight)) {
    tab_frame <- crunchtabs:::tabBookWeightSpec(
        dataset, weight,
        append_default_wt = include_original_weighted
    )
    tab_frame <- tab_frame[tab_frame$alias %in% vars,]

    book <- suppressWarnings(
        crunchtabs:::tabBook_crunchtabs(
            multitable,
            dataset = dataset[unique(c(vars, unique(tab_frame$weight)))],
            weight = weight,
            append_default_wt = include_original_weighted
        )
    )

} else {

    tab_frame <- crunchtabs:::tab_frame_generate(default_weight, vars)

    book <- suppressWarnings(
        crunchtabs:::tabBook_crunchtabs(
            multitable,
            dataset = dataset[vars],
            weight = weight
        )
    )

}

saveRDS(book, testthat::test_path("fixtures/tabBooks-tabBook_crunchtabs.rds"))

with_consent(delete(ds))
