options(stringsAsFactors = FALSE)
library(crunch)
login()

# dt <- read.csv('~/Dropbox (YouGov Analytics)/crunchtabs/tests/testthat/fixtures/ds1.csv')
# names(dt)[match(c('Weight', 'q1', 'q3'), names(dt))] <- c('weight', 'favpet', 'petname')
# dt$noweight <- sample(0:-5, 20, replace = TRUE)
# dt$wave <- as.Date(dt$wave)
# for (v in c('ndogs', 'ndogs_b')){
#     dt[[v]][dt[[v]] %in% 'No Data'] <- -1
#     dt[[v]][dt[[v]] %in% 'skipped'] <- -8
#     dt[[v]][dt[[v]] %in% 'not asked'] <- -9
# }
# 
# dt$ndogs <- as.numeric(dt$ndogs)
# dt$ndogs_b <- as.numeric(dt$ndogs_b)
# ds <- newDataset(dt, name='crunchtabs fixture')
# 
# df <- read.csv('~/Dropbox (YouGov Analytics)/crunchtabs/tests/new_ds_variables.csv')
# for (i in 1:nrow(df)){
#     if (df[i, 'type'] %in% 'categorical') type(ds[[df[i, 'alias']]]) <- 'categorical'
#     if (df$type[i] %in% c('categorical_array', 'multiple_response')){
#         subs <- read.csv(paste0('~/Dropbox (YouGov Analytics)/crunchtabs/tests/new_ds_', df$alias[i], '_subvariables.csv'))
#         for (j in 1:nrow(subs)){
#             type(ds[[subs[j, 'alias']]]) <- 'categorical'
#         }
#         names(variables(ds))[match(subs[['alias']], aliases(variables(ds)))] <- subs[['name']]
#         ds[[df[i, 'alias']]] <- makeArray(ds[subs[['alias']]], name=df[i, 'name'])
#         if (df[i, 'type'] == 'multiple_response') dichotomize(ds[[df[i, 'alias']]], 'selected')
#     }
# }
# names(variables(ds))[match(df$alias, aliases(variables(ds)))] <- df$name
# descriptions(variables(ds))[match(df$alias, aliases(variables(ds)))] <- df$description
# 
# for (v in setdiff(aliases(variables(ds))[types(variables(ds)) == 'numeric'], 'noweight')){
#     crPUT(shojiURL(ds[[v]], 'fragments', 'missing_rules'),body='{"rules":{"skipped":{"value":-9}, "not asked":{"value":-8}}}')
#     ds[[v]][ds[[v]] == -1] <- NA
# }
# 
# weight(ds) <- ds$weight
# 
# entities(ordering(ds)) <- ds[c("allpets", "favpet", 'petname', "petloc", "ndogs", "ndogs_a", "ndogs_b", "country", "age", "age2", "age3", "age5", "gender", "weight", 'wave', "noweight")]
# 
# categories(ds$age5) <- c(Category(name='16 to 24', id=1, numeric_value=1, missing=FALSE), 
#     Category(name='25 to 34', id=2, numeric_value=2, missing=FALSE), Category(name='35 to 44', id=3, numeric_value=3, missing=FALSE), 
#     Category(name='45 to 54', id=4, numeric_value=4, missing=FALSE), Category(name='55+', id=5, numeric_value=5, missing=FALSE))
# categories(ds$gender) <- c(Category(name='Male', id=1, numeric_value=1, missing=FALSE), Category(name='Female', id=2, numeric_value=2, missing=FALSE))
# 


library(httptest)
capture_requests(crGET(self(ds)), path = '~/Dropbox (YouGov Analytics)/crunchtabs/')
capture_requests(crGET(self(variables(ds))), path = '~/Dropbox (YouGov Analytics)/crunchtabs/')
for (v in aliases(variables(ds))) capture_requests(crGET(self(ds[[v]])), path = '~/Dropbox (YouGov Analytics)/crunchtabs/')
for (v in aliases(variables(ds))) capture_requests(as.vector(ds[[v]]), path = '~/Dropbox (YouGov Analytics)/crunchtabs/')
for (v in aliases(variables(ds))[types(variables(ds)) %in% c('multiple_response', 'categorical_array')]){
    capture_requests(crGET(self(subvariables(ds[[v]]))), path = '~/Dropbox (YouGov Analytics)/crunchtabs/')
    for (s in aliases(subvariables(ds[[v]]))){
        capture_requests(crGET(self(ds[[v]][[s]])), path = '~/Dropbox (YouGov Analytics)/crunchtabs/')
    }
}
capture_requests(crGET(self(ordering(ds))), path = '~/Dropbox (YouGov Analytics)/crunchtabs/')
# for (v in aliases(variables(ds))){
#     for (v1 in setdiff(aliases(variables(ds)), v)){
#         capture_requests(crtabs(paste0('~ ', v, ' + ', v1), ds), path = '~/Dropbox (YouGov Analytics)/crunchtabs/')
#     }
# }

banner_data <- jsonlite::unserializeJSON(readLines(con = file.path('~/Dropbox (YouGov Analytics)/crunchtabs/tests/testthat/fixtures/', "ds1_banner1.json")))
tabBook_vars <- c("allpets", "favpet", "petloc", "ndogs", "ndogs_a", "ndogs_b", "country", "age", "age2", "age3", "age5", "gender", "weight", "noweight")
capture_requests(crosstabs(ds, vars=tabBook_vars, banner=banner_data), path = '~/Dropbox (YouGov Analytics)/crunchtabs/')
capture_requests(tabBook(multitable=multitables(ds)[[1]], ds, weight=ds$weight), path = '~/Dropbox (YouGov Analytics)/crunchtabs/')




