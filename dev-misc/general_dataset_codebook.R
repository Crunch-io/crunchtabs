library(crunchtabs)

set.seed(42)

ds <- data.frame(
  a_factor = sample(letters[1:5], 1000, replace = TRUE),
  a_character = sample(letters, 1000, replace = TRUE),
  a_numeric = rnorm(1000),
  an_integer = round(runif(1000, 0, 1000)),
  a_factor_missings = sample(c(letters[1:5], NA), 1000, replace = TRUE),
  a_numeric_missings = sample(c(NA, rnorm(20)), 1000, replace = TRUE),
  an_integer_missings = sample(c(NA, round(runif(20, 0, 100))), 1000, replace = TRUE),
  a_character_missings = sample(c(NA, letters), 1000, replace = TRUE),
  stringsAsFactors = FALSE
)

meta <- data.frame(
  alias = names(ds),
  name = paste0("Lorem ipsum dolor sit amet ", names(ds)),
  description = substr(stringi::stri_rand_lipsum(8), 0, 160),
  notes = sample(c("", "Asked only of a specific group"), 8, replace = TRUE),
  labels = c(
    '"a", "b", "c", "d", "e"',
    NA,
    NA,
    NA,
    '"a", "b", "c", "d", "e"',
    NA,
    NA,
    NA
  )
)



writeCodeBookLatexGeneric(ds, meta)

