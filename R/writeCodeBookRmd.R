#' Create a codebook
#'
#' @param ds A crunch dataset
#' @param url A crunch dataset url
#' @param rmd Should we create an interim Rmd file? Defaults to TRUE
#' @param pdf Should we write directly to pdf? Defaults to TRUE
#'  title = getName(data_summary),
#' @param title An optional title. Defaults to the data summary title.
#' @param subtitle An optional character subtitle. Defaults to an empty string.
#' @param table_of_contents logical. Should a list of tables be included at the start
#' of the report Defaults to \code{FALSE}.
#' @param sample_desc A character string describing the sample.
#' @param field_period A character string describing the field period.
#' @param ... Additional arguments. Unused.
#' @export
# writeCodeBook <- function(ds, url = NULL, rmd = TRUE, pdf = TRUE, title = NULL,
#   subtitle = NULL, table_of_contents = FALSE, sample_desc = NULL,
#   field_period = NULL, ...) {
#
#   preamble <- readLines(
#     system.file(
#       "codebook_header.Rmd",
#       package = "crunchtabs"
#     )
#   )
#
#   fh = NULL
#   t1 = !is.null(title)
#   t2 = !is.null(subtitle)
#
#   if (t1 & t2) { # title and subtitle
#     tex = "\\fancyhead{}\n\\fancyhead[L]{{\\fontsize{16}{24}\\textbf{%s}}\\fontsize{12}{18}\\textbf{%s}}"
#     fh = sprintf(tex, title, subtitle)
#   }
#
#   if (t1 & !t2) { # title no subtitle
#     tex = "\\fancyhead{}\n\\fancyhead[L]{{\\fontsize{16}{24}\\textbf{%s}}}"
#     fh = sprintf(tex, title, subtitle)
#   }
#
#   if (t2 & !t1) # subtitle no title
#     stop("You specified a subtitle but not a title")
#
#   sample_description <- latexSampleDescription(
#     sample_desc = sample_desc,
#     field_period = field_period,
#     moe = NULL
#   )
#
#   # Some datasets are not visible using their name
#   if (is.null(url)) {
#     dataset = sprintf(
#       '```{r} \nds = loadDataset("%s") \n```\n\n',
#       name(ds)
#     )
#   } else {
#     dataset = sprintf(
#       '```{r} \nds = loadDataset("%s") \n```\n\n',
#       url
#     )
#   }
#
#
#   kables <- list()
#   nms <- names(ds)
#   for (nm in nms) {
#
#     if (codeBookSummary(ds[[nm]]) %>% nrow() > 20) {
#       kables[[nm]] <- sprintf(trimws(
#         "```{r, results = 'asis'}
#       codeBookItemTxtHeader(ds[['%s']])
#       codeBookItemTxtDescription(ds[['%s']])
#       ````\n\n
#       ```{r, results = 'asis'}
#       codeBookItemBody(ds[['%s']])
#       ```\n\n
#       "),
#         nm, nm, nm
#       )
#     } else {
#       kables[[nm]] <- sprintf(trimws(
#         "```{r, results = 'asis'}
#       codeBookItemTxtHeader(ds[['%s']])
#       codeBookItemTxtDescription(ds[['%s']])
#       codeBookItemBody(ds[['%s']])
#       ```\n\n
#       "),
#         nm, nm, nm
#       )
#
#     }
#
#     # manual rule example
#     # \\begin{center}\\rule{\\linewidth}{0.5pt}\\end{center}
#
#     kables[[nm]] <- gsub("      ", "", kables[[nm]])
#     kables[[nm]] <- gsub("++", "%>%", kables[[nm]], fixed = TRUE)
#   }
#
#   write(
#     c(
#       preamble,
#       fh,
#       sample_description,
#       dataset,
#       paste0(kables, collapse = "\n\n")
#     ), file = gsub(" ", "-", paste0(name(ds), ".Rmd"), fixed = T)
#   )
#   if (pdf) {
#     rmarkdown::render(gsub(" ", "-", paste0(name(ds), ".Rmd"), fixed = T))
#     file.open(gsub(" ", "-", paste0(name(ds), ".pdf"), fixed = T))
#   }
#
# }
