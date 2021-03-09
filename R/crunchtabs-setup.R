#' Setup for crunchtabs
#'
#' A helper function for first-time users.
#'
# #' @export
# crunchtabs_setup <- function(print_test_page = TRUE) {
#   message("We will attempt to install tinytex, if your system refuses to",
#           "install we recommend visiting the documentation for tinytex at",
#           " https://yihui.org/tinytex/. On your first use of crunchtabs you ",
#           "could experience a delay (5m) as tinytex loads CTAN (Comprehensive ",
#           "Tex Archive Network) packages used by crunchtabs.")
#   tinytex::install_tinytex()
#
#   if (print_test_page) {
#     message("We will now attempt to print a test page")
#     message("Loading an example dataset...")
#     ds <- crunch::newExampleDataset()
#     message("Calculating toplines...")
#     ct <- crosstabs(ds)
#     message("Writing to PDF and opening")
#     writeLatex(ct, pdf = TRUE, open = TRUE)
#   }
# }
