#' Basic information about a Crunch variable
#'
#' \code{toplineBase} returns basic information about a Crunch variable, e.g.
#' alias, name, type, description.
#'
#' @param x A Crunch variable.
#' @return An object containing basic information about the variable.
#' @export
toplineBase <- function(x) {
    res <- list(alias = alias(x), name = name(x), type = type(x), description = description(x), 
        notes = notes(x))
    class(res) <- c("ToplineBase")
    res
}
