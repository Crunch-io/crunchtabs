#' Basic information about a Crunch variable
#'
#' \code{toplineBase} returns basic information about a Crunch variable, e.g.
#' alias, name, type, description.
#'
#' @param var A Crunch variable.
#' @return An object containing basic information about the variable.
#' @importFrom crunch alias name type description notes
#' @export
toplineBase <- function (var) {
    return(structure(list(
            alias = alias(var),
            name = name(var),
            type = type(var),
            description = description(var),
            notes = notes(var)
        ),
        class="ToplineBase"))
}
