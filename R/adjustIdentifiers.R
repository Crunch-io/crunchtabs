#'
#' @export
adjustCrunchName <- function(nm) {
  nm <- gsub("Level of agreement (5-point scale): ","", nm, fixed = T)
  nm <- gsub('"', '', nm)
  if(nchar(nm) > 40) {
    nm <- substr(nm, 1, 37)
    nm <- paste0(nm, "...")
  }
  nm
}

#' @export
adjustCrunchAlias <- function(alias) {
  if(nchar(alias) > 35) {
    alias <- substr(alias, 1, 32)
    alias <- paste0(alias, "...")
  }
  alias
}

#' @export
adjustCrunchDescription <- function(nm, alias, description) {
  nm <- gsub("Level of agreement (5-point scale): ","", nm, fixed = T)
  nm <- gsub('"', '', nm)

  if(nchar(nm) > 40 & !grepl(nm, description)) {
    description <- paste0(nm, description)
  }

  if(nchar(alias) > 35 & description != "") {
    description <- paste0(description, " \n(", alias,")")
  } else if(description == "") {
    description <- paste0("(", alias, ")")
  }

  description
}
