#' Takes a noun and makes it plural
#'
#' @param gift A string or vector of strings
#'
#' @return A string or vector of strings with the pluralized words
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export

pluralize_gift <- function(gift) {
  stopifnot(is.character(gift))
  stopifnot(is.vector(gift) | is.atomic(gift))

  gift <- gift %>%
    str_replace("y$", "ie") %>%
    str_replace("$", "s") %>%
    str_replace("ooses", "eese")

  return(gift)
}
