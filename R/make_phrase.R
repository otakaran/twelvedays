#' Puts the various parts of speech together into a full phrase.
#'
#' @param num An integer
#' @param num_word A string corresponding to the integer
#' @param item A string
#' @param verb A string
#' @param adjective A string
#' @param location A string
#'
#' @return A string containing the words in grammatical order.
#'
#' @import stringr
#' @import glue
#' @import dplyr
#' @import purrr
#'
#' @export

make_phrase <- function(num, num_word, item, verb, adjective, location) {
  stopifnot(is.numeric(num))
  stopifnot(is.character(num_word))
  stopifnot(is.character(item) | is.na(item))
  stopifnot(is.character(verb) | is.na(verb))
  stopifnot(is.character(adjective) | is.na(adjective))
  stopifnot(is.character(location) | is.na(location))

  nums_str=c("and a", "two", "three", "four", "five", "six",
             "seven", "eight", "nine", "ten", "eleven", "twelve")

  if (num > 1) {
    item = pluralize_gift(item)
  }

  if ((is.na(verb) | verb == "") & (is.na(location) | location == "")) {
    line = str_glue(nums_str[num], adjective, item, .sep = " ")
  }
  else if (is.na(location) | location == "") {
    line = str_glue(nums_str[num], item, verb, .sep = " ")
  }
  else {
    line = str_glue(nums_str[num], item, location, .sep = " ")
  }

  return(line)
}

