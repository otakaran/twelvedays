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
  stopifnot(is.character(num_word) | in.na(num_word))
  stopifnot(is.character(item) | is.na(item))
  stopifnot(is.character(verb) | is.na(verb))
  stopifnot(is.character(adjective) | is.na(adjective))
  stopifnot(is.character(location) | is.na(location))

  nums_str=c("and a", "Two", "Three", "Four", "Five", "Six",
             "Seven", "Eight", "Nine", "Ten", "Eleven", "Twelve")

  # Grammar rule
  if (num == 1 & str_detect(substr(item, 1, 1), "a|e|i|o|u")) { nums_str = "and an" }

  # Pluralize if more than one of an item
  if (num > 1) { item = pluralize_gift(item) }

  # If function was given a num_word use it, otherwise use hard-coded list
  if (is.na(num_word) | num_word == "") { num_word = nums_str[num] }

  if ((is.na(verb) | verb == "") & (is.na(location) | location == "")) {
    line = str_glue(num_word, adjective, item, .sep = " ")
  }
  else if (is.na(location) | location == "") {
    line = str_glue(num_word, item, verb, .sep = " ")
  }
  else {
    line = str_glue(num_word, item, location, .sep = " ")
  }

  return(line)
}

