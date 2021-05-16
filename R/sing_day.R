#' Produces the string for one day of the song.
#'
#' @param dataset A data frame containing information about gifts
#' @param line The number of the line for the day you want to sing about
#' @param phrase_col The variable name for the column in the dataset that
#' contains the gift phrases
#'
#' @return A string singing the line of the song with all gifts for the given day.
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export

sing_day <- function(dataset, line, phrase_col) {
  stopifnot(is.data.frame(dataset))
  stopifnot(is.numeric(line))
  stopifnot(line > 0 & line <= length(row.names(dataset)))

  phrases <- dataset %>% pull({{phrase_col}})

  ret_string = str_glue("On the ", dataset[line, 2], " day of Christmas, my true love sent to me,")

  if (line == 1) {
    # We have to modify the phrase for line one if it is by itself
    phrase = str_replace(phrases[1], "and ", "")
    ret_string <- str_glue(ret_string, phrase, .sep = "\n")
  }
  else {
    phrases <- rev(phrases[1:line])
    ret_string <- str_glue(ret_string, paste(phrases, collapse = ",\n"), .sep = "\n")
  }
  ret_string <- str_glue(ret_string, ".")
  return(ret_string)
}
