#' Produces the string up to a given day in the song.
#'
#' @param dataset A data frame containing information about gifts
#' @param max_day The number of days to sing until
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

sing_all_untill_day <- function(dataset, max_day, phrase_col) {
  stopifnot(is.data.frame(dataset))
  stopifnot(is.numeric(max_day))
  stopifnot(line > 0 & line <= length(row.names(dataset)))

  result <- lapply(1:max_day, sing_day, dataset = dataset, phrase_col = Full.Phrase) %>%
    str_c(collapse = "\n\n") %>%
    str_glue()

  return(result)
}
