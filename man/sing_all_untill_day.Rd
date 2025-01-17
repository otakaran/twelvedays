% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sing_all_untill_day.R
\name{sing_all_untill_day}
\alias{sing_all_untill_day}
\title{Produces the string up to a given day in the song.}
\usage{
sing_all_untill_day(dataset, max_day, phrase_col)
}
\arguments{
\item{dataset}{A data frame containing information about gifts}

\item{max_day}{The number of days to sing until}

\item{phrase_col}{The variable name for the column in the dataset that
contains the gift phrases}
}
\value{
A string singing the line of the song with all gifts for the given day.
}
\description{
Produces the string up to a given day in the song.
}
