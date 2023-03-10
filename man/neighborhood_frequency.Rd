% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strsim.R
\name{neighborhood_frequency}
\alias{neighborhood_frequency}
\title{Function calculates the neighbourhood frequency
of a list of words based on the words and frequencies provided.}
\usage{
neighborhood_frequency(
  targets_spellings,
  all_words,
  all_frequencies,
  show = FALSE
)
}
\arguments{
\item{targets_spellings}{target strings}

\item{all_words}{all words}

\item{all_frequencies}{frequencies of all words}

\item{show}{show neighbours}
}
\value{
list of the number of higher frequency neighbours
}
\description{
Function calculates the neighbourhood frequency
of a list of words based on the words and frequencies provided.
}
\examples{
\dontrun{
neighborhood_frequency("book", subtlex_uk$Spelling, subtlex_uk$`LogFreq(Zipf)`, show=T)
}

}
