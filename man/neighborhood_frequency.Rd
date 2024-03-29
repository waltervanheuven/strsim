% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strsim.R
\name{neighborhood_frequency}
\alias{neighborhood_frequency}
\title{Neighbourhood Frequency of a list of words}
\usage{
neighborhood_frequency(
  targets_spellings,
  all_words,
  all_frequencies,
  show = FALSE,
  pb = FALSE,
  parallel = FALSE
)
}
\arguments{
\item{targets_spellings}{vector of target strings}

\item{all_words}{vector of words}

\item{all_frequencies}{vector of word frequencies}

\item{show}{show neighbours}

\item{pb}{show progress bar}

\item{parallel}{parallel processing}
}
\value{
list of the number of higher frequency neighbours
}
\description{
`neighborhood_frequency()` calculates the neighbourhood frequency
of a list of words based on the words and frequencies provided.
}
\examples{
\dontrun{
neighborhood_frequency("book", subtlex_uk$Spelling, subtlex_uk$`LogFreq(Zipf)`, show=T)
}

}
