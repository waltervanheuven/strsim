#' Are two strings neighbours?
#'
#' `is_neighbour()` checks whether two strings are orthographic neighbours
#' according to Coltheart et al.'s (1977) definition of a neighbour (Coltheart's N).
#'
#' @param w1 character string
#' @param w2 character string
#'
#' @return boolean
#'
#' @examples
#' {
#' # Orthographic Neighbour
#' is_neighbour("book", "look")
#' # Not a neighbour
#' is_neighbour("book", "boll")
#' # Not a neighbour
#' is_neighbour("book", "boo")
#' # Orthographic Neighbour
#' is_neighbour("book", "boök")
#' }
#'
#' @export
is_neighbour <- function(w1, w2) {
  return (stringdist::stringdist(w1, w2, method="hamming", nthread = 1) == 1)
}

#' Neighbourhood Density
#'
#' `nd()` calculates the neighbourhood density of a string. It is based
#' on the definition of an orthographic neighbour by Coltheart et al. (1977).
#'
#' @param target_str target character string
#' @param dt data table with words
#' @param show show neighours
#'
#' @return number of neighbours
#'
#' @examples
#' \dontrun{
#' nd("book", data.table(word = all_words))
#' }
#' @import data.table
#' @export
nd <- function(target_str, dt, show=FALSE) {
  # to prevent 'No visible binding for global variable' error,
  # asign NULL to word, see
  # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  word <- NULL
  # find neighbours
  neighbours_detected <- dt[ is_neighbour(target_str, word) ]
  # show them
  if (isTRUE(show) & nrow(neighbours_detected) > 0) {
    print(neighbours_detected)
  }
  # return the number of neighbours
  return(nrow(neighbours_detected))
}

#' Neighbourhood Density of a list of words
#'
#' neighborhood_density()` calculates the neighbourhood density of a list of words.
#'
#' @param targets_spellings target strings
#' @param all_words all words
#' @param show show neighbours
#' @param pb show progress bar
#' @param parallel parallel processing
#'
#' @return list of the number of neighbours
#'
#' @examples
#' \dontrun{
#' nd("book", subtlex_uk$Spelling)
#' nd(subtlex_uk$Spelling, subtlex_uk$Spelling)
#' }
#' @export
neighborhood_density <- function(targets_spellings, all_words, show=FALSE,
                                 pb = FALSE, parallel = FALSE) {
  # create DT
  dt <- data.table::data.table(
    word = all_words
  )
  if (isTRUE(parallel)) {
    # utilize all cores except 1
    cl <- parallel::detectCores() - 1

    if (isTRUE(pb)) {
      # run function in parallel
      nd <- pbapply::pblapply(targets_spellings, FUN=nd, dt=dt,
                              cl=cl, show=show)
    } else {
      nd <- parallel::mclapply(targets_spellings, FUN=nd, dt=dt,
                               mc.cores=cl, show=show)
    }
  } else {
    if (isTRUE(pb)) {
      # run function in parallel
      nd <- pbapply::pblapply(targets_spellings, FUN=nd, dt=dt,
                              cl=NULL, show=show)
    } else {
      nd <- lapply(targets_spellings, FUN=nd, dt=dt, show=show)
    }
  }
  # return
  return(unlist(nd))
}

#' Neighbourhood Frequency
#'
#' `nf()` calculates the neighbourhood frequency of a single string. Frequency
#' of the target string is looked up in the data table `dt_corpus`.
#'
#' @param target target string
#' @param dt_corpus data table with words and frequencies (word, frequency)
#' @param show show higher frequency neighours
#'
#' @return number of higher frequency neighbours
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table( word = subtlex_uk$Spelling, frequency = subtlex_uk$`LogFreq(Zipf)`)
#' nf("book", dt, show=T)
#' }
#' @export
nf <- function(target, dt_corpus, show=FALSE) {
  # asign NULL to column variables in data table
  word <- frequency <- NULL
  # find target frequency in dt_corpus
  if (target %in% dt_corpus$word) {
    target_frequency <- dt_corpus[word == target]$frequency
  } else {
    # not in corpus, assume frequency of zero
    target_frequency <- 0
  }
  # find higher frequency neighbours
  neighbours_detected <- dt_corpus[  is_neighbour(target, word) &
                                       frequency > target_frequency ]

  if (isTRUE(show) & nrow(neighbours_detected) > 0) {
    print(neighbours_detected)
  }

  return(nrow(neighbours_detected))
}

#' Neighbourhood Frequency of a list of words
#'
#' `neighborhood_frequency()` calculates the neighbourhood frequency
#' of a list of words based on the words and frequencies provided.
#'
#' @param targets_spellings target strings
#' @param all_words all words
#' @param all_frequencies frequencies of all words
#' @param show show neighbours
#' @param pb show progress bar
#' @param parallel parallel processing
#'
#' @return list of the number of higher frequency neighbours
#'
#' @examples
#' \dontrun{
#' neighborhood_frequency("book", subtlex_uk$Spelling, subtlex_uk$`LogFreq(Zipf)`, show=T)
#' }
#'
#' @export
neighborhood_frequency <- function(targets_spellings, all_words,
                                   all_frequencies, show=FALSE, pb = FALSE, parallel = FALSE) {
  # create DT
  dt_corpus <- data.table::data.table(
    word = all_words,
    frequency = all_frequencies
  )
  if (isTRUE(parallel)) {
    # utilize all cores except 1
    cl <- parallel::detectCores() - 1

    if (isTRUE(pb)) {
      # run function in parallel
      nf <- pbapply::pblapply(targets_spellings, FUN=nf, dt=dt_corpus,
                              cl=cl, show=show)
    } else {
      nf <- parallel::mclapply(targets_spellings, FUN=nf, dt=dt_corpus,
                               mc.cores=cl, show=show)
    }
  } else {
    if (isTRUE(pb)) {
      # run function in parallel
      nf <- pbapply::pblapply(targets_spellings, FUN=nf, dt=dt_corpus,
                              cl=NULL, show=show)
    } else {
      nf <- lapply(targets_spellings, FUN=nf, dt=dt_corpus, show=show)
    }
  }

  # return number of higher frequency neighbours
  return(unlist(nf))
}

#' Orthographic Levenshtein Distance
#'
#' `old()` calculates the average Orthographic Levenshtein Distance (OLD)
#' of a single string. When n_old is 20 (default value), the function
#' calculates OLD20 as defined by
#' \href{http://link.springer.com/article/10.3758/PBR.15.5.971}{Yarkoni et al. (2008)}.
#'
#' @param target_word target word
#' @param words list of words
#' @param old_n number of words to calculate old20, default is 20
#' @param show show the Levenshtein distance of each of the n_old (20) words that have
#' the lowest Levenshtein distance with the target word.
#'
#' @return average Levenshtein distance
#'
#' @examples
#' \dontrun{
#' old("book", subtlex_uk$Spelling)
#' }
#' @export
old <- function(target_word, words, old_n = 20, show = FALSE) {
  lv <- NULL

  dt <- data.table::data.table(
    Spelling = words,
    # Levenshtein distance
    lv = stringdist::stringdist(target_word, words, method = "lv", nthread = 1)
  )

  # sort by distance
  dt <- dt[ order(lv), ]

  # remove first one if that one is the target
  dt <- dt[!(dt$lv == 0), ]

  if(nrow(dt) < old_n) {
    stop("Not enough words to calculate OLD 'length(dt)' < 'old_n'")
  }

  # take first 20 (old_n)
  dt.old20 <- dt[1:old_n, ]

  if (isTRUE(show)) {
    # show the 20 words with the lowest Levenshtein distance
    print(dt.old20)
    # show sum and mean
    cat("sum: ", sum(dt.old20$lv), "\nmean:", mean(dt.old20$lv), "\n")
  }

  # return the mean Levenshtein distance of the 20 words
  # with the lowest Levenshtein distance
  return(mean(dt.old20$lv))
}

#' OLD20 of a list of words
#'
#' `old20()` calculates the OLD20 of a list of words.
#' The OLD20 definition was proposed by
#' \href{http://link.springer.com/article/10.3758/PBR.15.5.971}{Yarkoni et al. (2008)}.
#'
#' @param target_words list of target words
#' @param all_words list of words
#' @param old_n max number of most similar words to calculate OLD20
#' @param pb show progress bar
#' @param parallel parallel processing
#'
#' @return OLD20 for each of the target strings
#'
#' @examples
#' \dontrun{
#' old20(c("table", "tree"), subtlex_uk$Spelling)
#' old20(subtlex_uk$Spelling, subtlex_uk$Spelling, pb = TRUE, parallel = TRUE)
#' }
#' @export
old20 <- function(target_words, all_words, old_n = 20,
                            pb = FALSE, parallel = FALSE) {
  if (isTRUE(parallel)) {

    cl <- parallel::detectCores() - 1

    if (isTRUE(pb)) {
      old20_list <- pbapply::pblapply(target_words, FUN=old, words=all_words,
                                      old_n=old_n, cl=cl)
    } else {
      old20_list <- parallel::mclapply(target_words, FUN=old, words=all_words,
                                       old_n=old_n, mc.cores=cl)
    }
  } else {
    if (isTRUE(pb)) {
      old20_list <- pbapply::pblapply(target_words, FUN=old, words=all_words,
                                      old_n=old_n, cl=NULL)
    } else {
      old20_list <- lapply(target_words, FUN=old, words=all_words,
                                      old_n=old_n)
    }
  }

  return(unlist(old20_list))
}
