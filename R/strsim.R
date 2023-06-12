#' strsim
#'

#' Function that checks whether two words are orthographic neighbours.
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

#' Function calculates neighbourhood density of a single string. It is based
#' on the definition of a neighbour by Coltheart et al. (1977).
#'
#' @param target_str target character string
#' @param dt data table with words
#' @param show show neighours
#'
#' @return number of neighbours
#'
#' @examples
#' \dontrun{
#' nd_string("book", data.table(word = all_words))
#' }
#' @export
#' @import data.table
nd_string <- function(target_str, dt, show=FALSE) {
  # to prevent 'No visible binding for global variable' error, asign NULL to word
  # see https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  word <- NULL
  # find neighbours
  neighbours_detected <- dt[ is_neighbour(target_str, word) ]
  # show them
  if (show==TRUE) {
    print(neighbours_detected)
  }
  # return the number of neighbours
  return(nrow(neighbours_detected))
}

#' Function calculates the neighbourhood density of
#' a list of words.
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
#' neighborhood_density("book", subtlex_uk$Spelling)
#' neighborhood_density(subtlex_uk$Spelling, subtlex_uk$Spelling)
#' }
#' @export
neighborhood_density <- function(targets_spellings, all_words, show=FALSE, pb = FALSE, parallel = FALSE) {
  # create DT
  dt <- data.table::data.table(
    word = all_words
  )
  if (isTRUE(parallel)) {
    # utilize all cores except 1
    cl <- parallel::detectCores() - 1

    if (isTRUE(pb)) {
      # run function in parallel
      nd <- pbapply::pblapply(targets_spellings, FUN=nd_string, dt=dt, cl=cl, show=show)
    } else {
      nd <- parallel::mclapply(targets_spellings, FUN=nd_string, dt=dt, cl=cl, show=show)
    }
  } else {
    if (isTRUE(pb)) {
      # run function in parallel
      nd <- pbapply::pblapply(targets_spellings, FUN=nd_string, dt=dt, cl=NULL, show=show)
    } else {
      nd <- lapply(targets_spellings, FUN=nd_string, dt=dt, show=show)
    }
  }
  # return
  return(unlist(nd))
}

#' Function calculates the neighbourhood frequency of a single string. Frequency
#' of that target string is looked up in the data table.
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
#' nf_string("book", dt, show=T)
#' }
#' @export
nf_string <- function(target, dt_corpus, show=FALSE) {
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

  if (show==TRUE) {
    print(neighbours_detected)
  }

  return(nrow(neighbours_detected))
}

#' Function calculates the neighbourhood frequency
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
neighborhood_frequency <- function(targets_spellings, all_words, all_frequencies, show=FALSE, pb = FALSE, parallel = FALSE) {
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
      nf <- pbapply::pblapply(targets_spellings, FUN=nf_string, dt=dt_corpus, cl=cl, show=show)
    } else {
      nf <- parallel::mclapply(targets_spellings, FUN=nf_string, dt=dt_corpus, cl=cl, show=show)
    }
  } else {
    if (isTRUE(pb)) {
      # run function in parallel
      nf <- pbapply::pblapply(targets_spellings, FUN=nf_string, dt=dt_corpus, cl=NULL, show=show)
    } else {
      nf <- lapply(targets_spellings, FUN=nf_string, dt=dt_corpus, show=show)
    }
  }

  # return number of higher frequency neighbours
  return(unlist(nf))
}

#' Function that calculate the OLD20 of a single string. OLD20 is based on the
#' Levenshtein distance.
#'
#' OLD20 definition was proposed by Yarkoni et al. (2008), see
#' \href{http://link.springer.com/article/10.3758/PBR.15.5.971}{http://link.springer.com/article/10.3758/PBR.15.5.971}
#'
#' @param target_word target word
#' @param words list of words
#' @param old_n number of words to calculate old20, default is 20
#' @param show show the Levenshtein distance of each of the 20 words that have
#' the lowest Levenshtein distance with the target word.
#'
#' @return mean Levenshtein distance
#'
#' @examples
#' \dontrun{
#' old20("book", subtlex_uk$Spelling)
#' }
#' @export
old20 <- function(target_word, words, old_n = 20, show = FALSE) {
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
  # take first 20 (old_n)
  dt.old20 <- dt[1:old_n, ]

  if (show == TRUE) {
    # show the 20 words with the lowest Levenshtein distance
    print(dt.old20)
    # show sum and mean
    cat("sum: ", sum(dt.old20$lv), "\nmean:", mean(dt.old20$lv), "\n")
  }

  # return the mean Levenshtein distance of the 20 words
  # with the lowest Levenshtein distance
  return(mean(dt.old20$lv))
}

#' Function calculates OLD20 of a list of words using (max_cores - 1) cores.
#'
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
#' calculate_old20(c("table", "tree"), subtlex_uk$Spelling)
#' calculate_old20(subtlex_uk$Spelling, subtlex_uk$Spelling)
#' }
#' @export
calculate_old20 <- function(target_words, all_words, old_n = 20, pb = FALSE, parallel = FALSE) {
  if (isTRUE(parallel)) {

    cl <- parallel::detectCores() - 1

    if (isTRUE(pb)) {
      old20_list <- pbapply::pblapply(target_words, FUN=old20, words=all_words, old_n=old_n, cl=cl)
    } else {
      old20_list <- parallel::mclapply(target_words, FUN=old20, words=all_words, old_n=old_n, cl=cl)
    }
  } else {
    if (isTRUE(pb)) {
      old20_list <- pbapply::pblapply(target_words, FUN=old20, words=all_words, old_n=old_n, cl=NULL)
    } else {
      old20_list <- lapply(target_words, FUN=old20, words=all_words, old_n=old_n)
    }
  }

  return(unlist(old20_list))
}
