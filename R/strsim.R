#' strsim
#'

#' Function to check that two strings are orthographic neighbours
#'
#'
#' @param w1 character string
#' @param w2 character string
#'
#' @return boolean
#'
#' @examples
#' \dontrun{
#' # Orthographic Neighbour
#' is_neighbour("book", "look")
#' # Not a neighbour
#' is_neighbour("book", "boll")
#' # Not a neighbour
#' is_neighbour("book", "boo")
#' # Not a neighbour
#' is_neighbour("book", "boök")
#' }
#'
#' @export
is_neighbour <- function(w1, w2) {
  return (stringdist::stringdist(w1, w2, method="hamming", nthread = 1) == 1)
}

#' Function calculates the number of orthographic neighbours of a single string
#'
#' @param target_str target character string
#' @param all_words list of words
#' @param showN show neighours
#'
#' @return number of neighbours
#'
#' @examples
#' \dontrun{
#' coltheartN("book", subtlex_uk$Spelling)
#' }
#' @export
coltheartN <- function(target_str, all_words, showN=FALSE) {
  df <- data.frame(
    words = all_words,
    nletters = nchar(all_words)
  )
  df.subset <- subset(df, df$nletters == nchar(target_str))
  neighbours_detected <- lapply(df.subset$words, FUN=is_neighbour, w2=target_str)

  neighbours <- df.subset$words[ which(unlist(neighbours_detected)) ]
  if (showN==TRUE) {
    neighbours
  }

  return(length(neighbours))
}

#' Function uses multiple cores to calculate the orthographic neighbours of
#' a list of words
#'
#' @param targets target strings
#' @param all_words all words
#'
#' @return list of the number of neighbours
#'
#' @export
calculate_coltheartN <- function(targets, all_words) {
  cl <- parallel::detectCores() - 1
  neighbours <- pbapply::pblapply(targets, FUN=coltheartN, all_words=all_words, cl=cl)
  return(unlist(neighbours))
}

#' Function that calculate the OLD20 of a single string
#'
#' OLD20 defintion can be found in Yarkoni et al. (2008), see
#' http://link.springer.com/article/10.3758/PBR.15.5.971
#'
#' @param target_word target string
#' @param words list of words
#' @param old_n number of words to calculate old20, default is 20
#' @param showN show the orthographic similar words
#'
#' @return mean levenshtein distance
#'
#' @export
old20 <- function(target_word, words, old_n = 20, showN = FALSE) {
  df <- data.frame(
    Spelling = words,
    # Levenshtein distance
    lv = stringdist::stringdist(target_word, words, method = "lv", nthread = 1)
  )

  # sort by distance
  df <- df[ order(df$lv), ]

  # take first 20, skip first one if that one is the target (lv=0)
  if (utils::head(df, 1)$lv == 0) {
    df.old20 <- df[2:(old_n+1),]
  } else {
    df.old20 <- df[1:old_n,]
  }

  if (showN == TRUE) {
    print(df.old20)
  }

  return(mean(df.old20$lv))
}

#' Function calculates OLD20 of a list of strings using multiple cores
#'
#'
#' @param target_word target string
#' @param all_words list of all words
#' @param old_n max number of most similar words to calculate OLD20
#'
#' @return list of words with OLD20 values
#'
#' @export
calculate_old20 <- function(target_words, all_words, old_n = 20) {
  cl=parallel::detectCores() - 1
  old20_list <- pbapply::pblapply(target_words, FUN=old20, words=all_words, old_n=old_n, cl=cl)
  return(unlist(old20_list))
}
