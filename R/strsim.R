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

#' Function calculates the number of orthographic neighbours of a single string
#'
#' @param target_str target character string
#' @param all_words list of words
#' @param show show neighours
#'
#' @return number of neighbours
#'
#' @examples
#' \dontrun{
#' coltheartN("book", subtlex_uk$Spelling)
#' }
#' @export
coltheartN <- function(target_str, all_words, show=FALSE) {
  df <- data.frame(
    words = all_words,
    nletters = nchar(all_words)
  )
  df.subset <- subset(df, df$nletters == nchar(target_str))
  neighbours_detected <- lapply(df.subset$words, FUN=is_neighbour, w2=target_str)

  neighbours <- df.subset$words[ which(unlist(neighbours_detected)) ]
  if (show==TRUE) {
    print(neighbours)
  }

  return(length(neighbours))
}

#' Function calculates the orthographic neighbours of
#' a list of words using (max_cores - 1) cores.
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

#' Function that calculate the OLD20 of a single string.
#'
#' OLD20 defintion was proposed by Yarkoni et al. (2008), see
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

  if (show == TRUE) {
    # show the 20 words with the lowest Levenshtein distance
    print(df.old20)
  }

  # return the mean Levenshtein distance of the 20 words
  # with the lowest Levenshtein distance
  return(mean(df.old20$lv))
}

#' Function calculates OLD20 of a list of words using (max_cores - 1) cores.
#'
#'
#' @param target_words list of target words
#' @param all_words list of words
#' @param old_n max number of most similar words to calculate OLD20
#'
#' @return OLD20 for each of the target strings
#'
#' @examples
#' \dontrun{
#' calculate_old20(c("table", "tree"), subtlex_uk$Spelling)
#' }
#' @export
calculate_old20 <- function(target_words, all_words, old_n = 20) {
  cl=parallel::detectCores() - 1
  old20_list <- pbapply::pblapply(target_words, FUN=old20, words=all_words, old_n=old_n, cl=cl)
  return(unlist(old20_list))
}
