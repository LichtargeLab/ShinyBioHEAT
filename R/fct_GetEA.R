#' GetEA
#'
#' @description Annotate EA score for a given mutation
#'
#' @return The EA score for the mutation. Stored as character.
#'
#' @noRd
GetEA <- function(locus_tag, SUB, EA_list) {
  GetEA_locus_tag <- function(locus_tag, SUB, EA_list) {
    if (is.na(locus_tag)) {
      output <- rep(NA, length(SUB))
    } else if (!locus_tag %in% names(EA_list)) {
      output <- rep("No EA for this protein", length(SUB))
    } else {
      aa.ref <- stringr::str_sub(SUB, start = 1, end = 1)
      aa.alt <- stringr::str_sub(SUB, start = -1, end = -1)
      aa.pos <- stringr::str_sub(SUB, start = 2, end = -2)

      output <- as.character(EA_list[[locus_tag]][SUB])

      stop.id <- which(aa.alt %in% c("*", "X"))
      check.1 <- stringr::`str_sub<-`(SUB[stop.id], start = -1, end = -1, value = "A") %in% names(EA_list[[locus_tag]])
      check.2 <- stringr::`str_sub<-`(SUB[stop.id], start = -1, end = -1, value = "T") %in% names(EA_list[[locus_tag]])
      stop.id.check <- stop.id[check.1 | check.2]
      output[stop.id.check] <- "100"

      silent.id <- which(aa.alt == aa.ref)
      check.3 <- stringr::`str_sub<-`(SUB[silent.id], start = -1, end = -1, value = "A") %in% names(EA_list[[locus_tag]])
      check.4 <- stringr::`str_sub<-`(SUB[silent.id], start = -1, end = -1, value = "T") %in% names(EA_list[[locus_tag]])
      silent.id.check <- silent.id[check.3 | check.4]
      output[silent.id.check] <- "Synonymous"

      output[stringr::str_detect(aa.pos, "[^[:digit:]]") == TRUE |
               aa.pos == "" |
               stringr::str_detect(aa.ref, "[^[A-Z*]]") == TRUE |
               stringr::str_detect(aa.alt, "[^[A-Z*]]") == TRUE] <- "Invalid SUB format"

      output[is.na(output)] <- "False index"
    }
    return(output)
  }
  workdf <- dplyr::tibble(locus_tag, SUB) %>%
    dplyr::group_by(locus_tag) %>%
    dplyr::mutate(EA = GetEA_locus_tag(locus_tag[1], SUB, EA_list = EA_list)) %>%
    dplyr::ungroup()
  return(workdf$EA)
}
