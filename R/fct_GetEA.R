#' GetEA
#'
#' @description Annotate EA score for a given mutation
#'
#' @return The EA score for the mutation. Stored as character.
#'
#' @noRd
GetEA <- function(locus_tag, SUB, EA_list) {
  if(is.na(locus_tag)|is.na(SUB)) return(NA)
  aa.ref <- stringr::str_sub(SUB, start = 1, end = 1)
  aa.alt <- stringr::str_sub(SUB, start = -1, end = -1)
  aa.pos <- stringr::str_sub(SUB, start = 2, end = -2)
  if(stringr::str_detect(aa.pos, "[^[:digit:]]") == TRUE |
     aa.pos == "" |
     stringr::str_detect(aa.ref, "[^[A-Z]]") == TRUE |
     stringr::str_detect(aa.alt, "[^[A-Z*]]") == TRUE) {
    return("Invalid SUB format")
  }
  if (!locus_tag %in% names(EA_list)) {
    return("No EA for this protein")
  }
  else if (aa.ref == aa.alt) {
    return("Synonymous")
  }
  else if (aa.alt %in% c("*", "X")) {
    check_1 <- stringr::`str_sub<-`(SUB, start = -1, end = -1, value = "A") %>%
      purrr::pluck(EA_list, locus_tag, .)
    check_2 <- stringr::`str_sub<-`(SUB, start = -1, end = -1, value = "T") %>%
      purrr::pluck(EA_list, locus_tag, .)
    if(is.null(check_1)&is.null(check_2) == TRUE) {
      return("False index")
    } else {
      return("100")
    }
  }
  else if (is.null(purrr::pluck(EA_list, locus_tag, SUB))) {
    return("False index")
  }
  else {
    return(as.character(purrr::pluck(EA_list, locus_tag, SUB)))
  }
}
