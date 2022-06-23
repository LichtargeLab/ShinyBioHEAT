#' Annotate_SUB_file
#'
#' @description Adding EA scores to a substitution file
#'
#' @return A tibble with EA scores annotated.
#'
#' @noRd
Annotate_SUB_file <- function(input, EA_list, name_table, string_input = TRUE) {
  if (string_input == TRUE){
    input <- stringr::str_split(input, pattern = "\n")[[1]]
    input <- input[which(input != "")]
    df <- dplyr::tibble(input) %>%
      tidyr::separate(col = input, into = c("input_id", "SUB"), sep = "[ |\t|,]+")
  } else {
    df <- dplyr::tibble(input)
  }
  df <- dplyr::left_join(df, name_table, by = "input_id")
  df <- dplyr::mutate(df, EA = purrr::map2_chr(locus_tag, SUB, ~GetEA(.x, .y, EA_list = EA_list)))
  return(df)
}
