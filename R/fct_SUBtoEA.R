#' SUBtoEA
#'
#' @description Annotate substitution file with EA scores
#'
#' @return A tibble with mutations and annotated EA scores
#'
#' @noRd
SUBtoEA <- function(input_file_name, input_file_path, name_table, EA_list) {
  output <- dplyr::tibble(
    strain = input_file_name,
    file = input_file_path
  ) %>%
    dplyr::mutate(strain = stringr::str_split(strain, ".csv", simplify = TRUE)[, 1]) %>%
    dplyr::mutate(data = purrr::map(file, ~readr::read_csv(., col_types = "cc"))) %>%
    tidyr::unnest(cols = c(data)) %>%
    Annotate_SUB_file(., EA_list, name_table, string_input = FALSE) %>%
    dplyr::select(-file) %>%
    dplyr::mutate(match_reference = ifelse(EA %in% c("Invalid SUB format", "False index", NA),
                                    FALSE,
                                    TRUE))
  return(output)
}
