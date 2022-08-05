#' GetVenn
#'
#' @description A utils function that add text info for the venn diagram
#' @param sets_input tibble generated from GenelistToSets()
#' @param text_pos Text coordinates for the venn diagram
#'
#' @return A tibble to pass to ggplot
#'
#' @noRd
GetVenn <- function(sets_input, text_pos) {
  sets_count <- sets_input %>%
    dplyr::group_by(set) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    dplyr::ungroup()

  df_element <- dplyr::tibble(set = c("set1", "set2", "set3",
                                      "set1&2", "set1&3", "set2&3",
                                      "set1&2&3")) %>%
    dplyr::mutate(set1 = stringr::str_detect(set, "1"),
                  set2 = stringr::str_detect(set, "2"),
                  set3 = stringr::str_detect(set, "3")) %>%
    dplyr::left_join(sets_count, by = "set") %>%
    tidyr::replace_na(list(n = 0))

  fmt <- sprintf("%%d\n(%%.%df%%%%)", 1)
  output <- text_pos %>%
    dplyr::left_join(df_element, by = "set") %>%
    dplyr::mutate(text = sprintf(fmt, n, 100 * n / sum(n)))
  return(output)
}
