#' GDtoEA
#'
#' @description Annotate GD files from breseq and assign EA scores.
#'
#' @return A tibble with mutations and annotated EA scores
#'
#' @noRd
GDtoEA <- function(input_file_name, input_file_path, EA_list, ref_seq) {
  output <- dplyr::tibble(
    strain = input_file_name,
    file = input_file_path
  ) %>%
    dplyr::mutate(strain = stringr::str_split(strain, ".gd", simplify = TRUE)[, 1]) %>%
    dplyr::mutate(data = purrr::map(file, ~ProcessGD(., ref_seq = ref_seq))) %>%
    dplyr::select(-file) %>%
    tidyr::unnest(cols = c(data)) %>%
    AnnotateMutations(., .group = "strain") %>%
    dplyr::mutate(EA = GetEA(locus_tag, SUB, EA_list = EA_list)) %>%
    dplyr::mutate(
      POS = as.integer(POS),
      AA_pos = as.integer(AA_pos),
      codon_pos = as.integer(codon_pos)
    ) %>%
    dplyr::mutate(match_reference = TRUE)
  return(output)
}
