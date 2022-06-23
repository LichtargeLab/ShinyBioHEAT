#' VCFtoEA
#'
#' @description Annotate VCF file with mutations then assign EA scores
#'
#' @return A annotated VCF file
#'
#' @noRd
VCFtoEA <- function(input_file_name, input_file_path, EA_list, ref_seq) {
  output <- dplyr::tibble(
    strain = input_file_name,
    file = input_file_path
  ) %>%
    dplyr::mutate(strain = stringr::str_split(strain, ".vcf", simplify = TRUE)[, 1]) %>%
    dplyr::mutate(data = purrr::map(file, ReadVCF)) %>%
    dplyr::mutate(data = purrr::map(data, AnnotateMutations)) %>%
    tidyr::unnest(cols = c(data)) %>%
    dplyr::mutate(EA = purrr::map2_chr(locus_tag, SUB, ~GetEA(.x, .y, EA_list = EA_list))) %>%
    dplyr::select(-file) %>%
    dplyr::mutate(
      POS = as.integer(POS),
      AA_pos = as.integer(AA_pos),
      codon_pos = as.integer(codon_pos)
    ) %>%
    dplyr::mutate(match_reference = DNARefCheck(POS, REF, ref_seq))
  return(output)
}
