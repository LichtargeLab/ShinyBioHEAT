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
    dplyr::select(-file) %>%
    tidyr::unnest(cols = c(data)) %>%
    AnnotateMutations(., .group = "strain") %>%
    dplyr::mutate(EA = GetEA(locus_tag, SUB, EA_list = EA_list)) %>%
    dplyr::mutate(
      POS = as.integer(POS),
      AA_pos = as.integer(AA_pos),
      codon_pos = as.integer(codon_pos)
    ) %>%
    dplyr::mutate(match_reference = DNARefCheck(POS, REF, ref_seq))
  return(output)
}
