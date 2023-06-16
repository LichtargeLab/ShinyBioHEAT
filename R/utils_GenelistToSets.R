#' GenelistToSets
#'
#' @description A utils function that process top genes from EA/Freq analysis then calculates
#' their overlaps for graphing a venn diagram.
#'
#' @param gene_rankings Output from EA/Freq analysis
#' @param top An integer. Top genes to select from each list.
#'
#' @return A tibble with the top genes from each gene rankings, and which venn
#' diagram region they belong.
#'
#' @noRd
GenelistToSets <- function(gene_rankings, top) {
  top_list <- list("set1" = gene_rankings$locus_tag[gene_rankings$EAKS_rank <= top],
                   "set2" = gene_rankings$locus_tag[gene_rankings$EAsum_rank <= top],
                   "set3" = gene_rankings$locus_tag[gene_rankings$Freq_rank <= top])
  AnnotateSets <- function(input_list) {
    columns <- names(input_list)
    a2 <- unique(unlist(input_list[columns]))
    output <- lapply(input_list, `%in%`, x = a2) %>% as.data.frame() %>%
      dplyr::mutate(element = a2) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(vec = list(c(set1, set2, set3))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(vec = purrr::map(vec, ~c(1:3)[.])) %>%
      dplyr::mutate(vec = purrr::map_chr(vec, ~paste0(., collapse = "&")),
                    vec = paste0("set", vec)) %>%
      dplyr::rename(set = vec)
    return(output)
  }
  output <- AnnotateSets(top_list) %>%
    dplyr::select(locus_tag = element, set) %>%
    dplyr::left_join(gene_rankings, by = "locus_tag") %>%
    dplyr::select(gene, locus_tag, EAKS_rank, EAsum_rank, Freq_rank, set, string_id)
  return(output)
}
