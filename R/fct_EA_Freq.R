#' EA_Freq
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
EA_Freq <- function(evolve, background, ref_table = MG1655_ref,
                    adj.method = c("bonferroni", "fdr")) {
  adj.method <- match.arg(adj.method)
  len.map <- ref_table %>%
    dplyr::mutate(Len = width/3 -1) %>%
    dplyr::select(locus_tag, Len)
  totallen <- sum(len.map$Len)
  avg_EA <- mean(background$EA)
  output <- evolve %>%
    dplyr::select(locus_tag, gene, strain, SUB, EA) %>%
    unique() %>%
    dplyr::group_by(locus_tag, gene) %>%
    dplyr::summarize(EAKS_p = suppressWarnings(ks.test(EA, background$EA,alternative = "less"))$p.value,
                     mutation_count = dplyr::n(),
                     strain_count = dplyr::n_distinct(strain),
                     observed_EAsum = sum(EA)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(len.map, by = "locus_tag") %>%
    dplyr::mutate(expect_mutation_count = nrow(evolve)/totallen*Len,
                  Freq_p = ppois(mutation_count-1, nrow(evolve)/totallen*Len, lower.tail = FALSE),
                  expect_EAsum = expect_mutation_count * avg_EA,
                  EAsum = observed_EAsum - expect_EAsum) %>%
    dplyr::mutate(EAKS_rank = rank(EAKS_p),
                  EAsum_rank = rank(dplyr::desc(EAsum)),
                  Freq_rank = rank(Freq_p)) %>%
    dplyr::mutate(EAKS_p_adj = p.adjust(EAKS_p, method = adj.method),
                  Freq_p_adj = p.adjust(Freq_p, method = adj.method)) %>%
    dplyr::arrange(EAKS_rank) %>%
    dplyr::select(gene, locus_tag, EAKS_rank, EAsum_rank, Freq_rank, EAKS_p, EAKS_p_adj, protein_length = Len, mutation_count, expect_mutation_count,
                  observed_EAsum, expect_EAsum, EAsum, Freq_p, Freq_p_adj)
  return(output)
}
