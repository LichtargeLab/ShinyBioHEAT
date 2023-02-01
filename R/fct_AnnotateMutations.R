#' Annotate single nucleotide substitutions
#' @description Read in a vcf tibble then single nucleotide mutations are kept. Mutations
#' are then annotated according to the reference (gbk.tibble) genome.
#' @author Chen Wang
#' @param vcf A tibble with all the entries from vcf file. POS, REF, ALT and INFO columns are
#' required. Can be generated with ReadVCF function.
#' @param ref_df A tibble storing the genomic information of the reference genome. Can be generated
#' with ExtractGBK function. Default value is MG1655_ref.
#' @param genome_map A feature map list to quick trace the feature ids for given positions.
#' Can be generated using GenerateGenomeMapping function.
#' @param .group A character string. The column(s) that should be used to group nucleodides together
#' when annotating mutations. When annotating multiple VCFs, combine them into one file, while
#' saving the sample id as an extra column. Use this column name as .group variable so mutations
#' inside each group are annotated together. If no groupings, use NULL.
#' @return A tibble with annotated mutations. Combine entries in locus_tag with SUB columns in
#' the GetEA function to get the EA of that mutation.
#' @export
AnnotateMutations <- function(vcf, ref_df = MG1655_ref, genome_map = MG1655_genome_map,
                              .group = NULL) {
  if (!is.null(.group)) {
    if (sum(.group %in% names(vcf)) < length(.group)) {
      stop("Not all variables in .group are present in the vcf")
    }
  }
  mut.list <- vcf %>%
    dplyr::select(POS, REF, ALT, INFO, all_of(.group)) %>%
    dplyr::filter(nchar(REF) == 1 & nchar(ALT) == 1) %>%
    dplyr::mutate(id = genome_map[POS]) %>%
    tidyr::unnest(cols = c(id)) %>%
    dplyr::left_join(ref_df, by = "id") %>%
    dplyr::arrange(POS, locus_tag) %>%
    dplyr::mutate(SNP_type = "")

  if (nrow(mut.list) > 0) {
    cds.output <- dplyr::filter(mut.list, CDS == TRUE) %>%
      dplyr::mutate(AA_pos = GetAAPOS(POS, start, end, strand)) %>%
      dplyr::mutate(codon_pos = GetCodonPOS(POS, start, end, strand)) %>%
      dplyr::mutate(codon_ref = GetCodonREF(AA_pos, strand, ref_df$DNA[id])) %>%
      dplyr::mutate(AA_ref = codon_table[codon_ref]) %>%
      dplyr::mutate(ALT.1 = ifelse(strand == "+",
                                   ALT,
                                   DNA_pair[ALT])) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(.group)), locus_tag, AA_pos) %>%
      dplyr::mutate(codon_alt = GetCodonALT(codon_ref, codon_pos, ALT.1)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(AA_alt = codon_table[codon_alt]) %>%
      dplyr::mutate(SUB = paste0(AA_ref, AA_pos, AA_alt)) %>%
      dplyr::mutate(SNP_type = GetSNPType(AA_ref, AA_alt))
    output <- dplyr::filter(mut.list, CDS == FALSE) %>%
      dplyr::mutate(SNP_type = ifelse(stringr::str_detect(gene, "intergenic"),
                                      "intergenic",
                                      "non coding genes")) %>%
      dplyr::mutate(SNP_type = as.character(SNP_type)) %>%
      dplyr::bind_rows(., cds.output) %>%
      dplyr::select(.group, POS, REF, ALT, gene, locus_tag, CDS, SNP_type, SUB, AA_pos, AA_ref, AA_alt, codon_pos, codon_ref, codon_alt, INFO)
  } else {
    output <- dplyr::tibble(POS = double(), REF = character(), ALT = character(),
                            gene = character(), locus_tag = character(), CDS = logical(),
                            SNP_type = character(), SUB = character(), AA_pos = double(),
                            AA_ref = character(), AA_alt = character(), codon_pos = double(),
                            codon_ref = character(), codon_alt = character(), INFO = character())
    output[.group] <- character(0)
    output <- dplyr::relocate(output, all_of(.group), POS)
  }
  output <- output %>%
    dplyr::arrange(dplyr::across(c(dplyr::all_of(.group), "POS")))
  return(output)
}
