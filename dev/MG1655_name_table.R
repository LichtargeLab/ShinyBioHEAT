library(dplyr)
GetNameTable <- function(ref) {
  gene_info <- ref %>%
    filter(CDS == TRUE) %>%
    select(gene, locus_tag)
  gene_info_a <- gene_info %>%
    mutate(input_id = gene)
  gene_info_b <- gene_info %>%
    mutate(input_id = locus_tag)
  name_table <- bind_rows(gene_info_a, gene_info_b)
  return(name_table)
}
MG1655_name_table <- GetNameTable(BioHEAT::MG1655_ref)
saveRDS(MG1655_name_table, "inst/app/www/MG1655_name_table.rds", compress = FALSE)
