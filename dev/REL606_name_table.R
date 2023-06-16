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

REL606_ref <- readRDS("inst/app/www/REL606/REL606_ref.rds")
REL606_name_table <- GetNameTable(REL606_ref)
saveRDS(REL606_name_table, "inst/app/www/REL606/REL606_name_table.rds", compress = FALSE)
