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
REL606_name_table2 <- REL606_ref %>%
  filter(CDS == TRUE) %>%
  select(locus_tag, gene, input_id = old_locus_tag) %>%
  unique() %>%
  filter(!is.na(input_id))

REL606_name_table <- bind_rows(REL606_name_table,
                               REL606_name_table2) %>%
  unique()

saveRDS(REL606_name_table, "inst/app/www/REL606/REL606_name_table.rds", compress = FALSE)
