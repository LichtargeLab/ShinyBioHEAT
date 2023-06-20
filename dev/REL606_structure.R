library(tidyverse)

ET_data <- tibble(ET_file = list.files("~/Work/EA_genome_prep/NC_012967.1/ET/")) %>%
  filter(str_detect(ET_file, "nr|u90|u100") == FALSE) %>%
  mutate(locus_tag = str_sub(ET_file, 1, -5)) %>%
  mutate(data = map(ET_file, ~EvoTrace::ReadET(paste0("~/Work/EA_genome_prep/NC_012967.1/ET/",.)))) %>%
  select(-ET_file) %>%
  mutate(data = map(data, ~arrange(., POS))) %>%
  mutate(ET_vec = map(data, ~.$coverage))

ref_df <- readRDS("inst/app/www/REL606/REL606_ref.rds") %>%
  filter(CDS == TRUE)

gene_list <- ref_df %>%
  select(gene, locus_tag) %>%
  unique()

AF_df <- readRDS("dev/uniprot/AF_url_df.rds")

AF_structure <- inner_join(AF_df, ET_data, by = "locus_tag") %>%
  left_join(gene_list) %>%
  select(entry, locus_tag, gene, AF_url, ET_vec)

write_rds(AF_structure, "inst/app/www/REL606/REL606_structure.rds")
