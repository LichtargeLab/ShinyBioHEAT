library(tidyverse)

ET_data <- ET_data <- read_csv("~/Work/Colistin/20210304-EAapp_v2/www/MG1655_ET.csv", col_types = cols()) %>%
  select(locus_tag, gene, AA.POS, ET) %>%
  arrange(locus_tag) %>%
  group_by(locus_tag) %>%
  arrange(AA.POS) %>%
  summarize(ET_vec = list(ET))

ref_df <- readRDS("inst/app/www/MG1655/MG1655_ref.rds") %>%
  filter(CDS == TRUE)

gene_list <- ref_df %>%
  select(gene, locus_tag) %>%
  unique()

AF_df <- readRDS("dev/uniprot/AF_url_df.rds")

AF_structure <- inner_join(AF_df, ET_data, by = "locus_tag") %>%
  left_join(gene_list) %>%
  select(entry, locus_tag, gene, AF_url, ET_vec)

write_rds(AF_structure, "inst/app/www/MG1655/MG1655_structure.rds")
