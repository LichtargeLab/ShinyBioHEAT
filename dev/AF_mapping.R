library(tidyverse)

uniprot <- read_tsv("dev/uniprot-compressed_true_download_true_fields_accession_2Cid_2Cgene_n-2022.08.09-14.19.35.67.tsv") %>%
  mutate(locus_tag = str_extract(`Gene Names (ordered locus)`, "b[:digit:]{4}")) %>%
  select(entry = Entry, seq_uniprot = Sequence)


gbk <- BioHEAT::MG1655.gbk@transcripts %>%
  as.data.frame() %>%
  select(locus_tag, gene, seq_gbk = translation, db_xref, pseudo) %>%
  mutate(uniprot_filt = map_lgl(db_xref, ~str_starts(.[1], "UniProtKB"))) %>%
  filter(uniprot_filt) %>%
  mutate(entry = map_chr(db_xref, ~.[1])) %>%
  select(-db_xref, -uniprot_filt) %>%
  mutate(entry = str_split(entry, ":", simplify = TRUE)[, 2])

match_df <- left_join(gbk, uniprot, by = "entry") %>%
  filter(pseudo == FALSE) %>%
  mutate(seq_match = seq_uniprot == seq_gbk) %>%
  filter(seq_match == TRUE) %>%
  select(entry, locus_tag)

version <- 1
while(version < Inf) {
  pdb_link <- paste0("https://alphafold.ebi.ac.uk/files/AF-P30844-F1-model_v", version, ".pdb")
  if (RCurl::url.exists(pdb_link) == TRUE) {
    version <- version + 1
  } else {
    version <- version - 1
    stop()
  }
}

match_df <- match_df %>%
  mutate(AF_url = paste0("https://alphafold.ebi.ac.uk/files/AF-", entry, "-F1-model_v", version, ".pdb")) %>%
  mutate(check_url = RCurl::url.exists(AF_url))

AF_url_df <- match_df %>%
  filter(check_url == TRUE) %>%
  select(-check_url)

write_rds(AF_url_df, "inst/app/www/AF_url_df.rds")



# Prepare AF_ET mapping df

AF_df <- readRDS("inst/app/www/AF_url_df.rds")
ET_data <- read_csv("~/Work/Colistin/20210304-EAapp_v2/www/MG1655_ET.csv", col_types = cols()) %>%
  select(locus_tag, gene, AA.POS, ET) %>%
  arrange(locus_tag, AA.POS) %>%
  group_by(locus_tag, gene) %>%
  nest()

AF_ET_df <- inner_join(AF_df, ET_data, by = "locus_tag")
write_rds(AF_ET_df, "inst/app/www/AF_ET_df.rds")
