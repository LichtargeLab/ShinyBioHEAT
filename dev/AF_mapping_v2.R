library(tidyverse)
library(genbankr)

# Parse E coli protein mappings from Uniprot files
MG1655.uniprot <- read_tsv("dev/uniprot/uniprot_MG1655.tsv.gz") %>%
  filter(!is.na(AlphaFoldDB)) %>%
  mutate(strain = "MG1655") %>%
  mutate(AF_id = str_sub(AlphaFoldDB, 1, -2)) %>%
  select(entry = Entry, strain, AF_id, seq = Sequence)

BL21.uniprot <- read_tsv("dev/uniprot/uniprot_BL21.tsv.gz") %>%
  filter(!is.na(AlphaFoldDB)) %>%
  mutate(strain = "BL21") %>%
  mutate(AF_id = str_sub(AlphaFoldDB, 1, -2)) %>%
  select(entry = Entry, strain, AF_id, seq = Sequence)

REL606.uniprot <- read_tsv("dev/uniprot/uniprot_REL606.tsv.gz") %>%
  filter(!is.na(AlphaFoldDB)) %>%
  mutate(strain = "REL606") %>%
  mutate(AF_id = str_sub(AlphaFoldDB, 1, -2)) %>%
  select(entry = Entry, strain, AF_id, seq = Sequence)

ecoli.uniprot <- read_tsv("dev/uniprot/uniprot_Ecoli.tsv.gz") %>%
  filter(!is.na(AlphaFoldDB)) %>%
  mutate(AF_id = str_sub(AlphaFoldDB, 1, -2)) %>%
  select(entry = Entry, AF_id, seq = Sequence) %>%
  mutate(rank = 4,
         rank = ifelse(entry %in% BL21.uniprot$entry,
                       3, rank),
         rank = ifelse(entry %in% REL606.uniprot$entry,
                       2, rank),
         rank = ifelse(entry %in% MG1655.uniprot$entry,
                       1, rank))

# Get non redundant E coli sequences with AF structure. If
# multiple entries have the same AA seq, the one from MG1655
# will be used first, followed by REL606, BL21, and others.
ecoli.uniprot.nr <- ecoli.uniprot %>%
  group_by(seq) %>%
  arrange(rank) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(-rank)

write_tsv(ecoli.uniprot.nr, "dev/uniprot/ecoli_uniprot_nr.tsv")
rm(ecoli.uniprot)
rm(MG1655.uniprot)
rm(BL21.uniprot)
gc()


ecoli.uniprot.nr <- read_tsv("dev/uniprot/ecoli_uniprot_nr.tsv") %>%
  select(-AF_id)

MG1655.prot <- data.frame(GN = BioHEAT::MG1655.gbk@transcripts$gene,
                          product = BioHEAT::MG1655.gbk@transcripts$product,
                          id = BioHEAT::MG1655.gbk@transcripts$locus_tag,
                          pseudo = BioHEAT::MG1655.gbk@transcripts$pseudo,
                          seq = BioHEAT::MG1655.gbk@transcripts$translation,
                          stringsAsFactors = FALSE) %>%
  filter(pseudo == FALSE) %>%
  select(-pseudo) %>%
  left_join(ecoli.uniprot.nr, by = "seq") %>%
  filter(!is.na(entry))


REL606.gbk <- readGenBank("~/Work/EA_genome_prep/NC_012967.1/NC_012967.1.gb")

REL606.prot <- data.frame(GN = REL606.gbk@transcripts$gene,
                          product = REL606.gbk@transcripts$product,
                          id = REL606.gbk@transcripts$locus_tag,
                          pseudo = REL606.gbk@transcripts$pseudo,
                          seq = REL606.gbk@transcripts$translation,
                          stringsAsFactors = FALSE) %>%
  filter(pseudo == FALSE) %>%
  select(-pseudo) %>%
  left_join(ecoli.uniprot.nr, by = "seq") %>%
  filter(!is.na(entry))


match_df <- bind_rows(MG1655.prot, REL606.prot) %>%
  select(entry, locus_tag = id)

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

AF_url_df <- match_df %>%
  mutate(AF_url = paste0("https://alphafold.ebi.ac.uk/files/AF-", entry, "-F1-model_v", version, ".pdb")) %>%
  mutate(check_url = RCurl::url.exists(AF_url))

AF_url_df <- AF_url_df %>%
  filter(check_url == TRUE) %>%
  select(-check_url)

write_rds(AF_url_df, "dev/uniprot/AF_url_df.rds")
