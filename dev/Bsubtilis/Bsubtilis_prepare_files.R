library(genbankr)
library(tidyverse)

genome = "NC_000964.3"
genome_id <- "Bsubtilis168"

# Prepare gene reference df, sequence, and position mappings
gbk <- readGenBank(paste0("~/Work/EA_genome_prep/", genome, "/", genome, ".gb"))

genome_seq <- gbk@sequence %>%
  as.character() %>%
  str_split(., pattern = "", simplify = TRUE) %>%
  .[1,]

saveRDS(genome_seq, paste0("inst/app/www/", genome_id, "/", genome_id,"_seq.rds"),
        compress = FALSE)

rm(genome_seq)

transcripts <- gbk@transcripts %>%
  as.data.frame()

ref_df <- BioHEAT::ExtractGBK(gbk) %>%
  mutate(string_id = str_remove(locus_tag, "_")) %>%
  mutate(CDS = ifelse(locus_tag %in% gbk@transcripts$locus_tag,
                      TRUE,
                      FALSE)) %>%
  mutate(gene = ifelse(is.na(gene), locus_tag, gene))

genome_map <- BioHEAT::GenerateGenomeMapping(ref_df)

saveRDS(ref_df, file = paste0("inst/app/www/", genome_id, "/", genome_id,"_ref.rds"))
saveRDS(genome_map, file = paste0("inst/app/www/", genome_id, "/", genome_id, "_genome_map.rds"))

# Prepare EA list
EA.dir <- paste0("~/Work/EA_genome_prep/", genome, "/EA/")
EAfiles <- list.files(path = EA.dir, pattern = ".pred")
id <- str_split(EAfiles, pattern = ".pred", simplify = TRUE) [,1]
EA.list <- list()
for (i in seq_along(id)) {
  temp <- read_table2(file = paste0(EA.dir, EAfiles[i]), col_names = c("SUB", "ACTION"), col_types = cols())
  temp1 <- temp$ACTION
  names(temp1) <- temp$SUB
  EA.list[[i]] <- temp1
  cat(i, "\n")
}
names(EA.list) <- id
saveRDS(EA.list, paste0("inst/app/www/", genome_id, "/", genome_id,"_EA_list.rds"),
        compress = "xz")


# Prepare AF mappings
uniprot <- read_tsv("~/Work/EA_genome_prep/NC_000964.3/uniprot_Bsubtilis_168.gz") %>%
  filter(!is.na(AlphaFoldDB)) %>%
  mutate(AF_id = str_sub(AlphaFoldDB, 1, -2)) %>%
  group_by(Sequence) %>%
  mutate(rank = rank(Reviewed)) %>%
  filter(rank == min(rank)) %>%
  ungroup() %>%
  select(entry = Entry, AF_id, seq = Sequence)

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

uniprot <- uniprot %>%
  mutate(AF_url = paste0("https://alphafold.ebi.ac.uk/files/AF-", entry, "-F1-model_v", version, ".pdb"))

ref_prot <- data.frame(GN = gbk@transcripts$gene,
                       id = gbk@transcripts$locus_tag,
                       seq = gbk@transcripts$translation,
                       stringsAsFactors = FALSE) %>%
  left_join(uniprot, by = "seq") %>%
  filter(!is.na(entry)) %>%
  select(entry, locus_tag = id, AF_url)


# Prepare ET files
ET.dir <- paste0("~/Work/EA_genome_prep/", genome, "/ET/")

ET_data <- tibble(ET_file = list.files(ET.dir)) %>%
  filter(str_detect(ET_file, "nr|u90|u100") == FALSE) %>%
  mutate(locus_tag = str_sub(ET_file, 1, -5)) %>%
  mutate(data = map(ET_file, ~EvoTrace::ReadET(paste0(ET.dir,.)))) %>%
  select(-ET_file) %>%
  mutate(data = map(data, ~arrange(., POS))) %>%
  mutate(ET_vec = map(data, ~.$coverage))

cds_df <- readRDS(paste0("inst/app/www/", genome_id, "/", genome_id,"_ref.rds")) %>%
  filter(CDS == TRUE)

gene_list <- cds_df %>%
  select(gene, locus_tag) %>%
  unique()

AF_structure <- inner_join(ref_prot, ET_data, by = "locus_tag") %>%
  left_join(gene_list) %>%
  select(entry, locus_tag, gene, AF_url, ET_vec)

write_rds(AF_structure, paste0("inst/app/www/", genome_id, "/", genome_id,"_structure.rds"))


# Generate name table
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

name_table <- readRDS(paste0("inst/app/www/", genome_id, "/", genome_id,"_ref.rds")) %>%
  GetNameTable()

saveRDS(name_table, paste0("inst/app/www/", genome_id, "/", genome_id,"_name_table.rds"), compress = FALSE)


