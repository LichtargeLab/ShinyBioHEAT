# 2025-10-09
library(tidyverse)



genome_id <- "MG1655"
AF_structure <- read_rds(paste0("inst/app/www/", genome_id, "/", genome_id,"_structure.rds")) %>%
  mutate(AF_url = paste0("https://alphafold.ebi.ac.uk/files/AF-", entry, "-F1-model_v", version, ".pdb"))
write_rds(AF_structure, paste0("inst/app/www/", genome_id, "/", genome_id,"_structure.rds"))

# Uniprot ids for REL606 needs to be remapped using sequences, because some of the enteries are moved
# from Uniprot to UniPrac/UniRef. Thus no link for their AF structures.
source("dev/REL606_structure.R")


genome_id <- "Bsubtilis168"
AF_structure <- read_rds(paste0("inst/app/www/", genome_id, "/", genome_id,"_structure.rds")) %>%
  mutate(AF_url = paste0("https://alphafold.ebi.ac.uk/files/AF-", entry, "-F1-model_v", version, ".pdb"))
write_rds(AF_structure, paste0("inst/app/www/", genome_id, "/", genome_id,"_structure.rds"))
