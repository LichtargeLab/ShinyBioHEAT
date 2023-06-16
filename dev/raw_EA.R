library(tidyverse)

strain <- "MG1655"

EA <- readRDS(paste0("inst/app/www/", strain, "/", strain, "_EA_list.rds"))
locus_tag <- names(EA)
for (i in 1:length(EA)) {
  temp <- data.frame(SUB = names(EA[[i]]), EA = EA[[i]])
  write_tsv(temp, paste0("../Raw_EA_bacteria/", strain, "/EA/", locus_tag[i], ".tsv"))
  rm(temp)
  if (i %% 100 == 0) {
    gc()
    print(i)
  }
}
