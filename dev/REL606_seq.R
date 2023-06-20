library(tidyverse)
library(genbankr)


gbk <- readGenBank("~/Work/EA_genome_prep/NC_012967.1/NC_012967.1.gb")

REL606_seq <- gbk@sequence %>%
  as.character() %>%
  str_split(., pattern = "", simplify = TRUE) %>%
  .[1,]

saveRDS(REL606_seq, file = "inst/app/www/REL606/REL606_seq.rds", compress = FALSE)

load("data/MG1655_seq.rda")
saveRDS(MG1655_seq, file = "inst/app/www/MG1655/MG1655_seq.rds", compress = FALSE)
