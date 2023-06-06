library(tidyverse)
library(genbankr)


gbk <- readGenBank("~/Work/EA_genome_prep/NC_012967.1/NC_012967.1.gb")

REL606_seq <- gbk@sequence %>%
  as.character() %>%
  str_split(., pattern = "", simplify = TRUE) %>%
  .[1,]

save(REL606_seq, file = "data/REL606_seq.rda")
