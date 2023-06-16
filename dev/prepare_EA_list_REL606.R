library(dplyr)
library(stringr)
library(readr)


EAfiles <- list.files(path = "~/Work/EA_genome_prep/NC_012967.1/EA/", pattern = ".pred")
id <- str_split(EAfiles, pattern = ".pred", simplify = TRUE) [,1]
EA.list <- list()
for (i in seq_along(id)) {
  temp <- read_table(file = paste0("~/Work/EA_genome_prep/NC_012967.1/EA/", EAfiles[i]), col_names = c("SUB", "ACTION"), col_types = cols())
  temp1 <- temp$ACTION
  names(temp1) <- temp$SUB
  EA.list[[i]] <- temp1
  cat(i, "\n")
}
names(EA.list) <- id
saveRDS(EA.list, "inst/app/www/REL606/REL606_EA_list.rds", compress = "xz")
