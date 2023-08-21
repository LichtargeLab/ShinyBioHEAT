library(dplyr)
library(ShinyBioHEAT)

library_path <- devtools::session_info() %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "package") %>%
  filter(package == "ShinyBioHEAT") %>%
  .$packages.path

library_path <- paste0(library_path, "/app/www/")

EA_files <- list.files(library_path, "_EA_list.rds", recursive = TRUE, full.names = TRUE)

for (i in 1:length(EA_files)) {
  temp <- readRDS(EA_files[i])
  saveRDS(temp, file = EA_files[i], compress = FALSE)
}




library_path <- paste0(.libPaths()[1], "/ShinyBioHEAT/app/www/")

EA_files <- list.files(library_path, "_EA_list.rds", recursive = TRUE, full.names = TRUE)

for (i in 1:length(EA_files)) {
  temp <- readRDS(EA_files[i])
  saveRDS(temp, file = EA_files[i], compress = FALSE)
}

temp <- readRDS(EA_files[1])
