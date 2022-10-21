library(tidyverse)

all_mut <- read_csv("~/Work/Colistin/EA_antibiotics_resistance/data/ALE_mutations_with_SIFT.csv")

cip_wm <- all_mut %>%
  filter(abx == "CIP", mut == "WM") %>%
  select(strain, input_id = b_num, SUB) %>%
  group_by(strain) %>%
  nest()


dir.create("SUB_examples")

for (i in 1:nrow(cip_wm)) {
  write_csv(cip_wm$data[[i]], file = paste0("SUB_examples/", cip_wm$strain[i], ".csv"))
}

zip("inst/app/www/SUB_examples.zip", files = "SUB_examples")

unlink("SUB_examples", recursive = TRUE)
