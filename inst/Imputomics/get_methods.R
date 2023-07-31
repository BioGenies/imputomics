
library(googlesheets4)
library(stringr)
library(imputomics)
library(dplyr)

sheet_dat <- read_sheet("https://docs.google.com/spreadsheets/d/1NhkV2ZlhDBAkQOpd3CCJbDoUQWf4WnlC9aiOLPcr4KU/edit?usp=sharing",
                        sheet = "hyperparameters", na = "")

funs_imputomics <- ls("package:imputomics")
funs_imputomics <- funs_imputomics[grepl("impute_", funs_imputomics)]

methods_table <- sheet_dat %>%
  filter(!rejected) %>%
  dplyr::select(`full name`, `imputomics function`) %>%
  filter(`imputomics function` %in% funs_imputomics) %>%
  mutate(name = str_replace_all(str_remove(`imputomics function`, "impute_"),
                                "_", " ")) %>%
  rename("full_name" = `full name`,
         "imputomics_name" = `imputomics function`,
         "name" = name)

#setdiff(funs_imputomics, methods_table[["imputomics_name"]])

saveRDS(methods_table, "./inst/Imputomics/methods_table.RDS")


