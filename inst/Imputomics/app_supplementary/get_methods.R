
library(googlesheets4)
library(stringr)
library(imputomics)
library(dplyr)

sheet_dat <- read_sheet("https://docs.google.com/spreadsheets/d/1NhkV2ZlhDBAkQOpd3CCJbDoUQWf4WnlC9aiOLPcr4KU/edit?usp=sharing",
                        sheet = "final-list-of-MVIAs", na = "")

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
         "name" = name) %>%
  filter(!(imputomics_name %in% c("impute_gsimp",
                                  "impute_metabimpute_gsimp",
                                  "impute_mai")))


# performance:
res <- readRDS("./inst/Imputomics/total_median_computed_value.RDS")

best_methods <- res %>%
  filter(frac_computed > 0.80) %>%
  top_n(10, -median_value) %>%
  mutate(method = str_replace_all(method, "_", " ")) %>%
  pull(method)

fastest_methods <- res %>%
  top_n(10, -median_time) %>%
  mutate(method = str_replace_all(method, "_", " ")) %>%
  pull(method)

methods_table <- methods_table %>%
  mutate(best = name %in% best_methods,
         fastest = name %in% fastest_methods)


best_MCAR <- readRDS("./inst/Imputomics/res_app.RDS") %>%
  filter(frac_computed > 0.80) %>%
  filter(method != "softimpute") %>%
  filter(MAR == 0, MNAR == 0) %>%
  group_by(method) %>%
  summarise(mean_NRMSE = mean(median_NRMSE)) %>%
  arrange(mean_NRMSE) %>%
  slice_head(n = 5) %>%
  mutate(method = paste0("impute_", method)) %>%
  pull(method)

best_MAR <- readRDS("./inst/Imputomics/res_app.RDS") %>%
  filter(frac_computed >= 0.80) %>%
  filter(method != "softimpute") %>%
  filter(MCAR == 0, MNAR == 0) %>%
  group_by(method) %>%
  summarise(mean_NRMSE = mean(median_NRMSE)) %>%
  arrange(mean_NRMSE) %>%
  slice_head(n = 5) %>%
  mutate(method = paste0("impute_", method)) %>%
  pull(method)

best_MNAR <- readRDS("./inst/Imputomics/res_app.RDS") %>%
  filter(frac_computed > 0.80) %>%
  filter(method != "softimpute") %>%
  filter(MCAR == 0, MAR == 0) %>%
  group_by(method) %>%
  summarise(mean_NRMSE = mean(median_NRMSE)) %>%
  arrange(mean_NRMSE) %>%
  slice_head(n = 6) %>%
  mutate(method = paste0("impute_", method)) %>%
  pull(method)

methods_table <- methods_table %>%
  mutate(MCAR = imputomics_name %in% best_MCAR,
         MAR = imputomics_name %in% best_MAR,
         MNAR = imputomics_name %in% best_MNAR)

# saving
saveRDS(methods_table, "./inst/Imputomics/methods_table.RDS")



