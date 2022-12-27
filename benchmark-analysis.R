library(dplyr)
library(tidyr)
library(ggplot2)

res <- readRDS("./results/first-time-benchmark.RDS") %>% 
  unlist(recursive = FALSE)

png("./results/benchmark-summary.png", width = 480*1.5, height = 480*2, res = 100)
res[sapply(res, class) != "try-error"] %>% 
  bind_rows() %>% 
  select(name, n_metabolites, n_samples, mean, median) %>% 
  tidyr::complete(name, n_metabolites, n_samples) %>% 
  rename(method = name) %>% 
  mutate(method = gsub(pattern = "impute_", replacement = "", x = method, fixed = TRUE)) %>% 
  pivot_longer(cols = c(mean, median), values_to = "time") %>%
  filter(name == "mean") %>% 
  mutate(noconv = is.na(time)) %>% 
  ggplot(aes(x = method, y = time)) +
  geom_col(position = "dodge") +
  geom_point(mapping = aes(shape = noconv), y = 0, color = "red") +
  facet_wrap(~ n_metabolites + n_samples, scales = "free_y", labeller = label_both,
             ncol = 2) +
  scale_y_continuous("Time [s]") +
  scale_shape_manual("No convergence", values = c(NA, 16)) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "bottom")
dev.off()
