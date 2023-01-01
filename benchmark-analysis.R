library(dplyr)
library(tidyr)
library(ggplot2)
library(ggh4x)

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


dat <- readRDS("./results/second-time-benchmark.RDS") %>% 
  unlist(recursive = FALSE) %>% 
  unlist(recursive = FALSE) %>% 
  bind_rows() %>% 
  select(name, n_metabolites, n_samples, mean, converged) %>% 
  group_by(name, n_metabolites, n_samples) %>% 
  summarise(mean_time = mean(mean), sd_time = sd(mean), converged = mean(converged)) %>% 
  ungroup() %>% 
  mutate(name = gsub(pattern = "safe_impute_", replacement = "", x = name, fixed = TRUE)) 

png("./results/benchmark-summary.png", width = 480*2.5, height = 480*2.7, res = 110)
ggplot(dat, aes(x = name, y = mean_time, fill = converged)) +
  geom_col(position = "dodge") +
  facet_grid2(n_samples ~ n_metabolites, independent = "y", scales = "free_y", labeller = "label_both") +
  scale_y_continuous("Time [s]") +
  scale_fill_gradient("% converged", low = "red", high = "navyblue") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "bottom")
dev.off()

png("./results/benchmark-summary-log.png", width = 480*2.5, height = 480*2.7, res = 110)
ggplot(dat, aes(x = name, y = log10(mean_time), fill = converged)) +
  geom_col(position = "dodge") +
  facet_grid2(n_samples ~ n_metabolites, independent = "y", scales = "free_y", labeller = "label_both") +
  scale_y_continuous("Time [s] (log-scale)") +
  scale_fill_gradient("% converged", low = "red", high = "navyblue") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "bottom")
dev.off()
