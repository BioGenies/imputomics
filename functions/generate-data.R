create_df <- function(n_metabolites, n_samples, frac_na) {
  total <-n_metabolites * n_samples
  vals <- runif(total)
  vals[sample(1L:total, round(frac_na * total, 0))] <- NA
  
  data.frame(matrix(vals, nrow = n_samples))
}