library(ggplot2)

plot_mv_heatmap <- function(tmp_dat, missing_col, nonmissing_col) {

  gathercols <- colnames(tmp_dat)

  tmp_dat %>%
    mutate(Sample = 1:n()) %>%
    tidyr::gather(Variable, Value, all_of(gathercols)) %>%
    mutate(`Is missing` = is.na(Value)) %>%
    ggplot(aes(x = Sample, y = as.factor(Variable), fill = `Is missing`)) +
    geom_tile() +
    ylab("Variable") +
    scale_fill_manual(values = c(nonmissing_col, missing_col)) +
    theme_minimal() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          title = element_text(size = 18)) +
    ggtitle("Missing values pattern")
}


plot_mv_segment <- function(tmp_dat,
                            thresh,
                            below_col = "black",
                            above_col = "tomato") {
  level_order <- tmp_dat %>%
    arrange(`% Missing`) %>%
    pull(Variable) %>%
    as.factor()

  tmp_dat <- tmp_dat %>%
    arrange(`% Missing`) %>%
    mutate(above_limit = `% Missing` > thresh,
           colors_legend = ifelse(above_limit, above_col, below_col))

  tmp_dat %>%
    ggplot(Variable = factor(Variable, levels = Variable)) +
    geom_segment(aes(y = Variable, yend = Variable,
                     x = 0, xend = `% Missing`, col = above_limit),
                 size = 1.5) +
    scale_color_manual(values = unique(pull(tmp_dat, colors_legend))) +
    geom_segment(aes(y = Variable, yend = Variable,
                     xend = 100, x = `% Missing`),
                 size = 1.5, col = "grey") +
    geom_point(mapping = aes(y = Variable, x = `% Missing`), size = 2) +
    geom_label(mapping = aes(y = Variable, x = `% Missing`,
                             label = paste0(`% Missing`, "%"))) +
    theme_minimal() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          title = element_text(size = 18),
          legend.position = "none") +
    ggtitle("Percentage of missing values") +
    scale_y_discrete(limits = level_order) +
    xlab("% Missing")
}


plot_points_density <- function(dat, input) {

  method <- dat[["results"]][["success"]] %>%
    filter(name %in% input[["plot_methods"]]) %>%
    pull(imputomics_name)

  res <- dat[["results"]][["results"]]

  res_var <- res[[method]][, input[["plot_var"]]]
  miss_var <- dat[["missing_data"]][, input[["plot_var"]]]

  plt_dat <- data.frame(var = res_var,
                        missing_var = miss_var)

  points_plt <- plt_dat %>%
    mutate(imputed = is.na(missing_var)) %>%
    ggplot() +
    ggbeeswarm::geom_quasirandom(aes(y = var, x = imputed, col = imputed)) +
    theme_minimal() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          axis.text.x = element_blank(),
          title = element_text(size = 18),
          legend.position = "bottom") +
    xlab("") +
    ylab(input[["plot_var"]]) +
    scale_color_manual(values = c(input[["nonmissing_col_res"]],
                                  input[["missing_col_res"]]))

  density_plt_dat <- plt_dat %>%
    mutate(imputed = is.na(missing_var),
           missing = ifelse(imputed, var, NA),
           observed = ifelse(imputed, NA, var))

  if(length(unique(density_plt_dat[["missing"]])) < 5) {
    dens_plt <- plt_dat %>%
      mutate(imputed = is.na(missing_var),
             missing = ifelse(imputed, var, NA),
             observed = ifelse(imputed, NA, var)) %>%
      ggplot() +
      geom_density(aes(x = observed), fill = input[["nonmissing_col_res"]], alpha = 0.4) +
      geom_histogram(aes(x = missing, y = ..density..), alpha = 0.4,
                     fill = input[["missing_col_res"]], col = input[["missing_col_res"]]) +
      theme_minimal() +
      coord_flip() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            axis.title.x =  element_blank(),
            axis.text.y = element_blank(),
            title = element_text(size = 18),
            legend.position = "none") +
      xlab("")
  } else {
    dens_plt <- plt_dat %>%
      mutate(imputed = is.na(missing_var),
             missing = ifelse(imputed, var, NA),
             observed = ifelse(imputed, NA, var)) %>%
      ggplot() +
      geom_density(aes(x = var, fill = imputed, col = imputed, alpha = 0.4)) +
      theme_minimal() +
      coord_flip() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            axis.title.x =  element_blank(),
            axis.text.y = element_blank(),
            title = element_text(size = 18),
            legend.position = "none") +
      xlab("") +
      scale_fill_manual(values=c(input[["nonmissing_col_res"]], input[["missing_col_res"]])) +
      scale_color_manual(values=c(input[["nonmissing_col_res"]], input[["missing_col_res"]]))
  }

  points_plt + theme(legend.position = "right") + dens_plt +
    patchwork::plot_layout(guides = "collect", design = "112") +
    patchwork::plot_annotation(title = ggtitle(paste0("Variable: ",
                                           input[["plot_var"]],
                                           ",\nMethod: ",
                                           input[["plot_methods"]])),
                    caption = "Data imputed vs. observed",
                    theme = theme(plot.title = element_text(size = 16)))
}
