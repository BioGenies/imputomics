

#' Plotting missing values heatmap
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom tidyr gather
#'
#' @param dat a data.frame containing missing values denoted by NA.
#'
#' @returns A \code{ggplot2} object.
#'
#' @examples
#' data(sim_miss)
#' plot_mv_heatmap(sim_miss)
#'
#' @export 
#'


plot_mv_heatmap <- function(dat) {

  gathercols <- colnames(dat)
  dat %>%
    mutate(Sample = 1:n()) %>%
    gather(Variable, Value, all_of(gathercols)) %>%
    mutate(`Is missing` = is.na(Value)) %>%
    ggplot(aes(x = Sample, y = as.factor(Variable), fill = `Is missing`)) +
    geom_tile() +
    ylab("Variable") +
    scale_fill_manual(values = c("grey", "black")) +
    theme_minimal() +
    ggtitle("Missing values pattern")
}

#' Missing ratio summary
#'
#' This function calculates the ratios of missing values per each column from
#' the provided data in descending order.
#'
#' @inheritParams plot_mv_heatmap
#'
#' @returns A data.frame.
#'
#' @examples
#' data(sim_miss)
#' get_variables_table(sim_miss)
#'
#' @export 
#'

get_variables_table <- function(missing_data) {
  data.frame(
    Variable = colnames(missing_data),
    Percentage_Missing = 100*colMeans(is.na(missing_data))
  )%>%
    arrange(-Percentage_Missing) %>%
    rename(`% Missing` = Percentage_Missing)
}


#' Plotting missing values ratios
#'
#' This function makes a plot showing percentage content of missing values
#' per variable. When a variable has more than 20% missing values its color is
#' red.
#'
#' @inheritParams plot_mv_heatmap
#'
#' @returns A \code{ggplot2} object.
#'
#' @examples
#' data(sim_miss)
#' plot_mv_segment(sim_miss)
#'
#' @export 
#'


plot_mv_segment <- function(dat) {

  dat <- get_variables_table(dat)

  level_order <- dat %>%
    arrange(`% Missing`) %>%
    pull(Variable)

  dat <- dat %>%
    mutate(above_limit = `% Missing` > 20,
           colors_legend = ifelse(above_limit, "tomato", "black"))

  dat %>%
    ggplot(Variable = factor(Variable, levels = Variable)) +
    geom_segment(aes(y = Variable, yend = Variable,
                     x = 0, xend = `% Missing`, col = above_limit),
                 size = 1.5) +
    scale_color_manual(values = sort(unique(pull(dat, colors_legend)))) +
    geom_segment(aes(y = Variable, yend = Variable,
                     xend = 100, x = `% Missing`),
                 size = 1.5, col = "grey") +
    geom_point(mapping = aes(y = Variable, x = `% Missing`)) +
    geom_label(mapping = aes(y = Variable, x = `% Missing`,
                             label = paste0(`% Missing`, "%"))) +
    theme_minimal() +
    theme(legend.position = "none") +
    ggtitle("Percentage of missing values") +
    scale_y_discrete(limits = level_order) +
    xlab("% Missing")
}

#' Plotting densities of observed and imputed data points.
#'
#' @importFrom ggbeeswarm geom_quasirandom
#' @import patchwork
#'
#' @inheritParams plot_mv_heatmap
#' @param dat_imputed imputed dataset containing no NAs.
#' @param variable a character name of a variable. Should be contained in both
#' \code{dat} and \code{dat_imputed}. Default to first variable from the data.
#'
#' @returns A \code{ggplot2} object.
#'
#' @examples
#' data(sim_miss_large)
#' sim_miss_large_imputed <- impute_knn(sim_miss_large)
#' plot_points_density(sim_miss_large, sim_miss_large_imputed)
#'
#' @export 
#'

plot_points_density <- function(dat, dat_imputed, variable = colnames(dat)[1]) {

  if(!(variable %in% colnames(dat)))
    stop("Provided variable is not contained in dat.")
  if(!all(colnames(dat_imputed) == colnames(dat)))
    stop("colnames of dat and dat_imputed differs!")

  var <- dat_imputed[, variable]
  miss_var <- dat[, variable]

  plt_dat <- data.frame(var = var,
                        miss_var = miss_var)

  points_plt <- plt_dat %>%
    mutate(imputed = is.na(miss_var)) %>%
    ggplot() +
    geom_quasirandom(aes(y = var, x = imputed, col = imputed)) +
    theme_minimal() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          axis.text.x = element_blank(),
          title = element_text(size = 18),
          legend.position = "bottom") +
    xlab("") +
    ylab(variable) +
    scale_color_manual(values=c("#3d3b3c", "#fb5607"))

  density_plt_dat <- plt_dat %>%
    mutate(imputed = is.na(miss_var),
           missing = ifelse(imputed, var, NA),
           observed = ifelse(imputed, NA, var))

  if(length(unique(density_plt_dat[["missing"]])) < 5) {
    dens_plt <- plt_dat %>%
      mutate(imputed = is.na(miss_var),
             missing = ifelse(imputed, var, NA),
             observed = ifelse(imputed, NA, var)) %>%
      ggplot() +
      geom_density(aes(x = observed), fill = "#3d3b3c", alpha = 0.4) +
      geom_histogram(aes(x = missing, y = after_stat(density)), alpha = 0.4,
                     fill = "#fb5607", col = "#fb5607") +
      theme_minimal() +
      coord_flip() +
      theme(axis.title.x =  element_blank(),
            axis.text.y = element_blank(),
            legend.position = "none") +
      xlab("")
  } else {
    dens_plt <- plt_dat %>%
      mutate(imputed = is.na(miss_var),
             missing = ifelse(imputed, var, NA),
             observed = ifelse(imputed, NA, var)) %>%
      ggplot() +
      geom_density(aes(x = var, fill = imputed, col = imputed, alpha = 0.4)) +
      theme_minimal() +
      coord_flip() +
      theme(axis.title.x =  element_blank(),
            axis.text.y = element_blank(),
            legend.position = "none") +
      xlab("") +
      scale_fill_manual(values=c("#3d3b3c", "#fb5607")) +
      scale_color_manual(values=c("#3d3b3c", "#fb5607"))
  }

  points_plt + theme(legend.position = "right") + dens_plt +
    plot_layout(guides = "collect", design = "112") +
    plot_annotation(title = ggtitle(paste0("Variable: ", variable)),
                    caption = "Data imputed vs. observed",
                    theme = theme(plot.title = element_text(size = 16)))
}




