#' GraphWholeGenomeEADist
#'
#' @description Plot EA distribution. Make exponential fit for the binned EA data on
#' nonsynonymous mutations.
#'
#' @return A ggplot2 object.
#'
#' @noRd
GraphWholeGenomeEADist <- function(df, exp_fit = TRUE, title = NULL) {
  workdf <- df %>%
    dplyr::mutate(SNP.type = ifelse(
      stringr::str_detect(SUB, pattern = "X|\\*"),
      "nonsense",
      "nonsynonymous"
    )) %>%
    dplyr::mutate(SNP.type = factor(SNP.type, levels = c("nonsense", "nonsynonymous")))


  output_plot <- ggplot2::ggplot(workdf) +
    ggplot2::geom_histogram(
      ggplot2::aes(x = EA, y = ggplot2::after_stat(count), fill = SNP.type),
      breaks = 10 * 0:10,
      position = "stack",
      color = "black"
    ) +
    ggplot2::xlim(0, 100) +
    ggplot2::scale_fill_discrete(limits = levels(c("nonsense", "nonsynonymous")),
                                 breaks = c("nonsense", "nonsynonymous"),
                                 drop = FALSE) +
    ggplot2::scale_y_continuous("Count", breaks = integer_breaks) +
    ggplot2::labs(x = "EA bins", fill = "SNP type") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      title = ggplot2::element_text(size = 16, face = "bold"),
      axis.text = ggplot2::element_text(size = 12, face = "bold"),
      axis.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 14),
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(hjust = 0.5),
      aspect.ratio = 0.8
    ) +
    ggplot2::ggtitle(title)

  if(exp_fit == TRUE) {
    fit_para <- workdf %>%
      dplyr::filter(SNP.type == "nonsynonymous") %>%
      .$EA %>%
      Fit_EA_exp()
    if(!is.na(fit_para$lambda[[2]])) {
      fit_text <- bquote(atop(Count ~ " = " ~ .(round(fit_para$A[[2]], 2)) %.% e ^ {-.(signif(fit_para$lambda[[2]], 4)) %.% Bin},
                              R^2 ~ "=" ~.(fit_para$R2[[2]])))
      output_plot <- output_plot + ggplot2::stat_function(
        fun = function(x) {
          fit_para$A[[2]] * exp(-fit_para$lambda[[2]] * x)
        },
        xlim = c(5, 95), size = 0.6, linetype = 2, color = "black")
    } else {
      fit_text <- "Can't fit exponential decay with non linear regression"
    }

    output_plot <- output_plot +
      ggplot2::labs(x = "EA bins", fill = "SNP type", caption = fit_text)
  }
  return(output_plot)
}
