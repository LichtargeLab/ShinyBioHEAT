#' GraphGeneEA
#'
#' @description Graph EA distribution for a given gene and compare to background
#' EA distribution.
#'
#' @return A ggplot2 object
#'
#' @noRd
GraphGeneEA <- function(df, bg, locus, include.stop = TRUE) {
  if(include.stop == FALSE) {
    df <- dplyr::filter(df, EA < 100)
    bg <- dplyr::filter(bg, EA < 100)
  }
  plt_data <- df %>%
    dplyr::filter(locus_tag == locus)
  bg_data <- bg %>%
    dplyr::mutate(EA.bin = cut(EA, breaks = 10*0:10)) %>%
    dplyr::mutate(EA.bin = as.numeric(EA.bin) * 10 -5)
  KS.p <- plt_data %>%
    dplyr::summarise(ps = suppressWarnings(ks.test(EA, bg_data$EA,alternative = "less"))$p.value) %>%
    dplyr::mutate(ps = signif(ps, digits = 2))
  plt_data <- dplyr::mutate(plt_data, ps = KS.p)
  bg_factor <- bg$EA %>% cut(., breaks = 0:10*10) %>% table %>% max()
  evolve_factor <- plt_data$EA %>% cut(., breaks = 0:10*10) %>% table %>% max()
  scaling_factor <- evolve_factor/bg_factor
  plt <- ggplot2::ggplot() +
    ggplot2::geom_histogram(data = plt_data, ggplot2::aes(x = EA, y= ggplot2::after_stat(count)), fill = "red", color = "black", position="identity", alpha=1, breaks = 10*0:10) +
    ggplot2::geom_line(data = bg_data, ggplot2::aes(x = EA.bin, y = ggplot2::after_stat(count)*scaling_factor), stat = "count", size=1, linetype=3) +
    ggplot2::theme(text = ggplot2::element_text(size=15)) +
    ggplot2::xlim(0,100) +
    ggplot2::scale_y_continuous("Count", sec.axis = ggplot2::sec_axis(~./scaling_factor, name = "Count (Random)"),
                                breaks = integer_breaks) +
    ggplot2::labs(x = "EA bins") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none",
                   strip.text.y = ggplot2::element_text(size = 14), # set font for label bars
                   axis.text = ggplot2::element_text(size = 12), # set font for axis numbers
                   axis.title = ggplot2::element_text(size = 14)) + # set font for axis titles
    ggplot2::ggtitle(paste0(plt_data$gene[1], " - KS test p-value = ", KS.p))
  return(plt)
}
