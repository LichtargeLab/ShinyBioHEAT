#' CleanEAOutput
#'
#' @description Convert EA column to numeric then remove NA rows.
#'
#' @return A tibble with locus_tag, gene, SUB, EA, and strian (if it is present
#' in the input)
#'
#' @noRd
CleanEAOutput <- function(df) {
  if ("strain" %in% names(df)) {
    output_df <- df %>%
      dplyr::mutate(EA = suppressWarnings(as.numeric(EA))) %>%
      dplyr::filter(!is.na(EA)) %>%
      dplyr::select(strain, locus_tag, gene, SUB, EA) %>%
      unique()
  } else {
    output.df <- df %>%
      dplyr::mutate(EA = suppressWarnings(as.numeric(EA))) %>%
      dplyr::filter(!is.na(EA)) %>%
      dplyr::select(locus_tag, gene, SUB, EA) %>%
      unique()
  }

  return(output_df)
}
