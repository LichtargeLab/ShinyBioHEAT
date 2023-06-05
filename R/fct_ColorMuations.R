#' ColorMuations
#'
#' @description A function that convert mutation data into color codes
#' @param mutation_df A dataframe with mutation information. Each residue is one row.
#' The mutation_count, unique_mutation_count, sumEA and ET are stored for each residue.
#'
#' @return A dataframe that appends the color codes to the input dataframe.
#'
#' @noRd
ColorMutations <- function(mutation_df, ET_color = c("ET", "red_white_blue", "red_yellow_green")) {
  ET_color <- match.arg(ET_color)
  workdf <- mutation_df %>%
    dplyr::mutate(mutation_count_color = GetColor(mutation_count, lower_bound = 0,
                                                  color_type = "white_red"),
                  unique_mutation_count_color = GetColor(unique_mutation_count, lower_bound = 0,
                                                         color_type = "white_red"),
                  sumEA_color = GetColor(sumEA, lower_bound = 0,
                                         color_type = "white_red"),
                  ET_color = GetColor(ET, lower_bound = 0, upper_bound = 100, color = ET_color,
                                      include_both_limits = FALSE),
                  pLDDT_color = GetColor(pLDDT, lower_bound = 0, upper_bound = 100, color = "alphafold")
    )
  return(workdf)
}
