#' UpdateSelection
#'
#' @description A utils function that updates the selected regions for the venn diagram.
#'
#' @param selected_sets A string vector of previously selected region.
#' @param click_set A string of currently selected region.
#'
#' @return A string vector of selected region.
#'
#' @noRd
UpdateSelection <- function(selected_sets, click_set) {
  if(is.na(click_set)) {
    output <- selected_sets
  } else {
    if(click_set %in% selected_sets) {
      output <- selected_sets[selected_sets!=click_set]
    } else {
      output <- c(selected_sets, click_set)
    }
  }
  return(output)
}
