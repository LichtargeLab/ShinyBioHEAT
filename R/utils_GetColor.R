#' GetColor
#'
#' @description A function that coverts a numerical vector to a string vector that
#' stores the hex colors.
#' @param x The numerical vector
#' @param lower_bound The lower bound of the color conversion
#' @param upper_bound The upper bound of the color conversion
#' @param color_type The color scheme to use
#'
#' @return The converted hex color vector.
#'
#' @noRd
GetColor <- function(x, lower_bound = min(x), upper_bound = max(x),
                     color_type = c("ET", "red_white", "red_white_blue", "white_red", "alphafold")) {
  scaled_x <- (x - lower_bound)/(upper_bound - lower_bound) * 99
  colorRange <- SelectColor(color_type = color_type)
  output <- colorRange[ceiling(scaled_x)+1]
  return(output)
}
