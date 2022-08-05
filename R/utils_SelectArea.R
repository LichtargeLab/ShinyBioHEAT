#' SelectArea
#'
#' @description A utils function that return the clicked region in a
#' 3-sets venn diagram.
#'
#' @param center1 The center coordinates for circle 1. In vector format c(x, y)
#' @param center2 The center coordinates for circle 2. In vector format c(x, y)
#' @param center3 The center coordinates for circle 3. In vector format c(x, y)
#' @param click_coord The coordinates for the clicked point c(x, y)
#' @param radius The radius for all 3 circles.
#'
#' @return The name (string) of the clicked area.
#'
#' @noRd
SelectArea <- function(center1, center2, center3, click_coord, radius) {
  GetDist <- function(point1, point2) {
    distance <- sqrt(sum((point2 - point1)^2))
    return(distance)
  }
  distance_vec <- lapply(list(center1, center2, center3), FUN = GetDist, point2 = click_coord) %>%
    unlist()
  names(distance_vec) <- c("1", "2", "3")
  distance_vec <- distance_vec[distance_vec <= radius]
  if (length(distance_vec) == 0) {
    output <- NA
  } else {
    output <- paste0(names(distance_vec), collapse = "&")
    output <- paste0("set", output)
  }
  return(output)
}
