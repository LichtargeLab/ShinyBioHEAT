#' DisplayDT
#'
#' @description Display tibble as DT
#'
#' @return A DT::datatable to display in shiny
#'
#' @noRd
DisplayDT <- function(tibble, download.name) {
  tibble %>%
    DT::datatable(
      filter = list(
        position = "top",
        clear = FALSE,
        plain = TRUE
      ),
      selection = "single",
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(sDom  = '<"top">Blrt<"bottom">ip',
                     lengthMenu = list(c(10, 50, 100, -1), c("10", "50", "100", "All")),
                     scrollX = TRUE,
                     fixedColumns = TRUE,
                     stateSave = TRUE,
                     columnDefs = list(list(className = "dt-head-center dt-center", targets = "_all")),
                     buttons = list(
                       list(extend = 'copy', title = NULL),
                       list(extend = 'csv', filename =  download.name, title = NULL),
                       list(extend = 'excel', filename =  download.name, title = NULL)
                     ))
    )
}
