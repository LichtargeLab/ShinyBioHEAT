#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  name_table <- readRDS("inst/app/www/MG1655_name_table.rds")
  processed_evolve <- mod_data_input_server("input_page", name_table = name_table)
  random_bg <- mod_random_mut_server("random")
  mod_EA_analysis_server("EA_analysis", processed_evolve = processed_evolve, random_bg = random_bg)
  quick_search_output <- mod_EA_search_server("search", name_table = name_table)
  # structure_server("structure", processed_evolve = processed_evolve,
  #                  quick_search_output = quick_search_output, pdb_df = pdb_df,
  #                  ET_data = ET_data, pLDDT_data = pLDDT_data)
}
