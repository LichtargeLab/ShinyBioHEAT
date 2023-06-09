#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # the modal dialog where the user can enter the query details.
  init_modal <- modalDialog(
    title = "Initialize app",
    selectInput(
      inputId = "genome",
      label = "Select reference genome",
      choices = c("E. coli MG1655" = "MG1655",
                  "E. coli REL606" = "REL606"),
      selected = "MG1655",
      selectize = FALSE
    ),
    easyClose = F,
    footer = tagList(
      actionButton("confirm_genome", "Confirm genome")
    )
  )

  # Show the modal on start up ...
  showModal(init_modal)
  name_table <- reactiveVal()
  EA_list <- reactiveVal()
  observeEvent(input$confirm_genome, {
    removeModal()
    withProgress(
      message = "Loading files",
      expr = {
        name_table(readRDS(app_sys("app/www/MG1655_name_table.rds")))
        EA_list(readRDS(app_sys("app/www/MG1655/MG1655_EA_list.rds")))
      }
    )
  })
  processed_evolve <- mod_data_input_server("input_page", name_table = name_table(),
                                            EA_list = EA_list(), strain = reactive(input$genome))
  random_bg <- mod_random_mut_server("random", name_table = name_table(),
                                     EA_list = EA_list())
  gene_rankings <- mod_EA_analysis_server("EA_analysis", processed_evolve = processed_evolve,
                                          random_bg = random_bg)
  mod_gene_overlap_server("gene_overlap", gene_rankings = gene_rankings)
  quick_search_output <- mod_EA_search_server("search", name_table = name_table(),
                                              EA_list = EA_list())
  mod_structure_viewer_server("structure", processed_evolve = processed_evolve,
                              quick_search_output = quick_search_output)
}
