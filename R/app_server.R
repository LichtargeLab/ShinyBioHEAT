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
    selectizeInput(inputId = "genome", label = "Select reference genome",
                   choices = list(`<i>Escherichia coli</i> MG1655` = "MG1655",
                                  `<i>Escherichia coli</i> REL606` = "REL606",
                                  `<i>Bacillus subtilis</i> strain 168` = "Bsubtilis168"),
                   select = "MG1655",
                   options = list(render = I(
                     '{
                           item: function(item, escape) {
                           return "<div>" + item.label + "</div>"
                           },
                           option: function(item, escape) {
                           return "<div>" + item.label + "</div>"
                           }
  }'
                   ))
    ),
    easyClose = F,
    footer = tagList(
      actionButton("confirm_genome", "Confirm genome")
    )
  )

  # Show the modal on start up ...
  showModal(init_modal)
  ref_df <- reactiveVal()
  ref_seq <- reactiveVal()
  genome_map <- reactiveVal()
  name_table <- reactiveVal()
  EA_list <- reactiveVal()
  structure_df <- reactiveVal()
  bg_example <- reactiveVal()
  string_species_id <- reactiveVal()

  observeEvent(input$confirm_genome, {
    ref_df_file <- paste0("app/www/", input$genome, "/", input$genome, "_ref.rds")
    genome_map_file <- paste0("app/www/", input$genome, "/", input$genome, "_genome_map.rds")
    ref_seq_file <- paste0("app/www/", input$genome, "/", input$genome, "_seq.rds")
    name_table_file <- paste0("app/www/", input$genome, "/", input$genome, "_name_table.rds")
    EA_list_file <- paste0("app/www/", input$genome, "/", input$genome, "_EA_list.rds")
    structure_df_file <- paste0("app/www/", input$genome, "/", input$genome, "_structure.rds")
    bg_example_file <- paste0("app/www/", input$genome, "/", input$genome, "_random_mut.rds")

    withProgress(
      message = "Loading files",
      expr = {
        ref_df(readRDS(app_sys(ref_df_file)))
        ref_seq(readRDS(app_sys(ref_seq_file)))
        genome_map(readRDS(app_sys(genome_map_file)))
        name_table(readRDS(app_sys(name_table_file)))
        EA_list(readRDS(app_sys(EA_list_file)))
        structure_df(readRDS(app_sys(structure_df_file)))
        bg_example(readRDS(app_sys(bg_example_file)))
      }
    )
    if (input$genome == "Bsubtilis168") {
      string_species_id("224308")
    } else if (input$genome %in% c("MG1655", "REL606")) {
      string_species_id("511145")
    }
    removeModal()
    processed_evolve <- mod_data_input_server("input_page", name_table = name_table(),
                                              EA_list = EA_list(), ref_df = ref_df(),
                                              ref_seq = ref_seq(), genome_map = genome_map(),
                                              strain = reactive(input$genome))
    random_bg <- mod_random_mut_server("random", name_table = name_table(),
                                       EA_list = EA_list(), ref_df = ref_df(),
                                       ref_seq = ref_seq(), genome_map = genome_map(),
                                       bg_example = bg_example())
    gene_rankings <- mod_EA_analysis_server("EA_analysis", processed_evolve = processed_evolve,
                                            random_bg = random_bg, ref_df = ref_df(),
                                            string_species_id = string_species_id())
    mod_gene_overlap_server("gene_overlap", gene_rankings = gene_rankings,
                            string_species_id = string_species_id())
    quick_search_output <- mod_EA_search_server("search", name_table = name_table(),
                                                EA_list = EA_list(), strain = reactive(input$genome))
    mod_structure_viewer_server("structure", processed_evolve = processed_evolve,
                                quick_search_output = quick_search_output,
                                structure_df = structure_df())
  })

}
