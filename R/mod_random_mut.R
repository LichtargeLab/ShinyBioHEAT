#' random_mut UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_random_mut_ui <- function(id){
  ns <- NS(id)
  tabPanel("Step 2: Get background distribution",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               radioButtons(
                 inputId = ns("input_type"),
                 label = "Select input type",
                 choices = c("Simulate random mutations" = "simulation",
                             "Use example random mutations" = "example",
                             "Use custom mutations" = "custom"),
                 selected = "simulation"
               ),
               tags$hr(style="border-color: black;"),
               # Simulation UI
               div(id = ns("simulation_ui"),
                   tags$h4("Simulation settings", style = "margin-top: 0;"),
                   sliderInput(
                     ns("mut"),
                     label = "Simulated random coding mutations per strain (100-1000)",
                     value = 100,
                     min = 100,
                     max = 1000,
                     step = 100
                   ),
                   sliderInput(
                     ns("strain"),
                     label = "Number of simulated strains (1-20)",
                     value = 1,
                     min = 1,
                     max = 20,
                     step = 1
                   ),
                   actionButton(ns("simulate"),
                                label = "Simulate", width = "100%", class = "btn-primary"),
                   tags$hr(style="border-color: black;"),
                   tags$h4("Advanced settings", style = "margin-top: 0;"),
                   sliderInput(
                     ns("ti_ratio"),
                     label = "Percent of transition mutations",
                     value = 50,
                     min = 0,
                     max = 100,
                     step = 1,
                     post  = " %"
                   ),
                   checkboxInput(
                     ns("cds_only"), label = "Coding regions only", value =  TRUE
                   ),
                   numericInput(
                     ns("random_seed"), label = "Set random seed", value = 1000
                   )
               ),
               # Example UI
               div(id = ns("example_ui"),
                   actionButton(ns("load_example"),
                                label = "Load precalculated background", width = "100%", class = "btn-warning"),
               ),
               # Custom input UI
               div(id = ns("custom_ui"),
                   tagList(
                     p("Upload either VCF or Substitution (SUB) files"),
                     fileInput(
                       ns("vcf_files"),
                       label = "Upload VCF files for background mutations",
                       multiple = TRUE,
                       accept = ".vcf"
                     ),
                     actionButton(inputId = ns("submit_vcf"), label = "Submit VCF files", align = "center", class = "btn-primary"),
                     br(),
                     br(),
                     br(),
                     fileInput(
                       ns("sub_files"),
                       label = "Upload SUB files for background mutations",
                       multiple = TRUE,
                       accept = ".csv"
                     ),
                     actionButton(inputId = ns("submit_sub"), label = "Submit SUB files", align = "center", class = "btn-primary")
                   ))
               #
             ),
             mainPanel(
               width = 9,
               fluidRow(
                 column(6,
                        plotOutput(outputId = ns("bg_plot"))),
                 column(6,
                        align = "center",
                        DT::dataTableOutput(outputId = ns("bg_nonsyno")),
                        uiOutput(ns("download_ui")))
               )
             ))
  )
}

#' random_mut Server Functions
#'
#' @noRd
mod_random_mut_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    bg <- reactiveVal()
    bg(NULL)
    observeEvent(input$simulate, {
      if (input$strain * input$mut < 1000) {
        mut_check_text <- paste0("There are ", input$strain * input$mut, " mutations in the background. It is recommended to use at least 1000 mutations as background.")
        showModal(modalDialog(
          title = "More mutations are needed as background",
          mut_check_text
        ))
      }
      set.seed(input$random_seed)
      withProgress(
        message = "Running Simulation",
        simulated_bg <-
          dplyr::tibble(strain = paste0("random", 1:input$strain)) %>%
          dplyr::mutate(data = purrr::map(
            strain,
            ~ RandomMut(
              ref_table = dplyr::filter(MG1655_ref, CDS == TRUE),
              DNA_seq = MG1655_seq,
              n = input$mut,
              ti = ceiling(input$mut * input$ti_ratio/100),
              cds_only = input$cds_only,
            )
          )) %>%
          dplyr::mutate(data = purrr::map(data, AnnotateMutations)) %>%
          tidyr::unnest(cols = c(data)) %>%
          dplyr::mutate(EA = purrr::map2_chr(locus_tag, SUB, ~GetEA(.x, .y, EA_list = MG1655_EA_list))) %>%
          dplyr::mutate(
            POS = as.integer(POS),
            AA_pos = as.integer(AA_pos),
            codon_pos = as.integer(codon_pos)
          ) %>%
          dplyr::rename(ti_tv = INFO)
      )
      bg(simulated_bg)
    })

    observeEvent(input$load_example, {
      bg(readRDS(app_sys("app/www/input_example.rds")))
    })

    observeEvent(input$submit_vcf, {

    })

    observeEvent(input$submit_sub, {

    })


    bg_filtered <- reactive({
      if (is.null(bg())) {NULL}
      else {
        bg() %>%
          CleanEAOutput()
      }
    })

    output$bg_nonsyno <- DT::renderDataTable(server = TRUE, {
      req(bg_filtered())
      bg_filtered() %>%
        dplyr::select(strain, gene, locus_tag, SUB, EA) %>%
        DisplayDT(download.name = "coding_mutations_in_random_background")
    })
    output$bg_plot <- renderPlot({
      req(bg_filtered())
      bg_filtered() %>%
        GraphWholeGenomeEADist()
    })
    output$download_ui <- renderUI({
      req(bg_filtered())
      downloadButton(session$ns("download_bg_details"),
                     label = "Download detailed random mutations")
    })
    output$download_bg_details <- downloadHandler(
      filename = function() {
        paste("detailed_random_mutations", "xlsx", sep = ".")
      },
      content = function(file) {
        openxlsx::write.xlsx(list(bg()), file,
                             sheetName = "random_bg")
      }
    )
    observeEvent(input$input_type, {
      if (input$input_type == "simulation") {
        shinyjs::show("simulation_ui")
      } else {
        shinyjs::hide("simulation_ui")
        shinyjs::hide("download_bg_details")
      }
      if (input$input_type == "example") {
        shinyjs::show("example_ui")
      } else {
        shinyjs::hide("example_ui")
      }
      if (input$input_type == "custom") {
        shinyjs::show("custom_ui")
      } else {
        shinyjs::hide("custom_ui")
      }
    })
    return(bg_filtered)
  })
}

## To be copied in the UI
# mod_random_mut_ui("random_mut_1")

## To be copied in the server
# mod_random_mut_server("random_mut_1")
