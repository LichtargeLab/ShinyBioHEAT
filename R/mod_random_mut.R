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
  tabPanel("Step 2: Simulate random background",
           sidebarLayout(
             sidebarPanel(
               width = 3,
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
                            label = "Simulate", width = "50%", class = "btn-primary"),
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
             mainPanel(
               width = 9,
               fluidRow(
                 column(6,
                        plotOutput(outputId = ns("random_plot"))),
                 column(6,
                        align = "center",
                        DT::dataTableOutput(outputId = ns("random_nonsyno")),
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
    random_bg <- reactiveVal()
    random_bg(NULL)
    observeEvent(input$simulate, {
      if (input$strain * input$mut < 1000) {
        mut_check_text <- paste0("There are ", input$strain * input$mut, " mutations in the background. It is recommended to use at least 1000 mutations as background.")
        showModal(modalDialog(
          title = "More mutations are needed as background",
          mut_check_text
        ))
      }
      set.seed(input$random_seed)
      seeds <- sample(1:1e7, input$strain, replace = FALSE)
      withProgress(
        message = "Running Simulation",
        output <-
          dplyr::tibble(strain = paste0("random", 1:input$strain),
                        seeds = seeds) %>%
          dplyr::mutate(data = purrr::map(
            seeds,
            ~ RandomMut(
              ref_table = dplyr::filter(MG1655_ref, CDS == TRUE),
              DNA_seq = MG1655_seq,
              n = input$mut,
              ti = ceiling(input$mut * input$ti_ratio/100),
              cds_only = input$cds_only,
              seed = .
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
      random_bg(output)
    })
    random_bg.filtered <- reactive({
      if (is.null(random_bg())) {NULL}
      else {
        random_bg() %>%
          CleanEAOutput()
      }
    })
    output$random_nonsyno <- DT::renderDataTable(server = TRUE, {
      req(random_bg.filtered())
      random_bg.filtered() %>%
        dplyr::select(strain, gene, locus_tag, SUB, EA) %>%
        DisplayDT(download.name = "coding_mutations_in_random_background")
    })
    output$random_plot <- renderPlot({
      req(random_bg.filtered())
      random_bg.filtered() %>%
        GraphWholeGenomeEADist()
    })
    output$download_ui <- renderUI({
      req(random_bg.filtered())
      downloadButton(session$ns("download_bg_details"),
                     label = "Download detailed random mutations")
    })
    output$download_bg_details <- downloadHandler(
      filename = function() {
        paste("detailed.random.mutations", "xlsx", sep = ".")
      },
      content = function(file) {
        openxlsx::write.xlsx(list(random_bg()), file,
                             sheetName = "evolve")
      }
    )
    return(random_bg.filtered)
  })
}

## To be copied in the UI
# mod_random_mut_ui("random_mut_1")

## To be copied in the server
# mod_random_mut_server("random_mut_1")
