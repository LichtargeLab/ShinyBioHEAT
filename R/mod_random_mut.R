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
                             "Custom VCF file" = "VCF",
                             "Custom SUB file" = "SUB"),
                 selected = "simulation"
               ),
               tags$hr(style="border-color: black;"),
               # Simulation UI
               div(id = ns("simulation_ui"),
                   tags$h4("Simulation settings", style = "margin-top: 0;"),
                   sliderInput(
                     ns("mut"),
                     label = "Number of simulated random mutations per strain (100-1000)",
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
               # VCF input UI
               div(id = ns("VCF_ui"),
                   tagList(
                     fileInput(
                       ns("VCF_files"),
                       label = "Upload VCF files for background mutations",
                       multiple = TRUE,
                       accept = ".vcf"
                     ),
                     actionButton(inputId = ns("submit_vcf"), width = "100%", label = "Submit VCF files", align = "center", class = "btn-primary")
                   )),
               # SUB input UI
               div(id = ns("SUB_ui"),
                   tagList(
                     fileInput(
                       ns("SUB_files"),
                       label = "Upload SUB files for background mutations",
                       multiple = TRUE,
                       accept = ".csv"
                     ),
                     actionButton(inputId = ns("submit_sub"), width = "100%", label = "Submit SUB files", align = "center", class = "btn-primary")
                   ))
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
mod_random_mut_server <- function(id, name_table){
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
      withProgress(
        message = "Loading data",
        bg(readRDS(app_sys("app/www/MG1655_random_mut.rds")))
      )
    })

    observeEvent(input$submit_vcf, {
      req(input$VCF_files)
      withProgress(
        message = "Annotating VCF files",
        VCF_df <- VCFtoEA(input$VCF_files$name, input$VCF_files$datapath,
                          EA_list = MG1655_EA_list, ref_seq = MG1655_seq)
      )
      ref_mismatch_count <- sum(VCF_df$match_reference == FALSE)
      if (ref_mismatch_count != 0) {
        ref_check_text <- paste0("There are ", ref_mismatch_count, " entry(ies) in the evolved strain file(s) that don't match the reference genome. Please call mutations against the correct reference. Check detailed annotated mutations for more information.")
        showModal(modalDialog(
          title = "Check reference",
          ref_check_text
        ))}
      VCF_df <- dplyr::filter(VCF_df, match_reference == TRUE)
      bg(VCF_df)
    })

    observeEvent(input$submit_sub, {
      req(input$SUB_files)
      withProgress(
        message = "Annotating SUB files",
        SUB_df <- SUBtoEA(input$SUB_files$name, input$SUB_files$datapath,
                          name_table = name_table, EA_list = MG1655_EA_list)
      )
      bg(SUB_df)
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
      if (input$input_type == "VCF") {
        shinyjs::show("VCF_ui")
      } else {
        shinyjs::hide("VCF_ui")
      }
      if (input$input_type == "SUB") {
        shinyjs::show("SUB_ui")
      } else {
        shinyjs::hide("SUB_ui")
      }
    })
    return(bg_filtered)
  })
}

## To be copied in the UI
# mod_random_mut_ui("random_mut_1")

## To be copied in the server
# mod_random_mut_server("random_mut_1")
