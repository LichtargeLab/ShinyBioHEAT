#' data_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_input_ui <- function(id){
  ns <- NS(id)
  tabPanel("Step 1: Upload data",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               radioButtons(
                 inputId = ns("input_type"),
                 label = "Select input type",
                 choices = c("VCF" = "VCF",
                             "Amino acid substitutions" = "SUB"),
                 selected = "VCF"
               ),
               uiOutput(outputId = ns("input_selection"))
             ),
             mainPanel(
               width = 9,
               fluidRow(
                 column(6,
                        plotOutput(outputId = ns("evolve_plot"))),
                 column(6, align = "center",
                        fluidRow(align = "left",
                                 shinyjs::hidden(textOutput(outputId = "ref_check")),
                                 tags$style("#ref_check {color:red}")),
                        DT::dataTableOutput(outputId = ns("evolve_mut_non")),
                        tags$br(),
                        shinyjs::hidden(downloadButton(outputId = ns("download_evolve_mut_detail"),
                                                       label = "Download detailed annotated mutations"))
                 )
               )
             )
           )
  )
}

#' data_input Server Functions
#'
#' @noRd
mod_data_input_server <- function(id, name_table){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$input_selection <- renderUI({
      if(input$input_type == "VCF") {
        tagList(
          fileInput(
            session$ns("evolve_files"),
            label = "Upload VCFs of evolve strains here",
            multiple = TRUE,
            accept = ".vcf"
          ),
          fileInput(
            session$ns("founder_files"),
            label = "Upload VCFs of founder strains here (optional)",
            multiple = TRUE,
            accept = ".vcf"
          ),
          tags$p(downloadLink(outputId = session$ns("vcf_example"), label = "Download example VCF files")),
          splitLayout(cellWidths = c("50%", "50%"),
                      actionButton(inputId = session$ns("submit_files"), label = "Submit files", align = "center", class = "btn-primary"),
                      actionButton(inputId = session$ns("load_example"), label = "Load example", align = "center", class = "btn-warning"))
        )
      } else {
        tagList(
          fileInput(
            session$ns("evolve_files"),
            label = "Upload amino acid substitution files (CSV files) of evolve strains here",
            multiple = TRUE,
            accept = ".csv"
          ),
          fileInput(
            session$ns("founder_files"),
            label = "Upload amino acid substitution files of founder strains here (optional)",
            multiple = TRUE,
            accept = ".csv"
          ),
          tags$p(downloadLink(outputId = session$ns("sub_example"), label = "Download example substitution files")),
          splitLayout(cellWidths = c("50%", "50%"),
                      actionButton(inputId = session$ns("submit_files"), label = "Submit files", align = "center", class = "btn-primary"),
                      actionButton(inputId = session$ns("load_example"), label = "Load example", align = "center", class = "btn-warning")))
      }
    })
    ## Gather mutations in evolve strains, then annotate
    evolve_data <- eventReactive(input$submit_files, {
      req(input$evolve_files)
      withProgress(
        message = "Annotating evolve strains",
        switch(input$input_type,
               VCF = VCFtoEA(input$evolve_files$name, input$evolve_files$datapath,
                             EA_list = MG1655_EA_list, ref_seq = MG1655_seq),
               SUB = SUBtoEA(input$evolve_files$name, input$evolve_files$datapath,
                             name_table = name_table, EA_list = MG1655_EA_list))
      )
    })

    ## Gather mutations in founder strains, if any, then annotate
    founder_data <- eventReactive(input$submit_files, {
      req(input$founder_files)
      withProgress(
        message = "Annotating founder strains",
        switch(input$input_type,
               VCF = VCFtoEA(input$founder_files$name, input$founder_files$datapath,
                             EA_list = MG1655_EA_list, ref_seq = MG1655_seq),
               SUB = SUBtoEA(input$founder_files$name, input$founder_files$datapath,
                             name_table = name_table, EA_list = MG1655_EA_list))
      )
    })

    ## Remove founder mutations from evolve strains
    evolve_data_temp <- reactive({
      if (is.null(input$founder_files)) {
        evolve_data() %>%
          dplyr::filter(match_reference == TRUE) %>%
          CleanEAOutput()
      } else {
        if (input$input_type == "VCF") {
          temp <- dplyr::anti_join(evolve_data(), founder_data(), by = c("POS", "REF", "ALT"))
        } else {
          temp <- dplyr::anti_join(evolve_data(), founder_data(), by = c("locus_tag", "SUB"))
        }
        temp %>%
          dplyr::filter(match_reference == TRUE) %>%
          CleanEAOutput()
      }
    })
    evolve_data_filtered <- reactiveVal()
    observeEvent(input$load_example,
                 evolve_data_filtered(readRDS(app_sys("app/www/input_example.rds"))))
    observeEvent(input$submit_files,
                 evolve_data_filtered(evolve_data_temp()))

    output$evolve_mut_non <- DT::renderDataTable(server = TRUE, {
      req(evolve_data_filtered())
      evolve_data_filtered() %>%
        dplyr::select(strain, gene, locus_tag, SUB, EA) %>%
        DisplayDT(download.name = "coding_mutations_in_evolve_strains")
    })
    output$evolve_plot <- renderPlot({
      req(evolve_data_filtered())
      evolve_data_filtered() %>%
        GraphWholeGenomeEADist()
    })


    ## Ref check and Conditions for hide/show download button
    observeEvent(input$submit_files, {
      req(evolve_data_temp())
      ref_mismatch_count <- sum(evolve_data()$match_reference == FALSE)
      if (ref_mismatch_count != 0) {
        ref_check_text <- paste0("There are ", ref_mismatch_count, " entry(ies) in the evolved strain file(s) that don't match the reference genome. Please call mutations against the correct reference. Check detailed annotated mutations for more information.")
        showModal(modalDialog(
          title = "Check reference",
          ref_check_text
        ))
      }
      shinyjs::show("download_evolve_mut_detail")
    })
    observeEvent(input$load_example, {
      shinyjs::hide("download_evolve_mut_detail")
    })

    output$download_evolve_mut_detail <- downloadHandler(
      filename = function() {
        paste("detailed_mutations", "xlsx", sep = ".")
      },
      content = function(file) {
        if (is.null(input$founder_files) || input$founder_files == "") {
          openxlsx::write.xlsx(list(evolve_data()), file,
                               sheetName = "evolve")
        } else {
          openxlsx::write.xlsx(list(evolve_data(), founder_data()), file,
                               sheetName = c("evolve", "founder"))
        }
      }
    )
    ## VCF examples
    output$vcf_example <- downloadHandler(
      filename = function() {
        paste("VCF_examples", "zip", sep = ".")
      },
      content <- function(file) {
        file.copy(app_sys("app/www/VCF_examples.zip"), file)
      },
      contentType = "application/zip"
    )
    ## SUB examples
    output$sub_example <- downloadHandler(
      filename = function() {
        paste("SUB_examples", "zip", sep = ".")
      },
      content <- function(file) {
        file.copy(app_sys("app/www/SUB_examples.zip"), file)
      },
      contentType = "application/zip"
    )

    return(evolve_data_filtered)
  })
}

## To be copied in the UI
# mod_data_input_ui("data_input_1")

## To be copied in the server
# mod_data_input_server("data_input_1")
