#' EA_search UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_EA_search_ui <- function(id){
  ns <- NS(id)
  tabPanel("Quick EA Search",
           column(6, align = "center",
                  tags$h3("Search EA for missense mutations", align = "center"),
                  tags$br(),
                  tags$p("Gene id and substitution combination should be entered
                         to query the Evolutionary Action (EA). One entry per line.
                         Either locus tag or gene name can be used as gene id.
                         We recommend the using the locus tag to avoid the ambiguity
                         of gene name. Subsitution should follow the format as M1C.
                         Gene id and subsitution should be seperated with space or
                         tab. EA scores are scaled within each protein from 0 to 100,
                         with 100 as the most impactful mutation on protein function. ", align = "left"),
                  tags$br(),
                  textAreaInput(inputId = ns("EA_search_text"), label = NULL, height = "200px",
                                value = "b4011 M1D\nb4012 M1C\ngyrA S83A\nbasR G53E"),
                  actionButton(inputId = ns("EA_search_btn"), label = "Submit", class = "btn-primary"),
                  tags$br(),
                  tags$br(),
                  actionButton(inputId = ns("download_EA"),
                               label = "Access EA for all proteins here",
                               class ="btn-info")
           ),
           column(6,
                  DT::dataTableOutput(outputId = ns("EA_search_output"))))
}

#' EA_search Server Functions
#'
#' @noRd
mod_EA_search_server <- function(id, name_table, EA_list, strain){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(strain(), {
      if (strain() == "REL606") {
        updateTextAreaInput(inputId = "EA_search_text", label = NULL,
                            value = "ECB_RS16665 D50G\nECB_00097 M1C\ngyrA S83A\naraJ F239L")
      }
    })

    search_output <- eventReactive(input$EA_search_btn, {
      req(input$EA_search_text)
      Annotate_SUB_file(input = c(input$EA_search_text), EA_list, name_table, string_input = TRUE)
    })
    output$EA_search_output <- DT::renderDataTable({
      search_output() %>%
        dplyr::select(input_id, gene, locus_tag, SUB, EA) %>%
        DisplayDT(download.name = "quick_EA_search")
    })
    observeEvent(input$download_EA, {
      shinyjs::js$browseURL("https://github.com/LichtargeLab/Raw_EA_bacteria")
    })
    return(search_output)
  })
}

## To be copied in the UI
# mod_EA_search_ui("EA_search_1")

## To be copied in the server
# mod_EA_search_server("EA_search_1")
