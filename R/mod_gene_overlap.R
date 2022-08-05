#' gene_overlap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gene_overlap_ui <- function(id){
  ns <- NS(id)
  tabPanel("Step 4: Gene enrichment",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               tags$h4("Gene rankings plot settings", style = "margin-top: 0;"),
               sliderInput(ns("venn_cutoff"), label = "Overlapping genes for top predictions", min = 0, max = 100, post = "%",
                           value = 5,
                           step = 0.5),
               actionButton(ns("draw_venn"),
                            label = "Draw Venn diagram", width = "100%", class = "btn-primary"),
               tags$hr(style="border-color: black;"),
               tagList(
                 tags$p("Run STRING analysis on selected genes"),
                 actionButton(inputId = ns("visit_Stringdb"), label = "STRING Analysis", width = "100%", class = "btn-primary")
               )
             ),
             mainPanel(
               width = 9,
               fluidRow(column(
                 6, align = "center",
                 div(
                   style = "position:relative",
                   plotOutput(
                     outputId = ns("venn_plot"),
                     click = ns("venn_plot_click")
                   )
                 ),
                 verbatimTextOutput(ns("set_info"))
               ),
               column(6,
                      DT::dataTableOutput(outputId = ns("selected_genes_df")))               )
             )
           )
  )
}

#' gene_overlap Server Functions
#'
#' @noRd
mod_gene_overlap_server <- function(id, gene_rankings){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    venn_data <- readRDS(app_sys("app/www/venn_data.rds"))
    set_data <- reactiveVal()
    selected_genes <- reactiveVal()
    observeEvent(input$draw_venn, {
      req(gene_rankings())
      set_data(GenelistToSets(gene_rankings(),
                              top = floor(input$venn_cutoff/100 * nrow(gene_rankings()))))
      selected_genes(dplyr::filter(set_data(), set %in% selected_sets()) )
    })
    selected_sets <- reactiveVal()
    selected_sets(c("empty", "set1&2&3"))
    output$venn_plot <- renderPlot({
      req(selected_sets())
      req(set_data())
      venn_text <- GetVenn(set_data(), venn_data$text_pos)
      venn_data$shape_df %>%
        dplyr::filter(set %in% selected_sets()) %>%
        ggplot2::ggplot() +
        ggplot2::geom_polygon(ggplot2::aes(x = x, y = y, group = set), fill = "red", alpha = 0.2) +
        ggplot2::geom_polygon(data = venn_data$shape_df, ggplot2::aes(x = x, y = y, group = set),
                              fill = NA,
                              color = "black",
                              size = 1,
                              alpha = 1) +
        ggplot2::lims(x = c(-2,2), y = c(-2,2)) +
        ggplot2::geom_text(data = venn_data$label_pos,
                           ggplot2::aes(x = x, y = y, label = name, hjust = hjust, vjust = vjust),
                           size = 6.5) +
        ggplot2::geom_text(data = venn_text,
                           ggplot2::aes(x = x, y = y, label = text, hjust = hjust, vjust = vjust),
                           size = 5) +
        ggplot2::coord_fixed() +
        ggplot2::guides(fill = "none") +
        ggplot2::theme_void()
    })

    output$set_info <- renderText({
      req(set_data())
      req(selected_genes())
      text1 <- paste0("top ", floor(isolate(input$venn_cutoff)/100 * nrow(gene_rankings())),
                      " genes from each methods are plotted")
      text2 <- paste0("total genes   : ", nrow(set_data()), " (100%)")
      text3 <- paste0("selected genes : ", nrow(selected_genes()),
                      " (", round(nrow(selected_genes())/nrow(set_data()), 3) *100, "%)"  )
      paste(text1, text2, text3, sep = "\n")
    })

    output$selected_genes_df <- DT::renderDataTable({
      req(selected_genes())
      selected_genes() %>%
        dplyr::select(-set) %>%
        DisplayDT(download.name = "test")
    })

    observeEvent(input$venn_plot_click, {
      click_set <- SelectArea(venn_data$center[[1]],
                              venn_data$center[[2]],
                              venn_data$center[[3]],
                              c(input$venn_plot_click$x,
                                input$venn_plot_click$y), 1.1)
      selected_sets(UpdateSelection(selected_sets(), click_set))
      selected_genes(dplyr::filter(set_data(), set %in% selected_sets()))
    })

    observeEvent(input$visit_Stringdb, {
      if (is.null(selected_genes())) {
        showModal(modalDialog(
          title = "No Venn diagram",
          "Plot Venn diagram first"
        ))
      } else if (nrow(selected_genes()) <= 1) {
        showModal(modalDialog(
          title = "No gene selected",
          "Please select more than 1 gene on the Venn diagram"
        ))
      } else {
        gene_list <- selected_genes()$locus_tag %>%
          paste0(., collapse = "%0d")
        string_api <- paste0("https://string-db.org/api/tsv-no-header/get_link?identifiers=", gene_list, "&species=511145")
        string_link <- readLines(string_api)
        shinyjs::js$browseURL(string_link)
      }
    })

  })
}

## To be copied in the UI
# mod_gene_overlap_ui("gene_overlap_1")

## To be copied in the server
# mod_gene_overlap_server("gene_overlap_1")
