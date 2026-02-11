#' EA_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_EA_analysis_ui <- function(id){
  ns <- NS(id)
  tabPanel("Step 3: Run analysis",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               tags$h4("EA analysis settings", style = "margin-top: 0;"),
               selectInput(
                 inputId = ns("adj.type"),
                 label = "Multiple testing adjustment",
                 choices = c("Benjamini-Hochberg / FDR" = "fdr",
                             "Bonferroni" = "bonferroni"),
                 selected = "fdr",
                 selectize = FALSE
               ),
               actionButton(ns("run_analysis"),
                            label = "EA analysis", width = "100%", class = "btn-primary"),
               tags$hr(style="border-color: black;"),
               tags$h4("Gene rankings plot settings", style = "margin-top: 0;"),
               selectInput(
                 inputId = ns("scale.type"),
                 label = "Scale type",
                 choices = c("normal" = "normal",
                             "log10" = "log10"),
                 selected = "log10",
                 selectize = FALSE
               ),
               splitLayout(
                 selectInput(ns("x_var"), "X", c("EA_KS rank", "EA_sum rank", "Frequency rank"),
                             selected = "Frequency rank", selectize = FALSE),
                 selectInput(ns("y_var"), "Y", c("EA_KS rank", "EA_sum rank", "Frequency rank"),
                             selected = "EA_KS rank", selectize = FALSE)
               ),
               tags$hr(style="border-color: black;"),
               uiOutput(outputId = ns("Stringdb"))
             ),
             mainPanel(
               width = 9,
               fluidRow(column(
                 6, align = "center",
                 div(
                   style = "position:relative",
                   plotOutput(
                     outputId = ns("ranking_plot"),
                     click = ns("plot_click")
                   )
                 ),
                 shinyjs::hidden(downloadButton(outputId = ns("download_gene_rank_plot"),
                                                label = "Download gene rankings plot")),
                 tags$br(),
                 tags$br()
               ),
               column(6, align = "center",
                      plotOutput(outputId = ns("gene_plot")),
                      shinyjs::hidden(downloadButton(outputId = ns("download_gene_hist"),
                                                     label = "Download EA distribution plot")),
                      tags$br(),
                      tags$br())),
               fluidRow(
                 column(6,
                        DT::dataTableOutput(outputId = ns("gene_rankings"))),
                 column(6,
                        DT::dataTableOutput(outputId = ns("filtered.mutations")))
               )
             )
           ))
}

#' EA_analysis Server Functions
#'
#' @noRd
mod_EA_analysis_server <- function(id, processed_evolve, random_bg,
                                   ref_df, string_species_id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Run EA KS test and frequency based analyses
    gene_rankings_df <- eventReactive(input$run_analysis, {
      req(processed_evolve())
      req(random_bg())
      req(input$adj.type)
      req(ref_df)
      withProgress(
        message = "Analyzing Data",
        EA_Freq(evolve = processed_evolve(), background = random_bg(),
                adj.method = input$adj.type, ref_df = ref_df)
      )
    })
    observeEvent(input$run_analysis, {
      if (is.null(processed_evolve()) & is.null(random_bg())) {
        showModal(modalDialog(
          title = "No input data",
          "Please submit input files or load example and simulate background mutations"
        ))
      } else if (is.null(processed_evolve())) {
        showModal(modalDialog(
          title = "No evolve strain data",
          "Please submit input files or load example"
        ))
      } else if(is.null(random_bg())) {
        showModal(modalDialog(
          title = "No background mutation",
          "Please simulate background mutations"
        ))
      }
    })
    output$gene_rankings <- DT::renderDataTable(server = TRUE, {
      req(gene_rankings_df())
      gene_rankings_df() %>%
        DisplayDT(download.name = "gene_ranks") %>%
        DT::formatSignif(columns = c("EAKS_p", "Freq_p", "EAsum", "expect_EAsum",
                                     "expect_mutation_count", "EAKS_p_adj", "Freq_p_adj"),
                         digits = 3)
    })

    output$filtered.mutations <- DT::renderDataTable(server = TRUE, {
      req(gene_rankings_df())
      req(length(input$gene_rankings_rows_selected) > 0)
      if (req(length(input$gene_rankings_rows_selected) > 0)) {
        isolate({
          processed_evolve()
        }) %>%
          dplyr::filter(locus_tag == gene_rankings_df()$locus_tag[input$gene_rankings_rows_selected]) %>%
          dplyr::arrange(dplyr::desc(EA)) %>%
          dplyr::select(strain, gene, locus_tag, SUB, EA) %>%
          DisplayDT(download.name = paste0(
            gene_rankings_df()$gene[input$gene_rankings_rows_selected],
            "_mutations_in_evolve_strains"
          ))
      }
    })
    var_match <- c("EAKS_rank", "EAsum_rank", "Freq_rank")
    names(var_match) <- c("EA_KS rank", "EA_sum rank", "Frequency rank")

    # Save gene ranking scatter plot as reactive object
    gene_scatter <- reactive({
      req(gene_rankings_df())
      req(input$scale.type)
      output.plot <- ggplot2::ggplot() +
        ggplot2::geom_point(
          data = gene_rankings_df(),
          ggplot2::aes_string(x = var_match[input$x_var], y = var_match[input$y_var]),
          fill = "gray60",
          color = "gray60",
          size = 2,
          alpha = 0.5
        ) +
        ggplot2::geom_function(
          fun = function(x)
            x,
          color = "red",
          linetype = 2, alpha = 0.5
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(title = "")) +
        ggplot2::labs(x = input$x_var, y = input$y_var) +
        ggplot2::coord_fixed() +
        ggplot2::theme_classic() +
        ggplot2::theme(
          title = ggplot2::element_text(size = 16, face = "bold"),
          axis.title = ggplot2::element_text(face = "bold", size = 15),
          axis.text = ggplot2::element_text(face = "bold", size = 15),
          legend.text = ggplot2::element_text(face = "bold", size = 10)
        ) +
        ggplot2::ggtitle("Gene rankings")
      if (length(input$gene_rankings_rows_selected) == 0) {
        output.plot
      } else {
        selected.gene <- dplyr::filter(gene_rankings_df(),
                                       locus_tag == gene_rankings_df()$locus_tag[input$gene_rankings_rows_selected])
        output.plot <- output.plot +
          ggplot2::geom_point(
            data = selected.gene,
            ggplot2::aes_string(x = var_match[input$x_var], y = var_match[input$y_var]),
            fill = "black",
            color = "black",
            size = 3
          ) +
          ggrepel::geom_text_repel(
            data = selected.gene,
            ggplot2::aes_string(x = var_match[input$x_var], y = var_match[input$y_var], label = "gene"),
            fontface = "bold",
            size = 5
          )
      }
      output.plot <- switch(
        input$scale.type,
        normal = output.plot +
          ggplot2::scale_x_reverse() +
          ggplot2::scale_y_reverse(),
        log10 = output.plot +
          ggplot2::scale_x_continuous(trans = reverselog_trans(10),
                                      breaks = 10^c(0:10)) +
          ggplot2::scale_y_continuous(trans = reverselog_trans(10),
                                      breaks = 10^c(0:10)) +
          ggplot2::guides(y = ggplot2::guide_axis_logticks(cap = "upper",
                                                           expanded = FALSE),
                          x = ggplot2::guide_axis_logticks(cap = "upper",
                                                           expanded = FALSE))
      )
      output.plot
    })

    # Render gene ranking plot
    output$ranking_plot <- renderPlot({
      gene_scatter()
    })

    # Show download gene rank button after figure is plotted
    observeEvent(gene_scatter(), {
      req(gene_scatter())
      shinyjs::show("download_gene_rank_plot")
    })

    output$download_gene_rank_plot <- downloadHandler(
      filename = function(){
        paste("gene_rank_plot", ".", "pdf",  sep="")},
      content = function(file){
        ggplot2::ggsave(file, plot=gene_scatter(),width = 4, height = 4,
                        unit = "in", device = "pdf")
      }
    )

    proxy <- DT::dataTableProxy("gene_rankings")
    observeEvent(input$plot_click, {
      # The reverse log scale used here is a custom one, so nearPoints function
      # can't map to the correct points. The data needs to be reverse log transformed
      # (-log10) in order for nearPoints to map to the correct point.
      mapping.df <- switch(
        input$scale.type,
        normal = gene_rankings_df(),
        log10 = gene_rankings_df() %>%
          dplyr::mutate(
            Freq_rank = -log10(Freq_rank),
            EAKS_rank = -log10(EAKS_rank),
            EAsum_rank = -log10(EAsum_rank)
          )
      )
      click_point <-
        nearPoints(
          mapping.df,
          input$plot_click,
          threshold = 5,
          maxpoints = 1,
          addDist = TRUE
        )
      if (nrow(click_point) == 0)
        return(NULL)
      # gene.ranking is the output DT of gene_rankings_df, thus they share the same row ID
      click_id <- which(gene_rankings_df()$gene == click_point$gene)
      proxy %>% DT::selectRows(click_id) %>%
        DT::selectPage((which(input$gene_rankings_rows_all == click_id)-1)%/% input$gene_rankings_state$length + 1)
    })

    # save gene EA histogram as reactive object
    gene_hist <- reactive({
      req(length(input$gene_rankings_rows_selected) > 0)
      if (length(input$gene_rankings_rows_selected) > 0) {
        GraphGeneEA(
          df = isolate({
            processed_evolve()
          }),
          bg = isolate({
            random_bg()
          }),
          locus = gene_rankings_df()$locus_tag[input$gene_rankings_rows_selected]
        ) +
          ggplot2::theme_classic() +
          ggplot2::theme(
            title = ggplot2::element_text(size = 16, face = "bold"),
            axis.title = ggplot2::element_text(face = "bold", size = 15),
            axis.text = ggplot2::element_text(face = "bold", size = 15),
            legend.text = ggplot2::element_text(face = "bold", size = 10)
          )
      }
    })

    # render gene EA histogram
    output$gene_plot <- renderPlot({
      req(gene_hist())
      gene_hist()
    })

    observeEvent(gene_hist(), {
      req(gene_hist())
      shinyjs::show("download_gene_hist")
    })

    output$download_gene_hist <- downloadHandler(
      filename = function(){
        paste("EA_distribution_", gene_rankings_df()$locus_tag[input$gene_rankings_rows_selected],
              ".", "pdf",  sep="")},
      content = function(file){
        ggplot2::ggsave(file, plot=gene_hist(),width = 5, height = 4,
                        unit = "in", device = "pdf")
      }
    )

    output$Stringdb <- renderUI({
      req(gene_rankings_df())
      # top 5%
      tagList(
        tags$h4("STRING Analysis"),
        selectInput(inputId = session$ns("string_rank"), label = "Gene ranking method", choices = c("EA_KS rank", "EA_sum rank", "Frequency rank"),
                    selected = "EA_KS rank", selectize = FALSE),
        sliderInput(session$ns("string_gene_count"), label = "Top genes to run String analysis", min = 1, max = nrow(gene_rankings_df()),
                    value = ceiling(nrow(gene_rankings_df())/20),
                    step = 1),
        actionButton(inputId = session$ns("visit_Stringdb"), label = "STRING Analysis", width = "100%", class = "btn-primary")

      )
    })

    observeEvent(input$visit_Stringdb, {
      req(gene_rankings_df())
      req(input$string_gene_count)
      gene_count <- input$string_gene_count
      gene_list <- gene_rankings_df()$string_id[which(gene_rankings_df()[[var_match[input$string_rank]]] <= input$string_gene_count)] %>%
        paste0(., collapse = "%0d")
      string_api <- paste0("https://string-db.org/api/tsv-no-header/get_link?identifiers=", gene_list, "&species=", string_species_id)
      string_link <- readLines(string_api)
      shinyjs::js$browseURL(string_link)
    })
    return(gene_rankings_df)
  })
}

## To be copied in the UI
# mod_EA_analysis_ui("EA_analysis_1")

## To be copied in the server
# mod_EA_analysis_server("EA_analysis_1")
