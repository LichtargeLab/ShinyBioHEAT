library(tidyverse)
library(shiny)
# code to test venn diagram in shiny
gene_rankings_df <- read_csv("~/Work/Colistin/20210304-EAapp_v2/test/gene_ranks.csv")

ui <- fluidPage(
  tabPanel("Step 4: Gene enrichment",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               tags$h4("Gene rankings plot settings", style = "margin-top: 0;"),
               sliderInput("venn_cutoff", label = "Overlapping genes for top predictions", min = 1, max = nrow(gene_rankings_df),
                           value = ceiling(nrow(gene_rankings_df)/20),
                           step = 1),
               actionButton(("draw_venn"),
                            label = "Draw Venn diagram", width = "100%", class = "btn-primary"),
               tags$hr(style="border-color: black;"),
               tagList(
                 tags$p("Run STRING analysis on selected genes"),
                 actionButton(inputId = ("visit_Stringdb"), label = "STRING Analysis", width = "100%", class = "btn-primary")
               )
             ),
             mainPanel(
               width = 9,
               fluidRow(column(
                 6, align = "center",
                 div(
                   style = "position:relative",
                   plotOutput(
                     outputId = ("venn_plot"),
                     click = ("venn_plot_click")
                   )
                 ),
                 verbatimTextOutput("set_info")
               ),
               column(6,
                      DT::dataTableOutput(outputId = "selected_genes_df")),
               shinyjs::useShinyjs(),
               shinyjs::extendShinyjs(text = "shinyjs.browseURL = function(url) {window.open(url,'_blank');}", functions = 'browseURL')
               )
             )
           )
  )
)

server <- function(input, output) {
  venn_data <- readRDS(app_sys("app/www/venn_data.rds"))
  set_data <- reactiveVal()
  selected_genes <- reactiveVal()
  observeEvent(input$draw_venn, {
    set_data(GenelistToSets(gene_rankings_df, top = input$venn_cutoff))
    selected_genes(dplyr::filter(set_data(), set %in% selected_sets()) )
  })

  selected_sets <- reactiveVal()
  selected_sets(c("empty", "set1&2&3"))
  output$venn_plot <- renderPlot({
    req(selected_sets())
    req(set_data())
    venn_text <- GetVenn(set_data(), venn_data$text_pos)
    venn_data$shape_df %>%
      filter(set %in% selected_sets()) %>%
      ggplot() +
      geom_polygon(aes(x = x, y = y, group = set), fill = "red", alpha = 0.2) +
      geom_polygon(data = venn_data$shape_df, aes(x = x, y = y, group = set),
                   fill = NA,
                   color = "black",
                   size = 1,
                   alpha = 1) +  lims(x = c(-2,2), y = c(-2,2)) +
      geom_text(data = venn_data$label_pos,
                aes(x = x, y = y, label = name, hjust = hjust, vjust = vjust),
                size = 6.5) +
      geom_text(data = venn_text,
                aes(x = x, y = y, label = text, hjust = hjust, vjust = vjust),
                size = 5) +
      coord_fixed() +
      guides(fill = "none") +
      theme_void()
  })

  output$set_info <- renderText({
    req(set_data())
    req(selected_genes())
    text1 <- paste0("total genes   : ", nrow(set_data()), " (100%)")
    text2 <- paste0("selected genes : ", nrow(selected_genes()),
                    " (", round(nrow(selected_genes())/nrow(set_data()), 3) *100, "%)"  )
    paste(text1, text2, sep = "\n")
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

  SelectArea <- function(center1, center2, center3, click_coord, radius) {
    GetDist <- function(point1, point2) {
      distance <- sqrt(sum((point2 - point1)^2))
      return(distance)
    }
    distance_vec <- lapply(list(center1, center2, center3), FUN = GetDist, point2 = click_coord) %>%
      unlist()
    names(distance_vec) <- c("1", "2", "3")
    distance_vec <- distance_vec[distance_vec <= radius]
    if (length(distance_vec) == 0) {
      output <- NA
    } else {
      output <- paste0(names(distance_vec), collapse = "&")
      output <- paste0("set", output)
    }
    return(output)
  }

  UpdateSelection <- function(selected_sets, click_set) {
    if(is.na(click_set)) {
      output <- selected_sets
    } else {
      if(click_set %in% selected_sets) {
        output <- selected_sets[selected_sets!=click_set]
      } else {
        output <- c(selected_sets, click_set)
      }
    }
    return(output)
  }

  GenelistToSets <- function(gene_rankings, top) {
    top_list <- list("set1" = gene_rankings$locus_tag[gene_rankings$EAKS_rank <= top],
                     "set2" = gene_rankings$locus_tag[gene_rankings$EAsum_rank <= top],
                     "set3" = gene_rankings$locus_tag[gene_rankings$Freq_rank <= top])
    AnnotateSets <- function(input_list) {
      columns <- names(input_list)
      a2 <- unique(unlist(input_list[columns]))
      output <- lapply(input_list, `%in%`, x = a2) %>% as.data.frame() %>%
        mutate(element = a2) %>%
        rowwise() %>%
        mutate(vec = list(c(set1, set2, set3))) %>%
        ungroup() %>%
        mutate(vec = map(vec, ~c(1:3)[.])) %>%
        mutate(vec = map_chr(vec, ~paste0(., collapse = "&")),
               vec = paste0("set", vec)) %>%
        dplyr::rename(set = vec)
      return(output)
    }
    output <- AnnotateSets(top_list) %>%
      dplyr::select(locus_tag = element, set) %>%
      dplyr::left_join(gene_rankings, by = "locus_tag") %>%
      dplyr::select(gene, locus_tag, EAKS_rank, EAsum_rank, Freq_rank, set)
    return(output)
  }

  GetVenn <- function(sets_input, text_pos) {
    sets_count <- sets_input %>%
      group_by(set) %>%
      summarize(n = n()) %>%
      ungroup()

    df_element <- tibble(set = c("set1", "set2", "set3",
                                 "set1&2", "set1&3", "set2&3",
                                 "set1&2&3")) %>%
      mutate(set1 = str_detect(set, "1"),
             set2 = str_detect(set, "2"),
             set3 = str_detect(set, "3")) %>%
      left_join(sets_count, by = "set") %>%
      replace_na(list(n = 0))

    fmt <- sprintf("%%d\n(%%.%df%%%%)", 1)
    output <- text_pos %>%
      left_join(df_element, by = "set") %>%
      mutate(text = sprintf(fmt, n, 100 * n / sum(n)))
    return(output)
  }


}

shinyApp(ui, server)

