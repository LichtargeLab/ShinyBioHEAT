library(tidyverse)
library(shiny)
library(NGLVieweR)

# code to test venn diagram in shiny
quick_search_output <- read_csv("~/Work/Colistin/20210304-EAapp_v2/test/quick_EA_search.csv")
ET_data <- read_csv("~/Work/Colistin/20210304-EAapp_v2/www/MG1655_ET.csv", col_types = cols())
pLDDT_data <- read_csv("~/Work/Colistin/20210304-EAapp_v2/www/alpha_fold_bfactor.csv", col_types = cols())
pdb_df <- read_csv("~/Work/Colistin/20210304-EAapp_v2/www/alpha_fold_list.csv", col_types = cols()) %>%
  mutate(pdb_path = paste0("~/Work/Colistin/20210304-EAapp_v2/www/alpha_fold/", locus_tag, ".pdb")) %>%
  arrange(locus_tag) %>%
  filter(locus_tag %in% ET_data$locus_tag, locus_tag %in% pLDDT_data$locus_tag)


ui <- fluidPage(
  tabPanel("Structure Viewer",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               tags$h4("Input settings", style = "margin-top: 0;"),
               selectizeInput(
                 inputId = ("mut_input"),
                 label = "Input mutation data",
                 choices = c("No" = "no",
                             "From EA Analysis" = "EA_analysis",
                             "From Quick EA Search" = "quick_search"),
                 selected = "no"
               ),
               radioButtons(
                 inputId = ("input_type"),
                 label = "Select protein input type",
                 choices = c("Protein name" = "gene",
                             "Locus tag" = "locus_tag"),
                 selected = "gene",
                 inline = TRUE
               ),
               selectizeInput(("prot_id"),
                              label = "Select protein",
                              choices = NULL,
                              selected = NULL,
                              multiple = TRUE,
                              options = list(
                                placeholder = "Choose",
                                maxItems = 1
                              )),
               actionButton(
                 inputId = ("load_structure"),
                 label = "Load structure",
                 class = "btn-primary"
               ),
               tags$hr(style="border-color: black;"),
               tags$h4("Display settings", style = "margin-top: 0;"),
               selectizeInput(("color_option"),
                              label = "Select a coloring style",
                              choices = NULL,
                              selected = NULL,
                              multiple = FALSE),
               actionButton(("update_color"),
                            label = "Update color",
                            class = "btn-primary"),
               checkboxInput(("highlight_mut"),
                             label = "Show mutated residues as spheres",
                             value = FALSE),
               checkboxInput(("single_monomer"),
                             label = "Show single monomer",
                             value = FALSE)
             ),
             mainPanel(
               width = 9,
               NGLVieweROutput(
                 outputId = ("structure")
               )
             )
           ))
)

server <- function(input, output) {
  observeEvent(c(input$input_type, input$mut_input), {
    gene_choices <- NULL
    locus_tag_choices <- NULL
    if (input$mut_input == "quick_search") {
      gene_choices <- dplyr::intersect(pdb_df$gene, quick_search_output$gene)
      locus_tag_choices <- dplyr::intersect(pdb_df$locus_tag, quick_search_output$locus_tag)
    # } else if (input$mut_input == "EA_analysis") {
    #   gene_choices <- dplyr::intersect(pdb_df$gene, processed_evolve()$gene)
    #   locus_tag_choices <- dplyr::intersect(pdb_df$locus_tag, processed_evolve()$locus_tag)
    } else {
      gene_choices <- pdb_df$gene
      locus_tag_choices <- pdb_df$locus_tag
    }
    if(input$input_type == "gene") {
      updateSelectizeInput(inputId = "prot_id",
                           choices = gene_choices,
                           selected = NULL,
                           server = TRUE
      )
    } else {
      updateSelectizeInput(inputId = "prot_id",
                           choices = locus_tag_choices,
                           selected = NULL,
                           server = TRUE
      )
    }
  })

  # This chunk updates the choices for input$color_option.
  # When no mutation data is imported, hide the options for color by mutations/EAs
  observeEvent(input$mut_input, {
    if(input$mut_input == "no") {
      updateSelectizeInput(inputId = "color_option",
                           choices = c("none" = "none",
                                       "ET" = "ET_color",
                                       "pLDDT" = "pLDDT_color"),
                           selected = "none",
                           server = TRUE
      )
    } else {
      updateSelectizeInput(inputId = "color_option",
                           choices = c("none" = "none",
                                       "ET" = "ET_color",
                                       "sumEA" = "sumEA_color",
                                       "All mutation count" = "mutation_count_color",
                                       "Unique mutation count" = "unique_mutation_count_color",
                                       "pLDDT" = "pLDDT_color"),
                           selected = "none",
                           server = TRUE
      )
    }
  })

  # Get the pdb file storing location according to the gene selected.
  pdb_file <- eventReactive(input$load_structure, {
    pdb_df$pdb_path[which(pdb_df[isolate(input$input_type)] == isolate(input$prot_id))]
  })


  mut_list <- eventReactive(input$load_structure, {
    filter_var <- isolate(input$input_type)
    id <- isolate(input$prot_id)
    ET_data_filt <- ET_data[which(ET_data[[filter_var]] == id),]
    pLDDT_data_filt <- pLDDT_data[which(pLDDT_data[[filter_var]] == id),]
    mut_data <- switch(isolate(input$mut_input),
                       "no" = tibble(gene = character(), locus_tag = character(),
                                     EA = numeric(), SUB = character()),
                       # "EA_analysis" = processed_evolve(),
                       "quick_search" = quick_search_output)
    mut_data <- mut_data[which(mut_data[[filter_var]] == id),] %>%
      mutate(AA.POS = str_sub(SUB, start = 2, end = -2)) %>%
      mutate(AA.POS = as.numeric(AA.POS)) %>%
      mutate(EA = as.numeric(EA)) %>%
      filter(!is.na(EA)) %>%
      group_by(locus_tag, AA.POS) %>%
      summarize(mutation_count = n(),
                unique_mutation_count = length(unique(SUB)),
                sumEA = sum(EA)) %>%
      ungroup() %>%
      left_join(ET_data_filt, ., by = c("locus_tag", "AA.POS")) %>%
      left_join(pLDDT_data_filt, by = c("locus_tag", "AA.POS")) %>%
      replace_na(list(mutation_count = 0,
                      unique_mutation_count = 0,
                      sumEA = 0,
                      ET = 0))
    if(!is.null(mut_data)) {
      ColorMutations(mut_data)
    } else {
      NULL
    }
  })

  color_list <- reactiveVal()
  color_list("white")

  observeEvent(input$color_option, {
    req(mut_list())
    color_list(AddColorSelection(df = mut_list(),
                                 chain = "A",
                                 color_var = isolate(input$color_option)))
  })
  # color_list <- reactive({
  #   if (!is.null(mut_list())) {
  #     AddColorSelection(df = mut_list(),
  #                       chain = "A",
  #                       color_var = isolate(input$color_option))
  #   } else {
  #     "white"
  #   }
  # })

  output$structure <- renderNGLVieweR({
    req(pdb_file())
    NGLVieweR(pdb_file()) %>%
      addRepresentation("cartoon", param = list(name = "cartoon",
                                                color = isolate(color_list()))) %>%
      stageParameters(backgroundColor = "black") %>%
      setQuality("high")
  })

  observeEvent(input$update_color, {
    NGLVieweR_proxy("structure") %>%
      updateRepresentation("cartoon", param = list(name = "cartoon",
                                                   color = isolate(color_list())))
    print(color_list())
  })

  observeEvent(c(input$highlight_mut,
                 input$update_color,
                 input$load_structure), {
                   if (input$highlight_mut == TRUE & !is.null(mut_list())) {
                     mut_sele <- mut_list() %>%
                       filter(mutation_count > 0) %>%
                       .$AA.POS %>%
                       paste0(., collapse = " or ")
                     mut_sele <- paste0(":A and (", mut_sele, ")")
                     NGLVieweR_proxy("structure") %>%
                       addSelection("spacefill", param = list(name = "spacefill",
                                                              sele = mut_sele,
                                                              color = color_list()))
                   } else {
                     NGLVieweR_proxy("structure") %>%
                       removeSelection("spacefill")
                   }
                 })
##### Functions
  # Alpha fold color: >90, #0153d6, 90-70, #65cbf3, 50-70, #fcdb4b, <=50, #f17c42
  SelectColor <- function(color_type = c("ET", "red_white", "red_white_blue", "white_red", "alphafold")) {
    if (color_type == "ET") {
      colorRange <- c("ff0000", "ff0c00", "ff1800", "ff2400", "ff3000", "ff3d00", "ff4900", "ff5500", "ff6100", "ff6e00",
                      "ff7a00", "ff8600", "ff9200", "ff9f00", "ffab00", "ffb700", "ffc300", "ffd000", "ffdc00", "ffe800",
                      "fff400", "fcff00", "f0ff00", "e4ff00", "d8ff00", "cbff00", "bfff00", "b3ff00", "a7ff00", "9bff00",
                      "8eff00", "82ff00", "76ff00", "6aff00", "5dff00", "51ff00", "45ff00", "39ff00", "2cff00", "20ff00",
                      "14ff00", "08ff00", "00ff04", "00ff10", "00ff1c", "00ff28", "00ff35", "00ff41", "00ff4d", "00ff59",
                      "00ff66", "00ff72", "00ff7e", "00ff8a", "00ff96", "00ffa3", "00ffaf", "00ffbb", "00ffc7", "00ffd4",
                      "00ffe0", "00ffec", "00fff8", "00f8ff", "00ecff", "00e0ff", "00d4ff", "00c7ff", "00bbff", "00afff",
                      "00a3ff", "0096ff", "008aff", "007eff", "0072ff", "0066ff", "0059ff", "004dff", "0041ff", "0035ff",
                      "0028ff", "001cff", "0010ff", "0004ff", "0800ff", "1400ff", "2000ff", "2c00ff", "3900ff", "4500ff",
                      "5100ff", "5d00ff", "6a00ff", "7600ff", "8200ff", "8e00ff", "9b00ff", "a700ff", "b300ff", "bf00ff") %>%
        paste0("#", .)
    } else if (color_type == "alphafold") {
      colorRange <- c(rep("#f17c42", 50),
                      rep("#fcdb4b", 20),
                      rep("#65cbf3", 20),
                      rep("#0153d6", 10))
    } else if (color_type == "red_white") {
      colorRange <- colorRampPalette(c("red", "white"))(100)
    } else if (color_type == "red_white_blue") {
      colorRange <- colorRampPalette(c("red", "white", "blue"))(100)
    } else {
      colorRange <- colorRampPalette(c("white", "red"))(100)
    }
    return(colorRange)
  }

  GetColor <- function(x, lower_bound = min(x), upper_bound = max(x),
                       color_type = c("ET", "red_white", "red_white_blue", "white_red", "alphafold")) {
    scaled_x <- (x - lower_bound)/(upper_bound - lower_bound) * 99
    colorRange <- SelectColor(color_type = color_type)
    output <- colorRange[ceiling(scaled_x)+1]
    return(output)
  }

  ColorMutations <- function(mutation_df) {
    workdf <- mutation_df %>%
      mutate(mutation_count_color = GetColor(mutation_count, lower_bound = 0,
                                             color_type = "white_red"),
             unique_mutation_count_color = GetColor(unique_mutation_count, lower_bound = 0,
                                                    color_type = "white_red"),
             sumEA_color = GetColor(sumEA, lower_bound = 0,
                                    color_type = "white_red"),
             ET_color = GetColor(ET, lower_bound = 0, upper_bound = 100, color = "ET"),
             pLDDT_color = GetColor(pLDDT, lower_bound = 0, upper_bound = 100, color = "alphafold"))
    return(workdf)
  }

  AddColorSelection <- function(df, chain, color_var = c("mutation_count_color",
                                                         "unique_mutation_count_color",
                                                         "sumEA_color",
                                                         "ET_color",
                                                         "none")) {
    if (color_var == "none") {
      color_list <- list(list("white", "*"))
    } else {
      chain <- paste0(":", chain)
      if (length(chain) == 1) {
        chain_arg <- chain
      } else if (length(chain > 1)) {
        chain_arg <- paste0(chain, collapse = " or ")
        chain_arg <- paste0("(", chain_arg, ")")
      }
      workdf <- tibble(AA.POS = df[["AA.POS"]],
                       color = df[[color_var]]) %>%
        group_by(color) %>%
        mutate(group = cur_group_id()) %>%
        mutate(group = paste0("g", group)) %>%
        group_by(group, color) %>%
        summarize(sele = paste0(AA.POS, collapse = " or ")) %>%
        ungroup() %>%
        mutate(sele = paste0(chain_arg, " and (", sele, ")"))
      if (nrow(workdf > 0)) {
        color_list <- vector(mode = "list", length = nrow(workdf))
        for (i in 1:nrow(workdf)) {
          color_list[[i]] <- list(workdf$color[i], workdf$sele[i])
        }
      } else {
        color_list <- list(list("white", "*"))
      }
    }
    return(color_list)
  }

}

shinyApp(ui, server)

