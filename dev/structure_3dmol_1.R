library(tidyverse)
library(shiny)
library(r3dmol)

AF_ET_df <- readRDS(app_sys("app/www/AF_ET_df.rds"))
processed_evolve <- readRDS(app_sys("app/www/input_example.rds"))
quick_search_output <- read_csv("~/Work/Colistin/20210304-EAapp_v2/test/quick_EA_search.csv")
add_resource_path(
  "www",
  app_sys("app/www")
)

# SelectColor <- function(color_type = c("ET", "red_white", "red_white_blue", "white_red", "alphafold")) {
#   if (color_type == "ET") {
#     colorRange <- c("ff0000", "ff0c00", "ff1800", "ff2400", "ff3000", "ff3d00", "ff4900", "ff5500", "ff6100", "ff6e00",
#                     "ff7a00", "ff8600", "ff9200", "ff9f00", "ffab00", "ffb700", "ffc300", "ffd000", "ffdc00", "ffe800",
#                     "fff400", "fcff00", "f0ff00", "e4ff00", "d8ff00", "cbff00", "bfff00", "b3ff00", "a7ff00", "9bff00",
#                     "8eff00", "82ff00", "76ff00", "6aff00", "5dff00", "51ff00", "45ff00", "39ff00", "2cff00", "20ff00",
#                     "14ff00", "08ff00", "00ff04", "00ff10", "00ff1c", "00ff28", "00ff35", "00ff41", "00ff4d", "00ff59",
#                     "00ff66", "00ff72", "00ff7e", "00ff8a", "00ff96", "00ffa3", "00ffaf", "00ffbb", "00ffc7", "00ffd4",
#                     "00ffe0", "00ffec", "00fff8", "00f8ff", "00ecff", "00e0ff", "00d4ff", "00c7ff", "00bbff", "00afff",
#                     "00a3ff", "0096ff", "008aff", "007eff", "0072ff", "0066ff", "0059ff", "004dff", "0041ff", "0035ff",
#                     "0028ff", "001cff", "0010ff", "0004ff", "0800ff", "1400ff", "2000ff", "2c00ff", "3900ff", "4500ff",
#                     "5100ff", "5d00ff", "6a00ff", "7600ff", "8200ff", "8e00ff", "9b00ff", "a700ff", "b300ff", "bf00ff") %>%
#       paste0("#", .)
#   } else if (color_type == "alphafold") {
#     colorRange <- c(rep("#f17c42", 50),
#                     rep("#fcdb4b", 20),
#                     rep("#65cbf3", 20),
#                     rep("#0153d6", 10))
#   } else if (color_type == "red_white") {
#     colorRange <- colorRampPalette(c("red", "white"))(100)
#   } else if (color_type == "red_white_blue") {
#     colorRange <- colorRampPalette(c("red", "white", "blue"))(100)
#   } else {
#     colorRange <- colorRampPalette(c("white", "red"))(100)
#   }
#   return(colorRange)
# }
#
# GetColor <- function(x, lower_bound = min(x), upper_bound = max(x),
#                      color_type = c("ET", "red_white", "red_white_blue", "white_red", "alphafold")) {
#   scaled_x <- (x - lower_bound)/(upper_bound - lower_bound) * 99
#   colorRange <- SelectColor(color_type = color_type)
#   output <- colorRange[ceiling(scaled_x)+1]
#   return(output)
# }
#
# ColorMutations <- function(mutation_df) {
#   workdf <- mutation_df %>%
#     mutate(mutation_count_color = GetColor(mutation_count, lower_bound = 0,
#                                            color_type = "white_red"),
#            unique_mutation_count_color = GetColor(unique_mutation_count, lower_bound = 0,
#                                                   color_type = "white_red"),
#            sumEA_color = GetColor(sumEA, lower_bound = 0,
#                                   color_type = "white_red"),
#            ET_color = GetColor(ET, lower_bound = 0, upper_bound = 100, color = "ET")
#     )
#   return(workdf)
# }

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
                 class = "btn-primary",
                 width = "100%"
               ),
               tags$hr(style="border-color: black;"),
               shinyjs::hidden(div(id ="viewer_settings",
                                   tags$h4("Viewer settings", style = "margin-top: 0;"),
                                   selectizeInput("rep_style", "Select representation style",
                                                  choices = c("Cartoon" = "cartoon",
                                                              "Spheres" = "spheres",
                                                              "Ball+Stick" = "ballnstick"),
                                                  selected = "cartoon"),
                                   checkboxGroupInput("color_style",
                                                      label = HTML("Choose ",
                                                                   as.character(actionLink(inputId = "show_legend", label = "coloring styles")),
                                                                   ":"),
                                                      choiceNames =
                                                        list("ET coverage",
                                                             "AlphaFold pLDDT"),
                                                      choiceValues =
                                                        list("ET", "AF"),
                                                      selected = list("ET", "AF")
                                   ),
                                   checkboxInput(("label_mut"),
                                                 label = "Label mutated residues",
                                                 value = FALSE),
                                   checkboxInput(("label_front"),
                                                 label = "Label always in front",
                                                 value = FALSE),
                                   tags$hr(style="border-color: black;"),
                                   tags$h4("Download", style = "margin-top: 0;"),
                                   splitLayout(cellWidths = c("50%", "50%"),
                                               downloadButton(
                                                 outputId = "download_color_table",
                                                 label = "Color table"
                                               ),
                                               downloadButton(
                                                 outputId = "download_pse",
                                                 label = "Pymol session"
                                               )
                                   ),
               )),

             ),
             mainPanel(
               width = 9,
               r3dmol::r3dmolOutput(outputId = "structure", height = "600px"),
               shinyjs::useShinyjs()
             )
           ))
)

server <- function(input, output, session) {
  # This updates the prot_id SelectizeInput with gene or locus tag
  observeEvent(c(input$input_type, input$mut_input), {
    gene_choices <- NULL
    locus_tag_choices <- NULL

    if (input$mut_input == "quick_search") {
      gene_choices <- dplyr::intersect(AF_ET_df$gene, quick_search_output$gene)
      locus_tag_choices <- dplyr::intersect(AF_ET_df$locus_tag, quick_search_output$locus_tag)
    } else if (input$mut_input == "EA_analysis") {
      gene_choices <- dplyr::intersect(AF_ET_df$gene, processed_evolve$gene)
      locus_tag_choices <- dplyr::intersect(AF_ET_df$locus_tag, processed_evolve$locus_tag)
    } else {
      gene_choices <- AF_ET_df$gene
      locus_tag_choices <- AF_ET_df$locus_tag
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

  observeEvent(input$mut_input, {
    if (input$mut_input == "no") {
      updateCheckboxGroupInput(inputId = "color_style",

                               choiceNames =
                                 list("ET coverage",
                                      "AlphaFold pLDDT"),
                               choiceValues =
                                 list("ET", "AF"),
                               selected = list("ET", "AF")
      )
    } else {
      updateCheckboxGroupInput(inputId = "color_style",

                               choiceNames =
                                 list("ET coverage", "AlphaFold pLDDT",
                                      "sumEA for all mutations",
                                      "number of unique mutations"),
                               choiceValues =
                                 list("ET", "AF", "sumEA", "vari"),
                               selected = list("ET", "AF", "sumEA", "vari")
      )
    }
  })

  structure_para_list <- eventReactive(input$load_structure, {
    filter_var <- isolate(input$input_type)
    id <- isolate(input$prot_id)
    AF_ET_sel <- AF_ET_df[AF_ET_df[[filter_var]] == id,]
    AF_url <- AF_ET_sel$AF_url[1]
    AF_pdb <- readLines(AF_url, warn = FALSE)
    AF_pLDDT <- GetpLDDT(AF_pdb) %>%
      dplyr::select(AA.POS = POS, pLDDT)
    mut_data <- switch(isolate(input$mut_input),
                       "no" = tibble(gene = character(), locus_tag = character(),
                                     EA = numeric(), SUB = character()),
                       "EA_analysis" = processed_evolve,
                       "quick_search" = quick_search_output)
    color_df <- mut_data[mut_data[[filter_var]] == id,] %>%
      dplyr::mutate(AA.POS = stringr::str_sub(SUB, start = 2, end = -2)) %>%
      dplyr::mutate(AA.POS = as.numeric(AA.POS)) %>%
      dplyr::mutate(EA = as.numeric(EA)) %>%
      dplyr::filter(!is.na(EA)) %>%
      dplyr::group_by(locus_tag, AA.POS) %>%
      dplyr::summarize(mutation_count = dplyr::n(),
                       unique_mutation_count = length(unique(SUB)),
                       sumEA = sum(EA), .groups = "drop") %>%
      dplyr::ungroup() %>%
      dplyr::left_join(AF_ET_sel$data[[1]], ., by = c("AA.POS")) %>%
      dplyr::left_join(AF_pLDDT, by = c("AA.POS")) %>%
      tidyr::replace_na(list(mutation_count = 0,
                             unique_mutation_count = 0,
                             sumEA = 0)) %>%
      ColorMutations() %>%
      dplyr::select(-locus_tag)
    ET_color <- paste0('"', color_df$ET_color, '"', collapse = ",")
    sumEA_color <- paste0('"', color_df$sumEA_color, '"', collapse = ",")
    unique_mutation_count_color <- paste0('"', color_df$unique_mutation_count_color, '"', collapse = ",")
    mut_resi <- color_df$AA.POS[which(color_df$unique_mutation_count > 0)]
    para <- list(AF_url, AF_pdb, color_df, ET_color, sumEA_color, unique_mutation_count_color,
                 mut_resi)
    names(para) <- c("AF_url", "AF_pdb", "color_df", "ET_color", "sumEA_color", "unique_mutation_count_color",
                     "mut_resi")
    para
  })

  observeEvent(input$load_structure, {
    shinyjs::show("viewer_settings")
  })

  observeEvent(input$show_legend, {
    showModal(modalDialog(
      title = "Color legends",
      div(
        tags$h4("ET color"),
        tags$img(src = "www/legend_ET.png", width = "100%"),
        tags$br(),
        tags$br(),
        tags$h4("AlphaFold pLDDT color"),
        tags$img(src = "www/legend_pLDDT.png", width = "100%"),
        tags$br(),
        tags$br(),
        tags$h4("sumEA/number of unique mutations color"),
        tags$img(src = "www/legend_other.png", width = "100%"),
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  output$structure <- r3dmol::renderR3dmol({
    style_func <- switch(input$rep_style,
                         "cartoon" = r3dmol::m_style_cartoon,
                         "spheres" = r3dmol::m_style_sphere,
                         "ballnstick" = r3dmol::m_style_ballnstick)

    AF_structure <- r3dmol::r3dmol() %>%
      r3dmol::m_add_model(data = structure_para_list()[["AF_pdb"]], format = "pdb") %>%
      r3dmol::m_zoom_to()
    m1 <- AF_structure %>%
      r3dmol::m_set_style(style = style_func(
        colorfunc = paste0("
        function(atom) {
          const color = [", structure_para_list()[["ET_color"]], "];
          return color[atom.resi-1];
        }"
        ))) # js indexing from 0

    m2 <- AF_structure %>%
      r3dmol::m_set_style(style = style_func(colorfunc = "
        function(atom) {
          if (atom.b >= 90) {return '#0153d6'};
          if (atom.b < 90 && atom.b >= 70) {return '#65cbf3'};
          if (atom.b < 70 && atom.b >= 50) {return '#fcdb4b'};
          if (atom.b < 50) {return '#f17c42'};
          return 'white';
        }"))

    m3 <- AF_structure %>%
      r3dmol::m_set_style(style = style_func(
        colorfunc = paste0("
        function(atom) {
          const color = [", structure_para_list()[["sumEA_color"]], "];
          return color[atom.resi-1];
        }"
        )))

    m4 <- AF_structure %>%
      r3dmol::m_set_style(style = style_func(
        colorfunc = paste0("
        function(atom) {
          const color = [", structure_para_list()[["unique_mutation_count_color"]], "];
          return color[atom.resi-1];
        }"
        )))

    if(input$label_mut == TRUE & isolate(input$mut_input) != "no") {
      add_mut_lables <- function(r3dmol_object) {
        output <- r3dmol_object %>%
          r3dmol::m_add_res_labels(
            sel = r3dmol::m_sel(resi =  structure_para_list()[["mut_resi"]],
                                atom = "CA"),
            style = r3dmol::m_style_label(
              font = "Arial",
              fontSize = 12,
              fontColor = "white",
              inFront = input$label_front,
              backgroundColor = "black",
              showBackground = TRUE
            )
          )
        return(output)
      }

      m1 <- add_mut_lables(m1)
      m2 <- add_mut_lables(m2)
      m3 <- add_mut_lables(m3)
      m4 <- add_mut_lables(m4)
    }

    grid_list <- list(m1, m2, m3, m4)
    grid_list <- grid_list[c("ET", "AF", "sumEA", "vari") %in% input$color_style]
    grid <- r3dmol::m_grid(
      viewer = grid_list,
      control_all = TRUE,
      cols = 2,
      viewer_config = r3dmol::m_viewer_spec(
        backgroundColor = "white"
      )
    )
    grid$elementId <- NULL # suppress the `Shiny doesn't use them` warning
    grid
  })

  output$download_color_table <- downloadHandler(
    filename = function() {
      paste0(isolate(input$prot_id), "_color_table", ".xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(structure_para_list()[["color_df"]], file)
    }
  )

  output$download_pse <- downloadHandler(
    filename = function() {
      paste0(isolate(input$prot_id), ".pml")
    },
    content = function(file) {
      AF_url <- structure_para_list()[["AF_url"]]
      pymol_color_df <- structure_para_list()[["color_df"]]%>%
        dplyr::mutate(dplyr::across(dplyr::ends_with("color"),
                                    ~stringr::str_replace(., "#", "0x")))
      pdb_id <- str_split(AF_url, "files/|.pdb", simplify = TRUE)[,2]
      pymol_cmd <- c(paste0("load ", AF_url),
                     paste0("set_name ", pdb_id,", ET"),
                     "copy pLDDT, ET",
                     "copy sumEA, ET",
                     "copy unique_mut, ET",
                     "color white",
                     PymolColorChainByResidue(chain = "A", position = pymol_color_df$AA.POS,
                                              color = pymol_color_df$ET_color, object = "ET"),
                     PymolColorChainByResidue(chain = "A", position = pymol_color_df$AA.POS,
                                              color = pymol_color_df$pLDDT_color, object = "pLDDT"),
                     PymolColorChainByResidue(chain = "A", position = pymol_color_df$AA.POS,
                                              color = pymol_color_df$sumEA_color, object = "sumEA"),
                     PymolColorChainByResidue(chain = "A", position = pymol_color_df$AA.POS,
                                              color = pymol_color_df$unique_mutation_count_color, object = "unique_mut"),
                     "set grid_mode, 1"
      )
      if (input$mut_input != "no") {
        pymol_mut_resi <- paste0(structure_para_list()[["mut_resi"]], collapse = "+")
        pymol_cmd <- c(pymol_cmd,
                       paste0("select mut_resi, chain A and resi ", pymol_mut_resi),
                       "deselect")
      }
      writeLines(pymol_cmd, con = file)
    }
  )
}

shinyApp(ui, server)
