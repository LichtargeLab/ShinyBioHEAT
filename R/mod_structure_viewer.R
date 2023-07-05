#' structure_viewer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_structure_viewer_ui <- function(id){
  ns <- NS(id)
  tabPanel("Structure Viewer",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               tags$h4("Input settings", style = "margin-top: 0;"),
               selectizeInput(
                 inputId = ns("mut_input"),
                 label = "Input mutation data",
                 choices = c("No" = "no",
                             "From EA Analysis" = "EA_analysis",
                             "From Quick EA Search" = "quick_search"),
                 selected = "no"
               ),
               radioButtons(
                 inputId = ns("input_type"),
                 label = "Select protein input type",
                 choices = c("Protein name" = "gene",
                             "Locus tag" = "locus_tag"),
                 selected = "gene",
                 inline = TRUE
               ),
               selectizeInput(ns("prot_id"),
                              label = "Select protein",
                              choices = NULL,
                              selected = NULL,
                              multiple = TRUE,
                              options = list(
                                placeholder = "Choose",
                                maxItems = 1
                              )),
               selectizeInput(
                 inputId = ns("ET_color"),
                 label = "ET color scheme",
                 choices = c("Rainbow" = "ET",
                             "Red-white-blue" = "red_white_blue",
                             "Red-yellow-green" = "red_yellow_green"),
                 selected = "ET"
               ),
               actionButton(
                 inputId = ns("load_structure"),
                 label = "Load structure",
                 class = "btn-primary",
                 width = "100%"
               ),
               tags$hr(style="border-color: black;"),
               tags$h4("Viewer settings", style = "margin-top: 0;"),
               selectizeInput(ns("rep_style"), "Select representation style",
                              choices = c("Cartoon" = "cartoon",
                                          "Spheres" = "spheres",
                                          "Ball+Stick" = "ballnstick"),
                              selected = "cartoon"),
               actionButton(inputId = ns("show_legend"),
                            label = "Show color legends",
                            class = "btn-info",
                            width = "100%"),
               checkboxInput(ns("label_mut"),
                             label = "Label mutated residues",
                             value = FALSE),
               checkboxInput(ns("label_front"),
                             label = "Label always in front",
                             value = FALSE),
               tags$hr(style="border-color: black;"),
               tags$h4("Download", style = "margin-top: 0;"),
               splitLayout(cellWidths = c("50%", "50%"),
                           downloadButton(
                             outputId = ns("download_color_table"),
                             label = "Color table"
                           ),
                           downloadButton(
                             outputId = ns("download_pse"),
                             label = "Pymol session"
                           )
               )
             ),
             mainPanel(
               width = 9,
               shinyjs::hidden(splitLayout(id = ns("name1"),
                                           cellWidths = c("50%", "50%"),
                                           h4("ET", align = "center"),
                                           h4("AlphaFold pLDDT", align = "center"))),
               r3dmol::r3dmolOutput(outputId = ns("structure"), height = "500px"),
               shinyjs::hidden(splitLayout(id = ns("name2"),
                                           cellWidths = c("50%", "50%"),
                                           h4("sumEA", align = "center"),
                                           h4("number of unique mutations", align = "center")))
             )
           )
  )
}

#' structure_viewer Server Functions
#'
#' @noRd
mod_structure_viewer_server <- function(id, processed_evolve, quick_search_output,
                                        structure_df){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # This updates the prot_id SelectizeInput with gene or locus tag
    observeEvent(c(input$input_type, input$mut_input), {
      gene_choices <- NULL
      locus_tag_choices <- NULL
      if (input$mut_input == "quick_search") {
        gene_choices <- dplyr::intersect(structure_df$gene, quick_search_output()$gene)
        locus_tag_choices <- dplyr::intersect(structure_df$locus_tag, quick_search_output()$locus_tag)
      } else if (input$mut_input == "EA_analysis") {
        gene_choices <- dplyr::intersect(structure_df$gene, processed_evolve()$gene)
        locus_tag_choices <- dplyr::intersect(structure_df$locus_tag, processed_evolve()$locus_tag)
      } else {
        gene_choices <- structure_df$gene
        locus_tag_choices <- structure_df$locus_tag
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

    structure_para_list <- eventReactive(input$load_structure, {
      filter_var <- isolate(input$input_type)
      id <- isolate(input$prot_id)
      AF_ET_sel <- structure_df[structure_df[[filter_var]] == id,]
      AF_url <- AF_ET_sel$AF_url[1]
      if (is.na(AF_url)) {
        showModal(modalDialog(
          title = "Check input protein/locus tag",
          div(tags$p("Selected protein/locus tag is not included in the AlphaFold database."),
              tags$p("Currently structure viewer only support AlphaFold structure with 100%
                      match to the reference sequence."))
        ))
        para <- NA
        para
      } else {
        AF_pdb <- readLines(AF_url, warn = FALSE)
        AF_pLDDT <- GetpLDDT(AF_pdb) %>%
          dplyr::select(AA.POS = POS, pLDDT)
        ET_df <- data.frame(ET = AF_ET_sel$ET_vec[[1]]) %>%
          dplyr::mutate(AA.POS = 1:dplyr::n())
        mut_data <- switch(isolate(input$mut_input),
                           "no" = dplyr::tibble(gene = character(), locus_tag = character(),
                                                EA = numeric(), SUB = character()),
                           "EA_analysis" = processed_evolve(),
                           "quick_search" = quick_search_output())
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
          dplyr::left_join(ET_df, ., by = c("AA.POS")) %>%
          dplyr::left_join(AF_pLDDT, by = c("AA.POS")) %>%
          tidyr::replace_na(list(mutation_count = 0,
                                 unique_mutation_count = 0,
                                 sumEA = 0)) %>%
          ColorMutations(., ET_color = input$ET_color) %>%
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
      }

    })

    observeEvent(input$load_structure, {
      shinyjs::show("name1")
      if (input$mut_input == "no") {
        shinyjs::hide("name2")
      } else {
        shinyjs::show("name2")
      }
    })

    observeEvent(input$show_legend, {
      showModal(modalDialog(
        title = "Color legends",
        div(
          tags$h4("ET color (Rainbow)"),
          tags$img(src = "www/legend_ET.png", width = "100%"),
          tags$br(),
          tags$br(),
          tags$h4("ET color (Red-white-blue)"),
          tags$img(src = "www/legend_ET_rwb.png", width = "100%"),
          tags$br(),
          tags$br(),
          tags$h4("ET color (Red-yellow-green)"),
          tags$img(src = "www/legend_ET_ryg.png", width = "100%"),
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
      req(structure_para_list())
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

      if (isolate(input$mut_input) == "no") {
        grid_list <- list(m1, m2)
      } else {
        grid_list <- list(m1, m2, m3, m4)
      }


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
        pdb_id <- stringr::str_split(AF_url, "files/|.pdb", simplify = TRUE)[,2]
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
  })
}

## To be copied in the UI
# mod_structure_viewer_ui("structure_viewer_1")

## To be copied in the server
# mod_structure_viewer_server("structure_viewer_1")
