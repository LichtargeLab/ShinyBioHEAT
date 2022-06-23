#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage("E. coli driver gene prediction",
               theme = shinythemes::shinytheme("cerulean"),
               mod_intro_ui("intro"),
               tabPanel("EA Analysis",
                        tabsetPanel(
                          mod_data_input_ui("input_page"),
                          mod_random_mut_ui("random"),
                          mod_EA_analysis_ui("EA_analysis")
                        )),
               mod_EA_search_ui("search"),
               # structure_UI("structure"),
               shinyjs::useShinyjs(),
               shinyjs::extendShinyjs(text = "shinyjs.browseURL = function(url) {window.open(url,'_blank');}", functions = 'browseURL')
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ShinyBioHEAT"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
