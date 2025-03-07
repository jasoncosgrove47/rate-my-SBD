#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    page_navbar(
      title = "rmSBD",
      bg = "#24226A",
      underline = TRUE,


      nav_panel(title = "Home",
                mod_home_ui("home_1")),

      nav_panel(title = "Input your data",
              mod_data_ui("data_1")),

       nav_panel(title = "Visualise your data",
                 mod_vis_ui("vis_1")),

       nav_panel(title = "Comparisons against other lifters",
                 mod_compare_ui("compare_1")),
      #
      # nav_panel(title = "Summary and proposed actions",
      #           mod_cell_lines_ui("cell_lines_1"))



      )


    # fluidPage(
    #   golem::golem_welcome_page() # Remove this line to start building your UI
    # )
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
      app_title = "SBD"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
