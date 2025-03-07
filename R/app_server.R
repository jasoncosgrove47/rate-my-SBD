#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {


    lifts_data <- reactiveVal(list(selected_item = "No Lifter Selected",df = NULL))
    #lifts_data <- reactive({NULL})


    mod_data_server("data_1",lifts_data)  # Ensure this matches mod_data_ui()

    mod_vis_server("vis_1",lifts_data)  # Ensure this matches mod_vis_ui()

    mod_compare_server("compare_1",lifts_data)  # Ensure this matches mod_vis_ui()
}
