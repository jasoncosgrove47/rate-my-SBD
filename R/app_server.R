#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {


    lifts_data <- reactiveVal(list(selected_item = "No Lifter Selected",df = NULL))

    mod_data_server("data_1",lifts_data)

    mod_vis_server("vis_1",lifts_data)

    mod_compare_server("compare_1",lifts_data)

    mod_attempts_server("attempts_1",lifts_data)

    mod_minima_server("minima_1",lifts_data)
}
