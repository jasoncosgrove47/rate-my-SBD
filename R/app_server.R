#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {


    data <- init_data()

    lifts_data <- reactiveVal(list(selected_item = "No Lifter Selected",df = NULL))

    mod_home_server("home_1",openpl = data$openpl,minima = data$minima)

    mod_data_server("data_1",lifts_data,openpl = data$openpl, minima = data$minima,all_lifter_names = data$all_lifter_names)

    mod_vis_server("vis_1",lifts_data,openpl = data$openpl, minima = data$minima)

    mod_compare_server("compare_1",lifts_data,openpl = data$openpl, minima = data$minima)

    mod_attempts_server("attempts_1",lifts_data,openpl = data$openpl, minima = data$minima)

    mod_minima_server("minima_1",lifts_data,openpl = data$openpl, minima = data$minima)
}
