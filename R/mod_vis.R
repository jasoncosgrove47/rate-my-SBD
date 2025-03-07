#' vis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_vis_ui <- function(id) {
  ns <- NS(id)

  # fluidPage(
  #
  #   textOutput(ns("textOutput"))
  # )

  fluidPage(
    fluidRow(
      layout_columns(
        fill = TRUE,
        value_box(
          title = "Best Squat",
          value = textOutput(ns("best_squat")),
          showcase = bsicons::bs_icon("trophy"),
          style = "background-color: #FC8D62; color: white;"  # Using theme instead of color
        ),
        value_box(
          title = "Best Bench",
          value = textOutput(ns("best_bench")),
          showcase = bsicons::bs_icon("trophy"),
          style = "background-color: #8DA0CB; color: white;"  # Using theme instead of color
        ),
        value_box(
          title = "Best Deadlift",
          value = textOutput(ns("best_deadlift")),
          showcase = bsicons::bs_icon("trophy"),
          style = "background-color: #66C2A5; color: white;"  # Using theme instead of color
        )
      )
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        full_screen = TRUE,
        card_header("Squat Progress",
                    style = "background-color: #FC8D62; color: white; font-weight: bold; padding: 10px;"),
        plotOutput(ns("squat_progress"))
      ),
      card(
        full_screen = TRUE,
        card_header("Bench Progress",
                    style = "background-color: #8DA0CB; color: white; font-weight: bold; padding: 10px;"),
        plotOutput(ns("bench_progress"))
      ),
      card(
        full_screen = TRUE,
        card_header("Deadlift Progress",
                    style = "background-color: #66C2A5; color: white; font-weight: bold; padding: 10px;"),
        plotOutput(ns("deadlift_progress"))
      )
    )
  )

}

#' metrics Server Functions
#'
#' @noRd
mod_vis_server <- function(id,lifts_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Ensure lifts_data is called within a reactive context
    data <- reactive({
      lifts_data()
    })

    # output$textOutput <- renderText({
    #
    #   data_val <- data()
    #
    #   if (is.null(data_val)) {
    #     return("No data available")
    #   } else if (data_val$selected_item == "No Lifter Selected") {
    #     return(paste("test manual input:",nrow(data_val$df)))
    #   } else {
    #     return(paste("How ya now. Selected item:", nrow(data_val$df)))
    #   }
    #
    # })


    # Best lift outputs
    output$best_squat <- renderText({
      data_val <- data()

      max_squat <- max(data_val$df$Best3SquatKg, na.rm = TRUE)
      if (is.finite(max_squat)) paste(max_squat, "kg") else "No data"
    })

    output$best_bench <- renderText({

      data_val <- data()

      max_bench <- max(data_val$df$Best3BenchKg, na.rm = TRUE)
      if (is.finite(max_bench)) paste(max_bench, "kg") else "No data"
    })

    output$best_deadlift <- renderText({

      data_val <- data()
      max_deadlift <- max(data_val$df$Best3DeadliftKg, na.rm = TRUE)
      if (is.finite(max_deadlift)) paste(max_deadlift, "kg") else "No data"
    })



    output$DOTS_progress <- renderPlot({

      data_val <- data()
      create_lift_plot(data_val$df, "Dots")
    })


    output$squat_progress <- renderPlot({

      data_val <- data()
      create_lift_plot(data_val$df, "Best3SquatKg")
    })

    output$bench_progress <- renderPlot({

      data_val <- data()
      create_lift_plot(data_val$df, "Best3BenchKg")
    })

    output$deadlift_progress <- renderPlot({

      data_val <- data()
      create_lift_plot(data_val$df, "Best3DeadliftKg")
    })

    output$bodyweight_progress <- renderPlot({

      data_val <- data()
      create_lift_plot(data_val$df, "BodyweightKg")
    })



  })
}

## To be copied in the UI
# mod_metrics_ui("metrics_1")

## To be copied in the server
# mod_metrics_server("metrics_1")
