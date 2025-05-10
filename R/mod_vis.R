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

  fluidPage(
    titlePanel("Summarise your best competitive lifts"),

    # First Row: Text Output
    fluidRow(
      column(12, textOutput(ns("textOutput")))
    ),
    br(),
    br(),
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
    # Second Row: Plot Outputs
    fluidRow(
      column(4, plotOutput(ns("squat_hist")), height = "400px"),
      column(4, plotOutput(ns("bench_hist")), height = "400px"),
      column(4, plotOutput(ns("deadlift_hist")), height = "400px")
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


    data_filtered <- reactiveVal()

    observeEvent(lifts_data(), {

      req(lifts_data())  # ensures lifts_data() is not NULL
      data <- lifts_data()

      # req(data$df, data$equipped, data$tested)
      # data$df <- data$df %>%
      #   filter(Equipment == data$equipped, Tested == data$tested)
      data_filtered(data)
    })



    output$textOutput <- renderText({"The following plots shows where your performance stands compared to lifters of the same sex and weight class. This allows you to see how you compare to others—whether you're above average, near the middle of the pack, or aiming for further improvement."})


    # Best lift outputs
    output$best_squat <- renderText({

      max_squat <- max(data_filtered()$df$Best3SquatKg, na.rm = TRUE)
      if (is.finite(max_squat)) paste(max_squat, "kg") else "No data"
    })

    output$best_bench <- renderText({

      max_bench <- max(data_filtered()$df$Best3BenchKg, na.rm = TRUE)
      if (is.finite(max_bench)) paste(max_bench, "kg") else "No data"
    })

    output$best_deadlift <- renderText({

      max_deadlift <- max(data_filtered()$df$Best3DeadliftKg, na.rm = TRUE)
      if (is.finite(max_deadlift)) paste(max_deadlift, "kg") else "No data"
    })


    output$squat_progress <- renderPlot({

      create_lift_plot(data_filtered()$df, "Best3SquatKg")
    })

    output$bench_progress <- renderPlot({

      create_lift_plot(data_filtered()$df, "Best3BenchKg")
    })

    output$deadlift_progress <- renderPlot({

      create_lift_plot(data_filtered()$df, "Best3DeadliftKg")
    })

    output$squat_hist <- renderPlot({

      create_progress_bar(data_filtered()$percentages,'Best3SquatKg')

    })

    output$bench_hist <- renderPlot({

      create_progress_bar(data_filtered()$percentages,'Best3BenchKg')

    })

    output$deadlift_hist <- renderPlot({

      create_progress_bar(data_filtered()$percentages,'Best3DeadliftKg')

    })



  })
}

## To be copied in the UI
# mod_metrics_ui("metrics_1")

## To be copied in the server
# mod_metrics_server("metrics_1")
