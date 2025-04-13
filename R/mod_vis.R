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
mod_vis_server <- function(id,lifts_data,openpl,minima){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Ensure lifts_data is called within a reactive context
    data <- reactive({
      #lifts_data()

      data <- lifts_data()

      data$df <- data$df %>%
        filter(
          Equipment == data$equipped,
          Tested == data$tested
        )

      data  # return the full list with filtered df


    })

    output$textOutput <- renderText({"The following plots shows where your performance stands compared to lifters of the same sex, weight class, and age range (±5 years). This allows you to see how you compare to others—whether you're above average, near the middle of the pack, or aiming for further improvement."})


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

    output$squat_hist <- renderPlot({

      data_val <- data()

      create_progress_bar(data_val$df,'Best3SquatKg')

    })

    output$bench_hist <- renderPlot({

      data_val <- data()

      create_progress_bar(data_val$df,'Best3BenchKg')

    })

    output$deadlift_hist <- renderPlot({

      data_val <- data()

      create_progress_bar(data_val$df,'Best3DeadliftKg')

    })

    output$dots_hist <- renderPlot({

      data_val <- data()

      create_progress_bar(data_val$df,'Dots')

    })


    create_progress_bar <- function(lifter, column_name) {

      data_val <- data()


      # Ensure the column name exists in the data frame
      if (!column_name %in% colnames(openpl)) {
        stop("The specified column does not exist in the data frame.")
      }

      color_map <- c(
        "Best3SquatKg" = "#FC8D62",
        "Best3BenchKg" = "#8DA0CB",
        "Best3DeadliftKg" = "#66C2A5" ,
        "BodyweightKg" = "grey50" ,
        "Dots" = "purple",
        "TotalKg" = "black"
      )

      annot_map <- c(
        "Best3SquatKg" = "squat",
        "Best3BenchKg" = "bench",
        "Best3DeadliftKg" = "deadlift" ,
        "BodyweightKg" = "body weight" ,
        "Dots" = "DOTS score",
        "TotalKg" = "total"
      )


      age <- max(lifter$Age, na.rm = TRUE)
      sex <- lifter$Sex[1]
      age_range <- 5
      value <- max(lifter[[column_name]], na.rm = TRUE)
      weightclass <- names(which.max(table(lifter$WeightClassKg)))

      if(column_name == "Dots" | column_name == "TotalKg" | data_val$selected_item == "No Lifter Selected" ){
        df <- openpl%>%
          filter(year > 2010) %>%
          filter(age >= (age - age_range) & age <= (age + age_range)) %>%
          filter(WeightClassKg == weightclass) %>%
          filter(Sex == sex)
      }else{

        df <- openpl %>%
          filter(year > 2010) %>%
          filter(age >= (age - age_range) & age <= (age + age_range)) %>%
          filter(WeightClassKg == weightclass) %>%
          filter(Sex == sex)

      }

      # Don't allow negative values
      df <- df[df[[column_name]] > 0,]

      # Calculate the percentile
      percentile <- ecdf(df[[column_name]])(value) * 100

      # Ensure percentile is within bounds
      percentile <- max(0, min(percentile, 100))

      # Create a data frame for plotting
      data <- data.frame(x = c(0, percentile), y = 1)

      # Plot using ggplot2
      progress_bar <- ggplot() +
        # Background bar
        geom_rect(aes(xmin = 0, xmax = 100, ymin = 0.4, ymax = 1.6),
                  fill = "lightgray", color = "black", alpha = 0.5, linewidth = 1) +
        # Filled progress bar
        geom_rect(data = data, aes(xmin = 0, xmax = percentile, ymin = 0.4, ymax = 1.6),
                  fill = color_map[column_name], color = "black", alpha = 0.9, linewidth = 1) +
        # Add percentage text
        annotate("text", x = 50, y = 1, label = paste0(round(percentile, 2), "%"),
                 color = "white", size = 10, fontface = "bold") +
        # Add contextual information
        annotate("text", x = 50, y = 1.8, label = paste("Your",annot_map[column_name] ,"is better than that of \n", round(percentile, 2), "% of other lifters in your category."),
                 color = "black", size = 5, fontface = "bold") +
        # Formatting
        theme_void() +
        coord_cartesian(xlim = c(0, 100), ylim = c(0, 2)) +
        theme(plot.margin = margin(10, 10, 10, 10))

      return(progress_bar)
    }

  })
}

## To be copied in the UI
# mod_metrics_ui("metrics_1")

## To be copied in the server
# mod_metrics_server("metrics_1")
