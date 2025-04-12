#' minima UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import tidyr
#' @import plotly
#'
#' @importFrom shiny NS tagList
mod_minima_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Include custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

    fluidPage(
      # Title
      # titlePanel("Select the category that you would like to compete in:"),

      fluidRow(
        column(4, selectizeInput(ns("weight_class"), "Select Weight Class:", choices = NULL)),
        column(4, selectizeInput(ns("category"), "Select Category:", choices = NULL)),
        column(4, selectizeInput(ns("sex"), "Select Sex:", choices = NULL))
      ),

      br(), br(),  # Adding space between the input row and the plot

      fluidRow(
        column(10, offset = 1, plotlyOutput(ns("gauge_chart")))
      ),

      br(), br(),  # Adding space between the plot and the context text

      fluidRow(
        column(10, offset = 1, textOutput(ns("context_text")))
      )
    )
  )
}

#' minima Server Functions
#'
#' @noRd
mod_minima_server <- function(id,lifts_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    output$context_text <- renderText({
      "This plot shows your competitive level based on \n standards set by the French powerlifting association."
    })

    # Ensure lifts_data is called within a reactive context
    data <- reactive({
      lifts_data()
    })

    updateSelectizeInput(session, "weight_class", choices = unique(minima$WeightClass),server = TRUE)
    updateSelectizeInput(session, "category", choices = unique(minima$Category),server = TRUE)
    updateSelectizeInput(session, "sex", choices = unique(minima$Sex),server = TRUE)

    # Reactive expression to filter minima based on user input
    filtered_minima <- reactive({
      req(input$weight_class, input$category, input$sex)  # Ensure inputs are available
      minima %>%
        filter(
          WeightClass == input$weight_class,
          Category == input$category,
          Sex == input$sex
        )
    })

    # Display the selected values
    output$selected_weight_class <- renderText({
      paste("Selected Weight Class:", input$weight_class)
    })

    output$selected_category <- renderText({
      paste("Selected Category:", input$category)
    })

    output$selected_sex <- renderText({
      paste("Selected Sex:", input$sex)
    })

    output$textOutput <- renderText({"The following plots assess how your performances change depending on your lifting experience and your body weight and we then use this information to make comparisons to other lifters.
      This allows you to see how you compare to others—whether you're above average,
      near the middle of the pack, or aiming for further improvement.
      Please note that this plot will only appear if your attempt selection information
      is available on open powerlifting. This functionality is not available for users
      inputting their training data manually."})

    output$filtered_table <- renderTable({
      filtered_minima()
    })


    output$best_total <- renderText({

      data_val <- data()
      max_tot <- max(data_val$df$TotalKg, na.rm = TRUE)
      if (is.finite(max_tot)) paste(max_tot, "Kg") else "No data"
    })

    output$progress_bar <- renderPlot({
      data_val <- data()
      max_tot <- max(data_val$df$TotalKg, na.rm = TRUE)
      if (is.finite(max_tot)) {
        create_progress_bar(max_tot, input$weight_class, input$category, input$sex, minima)
      }
    })

    # Create and display the gauge chart
    output$gauge_chart <- renderPlotly({
      data_val <- data()
      max_tot <- max(data_val$df$TotalKg, na.rm = TRUE)
      if (is.finite(max_tot)) {
        create_gauge_chart(max_tot, input$weight_class, input$category, input$sex, minima)
      }
    })


    create_gauge_chart <- function(best_total, weight_class, category, sex, minima) {
      value <- best_total

      # Filter minima based on user inputs
      filtered_minima <- minima %>%
        filter(WeightClass == weight_class, Category == category, Sex == sex)

      if (nrow(filtered_minima) == 0) {
        stop("No matching data found in the minima data frame.")
      }

      # Extract the levels
      levels <- c("R3", "R2", "R1", "N2", "N1", "Europe", "Monde")

      # Calculate the progress as a percentage of the Monde level
      monde_value <- filtered_minima$Monde
      progress <- (value / monde_value) * 100

      # Ensure progress is within bounds
      progress <- max(0, min(progress, 100))

      # Calculate the percentage positions for each level
      level_percentages <- sapply(filtered_minima[levels], function(x) (x / monde_value) * 100)

      # Determine the lifter's level
      lifter_level <- ""
      for (i in seq_along(levels)) {
        if (progress <= level_percentages[i]) {
          lifter_level <- levels[i]
          break
        }
      }
      if (lifter_level == "") {
        lifter_level <- "Monde"
      }

      # Create the gauge chart using plotly
      gauge <- plot_ly(
        type = "indicator",
        mode = "gauge+number+delta",
        title = list(text = paste("Your competitive level is:", lifter_level)),
        value = value,  # Display the absolute value
        number = list(suffix = " Kg"),  # Add the unit for the absolute value
        gauge = list(
          axis = list(range = list(0, monde_value)),
          steps = list(
            list(range = c(0, level_percentages[1] * monde_value / 100), color = "#FFFFFF", name = "R3"),
            list(range = c(level_percentages[1] * monde_value / 100, level_percentages[2] * monde_value / 100), color = "#E5E5E5", name = "R2"),
            list(range = c(level_percentages[2] * monde_value / 100, level_percentages[3] * monde_value / 100), color = "#B3B3B3", name = "R1"),
            list(range = c(level_percentages[3] * monde_value / 100, level_percentages[4] * monde_value / 100), color = "#7F7F7F", name = "N2"),
            list(range = c(level_percentages[4] * monde_value / 100, level_percentages[5] * monde_value / 100), color = "#4C4C4C", name = "N1"),
            list(range = c(level_percentages[5] * monde_value / 100, level_percentages[6] * monde_value / 100), color = "#333333", name = "Europe"),
            list(range = c(level_percentages[6] * monde_value / 100, monde_value), color = "darkred", name = "Monde")
          ),
          threshold = list(
            line = list(color = "darkgreen", width = 4),
            thickness = 0.75,
            value = value  # Use the absolute value for the threshold
          )
        )
      )

      # Add annotations for the legend, lifter level, and required value for international level
      annotations <- list(
        list(
          x = 0.1, y = -0.1, text = "Regional 3", showarrow = FALSE, bgcolor = "#FFFFFF", bordercolor = "black"
        ),
        list(
          x = 0.2, y = -0.1, text = "Regional 2", showarrow = FALSE, bgcolor = "#E5E5E5", bordercolor = "black"
        ),
        list(
          x = 0.3, y = -0.1, text = "Regional 1", showarrow = FALSE, bgcolor = "#B3B3B3", bordercolor = "black"
        ),
        list(
          x = 0.5, y = -0.1, text = "National 2", showarrow = FALSE, bgcolor = "#7F7F7F", bordercolor = "black"
        ),
        list(
          x = 0.6, y = -0.1, text = "National 1", showarrow = FALSE, bgcolor = "#4C4C4C", bordercolor = "black", font = list(color = "white")
        ),
        list(
          x = 0.75, y = -0.1, text = "European", showarrow = FALSE, bgcolor = "#333333", bordercolor = "black", font = list(color = "white")
        ),
        list(
          x = 0.9, y = -0.1, text = "International", showarrow = FALSE, bgcolor = "darkred", bordercolor = "black", font = list(color = "white")
        ),
        list(
          x = 0.5, y = 0.0, text = paste("Total Required to be Competitive Internationally:", monde_value, "Kg"), showarrow = FALSE, font = list(size = 12, color = "black")
        )
      )

      gauge <- gauge %>% layout(
        annotations = annotations,
        margin = list(t = 100)  # Adjust top margin as needed
      )

      return(gauge)
    }


    create_progress_bar <- function(best_total, weight_class, category, sex, minima) {
      color_map <- c(
        "TotalKg" = "grey40"
      )

      annot_map <- c(
        "TotalKg" = "total"
      )

      value <- best_total

      # Filter minima based on user inputs
      filtered_minima <- minima %>%
        filter(WeightClass == weight_class, Category == category, Sex == sex)

      if (nrow(filtered_minima) == 0) {
        stop("No matching data found in the minima data frame.")
      }

      # Extract the levels
      levels <- c("R3", "R2", "R1", "N2", "N1", "Europe", "Monde")

      # Calculate the progress as a percentage of the Monde level
      monde_value <- filtered_minima$Monde
      progress <- (value / monde_value) * 100

      # Ensure progress is within bounds
      progress <- max(0, min(progress, 100))

      # Calculate the percentage positions for each level
      level_percentages <- sapply(filtered_minima[levels], function(x) (x / monde_value) * 100)

      # Create a data frame for plotting
      data <- data.frame(x = c(0, progress), y = 1)

      # Plot using ggplot2
      progress_bar <- ggplot() +
        # Background bar
        geom_rect(aes(xmin = 0, xmax = 100, ymin = 0.4, ymax = 1.6),
                  fill = "lightgray", color = "black", alpha = 0.5, linewidth = 1) +
        # Filled progress bar
        geom_rect(data = data, aes(xmin = 0, xmax = progress, ymin = 0.4, ymax = 1.6),
                  fill = color_map["TotalKg"], color = "black", alpha = 0.9, linewidth = 1) +

        # Add checkpoints for each level
        geom_vline(xintercept = level_percentages, color = "red", linetype = "dashed") +
        annotate("text", x = level_percentages, y = 0.2, label = levels, color = "red", size = 4, angle = 90, vjust = 1) +
        # Formatting
        theme_void() +
        coord_cartesian(xlim = c(0, 100), ylim = c(0, 2)) +
        theme(plot.margin = margin(10, 10, 10, 10))

      return(progress_bar)
    }

   })
}

