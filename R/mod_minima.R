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
mod_minima_server <- function(id,lifts_data,openpl,minima){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    output$context_text <- renderText({
      "This plot shows your competitive level based on \n standards set by the French powerlifting association."
    })

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
        create_gauge_chart7(max_tot, input$weight_class, input$category, input$sex, minima)
      }
    })


    create_gauge_chart7 <- function(best_total, weight_class, category, sex, minima) {
      value <- best_total

      # Filter for user inputs
      filtered_minima <- minima %>%
        filter(WeightClass == weight_class, Category == category, Sex == sex)

      if (nrow(filtered_minima) == 0) {
        stop("No matching data found in the minima data frame.")
      }

      # Define levels and styles
      levels <- c("R3", "R2", "R1", "N2", "N1", "Europe", "Monde")
      level_labels <- c("Regional 3", "Regional 2", "Regional 1", "National 2", "National 1", "European", "International")
      level_colors <- c("#333333", "#444444", "#555555", "#666666", "#888888", "#ff7f0e", "#ff4500")

      monde_value <- filtered_minima$Monde
      progress <- (value / monde_value) * 100
      progress <- max(0, min(progress, 100))  # clamp

      level_percentages <- sapply(filtered_minima[levels], function(x) (x / monde_value) * 100)

      # Determine lifter level
      lifter_level <- ""
      for (i in seq_along(levels)) {
        if (progress <= level_percentages[i]) {
          lifter_level <- levels[i]
          break
        }
      }
      if (lifter_level == "") lifter_level <- "Monde"

      next_level_index <- which(levels == lifter_level) + 1
      next_level_value <- if (next_level_index <= length(levels)) filtered_minima[[levels[next_level_index]]] else NA
      kg_to_next <- if (!is.na(next_level_value)) next_level_value - value else 0

      # Build gauge steps
      steps_list <- list()
      for (i in seq_along(level_percentages)) {
        from <- if (i == 1) 0 else level_percentages[i - 1] * monde_value / 100
        to <- level_percentages[i] * monde_value / 100
        steps_list[[i]] <- list(range = c(from, to), color = level_colors[i])
      }

      # Create annotations, two columns for better readability
      label_annotations_left <- lapply(seq(1, length(levels) / 2), function(i) {
        list(
          x = 0.35,  # Left side column
          y = -0.3 - 0.2 * (i - 1),  # Increased space between annotations
          xref = "paper", yref = "paper",
          text = paste0("<b>", level_labels[i], "</b>"),
          showarrow = FALSE,
          font = list(color = "white", size = 10),  # Increased font size
          bgcolor = level_colors[i],
          bordercolor = "white",
          borderwidth = 1,
          borderpad = 4,
          align = "center"
        )
      })

      label_annotations_right <- lapply(seq((length(levels) / 2) + 1, length(levels)), function(i) {
        list(
          x = 0.65,  # Right side column
          y = -0.3 - 0.2 * (i - length(levels) / 2 - 1),  # Increased space between annotations
          xref = "paper", yref = "paper",
          text = paste0("<b>", level_labels[i], "</b>"),
          showarrow = FALSE,
          font = list(color = "white", size = 10),  # Increased font size
          bgcolor = level_colors[i],
          bordercolor = "white",
          borderwidth = 1,
          borderpad = 4,
          align = "center"
        )
      })

      # Bottom annotation for kg to next level (move it beside title)
      next_level_text <- if (!is.na(kg_to_next) && kg_to_next > 0) {
        paste0("<b>", kg_to_next, " kg</b> to reach next level")
      } else if (is.na(kg_to_next) || kg_to_next <= 0) {
        "<b>Max Level Achieved!</b>"
      }

      # Create the title and place next to it
      # callout_annotation <- list(
      #   x = 0.5, y = 1.1,  # Place next to title
      #   text = next_level_text,
      #   showarrow = FALSE,
      #   font = list(color = "#ff7f0e", size = 16),  # Font size for better readability
      #   align = "center"
      # )

      # Plotly gauge
      gauge <- plot_ly(
        type = "indicator",
        mode = "gauge+number+delta",
        value = value,
        title = list(
          text = paste0("Current Level: <b style='color:orange;'>", lifter_level, "</b><br><span style='font-size:14px;'>", next_level_text, "</span>"),
          font = list(color = "white", size = 18)
        ),
        #title = list(text = paste0("Current Level: <b style='color:orange;'>", lifter_level," - ",next_level_text, "</b>")),
        number = list(suffix = " kg", font = list(color = "white")),
        delta = list(reference = next_level_value, increasing = list(color = "orange")),
        gauge = list(
          axis = list(range = list(0, monde_value), tickwidth = 1, tickcolor = "orange"),
          bar = list(color = "orange"),
          bgcolor = "black",
          borderwidth = 2,
          bordercolor = "white",
          steps = steps_list,
          threshold = list(
            line = list(color = "white", width = 4),
            thickness = 0.75,
            value = value
          )
        )
      ) %>% layout(
        paper_bgcolor = "black",
        font = list(color = "white"),
        annotations = c(label_annotations_left, label_annotations_right),
        margin = list(t = 80, b = 240, l = 40, r = 40),  # Increased bottom margin for more space
        height = 600,  # Set the height of the plot to make it bigger
        autosize = TRUE  # Make the plot responsive
      )

      return(gauge)
    }







    create_gauge_chart6 <- function(best_total, weight_class, category, sex, minima) {
      value <- best_total

      # Filter for user inputs
      filtered_minima <- minima %>%
        filter(WeightClass == weight_class, Category == category, Sex == sex)

      if (nrow(filtered_minima) == 0) {
        stop("No matching data found in the minima data frame.")
      }

      # Define levels and styles
      levels <- c("R3", "R2", "R1", "N2", "N1", "Europe", "Monde")
      level_labels <- c("Regional 3", "Regional 2", "Regional 1", "National 2", "National 1", "European", "International")
      level_colors <- c("#333333", "#444444", "#555555", "#666666", "#888888", "#ff7f0e", "#ff4500")

      monde_value <- filtered_minima$Monde
      progress <- (value / monde_value) * 100
      progress <- max(0, min(progress, 100))  # clamp

      level_percentages <- sapply(filtered_minima[levels], function(x) (x / monde_value) * 100)

      # Determine lifter level
      lifter_level <- ""
      for (i in seq_along(levels)) {
        if (progress <= level_percentages[i]) {
          lifter_level <- levels[i]
          break
        }
      }
      if (lifter_level == "") lifter_level <- "Monde"

      next_level_index <- which(levels == lifter_level) + 1
      next_level_value <- if (next_level_index <= length(levels)) filtered_minima[[levels[next_level_index]]] else NA
      kg_to_next <- if (!is.na(next_level_value)) next_level_value - value else 0

      # Build gauge steps
      steps_list <- list()
      for (i in seq_along(level_percentages)) {
        from <- if (i == 1) 0 else level_percentages[i - 1] * monde_value / 100
        to <- level_percentages[i] * monde_value / 100
        steps_list[[i]] <- list(range = c(from, to), color = level_colors[i])
      }

      # Create annotations, two columns for better readability
      label_annotations_left <- lapply(seq(1, length(levels) / 2), function(i) {
        list(
          x = 0.25,  # Left side column
          y = -0.3 - 0.2 * (i - 1),  # Increased space between annotations
          xref = "paper", yref = "paper",
          text = paste0("<b>", level_labels[i], "</b>"),
          showarrow = FALSE,
          font = list(color = "white", size = 14),  # Increased font size
          bgcolor = level_colors[i],
          bordercolor = "white",
          borderwidth = 1,
          borderpad = 4,
          align = "center"
        )
      })

      label_annotations_right <- lapply(seq((length(levels) / 2) + 1, length(levels)), function(i) {
        list(
          x = 0.75,  # Right side column
          y = -0.3 - 0.2 * (i - length(levels) / 2 - 1),  # Increased space between annotations
          xref = "paper", yref = "paper",
          text = paste0("<b>", level_labels[i], "</b>"),
          showarrow = FALSE,
          font = list(color = "white", size = 14),  # Increased font size
          bgcolor = level_colors[i],
          bordercolor = "white",
          borderwidth = 1,
          borderpad = 4,
          align = "center"
        )
      })

      # Bottom annotation for kg to next level (with proper handling for NA)
      next_level_text <- if (!is.na(kg_to_next) && kg_to_next > 0) {
        paste0("<b>", kg_to_next, " kg</b> to reach next level (", levels[next_level_index], ")")
      } else if (is.na(kg_to_next) || kg_to_next <= 0) {
        "<b>Max Level Achieved!</b>"
      }

      callout_annotation <- list(
        x = 0.5, y = -0.3 - 0.2 * (length(levels) + 1),  # Adjust the y position to keep it at the bottom
        text = next_level_text,
        showarrow = FALSE,
        font = list(color = "#ff7f0e", size = 16)  # Font size for better readability
      )

      # Plotly gauge
      gauge <- plot_ly(
        type = "indicator",
        mode = "gauge+number+delta",
        value = value,
        title = list(text = paste0("Current Level: <b style='color:orange;'>", lifter_level, "</b>")),
        number = list(suffix = " kg", font = list(color = "white")),
        delta = list(reference = monde_value, increasing = list(color = "orange")),
        gauge = list(
          axis = list(range = list(0, monde_value), tickwidth = 1, tickcolor = "orange"),
          bar = list(color = "orange"),
          bgcolor = "black",
          borderwidth = 2,
          bordercolor = "white",
          steps = steps_list,
          threshold = list(
            line = list(color = "white", width = 4),
            thickness = 0.75,
            value = value
          )
        )
      ) %>% layout(
        paper_bgcolor = "black",
        font = list(color = "white"),
        annotations = c(label_annotations_left, label_annotations_right, list(callout_annotation)),
        margin = list(t = 80, b = 240, l = 40, r = 40),  # Increased bottom margin for more space
        height = 600,  # Set the height of the plot to make it bigger
        autosize = TRUE  # Make the plot responsive
      )

      return(gauge)
    }




    create_gauge_chart5 <- function(best_total, weight_class, category, sex, minima) {
      value <- best_total

      # Filter for user inputs
      filtered_minima <- minima %>%
        filter(WeightClass == weight_class, Category == category, Sex == sex)

      if (nrow(filtered_minima) == 0) {
        stop("No matching data found in the minima data frame.")
      }

      # Define levels and styles
      levels <- c("R3", "R2", "R1", "N2", "N1", "Europe", "Monde")
      level_labels <- c("Regional 3", "Regional 2", "Regional 1", "National 2", "National 1", "European", "International")
      level_colors <- c("#333333", "#444444", "#555555", "#666666", "#888888", "#ff7f0e", "#ff4500")

      monde_value <- filtered_minima$Monde
      progress <- (value / monde_value) * 100
      progress <- max(0, min(progress, 100))  # clamp

      level_percentages <- sapply(filtered_minima[levels], function(x) (x / monde_value) * 100)

      # Determine lifter level
      lifter_level <- ""
      for (i in seq_along(levels)) {
        if (progress <= level_percentages[i]) {
          lifter_level <- levels[i]
          break
        }
      }
      if (lifter_level == "") lifter_level <- "Monde"

      next_level_index <- which(levels == lifter_level) + 1
      next_level_value <- if (next_level_index <= length(levels)) filtered_minima[[levels[next_level_index]]] else NA
      kg_to_next <- if (!is.na(next_level_value)) next_level_value - value else 0

      # Build gauge steps
      steps_list <- list()
      for (i in seq_along(level_percentages)) {
        from <- if (i == 1) 0 else level_percentages[i - 1] * monde_value / 100
        to <- level_percentages[i] * monde_value / 100
        steps_list[[i]] <- list(range = c(from, to), color = level_colors[i])
      }

      # Create vertically stacked annotations with more space between them
      label_annotations <- lapply(seq_along(levels), function(i) {
        list(
          x = 0.5,
          y = -0.25 - 0.08 * (i - 1),  # Increased space between annotations
          xref = "paper", yref = "paper",
          text = paste0("<b>", level_labels[i], "</b>"),
          showarrow = FALSE,
          font = list(color = "white", size = 10),  # Reduced size for mobile readability
          bgcolor = level_colors[i],
          bordercolor = "white",
          borderwidth = 1,
          borderpad = 4,
          align = "center"
        )
      })

      # Bottom annotation for kg to next level (with proper handling for NA)
      next_level_text <- if (!is.na(kg_to_next) && kg_to_next > 0) {
        paste0("<b>", kg_to_next, " kg</b> to reach next level (", levels[next_level_index], ")")
      } else if (is.na(kg_to_next) || kg_to_next <= 0) {
        "<b>Max Level Achieved!</b>"
      }

      callout_annotation <- list(
        x = 0.5, y = -0.25 - 0.08 * (length(levels) + 1),  # Adjust the y position to keep it at the bottom
        text = next_level_text,
        showarrow = FALSE,
        font = list(color = "#ff7f0e", size = 12)
      )

      # Plotly gauge
      gauge <- plot_ly(
        type = "indicator",
        mode = "gauge+number+delta",
        value = value,
        title = list(text = paste0("Current Level: <b style='color:orange;'>", lifter_level, "</b>")),
        number = list(suffix = " kg", font = list(color = "white")),
        delta = list(reference = monde_value, increasing = list(color = "orange")),
        gauge = list(
          axis = list(range = list(0, monde_value), tickwidth = 1, tickcolor = "orange"),
          bar = list(color = "orange"),
          bgcolor = "black",
          borderwidth = 2,
          bordercolor = "white",
          steps = steps_list,
          threshold = list(
            line = list(color = "white", width = 4),
            thickness = 0.75,
            value = value
          )
        )
      ) %>% layout(
        paper_bgcolor = "black",
        font = list(color = "white"),
        annotations = c(label_annotations, list(callout_annotation)),
        margin = list(t = 60, b = 200, l = 40, r = 40),  # Larger bottom margin for space
        autosize = TRUE  # Make the plot responsive
      )

      return(gauge)
    }




    create_gauge_chart4 <- function(best_total, weight_class, category, sex, minima) {
      value <- best_total

      # Filter for user inputs
      filtered_minima <- minima %>%
        filter(WeightClass == weight_class, Category == category, Sex == sex)

      if (nrow(filtered_minima) == 0) {
        stop("No matching data found in the minima data frame.")
      }

      levels <- c("R3", "R2", "R1", "N2", "N1", "Europe", "Monde")
      level_labels <- c("Regional 3", "Regional 2", "Regional 1", "National 2", "National 1", "European", "International")
      level_colors <- c("#333333", "#444444", "#555555", "#666666", "#888888", "#ff7f0e", "#ff4500")

      monde_value <- filtered_minima$Monde
      progress <- (value / monde_value) * 100
      progress <- max(0, min(progress, 100))

      level_percentages <- sapply(filtered_minima[levels], function(x) (x / monde_value) * 100)

      lifter_level <- ""
      for (i in seq_along(levels)) {
        if (progress <= level_percentages[i]) {
          lifter_level <- levels[i]
          break
        }
      }
      if (lifter_level == "") lifter_level <- "Monde"

      next_level_index <- which(levels == lifter_level) + 1
      next_level_value <- if (next_level_index <= length(levels)) filtered_minima[[levels[next_level_index]]] else NA
      kg_to_next <- if (!is.na(next_level_value)) next_level_value - value else 0

      # Add gauge chart
      gauge <- plot_ly(
        type = "indicator",
        mode = "gauge+number+delta",
        value = value,
        title = list(text = paste0("Current Level: <b style='color:orange;'>", lifter_level, "</b>")),
        number = list(suffix = " kg", font = list(color = "white")),
        delta = list(reference = monde_value, increasing = list(color = "orange")),
        gauge = list(
          axis = list(range = list(0, monde_value), tickwidth = 1, tickcolor = "orange"),
          bar = list(color = "orange"),
          bgcolor = "black",
          borderwidth = 2,
          bordercolor = "white",
          steps = list(
            list(range = c(0, level_percentages[1] * monde_value / 100), color = "#333333"),
            list(range = c(level_percentages[1] * monde_value / 100, level_percentages[2] * monde_value / 100), color = "#444444"),
            list(range = c(level_percentages[2] * monde_value / 100, level_percentages[3] * monde_value / 100), color = "#555555"),
            list(range = c(level_percentages[3] * monde_value / 100, level_percentages[4] * monde_value / 100), color = "#666666"),
            list(range = c(level_percentages[4] * monde_value / 100, level_percentages[5] * monde_value / 100), color = "#888888"),
            list(range = c(level_percentages[5] * monde_value / 100, level_percentages[6] * monde_value / 100), color = "#ff7f0e"),
            list(range = c(level_percentages[6] * monde_value / 100, monde_value), color = "#ff4500")
          ),
          threshold = list(
            line = list(color = "white", width = 4),
            thickness = 0.75,
            value = value
          )
        )
      )

      # Add left-side annotations (legend style)
      y_positions <- seq(1, 0, length.out = length(levels))  # from top to bottom
      left_annotations <- lapply(seq_along(levels), function(i) {
        list(
          x = -0.05,  # closer to the gauge so it doesn’t get cropped
          y = y_positions[i],
          xref = "paper", yref = "paper",
          text = paste0("<b>", level_labels[i], "</b>"),
          showarrow = FALSE,
          font = list(color = "white", size = 12),
          bgcolor = level_colors[i],
          bordercolor = "white",
          borderpad = 4,
          align = "center"
        )
      })

      # Add the bottom annotation for kg to next level
      callout_annotation <- list(
        x = 0.5, y = -0.2,
        text = if (!is.na(kg_to_next))
          paste0("<b>", kg_to_next, " kg</b> to reach next level (", levels[next_level_index], ")")
        else "<b>Max Level Achieved!</b>",
        showarrow = FALSE,
        font = list(color = "#ff7f0e", size = 14)
      )

      gauge <- gauge %>% layout(
        paper_bgcolor = "black",
        font = list(color = "white"),
        annotations = c(left_annotations, list(callout_annotation)),
        margin = list(t = 80, b = 60, l = 80)  # add left margin for annotations
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

