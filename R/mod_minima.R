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

    data_filtered <- reactiveVal()

    observeEvent(lifts_data(), {

      req(lifts_data())  # ensures lifts_data() is not NULL
      data <- lifts_data()

      req(data$df, data$equipped, data$tested)
      data$df <- data$df %>%
        filter(Equipment == data$equipped, Tested == data$tested)
      data_filtered(data)
    })



    updateSelectizeInput(session, "weight_class", choices = minima_weightclass,server = TRUE)
    updateSelectizeInput(session, "category", choices = minima_category,server = TRUE)
    updateSelectizeInput(session, "sex", choices = minima_sex,server = TRUE)

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

      max_tot <- max(data_filtered()$df$TotalKg, na.rm = TRUE)
      if (is.finite(max_tot)) paste(max_tot, "Kg") else "No data"
    })

    output$progress_bar <- renderPlot({

      max_tot <- max(data_filtered()$df$TotalKg, na.rm = TRUE)
      if (is.finite(max_tot)) {
        create_progress_bar(max_tot, input$weight_class, input$category, input$sex, minima)
      }
    })

    # Create and display the gauge chart
    output$gauge_chart <- renderPlotly({

      max_tot <- max(data_filtered()$df$TotalKg, na.rm = TRUE)
      if (is.finite(max_tot)) {
        create_gauge_chart(max_tot, input$weight_class, input$category, input$sex, minima)
      }
    })


    create_gauge_chart <- function(best_total, weight_class, category, sex, minima) {
      value <- best_total

      # Filter for user inputs
      filtered_minima <- minima %>%
        filter(WeightClass == weight_class, Category == category, Sex == sex)

      if (nrow(filtered_minima) == 0) {
        stop("No matching data found in the minima data frame.")
      }

      # Define levels and styles
      levels <- c("R3", "R2", "R1", "N2", "N1", "Europe", "Monde")
      level_labels <- c("Regional 3", "Regional 2", "Regional 1", "National 2", "National 1", "International", "International")
      level_colors <- c("#333333", "#444444", "#555555", "#666666", "#888888", "#ff7f0e", "#ff7f0e")

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



   })
}

