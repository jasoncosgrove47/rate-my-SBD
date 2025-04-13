#' compare UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_compare_ui <- function(id) {
  ns <- NS(id)


  fluidPage(
    # Title
    titlePanel("Comparisons against other lifters"),



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
          title = "Best Total",
          value = textOutput(ns("best_total")),
          showcase = bsicons::bs_icon("trophy"),
          style = "background-color: black; color: white;"  # Using theme instead of color
        ),
        value_box(
          title = "Best DOTS Score",
          value = textOutput(ns("best_DOTS")),
          showcase = bsicons::bs_icon("trophy"),
          style = "background-color: purple; color: white;"  # Using theme instead of color
        )
      )
    ),
    fluidRow(
      column(6, plotOutput(ns("total_hist")), height = "400px"),
      column(6, plotOutput(ns("dots_hist")), height = "400px")
    ),

    layout_columns(
      col_widths = c(6,6),

      card(
        full_screen = TRUE,
        card_header("Total Progress",
                    style = "background-color: black; color: white; font-weight: bold; padding: 10px;"),
        plotOutput(ns("total_progress"))
      ),
      card(
        full_screen = TRUE,
        card_header("DOTS Score Progress",
                    style = "background-color: purple; color: white; font-weight: bold; padding: 10px;"),
        plotOutput(ns("DOTS_progress"))
      )

    )
  )



}

#' metrics Server Functions
#'
#' @noRd
mod_compare_server <- function(id,lifts_data,openpl,minima){
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

    output$best_DOTS <- renderText({

      data_val <- data()
      max_dots <- max(data_val$df$Dots, na.rm = TRUE)
      if (is.finite(max_dots)) paste(max_dots) else "No data"
    })

    output$best_total <- renderText({

      data_val <- data()
      max_tot <- max(data_val$df$TotalKg, na.rm = TRUE)
      if (is.finite(max_tot)) paste(max_tot, "Kg") else "No data"
    })

    output$textOutput <- renderText({"The following plots shows where your lifting total stands compared to lifters of the same sex, weight class, and age range (±5 years). In addition we assess your DOTS score which is a way to compare all lifters to each other, controlling for differences in weight and sex. This allows you to see how you compare to others—whether you're above average, near the middle of the pack, or aiming for further improvement."})



    output$dots_hist <- renderPlot({

      data_val <- data()

      create_progress_bar(data_val$df,'Dots')

    })

    output$total_hist <- renderPlot({

      data_val <- data()


      create_progress_bar(data_val$df,'TotalKg')

    })


    output$total_progress <- renderPlot({

      data_val <- data()
      create_lift_plot(data_val$df, "TotalKg")
    })

    output$DOTS_progress <- renderPlot({

      data_val <- data()
      create_lift_plot(data_val$df, "Dots")
    })

    output$bodyweight_progress <- renderPlot({

      data_val <- data()
      create_lift_plot(data_val$df, "BodyweightKg")
    })


    create_progress_bar <- function(lifter, column_name) {
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

      print("SEX")
      print(sex)


      age_range <- 5
      value <- max(lifter[[column_name]], na.rm = TRUE)


      weightclass <- names(which.max(table(lifter$WeightClassKg)))


      if(column_name == "Dots" | column_name == "TotalKg"){
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
        annotate("text", x = 50, y = 1.8, label = paste("Your",annot_map[column_name] ,"is better than that of \n", round(percentile, 2), "% of other lifters at in your category."),
                 color = "black", size = 5, fontface = "bold") +
        # Formatting
        theme_void() +
        coord_cartesian(xlim = c(0, 100), ylim = c(0, 2)) +
        theme(plot.margin = margin(10, 10, 10, 10))

      return(progress_bar)
    }

   })
}

