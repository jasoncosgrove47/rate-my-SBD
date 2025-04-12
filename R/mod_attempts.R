#' attempts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import tidyr
#'
#' @importFrom shiny NS tagList
mod_attempts_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    # Title

    # First Row: Text Output
    fluidRow(
      column(12, textOutput(ns("textOutput")))
    ),
    fluidRow(
      column(10,offset=1, plotOutput(ns("success_plot")))
    ),
    fluidRow(
      column(12,
             card(
               full_screen = TRUE,
               card_header("Attempts Summary",
                           style = "background-color: black; color: white; font-weight: bold; padding: 10px;"),
               plotOutput(ns("attempts_plot"))
             )
      )
    )
  )



}

#' metrics Server Functions
#'
#' @noRd
mod_attempts_server <- function(id,lifts_data,openpl,minima){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Ensure lifts_data is called within a reactive context
    data <- reactive({
      lifts_data()
    })



    output$textOutput <- renderText({"The following plots assess how your performances change depending on your lifting experience and your body weight and we then use this information to make comparisons to other lifters. This allows you to see how you compare to others—whether you're above average, near the middle of the pack, or aiming for further improvement."})


    output$success_plot <- renderPlot({

      data_val <- data()

      calculate_success_rate(data_val$df)

    })

    output$attempts_plot <- renderPlot({

      data_val <- data()

      plot_Attempts(data_val$df)

    })

plot_Attempts <- function(lifter){



      tmp <- lifter %>% select(Comp, Squat1Kg, Squat2Kg, Squat3Kg,
                               Bench1Kg, Bench2Kg, Bench3Kg,
                               Deadlift1Kg, Deadlift2Kg, Deadlift3Kg)




      lifter_long <- tmp %>%
        pivot_longer(cols = -Comp, names_to = "Lift", values_to = "Value")


      # Create a new column for color based on the value
      lifter_long <- lifter_long %>%
        mutate(Color = ifelse(Value > 0, "Positive", "Negative"))


      lifter_long <- lifter_long %>%
        mutate(Lift2 = recode(Lift,
                              "Bench1Kg" = "Bench 1st attempt",
                              "Bench2Kg" = "Bench 2nd attempt",
                              "Bench3Kg" = "Bench 3rd attempt",

                              "Deadlift1Kg" = "Deadlift 1st attempt",
                              "Deadlift2Kg" = "Deadlift 2nd attempt",
                              "Deadlift3Kg" = "Deadlift 3rd attempt",

                              "Squat1Kg" = "Squat 1st attempt",
                              "Squat2Kg" = "Squat 2nd attempt",
                              "Squat3Kg" = "Squat 3rd attempt"))

      # Create the heatmap
      ggplot(lifter_long, aes(x = factor(Comp), y = Lift2, fill = Color)) +
        geom_tile(color = "white") +
        geom_text(aes(label = Value), color = "black", size = 2.5) +
        scale_fill_manual(values = c("Positive" = "lightgreen", "Negative" = "red")) +
        labs(
          title = "Attempt Selection",
          x = "Competition",
          y = "Attempt"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none"
        )

}



calculate_success_rate <- function(lifter){

  tmp <- lifter %>% select(Comp, Name, Squat1Kg, Squat2Kg, Squat3Kg,
                           Bench1Kg, Bench2Kg, Bench3Kg,
                           Deadlift1Kg, Deadlift2Kg, Deadlift3Kg)




  lifter_long <- tmp %>%
    pivot_longer(cols = -c(Comp,Name), names_to = "Lift", values_to = "Value")

  lifter_long <- lifter_long %>%
    filter(!is.na(Value))


  lifter_success_rate <- lifter_long %>%
    mutate(success = ifelse(Value > 0, 1, 0)) %>%
    group_by(Name, Comp) %>%
    summarise(
      total_lifts = n(),
      successful_lifts = sum(success),
      success_rate = (successful_lifts / total_lifts) * 100
    )


  percentile <- mean(lifter_success_rate$success_rate)

  data <- data.frame(x = c(0, percentile), y = 1)

  # Plot using ggplot2
  progress_bar <- ggplot() +
    # Background bar
    geom_rect(aes(xmin = 0, xmax = 100, ymin = 0.4, ymax = 1.6),
              fill = "lightgray", color = "black", alpha = 0.5, linewidth = 1) +
    # Filled progress bar
    geom_rect(data = data, aes(xmin = 0, xmax = percentile, ymin = 0.4, ymax = 1.6),
              fill = "darkblue", color = "black", alpha = 0.9, linewidth = 1) +
    # Add percentage text
    annotate("text", x = 50, y = 1, label = paste0(round(percentile, 2), "%"),
             color = "white", size = 10, fontface = "bold") +
    # Add contextual information
    annotate("text", x = 50, y = 1.8, label = paste("You have successfully made", round(percentile, 2), "% \n of all lifts attempted. The average success rate across all lifters is 77% "),
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
