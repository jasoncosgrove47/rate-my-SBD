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
    # First Row: Text Output
    fluidRow(
      column(12, textOutput(ns("textOutput")))
    ),

    # Second Row: Success Plot
    fluidRow(
      column(10, offset = 1, plotOutput(ns("success_plot")))
    ),

    # Third Row: Attempts Summary with scrollable plot
    fluidRow(
      column(12,
             card(
               full_screen = TRUE,
               card_header("Attempts Summary",
                           style = "background-color: black; color: white; font-weight: bold; padding: 10px;"),
               div(
                 style = "overflow-x: auto; width: 100%;",
                 plotOutput(ns("attempts_plot"), height = "500px", width = "1000px")  # Wider width triggers scroll
               )
             )
      )
    )
  )
}


#' metrics Server Functions
#'
#' @noRd
mod_attempts_server <- function(id,lifts_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    data_filtered <- reactiveVal()

    observeEvent(lifts_data(), {

      req(lifts_data())  # ensures lifts_data() is not NULL
      data <- lifts_data()

      req(data$df, data$equipped, data$tested)
      data$df <- data$df %>%
        filter(Equipment == data$equipped, Tested == data$tested)
      data_filtered(data)
    })




    output$textOutput <- renderText({"The following plots assess how your performances change depending on your lifting experience and your body weight and we then use this information to make comparisons to other lifters. This allows you to see how you compare to others—whether you're above average, near the middle of the pack, or aiming for further improvement."})


    output$success_plot <- renderPlot({

      calculate_success_rate(data_filtered()$df)

    })

    output$attempts_plot <- renderPlot({

      plot_Attempts(data_filtered()$df)

    })


    plot_Attempts_tmp <- function(lifter) {
      tmp <- lifter %>% select(
        Comp,
        Squat1Kg, Squat2Kg, Squat3Kg,
        Bench1Kg, Bench2Kg, Bench3Kg,
        Deadlift1Kg, Deadlift2Kg, Deadlift3Kg
      )

      lifter_long <- tmp %>%
        pivot_longer(cols = -Comp, names_to = "Lift", values_to = "Value") %>%
        mutate(Color = ifelse(Value > 0, "Made", "Missed")) %>%
        mutate(
          Lift2 = recode(Lift,
                         "Bench1Kg" = "Bench\n1st",
                         "Bench2Kg" = "Bench\n2nd",
                         "Bench3Kg" = "Bench\n3rd",
                         "Deadlift1Kg" = "Deadlift\n1st",
                         "Deadlift2Kg" = "Deadlift\n2nd",
                         "Deadlift3Kg" = "Deadlift\n3rd",
                         "Squat1Kg" = "Squat\n1st",
                         "Squat2Kg" = "Squat\n2nd",
                         "Squat3Kg" = "Squat\n3rd"
          )
        )

      ggplot(lifter_long, aes(x = factor(Comp), y = Lift2, fill = Color)) +
        geom_tile(color = "white", linewidth = 0.6) +
        geom_text(
          aes(label = Value),
          color = "black",
          size = 3.5,
          fontface = "bold"
        ) +
        scale_fill_manual(values = c("Made" = "#B9E7B1", "Missed" = "#FF4C4C")) +  # Brighter red
        labs(
          title = "Attempts Summary",
          x = "Competition",
          y = "Attempt"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          axis.title.x = element_text(face = "bold", size = 12),
          axis.title.y = element_text(face = "bold", size = 12),
          axis.text.x = element_text(angle = 60, hjust = 1, size = 10),
          axis.text.y = element_text(size = 10),
          panel.grid = element_blank(),
          legend.position = "none",
          plot.margin = margin(10, 10, 10, 10)
        )
    }

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
        geom_text(aes(label = Value), color = "black", size = 2.5, fontface = "bold") +
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
          axis.text.x = element_text(angle = 60, hjust = 1),
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
    annotate("text", x = 50, y = 1.8, label = paste("You have successfully made", round(percentile, 2), "% \n of all lifts attempted. The average \n success rate across all lifters is 77% "),
             color = "black", size = 4, fontface = "bold") +
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
