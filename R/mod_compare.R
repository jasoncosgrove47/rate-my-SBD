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

    textOutput(ns("textOutput"))
  )



}

#' metrics Server Functions
#'
#' @noRd
mod_compare_server <- function(id,lifts_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Ensure lifts_data is called within a reactive context
    data <- reactive({
      lifts_data()
    })

    output$textOutput <- renderText({"Test"})

    output$squat_hist <- renderPlot({

      data_val <- data()

      create_histogram(data_val$df,'Best3SquatKg')

    })

    create_histogram <- function(lifter, column_name) {


      # Ensure the column name exists in the data frame
      if (!column_name %in% colnames(openpl)) {
        stop("The specified column does not exist in the data frame.")
      }


      #lifter <- openpl %>% filter(Name == lifter_name)
      dots <- max(lifter$Dots,na.rm = TRUE)

      age <- max(lifter$Age,na.rm = TRUE)

      sex <- max(lifter$Sex,na.rm = TRUE)

      age_range <- 5

      value <- max(lifter[[column_name]],na.rm = TRUE)

      #take whichever weight class they've compted in the most
      weightclass <- names(which.max(table(lifter$WeightClassKg)))

      df <- openpl %>%
        filter(year > 2010)  %>%
        filter(age >= (age - age_range) & age <= (age + age_range)) %>%
        filter(WeightClassKg == weightclass) %>%
        filter(Sex == sex)



      #dont allow negative values
      df <- df[df[[column_name]] > 0,]


      p <- ggplot(df, aes_string(x = column_name)) +
        geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.2) +
        geom_vline(aes(xintercept = value), color = "red", linetype = "dashed", size = 1) +
        labs(
          title = paste("Distribution of", column_name, "Performances"),
          subtitle = paste("Age:", age - age_range, "to", age + age_range, "| Sex:", sex, "| Weight Class:", weightclass, "kg", "|", column_name, "=", value),
          x = paste(column_name, "(kg)"),
          y = "Frequency"
        ) +
        theme_minimal(base_size = 15) +
        theme(
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, color = "red")
        )



      return(p)
    }







  })
}

## To be copied in the UI
# mod_metrics_ui("metrics_1")

## To be copied in the server
# mod_metrics_server("metrics_1")
