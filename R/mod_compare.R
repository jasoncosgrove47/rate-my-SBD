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
mod_compare_server <- function(id,lifts_data){
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


    output$best_DOTS <- renderText({
      max_dots <- max(data_filtered()$df$Dots, na.rm = TRUE)
      if (is.finite(max_dots)) paste(max_dots) else "No data"
    })

    output$best_total <- renderText({

      #data_val <- data()
      max_tot <- max(data_filtered()$df$TotalKg, na.rm = TRUE)
      if (is.finite(max_tot)) paste(max_tot, "Kg") else "No data"
    })

    output$textOutput <- renderText({"The following plots shows where your lifting total stands compared to lifters of the same sex and weight class. In addition we assess your DOTS score which is a way to compare all lifters to each other, controlling for differences in weight and sex. This allows you to see how you compare to others—whether you're above average, near the middle of the pack, or aiming for further improvement."})



    output$dots_hist <- renderPlot({

      create_progress_bar(data_filtered()$percentages,'Dots')

    })

    output$total_hist <- renderPlot({

      create_progress_bar(data_filtered()$percentages,'TotalKg')

    })


    output$total_progress <- renderPlot({

      create_lift_plot(data_filtered()$df, "TotalKg")
    })

    output$DOTS_progress <- renderPlot({

      create_lift_plot(data_filtered()$df, "Dots")
    })

    output$bodyweight_progress <- renderPlot({

      create_lift_plot(data_filtered()$df, "BodyweightKg")
    })

   })
}

