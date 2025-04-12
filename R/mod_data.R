#' data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    br(),
    tags$h3("Option 1: Autofill your data from open powerlifting"),
    selectizeInput(
      inputId = ns("selected_item"),
      label = "Select your name, exactly as it appears on your openPL profile:",
      choices = NULL, # Set choices to NULL for server-side processing
      options = list(maxOptions = 50) # Optional: limit the number of options displayed at once
    ),


    textOutput(ns("selected_item_text")),


    br(),
    br(),

    tags$h3("Option 2: Manually input your details"),
    tags$h5("If using this functionality make sure that option 1 is set to << No Lifter Selected >>"),
    #"If using this functionality make sure that option 1 is set to No Lifter Selected"
    selectInput(ns("gender"), "Select Gender:", choices = c("Male", "Female")),
    numericInput(ns("age"), "Enter Age:", value = 25, min = 1, max = 100),
    selectInput(ns("tested"), "Compete in a tested federation?:", choices = c("Yes", "No")),
    selectizeInput(ns("weight_class"), "Select Weight Class:", choices = NULL),
    selectizeInput(ns("category"), "Select Category:", choices = NULL),




    br(),
    br(),

    # Input fields for 5 training block
    lapply(1:5, function(i) {
      card(
        full_screen = FALSE,
        fill = FALSE,
        card_header(
          tags$h4(paste("Training Block", i), style = "font-weight: bold;")
        ),
        layout_columns(
          fill = FALSE,
          col_widths = 12,
          card(
            class = "p-2",
            h6("Squat (kg)", class = "m-0"),
            numericInput(ns(paste0("squat", i)), NULL, value = NULL, min = 0)
          ),
          card(
            class = "p-2",
            h6("Bench (kg)", class = "m-0"),
            numericInput(ns(paste0("bench", i)), NULL, value = NULL, min = 0)
          ),
          card(
            class = "p-2",
            h6("Deadlift (kg)", class = "m-0"),
            numericInput(ns(paste0("deadlift", i)), NULL, value = NULL, min = 0)
          ),
          card(
            class = "p-2",
            h6("Bodyweight (kg)", class = "m-0"),
            numericInput(ns(paste0("bodyweight", i)), NULL, value = NULL, min = 0)
          )
        )
      )
    }) #lapply

  )#fluid page
}

#' metrics Server Functions
#'
#' @noRd
mod_data_server <- function(id,lifts_data,openpl,minima,all_lifter_names){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    updateSelectizeInput(session, "selected_item", choices = all_lifter_names, server = TRUE)

    output$selected_item_text <- renderText({
      paste("Your selection:", input$selected_item)
    })

    # Display the selected values
    output$selected_weight_class <- renderText({
      paste("Selected Weight Class:", input$weight_class)
    })

    output$selected_category <- renderText({
      paste("Selected Category:", input$category)
    })

    updateSelectizeInput(session, "weight_class", choices = unique(minima$WeightClass),server = TRUE)
    updateSelectizeInput(session, "category", choices = unique(minima$Category),server = TRUE)

    observe({

      selected_item <- input$selected_item %||% "No Lifter Selected"

      df <- if (selected_item == "No Lifter Selected") {

        data.frame(
            Comp = 1:5,
            year = 1:5,
            MeetName = paste("SBD",1:5),
            Best3SquatKg = sapply(1:5, function(i) input[[paste0("squat", i)]]),
            Best3BenchKg = sapply(1:5, function(i) input[[paste0("bench", i)]]),
            Best3DeadliftKg = sapply(1:5, function(i) input[[paste0("deadlift", i)]]),
            BodyweightKg = sapply(1:5, function(i) input[[paste0("bodyweight", i)]]),
            WeightClassKg = rep(input$weight_class, 5),
            Category = rep(input$category, 5),
            Sex = rep(input$gender, 5),
            Age = rep(input$age, 5),
            Tested = rep(input$tested, 5),
            Dots = sapply(1:5, function(i) {
              bodyweight <- input[[paste0("bodyweight", i)]]
              total_lift <- sum(input[[paste0("squat", i)]], input[[paste0("bench", i)]], input[[paste0("deadlift", i)]])
              is_female <- input$gender == "Female"
              calculate_dots(bodyweight, total_lift, is_female)
            }),

            TotalKg = sapply(1:5, function(i) {
              sum(input[[paste0("squat", i)]], input[[paste0("bench", i)]], input[[paste0("deadlift", i)]])
            })


            )


      } else {

        openpl %>% filter(Name == selected_item)

      }



      # Return the reactive expression so it can be used by other modules
      lifts_data(list(selected_item = selected_item,df = df))

    })

  })
}




