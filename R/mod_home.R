#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom bbcUtils html_bullets
#' @importFrom bslib value_box value_box_theme layout_column_wrap
#' @importFrom shiny NS tagList
#' @importFrom stringr str_glue




mod_home_ui <- function(id) {
  ns <- NS(id)

  tagList(

    div(style = "text-align: center;",
        h1(HTML("Welcome to Rate my <b>S</b>quat <b>B</b>ench <b>D</b>eadlift!")),
    ),
    layout_column_wrap(min_height = 200, max_height = 200, width = 1/4,
      value_box(title = NULL,
                value = format(nrow(openpl), big.mark = ","),
                showcase = shiny::icon("medal"),
                theme = value_box_theme(bg = "#9C7DCC"),
                p("competitive performances evaluated", class = "white-text")
                ), #value_box

      value_box(title = NULL,
                value =  format(length(unique(openpl$Name)), big.mark = ","),
                showcase = shiny::icon("person", class = "white-icon"),
                theme = value_box_theme(bg = "#33ABC5",fg = "white"),
                p("lifters evaluated", class = "white-text"),
                ), #value_box

      value_box(title = NULL,
                value = format(length(unique(openpl$Country)), big.mark = ","),
                showcase = shiny::icon("flag", class = "white-icon"),
                theme = value_box_theme(bg = "#E9B000",fg = "white"),
                p("countries evaluated", class = "white-text")
      ), #value_box

      value_box(title = NULL,
                value =  "62",
                showcase = shiny::icon("clock", class = "white-icon"),
                theme = value_box_theme(bg = "#91C461",fg = "white"),
                p("years of historical records", class = "white-text")
                ) #value_box
    ), # layout_column_wrap
    card(min_height = 190,
      card_header("Objectives"),
      HTML(html_bullets(
        c("Enhance performance outcomess",
          "AI-driven analytics to objectively assess your progress and to compare with elite lifters",
          "Obtain data-driven insights to optimise training and competition prep"
)))
         ), # card
    card(min_height = 240,
      card_header("Studies design"),
      HTML(html_bullets(
        c(
          "Evaluations are based on the most comprehensive database of competitive powerlifting performances",
          "Evaluations are based on 3.5 million competitive performances evaluated over a 62 year time span",
          "Annotations of age, sex, body weight, experience, tested vs untested, equipped vs raw allow you to make highly detailed comparisons"
          )))
         ), # card
    br(),
      tags$span(class = "versionapp", str_glue("SBD version {packageVersion('SBD')} powered by JC."))
    ) # tagList
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    total_entries <- nrow(openpl)
    output$total_entries <- renderText({ total_entries })


  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")
