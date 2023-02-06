#' brew UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_brew_ui <- function(id){
  ns <- NS(id)
  tagList(
      dataTableOutput(ns("table")),
      plotOutput(ns("equipment")),
      plotOutput(ns("styles")),
      plotOutput(ns("efficiency"))
  )
}

#' brew Server Functions
#'
#' @noRd
mod_brew_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    load(file = "data/my_dataset.rda")

    output$table <- renderDataTable(my_dataset,
                                    options = list(pageLength = 5))

    output$equipment <- renderPlot(
      my_dataset %>%
        mutate(equipment = fct_rev(fct_infreq(equipment))) %>%
        ggplot(aes(y = equipment)) +
        geom_bar() +
        labs(x = NULL,
             y = NULL) +
        theme_bw()
    )

    output$styles <- renderPlot(
      my_dataset %>%
        mutate(style = fct_rev(fct_infreq(style))) %>%
        ggplot(aes(y = style)) +
        geom_bar() +
        labs(x = NULL,
             y = NULL) +
        theme_bw()
    )

    output$efficiency <- renderPlot(
      my_dataset %>%
        ggplot(aes(x = equipment, y = brewhouse_efficiency)) +
        geom_boxplot() +
        theme_bw()
    )

  })
}

## To be copied in the UI
# mod_brew_ui("brew_1")

## To be copied in the server
# mod_brew_server("brew_1")
