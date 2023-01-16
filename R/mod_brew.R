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
 
  )
}
    
#' brew Server Functions
#'
#' @noRd 
mod_brew_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_brew_ui("brew_1")
    
## To be copied in the server
# mod_brew_server("brew_1")
