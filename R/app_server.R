#' Shiny App Server
#' 
#' @name server
#' @description The server logic for the Animal Units (AUs) Grazing Calculator Shiny app.
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
app_server <- function(input, output, session) {

  K_CONST <- 96.033

  data_inputs <- dataInputServer("data")
  parameter_inputs <- parameterServer("params")
  resultsServer("results", data_inputs, parameter_inputs, K_CONST)
  tableServer("table", data_inputs)
  cheerfulgif::gif_server("cheer")
}