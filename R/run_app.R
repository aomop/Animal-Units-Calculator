#' Launch the minimal Leaflet app
#'
#' @export
run_app <- function() {
  shiny::shinyApp(app_ui, app_server)
}
