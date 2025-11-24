#' Launch the minimal Leaflet app
#'
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom utils "globalVariables"

run_app <- function() {
  shiny::shinyApp(app_ui, app_server)
}
