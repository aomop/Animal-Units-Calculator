# parameter_module.R
#
# This module handles everything related to the numeric assumptions that feed
# into the AU calculation. Separating acreage and intake selections from the
# data upload keeps each module focused on a single idea.

# --- User interface -------------------------------------------------------
#' Parameter inputs UI
#'
#' @name parameterUI
#' @param id Module namespace identifier.
#' @return UI controls for the acreage, calculate button, and intake choice.
parameterUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::numericInput(ns("acreage"), "Pasture acreage (A):", value = NA, min = 0, step = 0.5),
    shiny::actionButton(ns("calculate"), "Calculate", class = "btn-primary"),
    shiny::radioButtons(
      ns("intake"),
      "Annual AU intake basis:",
      choices = c("3.0% (10,950 lb/yr)" = "10950", "2.6% (9,490 lb/yr)" = "9490"),
      selected = "10950",
      inline = TRUE
    )
  )
}

# --- Server logic ---------------------------------------------------------
#' Parameter server logic
#'
#' @name parameterServer
#' @param id Module namespace identifier.
#' @return A list with reactivity for acreage, intake, and button presses.
parameterServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Convert whatever the user types into a numeric so validations downstream
    # are comparing like-with-like.
    acreage <- shiny::reactive({
      suppressWarnings(as.numeric(input$acreage))
    })

    # Radio buttons already restrict choices; we coerce to numeric for maths.
    intake <- shiny::reactive({
      as.numeric(input$intake)
    })

    list(
      acreage = acreage,
      intake = intake,
      calculate = shiny::reactive(input$calculate)
    )
  })
}
