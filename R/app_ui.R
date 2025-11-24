#' AU Calculator user interface
#'
#' @return A [shiny::tagList()] containing the app UI definition.
app_ui <- function() {
  theme <- bslib::bs_theme(
    bootswatch = "darkly",   # dark background so the orange highlights pop
    bg        = "#161b22",
    fg        = "#ffffff",
    primary   = "#c9a227",
    base_font = bslib::font_google("Lato")
  )

  shiny::tagList(
    shiny::fluidPage(
      title = "Pteoptaye Grazing Calculator",
      theme = theme,

      # Put custom CSS in the head so classes like .orange / .green work
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(paste0(
            ".orange { color: #c9a227; }\n",
            ".green { color: #00bc8c; }\n"
          ))
        )
      ),
      shiny::titlePanel(
        shiny::tags$span(
          "Pteoptaye Grazing Calculator",
          class = "orange"
        ),
        windowTitle = "Grazing Calculator"
      ),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          # Module 1: gather the spreadsheet and column selections from the user.
          dataInputUI("data"),
          shiny::tags$hr(),

          shiny::h3(
            shiny::tags$span("Parameters", class = "orange")
          ),
          # Module 2: acreage, intake assumption, and calculate button.
          parameterUI("params"),

          shiny::helpText(
            shiny::HTML(
              paste0(
                "Calculations assume the plot size is 1ft",
                shiny::tags$sup("2"),
                " and weight measurements are taken in grams."
              )
            )
          ),

          shiny::tags$hr(),

          # Module 3 (sidebar portion): running summary of the current calculation.
          resultsSidebarUI("results")
        ),

        shiny::mainPanel(
          shiny::tags$div(
            id = "main-wrap",
            # Reserve space so the floating GIF button never blocks content.
            style = "position: relative; padding-bottom: 120px;",

            # Module 3 (main portion): AU value, download button, and formula help.
            resultsMainUI("results"),

            shiny::tags$hr(),
            shiny::h3(
              shiny::tags$span("Cleaned Data", class = "orange")
            ),

            # Module 4: review the cleaned records.
            tableUI("table"),

            # Cheerful GIF button for encouragement.
            cheerfulgif::gif_ui(
              "cheer",
              position = "fixed; bottom:20px; right:20px;",
              color    = "#c9a227"
            )
          )
        )
      )
    )
  )
}
