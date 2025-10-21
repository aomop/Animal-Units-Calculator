options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"))

# app.R
#
# The main file now focuses on wiring together feature-specific modules rather
# than holding all of the server logic. Each module is responsible for a single
# idea (data upload, parameter selection, displaying results, and showing the
# data table). This keeps the app approachable for newcomers who want to see how
# a Shiny project can be organised.

library(shiny)
library(readxl)
library(dplyr)
library(DT)
library(janitor)
library(bslib)

# Feature modules -----------------------------------------------------------
source("modules/data_input_module.R")
source("modules/parameter_module.R")
source("modules/results_module.R")
source("modules/table_module.R")
source("modules/gif_module.R")

# Constants from the original specification. Keeping them near the top makes it
# easy to spot the numbers that drive the calculations.
K_CONST <- 96.033       # multiplier
C_CONST <- 10950        # lbs forage per AU per year (default option)

my_theme <- bs_theme(
  bootswatch = "darkly",
  bg = "#161b22",
  fg = "#ffffff",
  primary = "#c9a227",
  base_font = font_google("Lato")
)

ui <- fluidPage(
  theme = my_theme,

  tags$head(
    tags$style(HTML("
      .orange { color: #c9a227; }
      .green { color: #00bc8c; }
    "))
  ),

  titlePanel(tags$span("Animal Units (AUs) — Grazing Calculator", class = "orange")),
  sidebarLayout(
    sidebarPanel(
      # Module 1: gather the spreadsheet and column selections from the user.
      dataInputUI("data"),
      tags$hr(),
      h3(tags$span("Parameters", class = "orange")),
      # Module 2: acreage, intake assumption, and calculate button.
      parameterUI("params"),
      helpText(HTML(paste0("Calculations assume the plot size is 1ft", tags$sup("2"), " and weight measurements are taken in grams."))),
      tags$hr(),
      # Module 3 (sidebar portion): running summary of the current calculation.
      resultsSidebarUI("results")
    ),
    mainPanel(
      # The wrapper gives the floating GIF button room without covering content.
      tags$div(
        id = "main-wrap",
        style = "position: relative; padding-bottom: 120px;",

        # Module 3 (main portion): AU value, download button, and formula help.
        resultsMainUI("results"),
        tags$hr(),
        h3(tags$span("Cleaned Data", class = "orange")),
        # Module 4: review the cleaned records.
        tableUI("table"),

        # Cheerful GIF button for encouragement.
        gif_ui("cheer", position = "fixed; bottom:20px ; right:20px;", color = "#c9a227")
      )
    )
  )
)

server <- function(input, output, session) {
  # Bring together each feature-specific module. The server now reads almost
  # like a checklist of app features.
  data_inputs <- dataInputServer("data")
  parameter_inputs <- parameterServer("params")
  resultsServer("results", data_inputs, parameter_inputs, K_CONST)
  tableServer("table", data_inputs)
  gif_server("cheer")
}

shinyApp(ui, server)
