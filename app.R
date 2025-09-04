options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"))

# app.R
library(shiny)
library(readxl)
library(dplyr)
library(DT)
library(janitor)
library(bslib)

source("modules/gif_module.R")

# Constants from your spec
K_CONST <- 96.033       # multiplier
C_CONST <- 10950        # lbs forage per AU per year

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
  
  titlePanel(tags$span("Animal Units (AUs) — Grazing Calculator",
             class = "orange")),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel (.xlsx/.xls)", accept = c(".xlsx", ".xls")),
      helpText("Required columns: Prairie_Unit, Grasses (percent), Dry wegith (lbs)."),
      uiOutput("sheet_picker"),
      uiOutput("unit_picker"),
      # --- New: unit selectors ---
      tags$hr(),
      h3(tags$span("Parameters", class = "orange")),
      numericInput("acreage", "Pasture acreage (A):", value = 163.5, min = 0, step = 0.5),
      radioButtons("intake", "Annual AU intake basis:",
                   choices = c("3.0% (10,950 lb/yr)" = "10950",
                               "2.6% (9,490 lb/yr)" = "9490"),
                   selected = "10950", inline = TRUE),
      helpText(HTML(paste0("Calculations assume the plot size is 1ft", tags$sup("2"), " and weight measurements are taken in grams."))),
      tags$hr(),
      verbatimTextOutput("n_info"),
    ),
    mainPanel(
      # Make a containing box for the main panel contents
      tags$div(
        id = "main-wrap",
        style = "position: relative; padding-bottom: 120px;",  # padding so GIF doesn’t cover content
        
        tags$hr(),
        h3(tags$span("Calculated AUs", class = "orange")),
        tags$h2(textOutput("au_value"), class = "green"),
        tags$hr(),
        fluidRow(
          column(
            width = 6,
            DTOutput("tbl")
          ),
          column(
            width = 6,
            h3(tags$span("Calculation Formula",
                         class = "orange")),
            tags$br(),
            withMathJax(
              tags$p("\\({\\Large \\text{AUs} = \\frac{A \\cdot \\sum_{i=1}^{n} (w_i \\cdot g_i \\cdot K)}{2 \\cdot C \\cdot n}}\\)",
                     style = "font-weight:bold; font-size: 18px;"),
              tags$p(
                tags$strong("Where:"),
                tags$ul(
                  tags$li("\\(A\\) = pasture acreage"),
                  tags$li("\\(w_i\\) = dry weight of sample \\(i\\) (lbs)"),
                  tags$li("\\(g_i\\) = proportion of grass in sample \\(i\\) (0–1)"),
                  tags$li(tags$span("\\(K\\) = conversion constant (", textOutput("k_show", inline = TRUE), ")")),
                  tags$li(tags$span("\\(C\\) = lbs of forage consumed per AU per year (", textOutput("c_show", inline = TRUE), ")")),
                  tags$li("\\(n\\) = total number of samples"),
                  tags$li("\\(2\\) = halving factor")
                )
              )
            )
          )
        ),
        # Anchor the GIF to the bottom-left *inside* this wrapper
        gif_ui("cheer", position = "fixed; bottom:20px ; right:20px;", color = "#c9a227")  # let CSS handle positioning
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive: C constant based on intake assumption
  C_reactive <- reactive({
    as.numeric(input$intake)  # "10950" or "9490"
  })
  
  # Nicely formatted for display
  output$k_show <- renderText({
    format(round(90.033, 3), big.mark = ",")
  })
  output$c_show <- renderText({
    format(C_reactive(), big.mark = ",")
  })
  
  # --- List sheets after upload ---
  sheet_names <- reactive({
    req(input$file)
    readxl::excel_sheets(input$file$datapath)
  })
  
  output$sheet_picker <- renderUI({
    req(sheet_names())
    selectInput(
      "sheet",
      "Choose sheet:",
      choices = sheet_names(),
      selected = sheet_names()[1]
    )
  })
  
  # --- Read selected sheet and clean ---
  dat <- reactive({
    req(input$file, input$sheet)
    raw <- read_excel(input$file$datapath, sheet = input$sheet)
    raw <- clean_names(raw)  # keeps 'dry_wegith' as-is
    
    # helper to coerce numerics if imported as text
    numify <- function(x) suppressWarnings(as.numeric(x))
    
    nm <- names(raw)
    guess_dry <- dplyr::coalesce(
      match("dry_wegith", nm),   # expected misspelled export
      match("dry_weight", nm),
      match("drywt", nm),
      match("dry", nm)
    )
    
    validate(
      need("prairie_unit" %in% nm, "Missing 'Prairie_Unit' column."),
      need("grasses" %in% nm, "Missing 'Grasses' column."),
      need(!is.na(guess_dry), "Missing 'Dry wegith' (dry weight) column.")
    )
    
    raw %>%
      rename(
        prairie_unit = prairie_unit,
        grasses      = grasses,
        dry_wegith   = !!sym(nm[guess_dry])
      ) %>%
      mutate(
        grasses    = numify(grasses),
        dry_wegith = numify(dry_wegith)
      )
  })
  
  # --- Unit selector (populated from data) ---
  output$unit_picker <- renderUI({
    req(dat())
    units <- sort(unique(dat()$prairie_unit))
    sel <- if ("Buffalo pasture" %in% units) "Buffalo pasture" else units[1]
    selectInput("unit", "Prairie Unit:", choices = units, selected = sel)
  })
  
  # --- Filter to selected unit and keep usable rows ---
  unit_df <- reactive({
    req(dat(), input$unit)
    dat() %>%
      filter(prairie_unit == input$unit)
  })
  
  buf <- reactive({
    req(unit_df())
    unit_df() %>%
      filter(!is.na(grasses), !is.na(dry_wegith))
  })
  
  # --- Compute AUs (single-fraction form) ---
  # AUs = [ A * sum_i( w_i * (g_i/100) * K ) ] / [ 2 * C * n ]
  au_value_num <- reactive({
    if (is.null(buf()) || nrow(buf()) == 0) return(NA_real_)
    A  <- input$acreage
    n  <- nrow(buf())
    w  <- buf()$dry_wegith
    g_prop <- buf()$grasses / 100
    
    K <- 96.033
    C <- C_reactive()
    
    num <- A * sum(w * g_prop * K, na.rm = TRUE)
    den <- 2 * C * n
    num / den
  })
  
  output$au_value <- renderText({
    if (is.null(unit_df()) || nrow(unit_df()) == 0) {
      return("No records found for the selected prairie unit.")
    }
    if (nrow(buf()) == 0) {
      return("No usable grazing rows (need non-missing Grasses and Dry wegith).")
    }
    val <- au_value_num()
    if (is.na(val)) {
      "No usable value could be calculated."
    } else {
      format(round(val, 3), big.mark = ",")
    }
  })
  
  output$n_info <- renderText({
    if (is.null(unit_df())) return("")
    paste0(
      "Selected unit: ", input$unit %||% "(none)",
      " | total records: ", nrow(unit_df()),
      " | usable for AU calc: ", if (!is.null(buf())) nrow(buf()) else 0
    )
  })
  
  empty_tbl <- datatable(
    data.frame(
      plot = character(),
      grasses = numeric(),
      dry_wegith = numeric()
    ),
    options = list(dom = 't'),
    rownames = FALSE
  )
  
  output$tbl <- renderDT({
    if (is.null(input$file)) {
      return(empty_tbl)   # show placeholder until data is uploaded
    }
    if (is.null(unit_df()) || nrow(unit_df()) == 0) {
      return(datatable(
        data.frame(Message = "No records for the selected prairie unit."),
        options = list(dom = 't')
      ))
    }
    if (nrow(buf()) == 0) {
      return(datatable(
        data.frame(Message = "No usable rows (missing Grasses or Dry wegith)."),
        options = list(dom = 't')
      ))
    }
    buf() %>%
      select(plot, grasses, dry_wegith) %>%
      datatable(options = list(pageLength = 10))
  })
  gif_server("cheer")
}

shinyApp(ui, server)