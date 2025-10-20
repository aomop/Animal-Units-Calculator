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
      helpText("Required columns: Prairie_Unit plus selectable grass percent and dry weight columns."),
      uiOutput("sheet_picker"),
      uiOutput("unit_picker"),
      selectInput("grass_col", "Grass % column:", choices = NULL),
      selectInput("dry_col", "Dry weight column:", choices = NULL),
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
                  tags$li("\\(w_i\\) = dry weight of sample \\(i\\) (grams per ft\\(^2\\))"),
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
    format(round(K_CONST, 3), big.mark = ",")
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

    nm <- names(raw)
    validate(
      need("prairie_unit" %in% nm, "Missing 'Prairie_Unit' column."),
      need(nrow(raw) > 0, "Uploaded sheet has no rows.")
    )
    raw
  })

  # helper to coerce numerics if imported as text
  numify <- function(x) {
    if (is.numeric(x)) return(as.numeric(x))
    suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
  }

  guess_column <- function(nm, patterns, exclude = character()) {
    candidates <- setdiff(nm, exclude)
    for (pat in patterns) {
      hits <- candidates[grepl(pat, candidates, ignore.case = TRUE)]
      if (length(hits) > 0) return(hits[1])
    }
    if (length(candidates) > 0) candidates[1] else NULL
  }

  observeEvent(dat(), {
    req(dat())
    nm <- names(dat())
    if (!length(nm)) return(NULL)

    grass_guess <- guess_column(
      nm,
      patterns = c(
        "grass.*(pct|percent|percentage|prop)",
        "(pct|percent).*grass",
        "^grasses$",
        "^grass$",
        "grasses",
        "grass"
      ),
      exclude = c("prairie_unit")
    )

    dry_guess <- guess_column(
      nm,
      patterns = c(
        "^dry_wegith$",
        "dry.*weight",
        "dry.*wt",
        "weight.*dry",
        "gram",
        "g_sq",
        "gper",
        "dry"
      ),
      exclude = c("prairie_unit", grass_guess)
    )

    fallback_grass <- setdiff(nm, "prairie_unit")
    if (!length(fallback_grass)) fallback_grass <- nm
    fallback_grass <- fallback_grass[1]

    fallback_dry <- setdiff(nm, c("prairie_unit", grass_guess))
    if (!length(fallback_dry)) fallback_dry <- setdiff(nm, "prairie_unit")
    if (!length(fallback_dry)) fallback_dry <- nm
    fallback_dry <- fallback_dry[1]

    selected_grass <- if (is.null(grass_guess)) fallback_grass else grass_guess
    selected_dry <- if (is.null(dry_guess)) fallback_dry else dry_guess

    updateSelectInput(
      session,
      "grass_col",
      choices = nm,
      selected = selected_grass
    )

    updateSelectInput(
      session,
      "dry_col",
      choices = nm,
      selected = selected_dry
    )
  }, ignoreNULL = FALSE)

  calc_df <- reactive({
    req(dat(), input$grass_col, input$dry_col)
    df <- dat()
    validate(
      need(input$grass_col %in% names(df), "Selected grass column missing from data."),
      need(input$dry_col %in% names(df), "Selected dry weight column missing from data.")
    )

    df$grass_pct <- numify(df[[input$grass_col]])
    df$dry_weight <- numify(df[[input$dry_col]])
    df
  })
  
  # --- Unit selector (populated from data) ---
  output$unit_picker <- renderUI({
    req(dat())
    units <- sort(unique(dat()$prairie_unit))
    units <- units[!is.na(units)]
    if (!length(units)) return(NULL)
    sel <- if ("Buffalo pasture" %in% units) "Buffalo pasture" else units[1]
    selectInput("unit", "Prairie Unit:", choices = units, selected = sel)
  })

  # --- Filter to selected unit and keep usable rows ---
  unit_df <- reactive({
    req(calc_df(), input$unit)
    calc_df() %>%
      filter(prairie_unit == input$unit)
  })

  buf <- reactive({
    req(unit_df())
    unit_df() %>%
      filter(!is.na(grass_pct), !is.na(dry_weight))
  })
  
  # --- Compute AUs (single-fraction form) ---
  # AUs = [ A * sum_i( w_i * (g_i/100) * K ) ] / [ 2 * C * n ]
  au_value_num <- reactive({
    if (is.null(buf()) || nrow(buf()) == 0) return(NA_real_)
    A  <- input$acreage
    n  <- nrow(buf())
    w  <- buf()$dry_weight
    g_prop <- buf()$grass_pct / 100

    K <- K_CONST
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
      return("No usable grazing rows (need non-missing values in the selected grass % and dry weight columns.)")
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
    data.frame(Message = "Upload a spreadsheet to begin."),
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
        data.frame(Message = "No usable rows (missing values in selected grass % or dry weight columns)."),
        options = list(dom = 't')
      ))
    }
    display_df <- unit_df()
    display_df <- display_df %>%
      mutate(
        `Grass %` = grass_pct,
        `Dry weight (g/ft^2)` = dry_weight
      )

    preferred <- intersect("plot", names(display_df))
    display_cols <- c(preferred, "prairie_unit", "Grass %", "Dry weight (g/ft^2)")
    display_cols <- display_cols[display_cols %in% names(display_df)]

    display_df %>%
      select(all_of(display_cols)) %>%
      datatable(options = list(pageLength = 10, autoWidth = TRUE), rownames = FALSE)
  })
  gif_server("cheer")
}

shinyApp(ui, server)