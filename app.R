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
      helpText("Select the sheet and columns that contain grass percent and dry weight data."),
      selectInput("sheet", "Choose sheet:", choices = character(0)),
      textInput("unit_label", "Unit name (optional):", value = ""),
      selectInput("grass_col", "Grass % column:", choices = NULL),
      selectInput("dry_col", "Dry weight column:", choices = NULL),
      # --- Parameters ---
      tags$hr(),
      h3(tags$span("Parameters", class = "orange")),
      numericInput("acreage", "Pasture acreage (A):", value = NA, min = 0, step = 0.5),
      actionButton("calculate", "Calculate", class = "btn-primary"),
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
        downloadButton("download_clean", "Download clean CSV", class = "btn-success"),
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
  sheet_choices <- reactiveVal(character(0))
  calc_state <- reactiveValues(payload = NULL, signature = NULL)

  observeEvent(input$file, {
    file <- input$file
    if (is.null(file)) {
      sheet_choices(character(0))
      calc_state$payload <- NULL
      calc_state$signature <- NULL
      return()
    }

    sheets <- tryCatch(
      readxl::excel_sheets(file$datapath),
      error = function(e) {
        showNotification(
          paste("Unable to read sheets from the uploaded file:", e$message),
          type = "error"
        )
        character(0)
      }
    )

    sheet_choices(sheets)
    calc_state$payload <- NULL
    calc_state$signature <- NULL
  })

  observe({
    sheets <- sheet_choices()
    selected <- NULL
    if (length(sheets) > 0) {
      if (!is.null(input$sheet) && input$sheet %in% sheets) {
        selected <- input$sheet
      } else {
        selected <- sheets[1]
      }
    }

    updateSelectInput(
      session,
      "sheet",
      choices = sheets,
      selected = selected
    )
  })

  # --- Read selected sheet and clean ---
  dat <- reactive({
    req(input$file)
    sheets <- sheet_choices()

    validate(
      need(length(sheets) > 0, "No sheets available in the uploaded file."),
      need(!is.null(input$sheet) && nzchar(input$sheet), "Select a sheet to load."),
      need(input$sheet %in% sheets, "Selected sheet not found in the uploaded file. Please choose another sheet.")
    )

    raw <- tryCatch(
      read_excel(input$file$datapath, sheet = input$sheet),
      error = function(e) {
        validate(need(FALSE, paste0("Unable to read sheet '", input$sheet, "': ", e$message)))
      }
    )
    raw <- clean_names(raw)  # keeps 'dry_wegith' as-is

    validate(
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
  
  # --- Keep usable rows ---
  usable_df <- reactive({
    req(calc_df())
    calc_df() %>%
      filter(!is.na(grass_pct), !is.na(dry_weight))
  })
  
  # --- Compute AUs (single-fraction form) ---
  # AUs = [ A * sum_i( w_i * (g_i/100) * K ) ] / [ 2 * C * n ]
  current_signature <- reactive({
    list(
      file = if (is.null(input$file)) NULL else input$file$datapath,
      sheet = if (is.null(input$sheet)) NULL else input$sheet,
      grass = if (is.null(input$grass_col)) NULL else input$grass_col,
      dry = if (is.null(input$dry_col)) NULL else input$dry_col,
      acreage = suppressWarnings(as.numeric(input$acreage))
    )
  })

  observeEvent(input$calculate, {
    df_all <- calc_df()
    df_use <- usable_df()

    acreage_val <- suppressWarnings(as.numeric(input$acreage))
    validate(
      need(!is.null(acreage_val) && !is.na(acreage_val), "Enter pasture acreage before calculating."),
      need(acreage_val > 0, "Pasture acreage must be greater than zero."),
      need(nrow(df_all) > 0, "No records available from the selected sheet."),
      need(nrow(df_use) > 0, "No usable grazing rows (need non-missing values in the selected grass % and dry weight columns.)")
    )

    df_all <- df_all %>%
      mutate(
        usable_for_au = !is.na(grass_pct) & !is.na(dry_weight),
        grass_proportion = if_else(usable_for_au, grass_pct / 100, NA_real_),
        au_component = if_else(usable_for_au, dry_weight * grass_proportion * K_CONST, NA_real_)
      )

    sum_component <- sum(df_all$au_component, na.rm = TRUE)

    calc_state$payload <- list(
      total = nrow(df_all),
      usable = nrow(df_use),
      acreage = acreage_val,
      sum_component = sum_component,
      clean_df = df_all,
      sheet = input$sheet
    )
    calc_state$signature <- current_signature()
  }, ignoreNULL = TRUE)

  output$au_value <- renderText({
    if (is.null(input$file)) {
      return("Upload a spreadsheet to begin.")
    }
    sheets <- sheet_choices()
    if (length(sheets) == 0) {
      return("No sheets available in the uploaded file.")
    }
    if (is.null(input$sheet) || !nzchar(input$sheet) || !(input$sheet %in% sheets)) {
      return("Selected sheet not found in the uploaded file. Please choose another sheet.")
    }
    payload <- calc_state$payload
    signature <- calc_state$signature
    current_sig <- current_signature()

    if (is.null(payload) || is.null(signature)) {
      return("Click Calculate to compute AUs.")
    }

    if (!identical(signature, current_sig)) {
      return("Inputs changed. Click Calculate to update results.")
    }

    if (payload$usable <= 0) {
      return("No usable value could be calculated.")
    }

    den <- 2 * C_reactive() * payload$usable
    val <- payload$acreage * payload$sum_component / den
    if (is.na(val)) {
      "No usable value could be calculated."
    } else {
      format(round(val, 3), big.mark = ",")
    }
  })

  output$n_info <- renderText({
    if (is.null(input$file)) {
      return("")
    }
    sheets <- sheet_choices()
    if (length(sheets) == 0 || is.null(input$sheet) || !nzchar(input$sheet) || !(input$sheet %in% sheets)) {
      return("")
    }
    payload <- calc_state$payload
    signature <- calc_state$signature
    current_sig <- current_signature()

    if (is.null(payload) || is.null(signature)) {
      return("Click Calculate to view unit summary.")
    }

    if (!identical(signature, current_sig)) {
      return("Inputs changed. Click Calculate to update results.")
    }

    label_input <- trimws(if (is.null(input$unit_label)) "" else input$unit_label)
    label_display <- if (label_input == "") "(no unit name provided)" else label_input

    paste0(
      "Unit name: ", label_display,
      " | total records: ", payload$total,
      " | usable for AU calc: ", payload$usable
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
    if (is.null(calc_df()) || nrow(calc_df()) == 0) {
      return(datatable(
        data.frame(Message = "No records available from the selected sheet."),
        options = list(dom = 't')
      ))
    }
    if (nrow(usable_df()) == 0) {
      return(datatable(
        data.frame(Message = "No usable rows (missing values in selected grass % or dry weight columns)."),
        options = list(dom = 't')
      ))
    }
    display_df <- usable_df()
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

  output$download_clean <- downloadHandler(
    filename = function() {
      base <- if (!is.null(input$file) && !is.null(input$file$name)) {
        tools::file_path_sans_ext(basename(input$file$name))
      } else {
        "au_results"
      }

      payload <- calc_state$payload
      sheet <- if (!is.null(payload)) payload$sheet else NULL
      sheet_suffix <- if (!is.null(sheet) && nzchar(sheet)) paste0("_", sheet) else ""
      paste0(base, sheet_suffix, "_clean.csv")
    },
    content = function(file) {
      payload <- calc_state$payload
      signature <- calc_state$signature
      current_sig <- current_signature()

      if (is.null(payload) || is.null(signature) || !identical(signature, current_sig)) {
        showNotification("Recalculate before downloading clean results.", type = "error")
        return(invisible(NULL))
      }

      if (payload$usable <= 0) {
        showNotification("No usable records available for download.", type = "error")
        return(invisible(NULL))
      }

      den <- 2 * C_reactive() * payload$usable
      au_val <- payload$acreage * payload$sum_component / den

      clean_df <- payload$clean_df
      clean_df$acreage_used <- payload$acreage
      clean_df$intake_lbs_per_au <- C_reactive()
      clean_df$au_estimate <- au_val

      write.csv(clean_df, file, row.names = FALSE)
    }
  )
  gif_server("cheer")
}

shinyApp(ui, server)