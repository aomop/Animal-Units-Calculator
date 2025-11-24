# data_input_module.R
#
# This module bundles together everything related to getting raw data into the
# app. Keeping the file upload, sheet picker, and column selectors in one place
# makes it easier for new Shiny developers to follow how the information flows
# from the UI into the calculations.

# --- User interface -------------------------------------------------------
#' Data upload and column selection UI
#'
#' @name dataInputUI
#' @param id Module namespace identifier.
#' @return A list of UI elements that render the file upload tools.
dataInputUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fileInput(ns("file"), "Upload Excel (.xlsx/.xls)", accept = c(".xlsx", ".xls")),
    shiny::helpText("Select the sheet and columns that contain grass percent and dry weight data."),
    shiny::selectInput(ns("sheet"), "Choose sheet:", choices = character(0)),
    shiny::textInput(ns("unit_label"), "Unit name (optional):", value = ""),
    shiny::selectInput(ns("grass_col"), "Grass % column:", choices = NULL),
    shiny::selectInput(ns("dry_col"), "Dry weight column:", choices = NULL)
  )
}

# --- Server logic ---------------------------------------------------------
#' Data upload server logic
#'
#' @name dataInputServer
#' @param id Module namespace identifier.
#' @return A list of reactive helpers that expose the cleaned dataset and the
#'         selected column names to the rest of the app.
dataInputServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Holds the available sheet names so both the UI and validation can reuse
    # the same source of truth.
    sheet_choices <- shiny::reactiveVal(character(0))

    # When a file is uploaded we list out the available sheets. This mirrors the
    # behaviour from the single-file app but keeps the scope inside the module.
    shiny::observeEvent(input$file, {
      file <- input$file
      if (is.null(file)) {
        sheet_choices(character(0))
        return()
      }

      sheets <- tryCatch(
        readxl::excel_sheets(file$datapath),
        error = function(e) {
          shiny::showNotification(
            paste("Unable to read sheets from the uploaded file:", e$message),
            type = "error"
          )
          character(0)
        }
      )

      sheet_choices(sheets)
    })

    # Keep the sheet select box in sync with whatever is available.
    shiny::observe({
      sheets <- sheet_choices()
      selected <- NULL
      if (length(sheets) > 0) {
        if (!is.null(input$sheet) && input$sheet %in% sheets) {
          selected <- input$sheet
        } else {
          selected <- sheets[1]
        }
      }

      # Send the freshly computed sheet names back to the UI input.
      shiny::updateSelectInput(session, "sheet", choices = sheets, selected = selected)
    })

    # Read in the selected sheet, keeping the names tidy so users can work with
    # consistent column names.
    dat <- shiny::reactive({
      shiny::req(input$file)
      sheets <- sheet_choices()

      shiny::validate(
        shiny::need(length(sheets) > 0, "No sheets available in the uploaded file."),
        shiny::need(!is.null(input$sheet) && nzchar(input$sheet), "Select a sheet to load."),
        shiny::need(input$sheet %in% sheets, "Selected sheet not found in the uploaded file. Please choose another sheet.")
      )

      raw <- tryCatch(
        readxl::read_excel(input$file$datapath, sheet = input$sheet),
        error = function(e) {
          shiny::validate(shiny::need(FALSE, paste0("Unable to read sheet '", input$sheet, "': ", e$message)))
        }
      )

      raw <- janitor::clean_names(raw)
      shiny::validate(shiny::need(nrow(raw) > 0, "Uploaded sheet has no rows."))
      raw
    })

    # Small helper that tries to coerce values into numerics even if they were
    # imported as text. It is defined here so that both the guessing logic and
    # the downstream calculations can use the same behaviour.
    numify <- function(x) {
      if (is.numeric(x)) return(as.numeric(x))
      suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
    }

    guess_column <- function(nm, patterns, exclude = character()) {
      # Quickly search column names for the first match to any pattern.
      candidates <- setdiff(nm, exclude)
      for (pat in patterns) {
        hits <- candidates[grepl(pat, candidates, ignore.case = TRUE)]
        if (length(hits) > 0) return(hits[1])
      }
      if (length(candidates) > 0) candidates[1] else NULL
    }

    # Once data is available we suggest sensible defaults for the grass and dry
    # weight columns so that beginners do not have to guess.
    shiny::observeEvent(dat(), {
      shiny::req(dat())
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

      # Push the guesses back into the UI so users can accept or override them.
      shiny::updateSelectInput(session, "grass_col", choices = nm, selected = selected_grass)
      shiny::updateSelectInput(session, "dry_col", choices = nm, selected = selected_dry)
    }, ignoreNULL = FALSE)

    # The cleaned dataset keeps the user-selected columns available in a tidy
    # format that downstream modules can depend on.
    calc_df <- shiny::reactive({
      shiny::req(dat(), input$grass_col, input$dry_col)
      df <- dat()
      shiny::validate(
        shiny::need(input$grass_col %in% names(df), "Selected grass column missing from data."),
        shiny::need(input$dry_col %in% names(df), "Selected dry weight column missing from data.")
      )
      
      # Create consistently named numeric columns for downstream modules.
      df$grass_pct  <- numify(df[[input$grass_col]])
      df$dry_weight <- numify(df[[input$dry_col]])
      
      # --- NEW: normalize grass to 0-100% if user uploaded proportions (0-1) ---
      if (any(!is.na(df$grass_pct))) {
        # Heuristics: either all values ≤ 1.05 OR at least 80% of non-NA values ≤ 1
        prop_like <- (max(df$grass_pct, na.rm = TRUE) <= 1.05) ||
          (mean(df$grass_pct <= 1, na.rm = TRUE) >= 0.80)
        
        if (prop_like) {
          df$grass_pct <- df$grass_pct * 100
          shiny::showNotification(
            "Interpreting grass values as proportions (0-1). Converted to percents (0-100).",
            type = "message"
          )
        }
      }
      
      # Clamp to a sensible range and notify if anything was out of bounds
      if (any(df$grass_pct < 0 | df$grass_pct > 100, na.rm = TRUE)) {
        shiny::showNotification(
          "Some grass % values were outside 0-100; clamped to bounds.",
          type = "warning"
        )
        df$grass_pct <- pmin(pmax(df$grass_pct, 0), 100)
      }
      
      df
    })
    
    usable_df <- shiny::reactive({
      shiny::req(calc_df())
      # Remove rows missing either component because they cannot contribute to
      # the AU computation.
      dplyr::filter(calc_df(), !is.na(.data$grass_pct), !is.na(.data$dry_weight))
    })

    list(
      file = shiny::reactive(input$file),
      sheet = shiny::reactive(input$sheet),
      unit_label = shiny::reactive(input$unit_label),
      grass_col = shiny::reactive(input$grass_col),
      dry_col = shiny::reactive(input$dry_col),
      dat = dat,
      calc_df = calc_df,
      usable_df = usable_df,
      sheets = shiny::reactive(sheet_choices())
    )
  })
}
