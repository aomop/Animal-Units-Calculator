# results_module.R
#
# This module combines the calculation logic with the result displays. By
# keeping the maths and the descriptive outputs together we make it easier for
# new readers to understand how the calculations connect to what they see on the
# screen.

# --- User interfaces ------------------------------------------------------
#' Sidebar summary output
#'
#' @param id Module namespace identifier.
#' @return A verbatim text output that can be dropped into the sidebar.
resultsSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::verbatimTextOutput(ns("unit_summary"))
}

#' Main results panel
#'
#' @param id Module namespace identifier.
#' @return The UI layout used in the main panel (AU value, download, formula).
resultsMainUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$hr(),
    shiny::h3(shiny::tags$span("Calculated AUs", class = "orange")),
    shiny::tags$h2(shiny::textOutput(ns("au_value")), class = "green"),
    shiny::downloadButton(ns("download_clean"), "Download clean CSV", class = "btn-success"),
    shiny::tags$hr(),
    shiny::h3(shiny::tags$span("Calculation Formula", class = "orange")),
    shiny::tags$br(),
    shiny::withMathJax(
      shiny::tags$p(
        "\\({\\Large \\text{AUs} = \\frac{A \\cdot \\sum_{i=1}^{n} (w_i \\cdot g_i \\cdot K)}{2 \\cdot C \\cdot n}}\\)",
        style = "font-weight:bold; font-size: 18px;"
      ),
      shiny::tags$p(
        shiny::tags$strong("Where:"),
        shiny::tags$ul(
          shiny::tags$li("\\(A\\) = pasture acreage"),
          shiny::tags$li("\\(w_i\\) = dry weight of sample \\(i\\) (grams per ft\\(^2\\))"),
          shiny::tags$li("\\(g_i\\) = proportion of grass in sample \\(i\\) (0-1)"),
          shiny::tags$li(shiny::tags$span("\\(K\\) = conversion constant (", shiny::textOutput(ns("k_show"), inline = TRUE), ")")),
          shiny::tags$li(shiny::tags$span("\\(C\\) = lbs of forage consumed per AU per year (", shiny::textOutput(ns("c_show"), inline = TRUE), ")")),
          shiny::tags$li("\\(n\\) = total number of samples"),
          shiny::tags$li("\\(2\\) = halving factor")
        )
      )
    )
  )
}

# --- Server logic ---------------------------------------------------------
#' Results server logic
#'
#' @param id Module namespace identifier.
#' @param data_inputs Reactive list returned from `dataInputServer()`.
#' @param parameter_inputs Reactive list returned from `parameterServer()`.
#' @param k_const Numeric conversion factor.
resultsServer <- function(id, data_inputs, parameter_inputs, k_const) {
  shiny::moduleServer(id, function(input, output, session) {
    # Keep both the raw calculation results and the inputs that produced them.
    calc_state <- shiny::reactiveValues(payload = NULL, signature = NULL)

    # The intake value is controlled by the parameter module. We keep it here so
    # we can both display it and use it inside the calculations.
    c_reactive <- shiny::reactive({
      parameter_inputs$intake()
    })

    output$k_show <- shiny::renderText({
      # Present the conversion constant with thousands separators for readability.
      format(round(k_const, 3), big.mark = ",")
    })

    output$c_show <- shiny::renderText({
      # Same story for the intake figure that comes from the parameter module.
      format(c_reactive(), big.mark = ",")
    })

    # We track a signature of the inputs. Whenever something changes the user is
    # prompted to press Calculate again so the results are reproducible.
    current_signature <- shiny::reactive({
      file_info <- data_inputs$file()
      list(
        file = if (is.null(file_info)) NULL else file_info$datapath,
        sheet = data_inputs$sheet(),
        grass = data_inputs$grass_col(),
        dry = data_inputs$dry_col(),
        acreage = parameter_inputs$acreage()
      )
    })

    # Re-run the numerical work only when the user explicitly asks for it.
    shiny::observeEvent(parameter_inputs$calculate(), {
      df_all <- data_inputs$calc_df()
      df_use <- data_inputs$usable_df()
      acreage_val <- parameter_inputs$acreage()

      # Guard against incomplete setups so the calculation step is predictable.
      shiny::validate(
        shiny::need(!is.null(acreage_val) && !is.na(acreage_val), "Enter pasture acreage before calculating."),
        shiny::need(acreage_val > 0, "Pasture acreage must be greater than zero."),
        shiny::need(nrow(df_all) > 0, "No records available from the selected sheet."),
        shiny::need(nrow(df_use) > 0, "No usable grazing rows (need non-missing values in the selected grass % and dry weight columns.)")
      )

      df_all <- dplyr::mutate(
        df_all,
        # Flag the rows that contain both inputs needed for the calculation.
        usable_for_au = !is.na(rlang::.data$grass_pct) & !is.na(rlang::.data$dry_weight),
        # Convert the percent into a proportion so the formula works as expected.
        grass_proportion = dplyr::if_else(rlang::.data$usable_for_au, rlang::.data$grass_pct / 100, NA_real_),
        # Pre-compute each row's contribution to the numerator of the AU formula.
        au_component = dplyr::if_else(rlang::.data$usable_for_au, rlang::.data$dry_weight * rlang::.data$grass_proportion * k_const, NA_real_)
      )

      calc_state$payload <- list(
        total = nrow(df_all),
        usable = nrow(df_use),
        acreage = acreage_val,
        # Summing the components now saves recomputing them on every render.
        sum_component = sum(df_all$au_component, na.rm = TRUE),
        clean_df = df_all,
        sheet = data_inputs$sheet(),
        unit_label = data_inputs$unit_label()
      )
      calc_state$signature <- current_signature()
    }, ignoreNULL = TRUE)

    output$au_value <- shiny::renderText({
      file_info <- data_inputs$file()
      if (is.null(file_info)) {
        return("Upload a spreadsheet to begin.")
      }

      sheets <- data_inputs$sheets()
      if (length(sheets) == 0) {
        return("No sheets available in the uploaded file.")
      }

      current_sheet <- data_inputs$sheet()
      if (is.null(current_sheet) || !nzchar(current_sheet) || !(current_sheet %in% sheets)) {
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

      den <- 2 * c_reactive() * payload$usable  # denominator of the AU equation
      val <- payload$acreage * payload$sum_component / den
      if (is.na(val)) {
        "No usable value could be calculated."
      } else {
        format(round(val, 3), big.mark = ",")
      }
    })

    output$unit_summary <- shiny::renderText({
      file_info <- data_inputs$file()
      if (is.null(file_info)) {
        return("")
      }

      sheets <- data_inputs$sheets()
      current_sheet <- data_inputs$sheet()
      if (length(sheets) == 0 || is.null(current_sheet) || !nzchar(current_sheet) || !(current_sheet %in% sheets)) {
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

      label_input <- trimws(if (is.null(payload$unit_label)) "" else payload$unit_label)
      label_display <- if (label_input == "") "(no unit name provided)" else label_input

      paste0(
        "Unit name: ", label_display,
        " | total records: ", payload$total,
        " | usable for AU calc: ", payload$usable
      )
    })

    output$download_clean <- shiny::downloadHandler(
      filename = function() {
        file_info <- data_inputs$file()
        base <- if (!is.null(file_info) && !is.null(file_info$name)) {
          tools::file_path_sans_ext(basename(file_info$name))
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
          shiny::showNotification("Recalculate before downloading clean results.", type = "error")
          return(invisible(NULL))
        }

        if (payload$usable <= 0) {
          shiny::showNotification("No usable records available for download.", type = "error")
          return(invisible(NULL))
        }

        den <- 2 * c_reactive() * payload$usable
        au_val <- payload$acreage * payload$sum_component / den

        clean_df <- payload$clean_df %>%
          dplyr::transmute(
            plot = rlang::.data$plot,
            grass_pct = rlang::.data$grass_pct,
            dry_weight = rlang::.data$dry_weight,
            usable_for_au = rlang::.data$usable_for_au,
            au_component = rlang::.data$au_component,
            # Stamp the overall AU estimate on every row for easy reference.
            au_estimate = au_val
          ) %>%
          dplyr::mutate(plot = as.character(rlang::.data$plot)) %>%
          dplyr::filter(!is.na(rlang::.data$grass_pct))

        label_input <- trimws(if (is.null(payload$unit_label)) "" else payload$unit_label)
        # Build a single line of metadata so exported CSVs carry context.
        header_parts <- c(
          if (nzchar(label_input)) paste0("Unit: ", label_input) else NULL,
          paste0("Acreage: ", payload$acreage),
          paste0("Usable samples: ", payload$usable),
          paste0("Intake (lbs/AU): ", c_reactive()),
          paste0("Conversion constant (K): ", k_const)
        )

        con <- file(file, open = "wt")
        writeLines(paste(header_parts, collapse = "|"), con)

        utils::write.table(clean_df,
                    con,
                    sep = ",",
                    row.names = FALSE,
                    col.names = TRUE,  # still write the column labels
                    na = "",
                    qmethod = "double",
                    append = TRUE)

        close(con)
      }
    )

    # Provide a list of helper reactives for other modules (like the table) to
    # reuse if needed in the future.
    list(
      payload = shiny::reactive(calc_state$payload),     # full calculation details
      signature = shiny::reactive(calc_state$signature), # inputs that produced payload
      c_value = c_reactive                         # expose current intake choice
    )
  })
}
