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
  ns <- NS(id)
  verbatimTextOutput(ns("unit_summary"))
}

#' Main results panel
#'
#' @param id Module namespace identifier.
#' @return The UI layout used in the main panel (AU value, download, formula).
resultsMainUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$hr(),
    h3(tags$span("Calculated AUs", class = "orange")),
    tags$h2(textOutput(ns("au_value")), class = "green"),
    downloadButton(ns("download_clean"), "Download clean CSV", class = "btn-success"),
    tags$hr(),
    h3(tags$span("Calculation Formula", class = "orange")),
    tags$br(),
    withMathJax(
      tags$p(
        "\\({\\Large \\text{AUs} = \\frac{A \\cdot \\sum_{i=1}^{n} (w_i \\cdot g_i \\cdot K)}{2 \\cdot C \\cdot n}}\\)",
        style = "font-weight:bold; font-size: 18px;"
      ),
      tags$p(
        tags$strong("Where:"),
        tags$ul(
          tags$li("\\(A\\) = pasture acreage"),
          tags$li("\\(w_i\\) = dry weight of sample \\(i\\) (grams per ft\\(^2\\))"),
          tags$li("\\(g_i\\) = proportion of grass in sample \\(i\\) (0–1)"),
          tags$li(tags$span("\\(K\\) = conversion constant (", textOutput(ns("k_show"), inline = TRUE), ")")),
          tags$li(tags$span("\\(C\\) = lbs of forage consumed per AU per year (", textOutput(ns("c_show"), inline = TRUE), ")")),
          tags$li("\\(n\\) = total number of samples"),
          tags$li("\\(2\\) = halving factor")
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
  moduleServer(id, function(input, output, session) {
    calc_state <- reactiveValues(payload = NULL, signature = NULL)

    # The intake value is controlled by the parameter module. We keep it here so
    # we can both display it and use it inside the calculations.
    c_reactive <- reactive({
      parameter_inputs$intake()
    })

    output$k_show <- renderText({
      format(round(k_const, 3), big.mark = ",")
    })

    output$c_show <- renderText({
      format(c_reactive(), big.mark = ",")
    })

    # We track a signature of the inputs. Whenever something changes the user is
    # prompted to press Calculate again so the results are reproducible.
    current_signature <- reactive({
      file_info <- data_inputs$file()
      list(
        file = if (is.null(file_info)) NULL else file_info$datapath,
        sheet = data_inputs$sheet(),
        grass = data_inputs$grass_col(),
        dry = data_inputs$dry_col(),
        acreage = parameter_inputs$acreage()
      )
    })

    observeEvent(parameter_inputs$calculate(), {
      df_all <- data_inputs$calc_df()
      df_use <- data_inputs$usable_df()
      acreage_val <- parameter_inputs$acreage()

      validate(
        need(!is.null(acreage_val) && !is.na(acreage_val), "Enter pasture acreage before calculating."),
        need(acreage_val > 0, "Pasture acreage must be greater than zero."),
        need(nrow(df_all) > 0, "No records available from the selected sheet."),
        need(nrow(df_use) > 0, "No usable grazing rows (need non-missing values in the selected grass % and dry weight columns.)")
      )

      df_all <- dplyr::mutate(
        df_all,
        usable_for_au = !is.na(grass_pct) & !is.na(dry_weight),
        grass_proportion = dplyr::if_else(usable_for_au, grass_pct / 100, NA_real_),
        au_component = dplyr::if_else(usable_for_au, dry_weight * grass_proportion * k_const, NA_real_)
      )

      calc_state$payload <- list(
        total = nrow(df_all),
        usable = nrow(df_use),
        acreage = acreage_val,
        sum_component = sum(df_all$au_component, na.rm = TRUE),
        clean_df = df_all,
        sheet = data_inputs$sheet(),
        unit_label = data_inputs$unit_label()
      )
      calc_state$signature <- current_signature()
    }, ignoreNULL = TRUE)

    output$au_value <- renderText({
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

      den <- 2 * c_reactive() * payload$usable
      val <- payload$acreage * payload$sum_component / den
      if (is.na(val)) {
        "No usable value could be calculated."
      } else {
        format(round(val, 3), big.mark = ",")
      }
    })

    output$unit_summary <- renderText({
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

    output$download_clean <- downloadHandler(
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
          showNotification("Recalculate before downloading clean results.", type = "error")
          return(invisible(NULL))
        }

        if (payload$usable <= 0) {
          showNotification("No usable records available for download.", type = "error")
          return(invisible(NULL))
        }

        den <- 2 * c_reactive() * payload$usable
        au_val <- payload$acreage * payload$sum_component / den

        clean_df <- payload$clean_df
        clean_df$acreage_used <- payload$acreage
        clean_df$intake_lbs_per_au <- c_reactive()
        clean_df$au_estimate <- au_val

        write.csv(clean_df, file, row.names = FALSE)
      }
    )

    # Provide a list of helper reactives for other modules (like the table) to
    # reuse if needed in the future.
    list(
      payload = reactive(calc_state$payload),
      signature = reactive(calc_state$signature),
      c_value = c_reactive
    )
  })
}
