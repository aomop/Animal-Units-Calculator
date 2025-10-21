# table_module.R
#
# This module renders the data table that lets users review the cleaned grazing
# records. Isolating the table logic keeps the main server function tidy.

# --- User interface -------------------------------------------------------
#' Data table UI
#'
#' @param id Module namespace identifier.
#' @return A DT output widget.
tableUI <- function(id) {
  ns <- NS(id)
  DTOutput(ns("tbl"))
}

# --- Server logic ---------------------------------------------------------
#' Data table server logic
#'
#' @param id Module namespace identifier.
#' @param data_inputs Reactive list returned from `dataInputServer()`.
tableServer <- function(id, data_inputs) {
  moduleServer(id, function(input, output, session) {
    # Pre-build a small datatable so we can reuse it when nothing is uploaded.
    empty_tbl <- datatable(
      data.frame(Message = "Upload a spreadsheet to begin."),
      options = list(dom = 't'),
      rownames = FALSE
    )

    output$tbl <- renderDT({
      file_info <- data_inputs$file()
      if (is.null(file_info)) {
        return(empty_tbl)
      }

      calc_df <- data_inputs$calc_df()
      if (is.null(calc_df) || nrow(calc_df) == 0) {
        # Inform the user that the sheet itself did not produce any rows.
        return(datatable(
          data.frame(Message = "No records available from the selected sheet."),
          options = list(dom = 't')
        ))
      }

      usable_df <- data_inputs$usable_df()
      if (nrow(usable_df) == 0) {
        # Acknowledge when all rows were filtered because of missing values.
        return(datatable(
          data.frame(Message = "No usable rows (missing values in selected grass % or dry weight columns)."),
          options = list(dom = 't')
        ))
      }

      display_df <- usable_df %>%
        dplyr::mutate(
          # Rename columns into friendly labels without changing the raw data.
          `Grass %` = grass_pct,
          `Dry weight (g/ft^2)` = dry_weight
        )

      preferred <- intersect("plot", names(display_df))
      display_cols <- c(preferred, "prairie_unit", "Grass %", "Dry weight (g/ft^2)")
      # Keep only the columns that actually exist, preserving the column order.
      display_cols <- display_cols[display_cols %in% names(display_df)]

      display_df %>%
        dplyr::select(dplyr::all_of(display_cols)) %>%
        datatable(options = list(pageLength = 10, autoWidth = TRUE), rownames = FALSE)
    })
  })
}
