# Helper to create fixture Excel files for tests
fixtures_dir <- testthat::test_path("fixtures")
if (!dir.exists(fixtures_dir)) dir.create(fixtures_dir, recursive = TRUE)

base_df <- data.frame(
  Plot = 1:3,
  Prairie_Unit = "Buffalo pasture",
  Grasses = c(50, 60, 70),
  `Dry wegith` = c(100, 120, 80),
  check.names = FALSE
)

writexl::write_xlsx(base_df, file.path(fixtures_dir, "sample_good.xlsx"))

alt_df <- base_df
alt_df$Prairie_Unit <- "Other pasture"
writexl::write_xlsx(alt_df, file.path(fixtures_dir, "sample_altname.xlsx"))

missing_dry_df <- base_df[c("Plot", "Prairie_Unit", "Grasses")]
writexl::write_xlsx(missing_dry_df, file.path(fixtures_dir, "sample_missing_dry.xlsx"))
