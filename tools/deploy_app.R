options(renv.config.auto.activate = FALSE)
Sys.unsetenv("RENV_PROJECT")

options(repos = c(CRAN = "https://cran.r-project.org"))

getOption("repos")

rsconnect::deployApp(
  appDir        = ".",
  appName       = "GrazingCalculator",
  account       = "smsc2",
  server        = "shinyapps.io",
  appFiles      = c(
    "app.R", "R", "inst", "DESCRIPTION", "NAMESPACE"
  )
)
