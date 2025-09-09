# Animal Units Calculator

This is an R Shiny app for estimating grazing animal units from vegetation sampling data.

## Key features
- Upload Excel workbooks and pick a sheet and prairie unit for analysis
- Enter pasture acreage and choose an annual intake assumption to tune the AU formula
- Calculates AUs using \(A\), dry weight, grass proportion, conversion constant \(K\) and intake constant \(C\)
- Displays data tables of usable samples and explains when data are missing
- Optional button that shows a random motivational GIF and message
  
## Requirements
- R 4.4.2 (per `renv.lock`)
- Packages: shiny, readxl, dplyr, DT, janitor, bslib

## Installation & setup
This project uses **renv** for dependency management. Restore the locked package versions with:
```r
renv::restore()
```
`renv/settings.json` contains project-specific settings.

## Running the app
From an R console:
```r
shiny::runApp('app.R')
```
In RStudio, open `app.R` and click **Run App**.

## Project structure
- `app.R` – main Shiny application
- `modules/gif_module.R` – motivational GIF module
- `renv/` and `renv.lock` – package management via renv

## Usage guide
1. Upload an Excel file.
2. Choose the worksheet and prairie unit.
3. Review calculated AUs and sample table.
4. Click the **Cheer me up** button for a random GIF.

## Contributing
Authored by **Sam Swanson**. Please open an issue or pull request for improvements.

## License
This project is licensed under the [MIT License](LICENSE).

## Acknowledgments / Citation
If you use this app in research, please cite as:
> Swanson, S. (2024). *Animal Units Calculator*. R package/shiny application.

