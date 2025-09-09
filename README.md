# Animal Units Calculator

Animal Units (AUs) — Grazing Calculator is an R Shiny app for estimating grazing animal units from vegetation sampling data.

## Key features
- Upload Excel workbooks and pick a sheet and prairie unit for analysis
- Enter pasture acreage and choose an annual intake assumption to tune the AU formula
- Calculates AUs using \(A\), dry weight, grass proportion, conversion constant \(K\) and intake constant \(C\)
- Displays data tables of usable samples and explains when data are missing
- Optional button that shows a random motivational GIF and message

## Screenshots / demo
*TODO: Add screenshots of the app.*

## Quick start (TL;DR)
```r
renv::restore()   # install packages
shiny::runApp('app.R')
```

## Requirements
- R 4.4.2 (per `renv.lock`)
- Packages: shiny, readxl, dplyr, DT, janitor, bslib

## Installation & setup
This project uses **renv** for dependency management. Restore the locked package versions with:
```r
renv::restore()
```
`renv/settings.json` contains project-specific settings.

## System requirements
No external system libraries are specified in the repository. *TODO: document any OS-level dependencies if they arise.*

## Configuration
- Upload Excel files containing `Prairie_Unit`, `Grasses` (%), and `Dry wegith` (lbs) columns.
- Select acreage and intake basis (10,950 or 9,490 lb/year).
- A `cheerfulgif-assets/` folder with GIFs is expected for the motivational button. *TODO: provide these assets or document where to obtain them.*

## Running the app
From an R console:
```r
shiny::runApp('app.R')
```
In RStudio, open `app.R` and click **Run App**.

## Options/flags
Use `options(shiny.port = 3838)` or `shiny::runApp(port = 3838)` to change the listening port.

## Testing & QA
```r
lintr::lint('app.R')
```
*TODO: Add automated tests.*

## Project structure
- `app.R` – main Shiny application
- `modules/gif_module.R` – motivational GIF module
- `renv/` and `renv.lock` – package management via renv

## Usage guide
1. Upload an Excel file.
2. Choose the worksheet and prairie unit.
3. Review calculated AUs and sample table.
4. Click the **Cheer me up** button for a random GIF.

## Performance & ops
*TODO: Document performance expectations or deployment strategies.*

## Security & privacy
User-uploaded data remain in memory only; do not upload sensitive information. *TODO: expand this section if authentication or logging is added.*

## Troubleshooting
- **Missing columns**: The app warns when required columns are absent.
- **No usable rows**: Ensure `Grasses` and `Dry wegith` contain numeric data.

## Contributing
Authored by **Sam Swanson**. Please open an issue or pull request for improvements.

## Coding standards
No explicit lintr or style guide is defined. Use `lintr` to check for common issues.

## Changelog / Releases
*TODO: Initialize changelog once releases are made.*

## License
This project is licensed under the [MIT License](LICENSE).

## Acknowledgments / Citation
If you use this app in research, cite as:
> Swanson, S. (2024). *Animal Units Calculator*. R package/shiny application.

