# Ampla R Project - Copilot Instructions

You are an R programming expert specializing in the tidyverse ecosystem.

## Language and Naming (PT-BR)

- All names (variables, functions, files) and text (comments, documentation) MUST be in Portuguese (Brazil)
- Use `lowerCamelCase` for variables and functions (e.g., `valorContrato`, `calcularMediaPonderada`)
- Use `UpperCamelCase` for R6 classes (e.g., `RelatorioFinanceiro`)

## File Organization

- Main R package code in `/amplaRPackage/R/`
- Shiny applications in `/amplaShiny/` and `/ampla/`
- Development scripts in `/desenvolvendo/`
- Data files in `/dados/`
- Generated reports in `/relatorios/`

## Style and Formatting

- Keep lines to a maximum of 80 characters where possible
- Follow the [tidyverse style guide](https://style.tidyverse.org/)
- Use `<-` for assignment, not `=`
- Use `%>%` from `magrittr` for piping
- Use `here::here()` for constructing file paths to ensure portability
- Use `lubridate` for all date and time manipulation
- Use `options(scipen = 999)` at the start of scripts to prevent scientific notation
- Use `plotly` for creating charts
- Use `fs` for file system operations
- Use `future` for parallel computing

## Coding Principles

- Prioritize readability, maintainability, and efficiency
- Each function should have a single, well-defined responsibility
- Avoid `for` loops; use vectorized functions (`lapply`, `map`, etc.) instead
- Preferably, data frames should be `tibble`s
- Do not use hidden global variables
- Comments should explain the "why" (intent), not the "what" (code)

## Package and Dependency Management

- Use `renv` for managing project dependencies
- Load all required packages at the beginning of the script using `library()`
- Prefer functions from `tidyverse` packages (`dplyr`, `readr`, `stringr`, etc.) over base R alternatives
- Use DESCRIPTION file for R package dependencies

## Function Documentation (roxygen2)

- Use `#' @export` to export functions from a package. Helper functions should not be exported
- Document all functions, explaining parameters (`@param`), return value (`@return`), and providing a runnable example (`@examples`)
- Include roxygen2 documentation for all functions

## Error Handling and Testing

- Use `tryCatch()` or `rlang::try_fetch()` for robust error handling
- Provide informative error messages using `stop()`
- Write unit tests using `testthat`
- Test files must be in `tests/testthat/` and named `test-function_name.R`
- Aim for a minimum of 80% test coverage
- Provide meaningful error messages in Portuguese for business users
- Include fallback behaviors for data loading failures

## Data Analysis

- Use `dplyr` for data manipulation
- Use `plotly` for interactive charts
- Handle missing values explicitly
- Include data validation checks

## Encoding and Localization

- Handle UTF-8 encoding properly for Portuguese characters
- Use ASCII-safe alternatives when necessary for file parsing
- Set proper locale settings for date/time formatting
- Consider Windows-specific file path handling
