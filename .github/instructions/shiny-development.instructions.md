---
description: "Shiny application development guidelines for Ampla R projects"
applyTo: "**/ampla/**/*.R"
---

# Shiny Development Instructions

## Language and Naming

- All names (variables, functions, files) and text MUST be in Portuguese (BR)
- Use `lowerCamelCase` for variables and functions (e.g., `valorContrato`,
  `selecionarEmpresa`)
- Comments should explain the "why" (intent), not the "what" (code)

## Style and Formatting

- Keep lines to a maximum of 80 characters where possible
- Use `<-` for assignment, not `=`
- Use `%>%` from `magrittr` for piping
- Use `here::here()` for file paths
- Use `options(scipen = 999)` at the start of scripts

## UI Components

- Use semantic UI element names
- Implement responsive design with `fluidRow()` and `column()`
- Use `tabsetPanel()` for multi-page applications
- Prefer `selectInput()` with proper choices and selected values
- Use modular approach with separate files for UI and server logic

## Server Logic

- Use reactive expressions for data processing
- Implement proper reactive dependencies
- Use `observeEvent()` for side effects
- Handle user inputs with `req()` validation
- Avoid `for` loops; use vectorized functions (`lapply`, `map`, etc.) instead
- Do not use hidden global variables
- Each function should have a single, well-defined responsibility

## Interactive Charts

- Use `plotly` for interactive visualizations
- Implement click events with `event_data("plotly_click")`
- Handle encoding issues proactively for Portuguese text

## Package Management

- Load all required packages at the beginning using `library()`
- Prefer functions from `tidyverse` packages over base R alternatives
- Use `dplyr` for data manipulation
- Preferably, data frames should be `tibble`s

## Performance

- Use `isolate()` to break unnecessary reactive dependencies
- Cache expensive computations with `reactive()`
- Minimize reactive updates in UI elements

## Error Handling

- Use `tryCatch()` or `rlang::try_fetch()` for robust error handling
- Provide user-friendly error messages in Portuguese
- Use `validate()` for input validation
- Include loading indicators for long-running operations
- Provide informative error messages using `stop()`

## Testing

- Write unit tests using `testthat` for Shiny modules
- Test files must be in `tests/testthat/` and named `test-function_name.R`
- Aim for a minimum of 80% test coverage
