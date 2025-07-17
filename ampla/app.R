# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# Custom warning handler for expected warnings during package loading
suppressMessages({
  # Load the package quietly
  pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
})

# Set app options
options(scipen = 999) # Prevent scientific notation

# Run the application directly
shiny::shinyApp(ui = app_ui, server = app_server)
