# Deploy script for local golem app
library(rsconnect)

# Load all required packages to ensure they're detected
library(shiny)
library(golem)
library(plotly)
library(dplyr)
library(lubridate)

cat("Deploying golem app to shinyapps.io...\n")

# Deploy the entire golem structure with environment management disabled
rsconnect::deployApp(
  appDir = ".",
  appName = "ampla-dashboard",
  account = "vitortaira",
  forceUpdate = TRUE,
  launch.browser = FALSE,
  envManagement = FALSE # This prevents renv snapshot issues
)

cat("✅ Deployed successfully!\n")
cat("🌐 App URL: https://vitortaira.shinyapps.io/ampla-dashboard/\n")
