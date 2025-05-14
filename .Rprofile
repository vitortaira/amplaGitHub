# Set locale and encoding to UTF-8
suppressWarnings(Sys.setlocale("LC_ALL", "pt_BR.UTF-8"))
Sys.setenv(LANG = "pt_BR.UTF-8", R_POPPLER_QUIET = "TRUE")
options(encoding = "UTF-8", tidyverse.quiet = TRUE)

# Define a helper function to safely load packages
load_pkg <- function(f_pkg) {
  if (!suppressWarnings(suppressPackageStartupMessages(
    require(f_pkg, character.only = TRUE, quietly = TRUE)
  ))) {
    message(sprintf("Package '%s' not available.", f_pkg))
  }
}

# List of desired packages
packages_c <- c(
  "devtools", "fs", "here", "htmlwidgets", "lubridate", "magrittr", "openxlsx", "pdftools", "readxl",
  "tidyverse", "usethis", "visNetwork"
)

# Load each package quietly
invisible(lapply(packages_c, load_pkg))

# Load the 'amplaRPackage' package quietly
invisible(suppressMessages(suppressPackageStartupMessages(devtools::load_all(
  "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Controladoria - Documentos/amplaGitHub/amplaRPackage"
))))

# Display a startup message
message("Custom .Rprofile loaded: LOCALE set to pt_BR.UTF-8 and packages loaded.")
