# Set locale and encoding to UTF-8
invisible(suppressWarnings(Sys.setlocale("LC_ALL", "pt_BR.UTF-8")))
# Sys.setenv(LANG = "pt_BR.UTF-8", R_POPPLER_QUIET = "TRUE")
# options(encoding = "UTF-8", tidyverse.quiet = TRUE)
options(warn = 0)

# Desabilitar ativação automática do conda no terminal R
Sys.setenv(CONDA_AUTO_ACTIVATE_BASE = "false")

# Define a helper function to safely load packages
load_pkg <- function(f_pkg) {
  tryCatch(
    suppressPackageStartupMessages(
      library(f_pkg, character.only = TRUE, quietly = TRUE)
    ),
    error = function(e) {
      message(sprintf(
        "Package '%s' not available: %s", f_pkg, conditionMessage(e)
      ))
    }
  )
}

# List of desired packages
packages_c <- c(
  "devtools", "fs", "future", "future.apply", "here", "htmlwidgets", "lubridate", "magrittr", "openxlsx", "pdftools", "plotly", "readxl",
  "tidyverse", "usethis", "visNetwork"
)

if (interactive()) {
  # Load each package quietly
  invisible(lapply(packages_c, load_pkg))

  # Load the 'amplaRPackage' package quietly
  invisible(suppressMessages(suppressPackageStartupMessages(devtools::load_all(
    "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Controladoria - Documentos/amplaGitHub/amplaRPackage"
  ))))
}
