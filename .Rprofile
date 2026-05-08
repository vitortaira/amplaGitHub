# Set locale and encoding to UTF-8
invisible(suppressWarnings(Sys.setlocale("LC_ALL", "pt_BR.UTF-8")))
# Sys.setenv(LANG = "pt_BR.UTF-8", R_POPPLER_QUIET = "TRUE")
# options(encoding = "UTF-8", tidyverse.quiet = TRUE)
options(warn = 0)

# Desabilitar ativação automática do conda no terminal R
Sys.setenv(CONDA_AUTO_ACTIVATE_BASE = "false")

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
  "devtools", "fs", "future", "future.apply", "here", "htmlwidgets", "lubridate", "magrittr", "openxlsx", "pdftools", "plotly", "readxl",
  "tidyverse", "usethis", "visNetwork"
)

# Load each package quietly
invisible(lapply(packages_c, load_pkg))

if (interactive() && Sys.getenv("RSTUDIO") == "") {
  source(file.path(
    Sys.getenv(if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"),
    ".vscode-R", "init.R"
  ))
}

# Load the 'amplaRPackage' package quietly
invisible(suppressMessages(suppressPackageStartupMessages(devtools::load_all(
  "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Controladoria - Documentos/amplaGitHub/amplaRPackage"
))))
