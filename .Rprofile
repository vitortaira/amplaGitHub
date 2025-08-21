# Set locale and encoding to UTF-8
suppressWarnings(Sys.setlocale("LC_ALL", "pt_BR.UTF-8"))
# Sys.setenv(LANG = "pt_BR.UTF-8", R_POPPLER_QUIET = "TRUE")
# options(encoding = "UTF-8", tidyverse.quiet = TRUE)
options(warn = 0)

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

# Load the 'amplaRPackage' package quietly (using library for proper help system)
tryCatch({
  if ("amplaRPackage" %in% utils::installed.packages()[, "Package"]) {
    invisible(suppressMessages(suppressPackageStartupMessages(library(amplaRPackage))))
  } else {
    # Fallback to load_all if package not installed
    invisible(suppressMessages(suppressPackageStartupMessages(devtools::load_all(
      "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Controladoria - Documentos/amplaGitHub/amplaRPackage"
    ))))
  }
}, error = function(e) {
  # If there's any error, just use load_all as fallback
  invisible(suppressMessages(suppressPackageStartupMessages(devtools::load_all(
    "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Controladoria - Documentos/amplaGitHub/amplaRPackage"
  ))))
})

# Display a startup message
message("Custom .Rprofile loaded: LOCALE set to pt_BR.UTF-8 and packages loaded.")
