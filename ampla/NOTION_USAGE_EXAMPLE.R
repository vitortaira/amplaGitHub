# Notion API Integration for Ampla
# Script to edit the "Cobertura temporal dos arquivos" page

library(httr)
library(jsonlite)
library(readxl)
library(base64enc)

# Load environment variables from .Renviron file explicitly
# Try different paths for .Renviron file
renviron_paths <- c(
  ".Renviron", # Current directory
  "../.Renviron", # Parent directory
  "~/.Renviron", # Home directory
  file.path(dirname(getwd()), ".Renviron") # Project root
)

# Find and load .Renviron file
renviron_loaded <- FALSE
for (path in renviron_paths) {
  if (file.exists(path)) {
    readRenviron(path)
    cat("✅ Loaded .Renviron from:", path, "\n")
    renviron_loaded <- TRUE
    break
  }
}

if (!renviron_loaded) {
  cat("⚠️  .Renviron file not found in any of these locations:\n")
  for (path in renviron_paths) {
    cat("  -", path, "\n")
  }
}

# Get the token from environment
notion_token <- Sys.getenv("NOTION_SECRET")

# Always check if the token exists
if (notion_token == "") {
  stop("NOTION_SECRET environment variable not found. Please check your .Renviron file.")
}

# Extract page ID from the URL: https://www.notion.so/Cobertura-temporal-dos-arquivos-231e63ea90bb8168bc6dc8e35010b459
page_id <- "231e63ea90bb8168bc6dc8e35010b459"

# Function to get page information
get_notion_page <- function(page_id, token) {
  response <- GET(
    url = paste0("https://api.notion.com/v1/pages/", page_id),
    add_headers(
      Authorization = paste("Bearer", token),
      "Notion-Version" = "2022-06-28"
    )
  )

  if (status_code(response) == 200) {
    return(content(response, "parsed"))
  } else {
    stop(paste(
      "Failed to fetch page. Status:", status_code(response),
      "Response:", content(response, "text")
    ))
  }
}

# Function to create an embedded OneDrive file block
create_onedrive_embed <- function(onedrive_url, file_name = NULL) {
  # Extract file name from path if not provided
  if (is.null(file_name)) {
    file_name <- "Inadimplencia-2025_07.xlsx"
  }

  # Create an embed block for OneDrive
  embed_block <- list(
    object = "block",
    type = "embed",
    embed = list(
      url = onedrive_url
    )
  )

  return(embed_block)
}

# Function to create a file reference block (alternative approach)
create_file_reference <- function(onedrive_url, file_name, description = NULL) {
  # Create a paragraph with a link to the OneDrive file
  rich_text_items <- list(
    list(
      type = "text",
      text = list(content = "📊 ")
    ),
    list(
      type = "text",
      text = list(
        content = file_name,
        link = list(url = onedrive_url)
      ),
      annotations = list(
        bold = TRUE,
        color = "blue"
      )
    )
  )

  # Add description if provided
  if (!is.null(description)) {
    rich_text_items <- append(rich_text_items, list(
      list(
        type = "text",
        text = list(content = paste0(" - ", description)),
        annotations = list(italic = TRUE)
      )
    ))
  } else {
    rich_text_items <- append(rich_text_items, list(
      list(
        type = "text",
        text = list(content = " (OneDrive)"),
        annotations = list(italic = TRUE, color = "gray")
      )
    ))
  }

  link_block <- list(
    object = "block",
    type = "paragraph",
    paragraph = list(
      rich_text = rich_text_items
    )
  )

  return(link_block)
}

# Function to get file information
get_file_info <- function(file_path) {
  if (!file.exists(file_path)) {
    return(list(
      exists = FALSE,
      message = paste("File not found:", file_path)
    ))
  }

  file_info <- file.info(file_path)

  return(list(
    exists = TRUE,
    name = basename(file_path),
    size = file_info$size,
    modified = file_info$mtime,
    size_mb = round(file_info$size / 1024 / 1024, 2)
  ))
}
append_to_notion_page <- function(page_id, token, content_blocks) {
  response <- PATCH(
    url = paste0("https://api.notion.com/v1/blocks/", page_id, "/children"),
    body = list(children = content_blocks),
    add_headers(
      Authorization = paste("Bearer", token),
      "Notion-Version" = "2022-06-28",
      "Content-Type" = "application/json"
    ),
    encode = "json"
  )

  if (status_code(response) == 200) {
    cat("Content successfully added to Notion page!\n")
    return(content(response, "parsed"))
  } else {
    stop(paste(
      "Failed to update page. Status:", status_code(response),
      "Response:", content(response, "text")
    ))
  }
}

# Test: Get page information
cat("Getting page information...\n")
page_info <- get_notion_page(page_id, notion_token)
cat("Page title:", page_info$properties$title$title[[1]]$text$content, "\n")

# STEP 1: Get the OneDrive share link for your Excel file
# You need to:
# 1. Go to OneDrive and find your file: "Inadimplencia-2025_07.xlsx"
# 2. Right-click -> Share -> Copy link
# 3. Replace the URL below with your actual OneDrive share link

onedrive_url <- "https://incorporadoraampla.sharepoint.com/:x:/s/Relatorios/EdJIJLWkqxdCt74TDrXL52ABJTnVTW0n43Z-DT4korEKKg?e=tao2AZ"
# Example: "https://1drv.ms/x/s!Abc123..." or "https://amplaincorporadora.sharepoint.com/..."

# Check if OneDrive URL was set
if (onedrive_url == "YOUR_ONEDRIVE_SHARE_LINK_HERE") {
  cat("⚠️  Please set the OneDrive share link for your Excel file!\n")
  cat("1. Go to OneDrive\n")
  cat("2. Find: Inadimplencia-2025_07.xlsx\n")
  cat("3. Right-click -> Share -> Copy link\n")
  cat("4. Replace 'YOUR_ONEDRIVE_SHARE_LINK_HERE' in this script\n")
  stop("OneDrive URL not configured")
}

# Create content with file reference - simplified for debugging
new_content <- list(
  list(
    object = "block",
    type = "heading_2",
    heading_2 = list(
      rich_text = list(
        list(
          type = "text",
          text = list(content = "📊 Relatório de Inadimplência - Julho 2025")
        )
      )
    )
  ),
  list(
    object = "block",
    type = "paragraph",
    paragraph = list(
      rich_text = list(
        list(
          type = "text",
          text = list(content = paste("Última atualização:", format(Sys.time(), "%d/%m/%Y %H:%M")))
        )
      )
    )
  ),
  list(
    object = "block",
    type = "bookmark",
    bookmark = list(
      url = onedrive_url
    )
  )
)

# Add content to the page
cat("Adding content to the page...\n")
result <- append_to_notion_page(page_id, notion_token, new_content)

cat("✅ Notion page updated successfully!\n")
