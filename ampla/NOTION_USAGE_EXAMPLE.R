# Example: Using the Notion MCP (Official API) in R
#
# This script demonstrates how to use the official Notion API (MCP) from R.
# It does NOT use notionR. It uses httr and your NOTION_SECRET from .Renviron.
#
# 1. Get your token from the environment
notion_token <- Sys.getenv("NOTION_SECRET")
if (notion_token == "") stop("NOTION_SECRET environment variable not found. Please check your .Renviron file.")

library(httr)

# 2. Query a Notion database (replace {database_id} with your actual ID)
# database_id <- "your_database_id_here"
# query_url <- paste0("https://api.notion.com/v1/databases/", database_id, "/query")
#
# response <- POST(
#   url = query_url,
#   add_headers(
#     Authorization = paste("Bearer", notion_token),
#     "Notion-Version" = "2022-06-28",
#     "Content-Type" = "application/json"
#   ),
#   body = "{}"
# )
#
# if (http_error(response)) stop("API request failed: ", content(response, "text"))
# cat("Database query result:\n")
# print(content(response))

# 3. Create a new block (e.g., add a paragraph to a page)
# Replace {page_id} with your actual page ID
page_id <- "231e63ea-90bb-81fc-b6c3-fda5b788488e"
create_block_url <- paste0("https://api.notion.com/v1/blocks/", page_id, "/children")

block_body <- list(
  children = list(
    list(
      object = "block",
      type = "paragraph",
      paragraph = list(
        rich_text = list(
          list(
            type = "text",
            text = list(content = "Hello from R using Notion MCP!")
          )
        )
      )
    )
  )
)

response_block <- PATCH(
  url = create_block_url,
  add_headers(
    Authorization = paste("Bearer", notion_token),
    "Notion-Version" = "2022-06-28",
    "Content-Type" = "application/json"
  ),
  body = block_body,
  encode = "json"
)

if (http_error(response_block)) stop("Block creation failed: ", content(response_block, "text"))
cat("Block creation result:\n")
print(content(response_block))

# Note: You must restart R after editing .Renviron for changes to take effect.
