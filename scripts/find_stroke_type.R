# Find the Stroke type - deeper introspection

library(httr2)
library(jsonlite)

url <- "https://orchestrator.pgatour.com/graphql"
api_key <- Sys.getenv("PGA_TOUR_API_KEY")

# Search for types containing "Stroke" in name
query <- '
{
  __schema {
    types {
      name
      kind
      fields {
        name
      }
    }
  }
}
'

req <- request(url) |>
  req_headers(
    "Content-Type" = "application/json",
    "x-api-key" = api_key
  ) |>
  req_body_json(list(query = query))

cat("Fetching all types...\n\n")
resp <- req_perform(req)
data <- resp_body_json(resp)

# Find types with "stroke" in name (case insensitive)
cat("=== Types containing 'stroke' ===\n")
for (t in data$data$`__schema`$types) {
  if (grepl("stroke", t$name, ignore.case = TRUE)) {
    cat(sprintf("\n%s (%s):\n", t$name, t$kind))
    if (!is.null(t$fields)) {
      for (f in t$fields) {
        cat(sprintf("  • %s\n", f$name))
      }
    }
  }
}

# Save all type names for reference
type_names <- sapply(data$data$`__schema`$types, function(t) t$name)
writeLines(sort(type_names), "inst/extdata/all_type_names.txt")
cat("\nAll type names saved to inst/extdata/all_type_names.txt\n")
