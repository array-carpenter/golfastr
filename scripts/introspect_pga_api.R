# PGA Tour GraphQL API Schema Introspection
# Run once to discover available queries and types

library(httr2)
library(jsonlite)

# API endpoint
url <- "https://orchestrator.pgatour.com/graphql"

# Introspection query - returns full schema
introspection_query <- '
{
  __schema {
    queryType {
      name
      fields {
        name
        description
        args {
          name
          type {
            name
            kind
          }
        }
        type {
          name
          kind
          ofType {
            name
            kind
          }
        }
      }
    }
  }
}
'

# Build request
req <- request(url) |>
  req_headers(
    "Content-Type" = "application/json",
    "x-api-key" = "da2-gsrx5bibzbb4njvhl7t37wqyl4"
  ) |>
  req_body_json(list(query = introspection_query))

# Execute
cat("Fetching schema...\n")
resp <- req_perform(req)
data <- resp_body_json(resp)

# Extract query names
queries <- data$data$`__schema`$queryType$fields

cat("\n=== AVAILABLE QUERIES ===\n\n")

for (q in queries) {
  cat(sprintf("• %s\n", q$name))
  if (!is.null(q$description) && nchar(q$description) > 0) {
    cat(sprintf("  %s\n", substr(q$description, 1, 80)))
  }
}

# Save full schema for reference
output_path <- "inst/extdata/pga_schema.json"
if (!dir.exists(dirname(output_path))) {
  dir.create(dirname(output_path), recursive = TRUE)
}
write_json(data, output_path, pretty = TRUE, auto_unbox = TRUE)
cat(sprintf("\nFull schema saved to: %s\n", output_path))

# Look for shot/scorecard related queries
cat("\n=== POTENTIALLY RELEVANT QUERIES ===\n")
keywords <- c("shot", "score", "card", "hole", "round", "tour", "cast", "stroke", "play")

for (q in queries) {
  name_lower <- tolower(q$name)
  if (any(sapply(keywords, function(k) grepl(k, name_lower)))) {
    cat(sprintf("\n→ %s\n", q$name))
    if (!is.null(q$description)) {
      cat(sprintf("  Description: %s\n", q$description))
    }
    if (length(q$args) > 0) {
      cat("  Arguments:\n")
      for (arg in q$args) {
        type_name <- if (!is.null(arg$type$name)) arg$type$name else arg$type$kind
        cat(sprintf("    - %s: %s\n", arg$name, type_name))
      }
    }
  }
}
