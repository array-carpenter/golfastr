# Check current/recent tournaments and try finding one with shot data

library(httr2)
library(jsonlite)

url <- "https://orchestrator.pgatour.com/graphql"
api_key <- "da2-gsrx5bibzbb4njvhl7t37wqyl4"

# Get schedule and current tournament
query <- '
{
  schedule(tourCode: "R", year: 2025) {
    completed {
      tournamentName
      id
      startDate
      endDate
    }
    upcoming {
      tournamentName
      id
      startDate
    }
    inProgress {
      tournamentName
      id
    }
  }
}
'

req <- request(url) |>
  req_headers("Content-Type" = "application/json", "x-api-key" = api_key) |>
  req_body_json(list(query = query))

resp <- req_perform(req)
data <- resp_body_json(resp)$data$schedule

cat("=== IN PROGRESS ===\n")
if (length(data$inProgress) > 0) {
  for (t in data$inProgress) {
    cat(sprintf("%s (ID: %s)\n", t$tournamentName, t$id))
  }
} else {
  cat("None\n")
}

cat("\n=== RECENTLY COMPLETED ===\n")
for (t in head(data$completed, 3)) {
  cat(sprintf("%s (ID: %s) - %s to %s\n", t$tournamentName, t$id, t$startDate, t$endDate))
}

cat("\n=== UPCOMING ===\n")
for (t in head(data$upcoming, 3)) {
  cat(sprintf("%s (ID: %s) - starts %s\n", t$tournamentName, t$id, t$startDate))
}
