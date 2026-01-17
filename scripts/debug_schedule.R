# Debug schedule query

library(httr2)
library(jsonlite)

url <- "https://orchestrator.pgatour.com/graphql"
api_key <- "da2-gsrx5bibzbb4njvhl7t37wqyl4"

query <- '
{
  schedule(tourCode: "R", year: 2025) {
    completed { tournamentName id startDate }
    inProgress { tournamentName id }
    upcoming { tournamentName id startDate }
  }
}
'

req <- request(url) |>
  req_headers("Content-Type" = "application/json", "x-api-key" = api_key) |>
  req_body_json(list(query = query))

resp <- req_perform(req)
data <- resp_body_json(resp)

cat("Raw response:\n")
print(toJSON(data, pretty = TRUE, auto_unbox = TRUE))
