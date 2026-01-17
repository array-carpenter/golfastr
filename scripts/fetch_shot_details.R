# Get full stroke data - Ben Griffin, Sony Open 2026

library(httr2)
library(jsonlite)

url <- "https://orchestrator.pgatour.com/graphql"
api_key <- "da2-gsrx5bibzbb4njvhl7t37wqyl4"

tournament_id <- "R2026006"
player_id <- "54591"  # Ben Griffin

shot_query <- sprintf('
{
  shotDetailsV3(tournamentId: "%s", playerId: "%s", round: 1, includeRadar: false) {
    playerId
    holes {
      holeNumber
      par
      score
      yardage
      strokes {
        strokeNumber
        distance
        distanceRemaining
        strokeType
        fromLocation
        toLocation
        fromLocationCode
        toLocationCode
        playByPlay
        finalStroke
      }
    }
  }
}
', tournament_id, player_id)

req <- request(url) |>
  req_headers("Content-Type" = "application/json", "x-api-key" = api_key) |>
  req_body_json(list(query = shot_query))

cat("Fetching Ben Griffin shot-level data...\n\n")
resp <- req_perform(req)
shots <- resp_body_json(resp)

write_json(shots, "inst/extdata/ben_griffin_full.json", pretty = TRUE, auto_unbox = TRUE)

if (!is.null(shots$errors)) {
  cat("Errors:\n")
  for (e in shots$errors) cat(sprintf("  %s\n", e$message))
} else {
  # Show first 2 holes
  shown <- 0
  for (h in shots$data$shotDetailsV3$holes) {
    if (length(h$strokes) > 0 && shown < 2) {
      cat(sprintf("=== Hole %d (Par %s, %s yds, Score %s) ===\n",
                  h$holeNumber, h$par, h$yardage, h$score))

      for (s in h$strokes) {
        cat(sprintf("\n  Stroke %s:\n", s$strokeNumber))
        cat(sprintf("    From: %s (%s)\n", s$fromLocation, s$fromLocationCode))
        cat(sprintf("    To: %s (%s)\n", s$toLocation, s$toLocationCode))
        cat(sprintf("    Distance: %s\n", s$distance))
        cat(sprintf("    Remaining: %s\n", s$distanceRemaining))
        cat(sprintf("    Type: %s\n", s$strokeType))
        cat(sprintf("    Final: %s\n", s$finalStroke))
        cat(sprintf("    Play-by-play: %s\n", s$playByPlay))
      }
      shown <- shown + 1
      cat("\n")
    }
  }
}

cat("\nFull data saved to inst/extdata/ben_griffin_full.json\n")
