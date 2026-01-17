# PGA Tour GraphQL API Notes

For future shot-level data implementation.

## Endpoint
```
https://orchestrator.pgatour.com/graphql
```

## Key Queries

### shotDetailsV3
Shot-level data for a player's round.

```graphql
shotDetailsV3(tournamentId: "R2026006", playerId: "54591", round: 1, includeRadar: false) {
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
```

### Data Available
- fromLocation/toLocation: Tee Box, Fairway, Green, Rough, etc.
- fromLocationCode/toLocationCode: OTB, OFW, OGR, etc.
- distance: Shot distance (e.g., "301 yds")
- distanceRemaining: Distance to hole after shot (e.g., "37 yds")
- strokeType: STROKE, DROP, etc.
- playByPlay: Human-readable description

### Notes
- Shot data only available during/shortly after live play
- Completed tournaments may not retain shot-level detail
- Tournament IDs follow pattern: R{YEAR}{NUM} (e.g., R2026006 for Sony Open)
