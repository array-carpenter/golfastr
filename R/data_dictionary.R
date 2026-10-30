#' Get Field Descriptions for PGA Data
#'
#' Returns a data frame with field names and descriptions for
#' leaderboard, hole-by-hole, logo or what's in the bag data.
#'
#' @param data_type Character. One of "leaderboard", "holes", "logos",
#'   "witb" ([load_witb()]) or "witb_switches" ([load_witb_switches()]).
#' @return A tibble with field and description columns.
#' @export
#' @examples
#' pga_field_descriptions("leaderboard")
#' pga_field_descriptions("holes")
#' pga_field_descriptions("logos")
#' pga_field_descriptions("witb")
pga_field_descriptions <- function(data_type = c("leaderboard", "holes", "logos",
                                                 "witb", "witb_switches")) {
  data_type <- match.arg(data_type)

  if (data_type == "leaderboard") {
    tibble::tibble(
      field = c(
        "position",
        "player_id",
        "player_name",
        "total_score",
        "score_to_par",
        "status",
        "tournament_id",
        "tournament_name",
        "year"
      ),
      description = c(
        "Final tournament standing",
        "ESPN athlete ID",
        "Player display name",
        "Total strokes across completed rounds",
        "Score relative to par (e.g., '-11', 'E', '+5')",
        "Player status (live API only)",
        "ESPN event ID",
        "Name of the tournament",
        "Tournament year"
      )
    )
  } else if (data_type == "logos") {
    tibble::tibble(
      field = c(
        "logo_type",
        "name",
        "logo_url",
        "logo_source",
        "source_page",
        "tour",
        "pga_tournament_id",
        "espn_tournament_id",
        "course_id",
        "club_id",
        "primary_color",
        "secondary_color",
        "checked_at"
      ),
      description = c(
        "tour, tournament, course or club",
        "Display name",
        "Logo image URL",
        "Where the logo was found: pgatour, wikimedia, club_site or tour_site",
        "Page the logo was found on",
        "Tour code (pga, lpga, dpwt, liv, kft, champions, ...); NA for courses and clubs",
        "PGA Tour tournament ID (tournaments)",
        "ESPN event ID (tournaments)",
        "ShotLink course ID (courses)",
        "Club slug, e.g. augusta-national; one club can have many courses",
        "Primary color as hex, where known",
        "Secondary color as hex, where known",
        "Date the URL last returned an image"
      )
    )
  } else if (data_type == "witb") {
    tibble::tibble(
      field = c(
        "bag_id",
        "player_id",
        "player_name",
        "as_of_date",
        "event_name",
        "tournament_id",
        "source",
        "source_url",
        "slot",
        "club_label",
        "club_order",
        "head_brand",
        "head_model",
        "model_variant",
        "is_prototype",
        "finish",
        "loft_stated",
        "loft_actual",
        "lie",
        "bounce",
        "grind",
        "offset",
        "head_weight",
        "adapter_setting",
        "shaft_brand",
        "shaft_model",
        "shaft_weight_g",
        "shaft_flex",
        "shaft_profile",
        "tipping_in",
        "length_in",
        "swing_weight",
        "grip_brand",
        "grip_model",
        "grip_size",
        "grip_wraps",
        "grip_notes",
        "putter_length_in",
        "putter_loft",
        "putter_lie",
        "head_shape",
        "hosel",
        "toe_hang",
        "head_source",
        "shaft_source",
        "grip_source",
        "putter_source",
        "notes",
        "qc_flags"
      ),
      description = c(
        "Bag ID: source and its post key, e.g. golfwrx:687118",
        "PGA Tour player ID, as in load_shots(); NA for players without one",
        "Player name as printed by the source",
        "Date of the bag post",
        "Event the bag was seen at, where known",
        "PGA Tour tournament ID, where the event is known",
        "golfwrx or clubtracker (PGA Tour Club Tracker)",
        "Page the bag was published on",
        "driver, fairway, hybrid, utility, iron, wedge or putter",
        "Club as listed, e.g. 3W, 7i, 56 or Putter",
        "Order in the bag, 1 = longest club",
        "Head brand",
        "Head model",
        "Model variant, e.g. LS",
        "TRUE for a prototype head",
        "Head finish",
        "Stated loft in degrees",
        "Loft in degrees after bending, where given",
        "Lie angle in degrees",
        "Bounce in degrees (wedges)",
        "Wedge grind",
        "Offset",
        "Head weight as written",
        "Adjustable hosel setting",
        "Shaft brand",
        "Shaft model",
        "Shaft weight in grams",
        "Shaft flex",
        "Shaft profile, e.g. soft stepped",
        "Tipping in inches",
        "Club length in inches",
        "Swing weight",
        "Grip brand",
        "Grip model",
        "Grip size",
        "Extra wraps of tape under the grip",
        "Other grip notes",
        "Putter length in inches",
        "Putter loft in degrees",
        "Putter lie angle in degrees",
        "Putter head shape",
        "Putter hosel",
        "Putter toe hang",
        "Source of the head fields: golfwrx, clubtracker or both",
        "Source of the shaft fields",
        "Source of the grip fields",
        "Source of the putter fields",
        "Other spec notes",
        "Data checks the bag did not pass, '; '-separated (e.g. club_count=15, team_post)"
      )
    )
  } else if (data_type == "witb_switches") {
    tibble::tibble(
      field = c(
        "player_id",
        "player_name",
        "slot",
        "component",
        "old_value",
        "new_value",
        "last_seen_old",
        "first_seen_new",
        "first_seen_event",
        "tournament_id",
        "old_bag_id",
        "new_bag_id",
        "gap_days",
        "uncertain",
        "reason",
        "brand_change"
      ),
      description = c(
        "PGA Tour player ID",
        "Player name",
        "driver, fairway, hybrid, utility, iron, wedge or putter",
        "head, shaft or grip",
        "Equipment before the switch",
        "Equipment after the switch",
        "Date of the last bag with the old equipment",
        "Date of the first bag with the new equipment",
        "Event of the first bag with the new equipment",
        "PGA Tour tournament ID of that event, where known",
        "bag_id of the last bag with the old equipment",
        "bag_id of the first bag with the new equipment",
        "Days between the two bags",
        "TRUE when the switch may not be real",
        "Why it is uncertain, e.g. old value back within 30 days",
        "TRUE when the old and new equipment share no brand"
      )
    )
  } else {
    tibble::tibble(
      field = c(
        "player_id",
        "player_name",
        "position",
        "tournament_id",
        "tournament_name",
        "round",
        "hole",
        "par",
        "score",
        "score_type",
        "year",
        "field_avg",
        "to_par",
        "vs_field",
        "cume_to_par"
      ),
      description = c(
        "ESPN athlete ID",
        "Player display name",
        "Final leaderboard standing",
        "ESPN event ID",
        "Name of the tournament",
        "Round number (5 = playoff)",
        "Hole number (1-18)",
        "Par for the hole",
        "Strokes taken on the hole",
        "Score classification (EAGLE, BIRDIE, PAR, BOGEY, etc.)",
        "Tournament year",
        "Field scoring average on the hole in that round (hosted data only)",
        "Score relative to par on the hole (hosted data only)",
        "Strokes vs the field average on the hole (hosted data only)",
        "Running tournament score to par through the hole, playoff holes included (hosted data only)"
      )
    )
  }
}


#' Get PGA Score Types
#'
#' Returns a data frame with score type classifications
#' used in hole-by-hole data.
#'
#' @return A tibble with score_type, strokes_vs_par, and description columns.
#' @export
#' @examples
#' pga_score_types()
pga_score_types <- function() {
  tibble::tibble(
    score_type = c(
      "DOUBLE_EAGLE",
      "EAGLE",
      "BIRDIE",
      "PAR",
      "BOGEY",
      "DOUBLE_BOGEY",
      "TRIPLE_BOGEY",
      "OTHER"
    ),
    strokes_vs_par = c(-3L, -2L, -1L, 0L, 1L, 2L, 3L, NA_integer_),
    description = c(
      "Three under par (also called albatross)",
      "Two under par",
      "One under par",
      "Equal to par",
      "One over par",
      "Two over par",
      "Three over par",
      "Four or more over par"
    )
  )
}


#' Get PGA Major Championships
#'
#' Returns a data frame with the four major championships
#' and their typical schedule.
#'
#' @return A tibble with tournament, month, and course columns.
#' @export
#' @examples
#' pga_majors()
pga_majors <- function() {
  tibble::tibble(
    tournament = c(
      "Masters Tournament",
      "PGA Championship",
      "U.S. Open",
      "The Open Championship"
    ),
    month = c("April", "May", "June", "July"),
    course = c(
      "Augusta National Golf Club",
      "Varies",
      "Varies",
      "Varies (UK links courses)"
    )
  )
}
