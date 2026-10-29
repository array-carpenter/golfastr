## Logo URL table for load_logos(): tours, tournaments, courses and clubs
##
##   Rscript --vanilla data-raw/build_logos.R [--upload]
##
## Only URLs are published, never image files.
##
## Tours: hand list below, logo from Wikidata/Wikipedia, else the tour site.
## Tournaments: tournamentLogo from the PGA Tour schedule, 2012 on (the
## default r000 image counts as no logo), plus Wikimedia files for the majors,
## team cups and LPGA events.
## Clubs: every host of a men's or women's major, the Ryder Cup, Presidents
## Cup or Solheim Cup from 1960 on (from the Wikipedia host lists), plus the
## clubs of ShotLink courses. Logo from Wikidata (P154) or a logo file on the
## club's Wikipedia page, else the club's own site.
## Courses: ShotLink course_id, matched to a club, takes the club's logo.
##
## data-raw/logo_overrides.csv (hand edits) wins over everything. Clubs and
## courses with no logo go to data-raw/logos_missing.csv. Every response is
## cached under data-raw/logos-cache/, so reruns make no new requests.

suppressMessages(library(dplyr))
library(httr2)
library(jsonlite)

cache_dir <- "data-raw/logos-cache"
shotlink_out <- "data-raw/shotlink-cache/out"
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
this_year <- as.integer(format(Sys.Date(), "%Y"))
ua <- "golfastr data refresh (github.com/array-carpenter/golfastr)"
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# --- requests ---------------------------------------------------------------

fetch <- function(url, sub, ext = "txt", headers = list(), body = NULL, delay = 1) {
  path <- file.path(cache_dir, sub, paste0(digest::digest(c(url, body %||% "")), ".", ext, ".gz"))
  if (file.exists(path)) {
    return(paste(readLines(gzfile(path), warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  api <- sub %in% c("wiki", "pgatour")
  req <- request(url) |>
    req_user_agent(ua) |>
    req_headers(!!!headers) |>
    req_timeout(if (api) 60 else 20) |>
    req_retry(max_tries = if (api) 6 else 1, retry_on_failure = TRUE,
              is_transient = function(r) resp_status(r) %in% c(429, 500, 502, 503, 504),
              backoff = function(i) min(60, 5 * 2^(i - 1))) |>
    req_error(is_error = function(r) FALSE)
  if (!is.null(body)) req <- req_body_raw(req, body, type = "application/json")
  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  Sys.sleep(delay)
  ok <- !is.null(resp) && resp_status(resp) == 200
  # a failed API call stops the run rather than caching an empty answer;
  # a club site that fails is cached as empty and skipped
  if (!ok && api) {
    stop("request failed (", if (is.null(resp)) "no response" else resp_status(resp), "): ", url)
  }
  txt <- if (ok) tryCatch(resp_body_string(resp), error = function(e) "") else ""
  con <- gzfile(path, "w")
  writeLines(txt, con)
  close(con)
  txt
}

wiki <- function(..., host = "en.wikipedia.org") {
  params <- c(list(...), format = "json", formatversion = 2)
  q <- paste(names(params), vapply(params, function(v) utils::URLencode(as.character(v), reserved = TRUE), ""),
             sep = "=", collapse = "&")
  txt <- fetch(paste0("https://", host, "/w/api.php?", q), "wiki", "json", delay = 2)
  if (!nzchar(txt)) return(NULL)
  fromJSON(txt, simplifyVector = FALSE)
}

chunks <- function(x, n = 50) split(x, ceiling(seq_along(x) / n))

# Query a batch of titles, following continuation. Returns the pages (by
# title) and a lookup from every input title to its resolved title.
wiki_pages <- function(titles, ...) {
  pages <- list()
  resolve <- stats::setNames(titles, titles)
  for (ch in chunks(unique(titles))) {
    cont <- list()
    repeat {
      r <- do.call(wiki, c(list(action = "query", titles = paste(ch, collapse = "|"), redirects = 1), list(...), cont))
      if (is.null(r)) break
      for (x in c(r$query$normalized, r$query$redirects)) {
        resolve[resolve == x$from] <- x$to
      }
      for (pg in r$query$pages) {
        old <- pages[[pg$title]]
        for (k in names(old)) {
          pg[[k]] <- if (is.list(old[[k]])) c(old[[k]], pg[[k]]) else pg[[k]] %||% old[[k]]
        }
        pages[[pg$title]] <- pg
      }
      if (is.null(r$continue)) break
      cont <- r$continue
    }
  }
  list(pages = pages, resolve = resolve)
}

is_club <- function(cats) {
  any(grepl("golf clubs and courses|golf courses|golf clubs in|links golf|golf\\)? venues|Ryder Cup venues|Cup venues",
            cats, ignore.case = TRUE)) &&
    !any(grepl("golfers|births|architects", cats, ignore.case = TRUE))
}

page_categories <- function(titles) {
  q <- wiki_pages(titles, prop = "categories", cllimit = "max", clshow = "!hidden")
  cats <- lapply(q$pages, function(pg) vapply(pg$categories %||% list(), function(x) x$title, ""))
  list(cats = cats, resolve = q$resolve)
}

# --- 4. host clubs ----------------------------------------------------------

host_pages <- c(
  "U.S. Open (golf)", "PGA Championship", "The Open Championship",
  "U.S. Women's Open", "Women's PGA Championship", "Chevron Championship",
  "Women's British Open", "Evian Championship",
  "Ryder Cup", "Presidents Cup", "Solheim Cup"
)
# Single-venue events, and venues the event pages list without a link
host_extra <- c("Augusta National Golf Club", "Mission Hills Country Club",
                "The Club at Carlton Woods", "Evian Resort Golf Club")

wiki_links <- function(node) {
  a <- xml2::xml_find_all(node, ".//a[starts-with(@href,'/wiki/') and not(contains(@href,':'))]")
  t <- sub("#.*", "", sub("^/wiki/", "", xml2::xml_attr(a, "href")))
  unique(gsub("_", " ", vapply(t, utils::URLdecode, "", USE.NAMES = FALSE)))
}

host_links <- bind_rows(lapply(host_pages, function(p) {
  r <- wiki(action = "parse", page = p, prop = "text", redirects = 1)
  rows <- xml2::xml_find_all(xml2::read_html(r$parse$text), "//table[contains(@class,'wikitable')]//tr")
  bind_rows(lapply(rows, function(row) {
    txt <- xml2::xml_text(xml2::xml_find_all(row, "./td|./th"))
    yr <- regmatches(txt, regexpr("\\b(19|20)\\d{2}\\b", txt))
    if (!length(yr) || as.integer(yr[1]) < 1960 || as.integer(yr[1]) > this_year) return(NULL)
    tibble(event = p, year = as.integer(yr[1]), title = wiki_links(row))
  }))
}))
host_cats <- page_categories(c(unique(host_links$title), host_extra))
host_links$title <- unname(host_cats$resolve[host_links$title])
club_titles <- names(host_cats$cats)[vapply(host_cats$cats, is_club, TRUE)]
hosts <- host_links |>
  filter(title %in% club_titles) |>
  bind_rows(tibble(event = "extra", year = NA_integer_, title = unname(host_cats$resolve[host_extra]))) |>
  distinct(title, event)
message(length(unique(hosts$title)), " host clubs from ", length(host_pages), " event pages")

# --- logos from Wikidata and Wikipedia ---------------------------------------

logo_words <- "logo|emblem|crest|badge|wordmark|insignia"
noise_files <- "^(Commons-logo|Wiki[a-z]*-logo|OOjs|Symbol |Icona |Flag of|Red pog|Ambox|Edit-clear|Question book)"

wikidata_claims <- function(ids) {
  out <- list()
  for (ch in chunks(unique(ids[!is.na(ids)]))) {
    r <- wiki(action = "wbgetentities", ids = paste(ch, collapse = "|"), props = "claims",
              host = "www.wikidata.org")
    for (e in r$entities) {
      val <- function(p) {
        v <- e$claims[[p]][[1]]$mainsnak$datavalue$value
        if (is.null(v)) NA_character_ else as.character(v)
      }
      out[[e$id]] <- c(logo = val("P154"), website = val("P856"))
    }
  }
  out
}

# Token that identifies a page in its file names, e.g. "augusta" for
# Augusta National Golf Club
name_tokens <- function(x) {
  t <- strsplit(tolower(iconv(x, to = "ASCII//TRANSLIT")), "[^a-z0-9]+")[[1]]
  setdiff(t[nchar(t) > 2], c("the", "and", "golf", "club", "country", "course", "links", "resort",
                             "royal", "national", "championship", "tour", "open", "women"))
}

# Website link in the page's infobox, for pages with none on Wikidata
infobox_site <- function(title) {
  r <- wiki(action = "parse", page = title, prop = "text", section = 0)
  if (is.null(r$parse$text)) return(NA_character_)
  a <- xml2::xml_find_first(xml2::read_html(r$parse$text),
                            "//table[contains(@class,'infobox')]//a[contains(@class,'external')]")
  if (inherits(a, "xml_missing")) NA_character_ else xml2::xml_attr(a, "href")
}

# One row per title: the chosen logo file (if any) and the official website
wiki_logos <- function(titles, sites = FALSE) {
  q <- wiki_pages(titles, prop = "pageprops|images", imlimit = "max")
  pages <- q$pages
  claims <- wikidata_claims(vapply(pages, function(p) p$pageprops$wikibase_item %||% NA_character_, ""))
  picks <- bind_rows(lapply(pages, function(p) {
    qid <- p$pageprops$wikibase_item %||% NA_character_
    cl <- if (is.na(qid)) c(logo = NA, website = NA) else claims[[qid]] %||% c(logo = NA, website = NA)
    lead <- p$pageprops$page_image %||% NA_character_
    files <- sub("^File:", "", vapply(p$images %||% list(), function(x) x$title, ""))
    files <- files[grepl(logo_words, files, ignore.case = TRUE) & !grepl(noise_files, files)]
    toks <- name_tokens(p$title)
    files <- files[vapply(files, function(f) any(name_tokens(f) %in% toks), TRUE)]
    file <- if (!is.na(cl[["logo"]])) {
      cl[["logo"]]
    } else if (!is.na(lead) && grepl(logo_words, lead, ignore.case = TRUE)) {
      gsub("_", " ", lead)
    } else if (length(files)) {
      files[1]
    } else {
      NA_character_
    }
    tibble(title = p$title, qid = qid, file = file, website = cl[["website"]])
  }))
  files <- unique(picks$file[!is.na(picks$file)])
  info <- wiki_pages(paste0("File:", files), prop = "imageinfo", iiprop = "url|mime", iiurlwidth = 500)
  info_tbl <- bind_rows(lapply(info$pages, function(p) {
    ii <- p$imageinfo[[1]]
    if (is.null(ii)) return(NULL)
    tibble(file = sub("^File:", "", p$title),
           logo_url = sub("\\?.*$", "", if (grepl("svg", ii$mime)) ii$thumburl else ii$url),
           source_page = sub("\\?.*$", "", ii$descriptionurl))
  }))
  picks$file <- sub("^File:", "", unname(info$resolve[paste0("File:", picks$file)]))
  no_site <- sites & is.na(picks$website)
  picks$website[no_site] <- vapply(picks$title[no_site], infobox_site, "")
  tibble(title = names(q$resolve), resolved = unname(q$resolve)) |>
    left_join(left_join(picks, info_tbl, by = "file"), by = c("resolved" = "title"))
}

# --- link check -------------------------------------------------------------

# Every logo URL is fetched once a week at most; the result (status, content
# type, md5 of the bytes) is kept in the cache.
check_file <- file.path(cache_dir, "checks.rds")
checks <- if (file.exists(check_file)) readRDS(check_file) else
  tibble(url = character(), status = integer(), type = character(), md5 = character(), checked_at = as.Date(character()))

# Uses the curl command line tool: upload.wikimedia.org answers most requests
# from R's libcurl with 429.
check_urls <- function(urls) {
  fresh <- checks$url[checks$checked_at > Sys.Date() - 7 & checks$status %in% 200]
  todo <- setdiff(unique(urls[!is.na(urls)]), fresh)
  body <- tempfile()
  for (u in todo) {
    out <- suppressWarnings(system2("curl", c("-sL", "--max-time", "60", "--retry", "5", "--retry-delay", "10",
                                              "-A", shQuote(ua), "-o", body, "-w", shQuote("%{http_code} %{content_type}"),
                                              shQuote(u)), stdout = TRUE))
    Sys.sleep(if (grepl("wikimedia", u)) 1 else 0.3)
    parts <- strsplit(paste(out, collapse = ""), " ")[[1]]
    row <- tibble(
      url = u,
      status = suppressWarnings(as.integer(parts[1])),
      type = if (length(parts) > 1) sub(";.*", "", parts[2]) else NA_character_,
      md5 = if (file.exists(body)) digest::digest(file = body) else NA_character_,
      checked_at = Sys.Date()
    )
    unlink(body)
    checks <<- bind_rows(filter(checks, url != u), row)
    saveRDS(checks, check_file)
  }
  checks[match(urls, checks$url), ]
}

image_ok <- function(urls) {
  ck <- check_urls(urls)
  !is.na(ck$status) & ck$status == 200 & grepl("^image/", ck$type)
}

# --- 1. tours ---------------------------------------------------------------

tours <- tribble(
  ~tour,        ~name,                     ~title,                     ~site,
  "pga",        "PGA Tour",                "PGA Tour",                 "https://www.pgatour.com",
  "lpga",       "LPGA Tour",               "LPGA",                     "https://www.lpga.com",
  "dpwt",       "DP World Tour",           "European Tour",            "https://www.europeantour.com",
  "liv",        "LIV Golf",                "LIV Golf",                 "https://www.livgolf.com",
  "kft",        "Korn Ferry Tour",         "Korn Ferry Tour",          "https://www.pgatour.com/korn-ferry-tour",
  "champions",  "PGA Tour Champions",      "PGA Tour Champions",       "https://www.pgatour.com/pgatour-champions",
  "epson",      "Epson Tour",              "Epson Tour",               "https://www.epsontour.com",
  "americas",   "PGA Tour Americas",       "PGA Tour Americas",        "https://www.pgatour.com/americas",
  "asian",      "Asian Tour",              "Asian Tour",               "https://www.asiantour.com",
  "japan",      "Japan Golf Tour",         "Japan Golf Tour",          "https://www.jgto.org",
  "sunshine",   "Sunshine Tour",           "Sunshine Tour",            "https://sunshinetour.com",
  "australasia", "PGA Tour of Australasia", "PGA Tour of Australasia", "https://www.pga.org.au"
)

# --- logos from club and tour sites ----------------------------------------

robots_allows <- function(site, path = "/") {
  txt <- fetch(paste0(site, "/robots.txt"), "robots")
  if (!nzchar(txt)) return(TRUE)
  lines <- trimws(sub("#.*", "", strsplit(txt, "\n")[[1]]))
  agent <- NA_character_
  rules <- character()
  for (l in lines) {
    if (grepl("^user-agent:", l, ignore.case = TRUE)) agent <- trimws(sub("^[^:]+:", "", l))
    if (identical(agent, "*") && grepl("^disallow:", l, ignore.case = TRUE)) rules <- c(rules, trimws(sub("^[^:]+:", "", l)))
  }
  rules <- rules[nzchar(rules)]
  !any(startsWith(path, rules))
}

# The site's own logo: an <img> marked as the logo, else a logo og:image, else
# the apple-touch-icon
site_logo <- function(site) {
  if (is.na(site)) return(NA_character_)
  site <- sub("/+$", "", site)
  origin <- sub("^(https?://[^/]+).*$", "\\1", site)
  if (!robots_allows(origin, sub("^https?://[^/]+", "", site) |> sub(pattern = "^$", replacement = "/"))) {
    return(NA_character_)
  }
  html <- fetch(site, "sites", "html")
  if (!nzchar(html)) return(NA_character_)
  doc <- tryCatch(xml2::read_html(html), error = function(e) NULL)
  if (is.null(doc)) return(NA_character_)
  pick <- function(xp, attr) {
    v <- xml2::xml_attr(xml2::xml_find_all(doc, xp), attr)
    v <- v[!is.na(v) & nzchar(v) & !startsWith(v, "data:")]
    if (length(v)) v[1] else NA_character_
  }
  lc <- "translate(%s,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz')"
  has_logo <- function(a) sprintf("contains(%s,'logo')", sprintf(lc, a))
  img_xp <- sprintf("//img[%s or %s or %s or %s or ancestor::*[%s or %s]]",
                    has_logo("@src"), has_logo("@alt"), has_logo("@class"), has_logo("@id"),
                    has_logo("@class"), has_logo("@id"))
  src <- pick(img_xp, "src")
  if (is.na(src)) src <- pick(sprintf("%s[%s]", "//meta[@property='og:image']", has_logo("@content")), "content")
  if (is.na(src)) src <- pick("//link[@rel='apple-touch-icon']", "href")
  if (is.na(src)) return(NA_character_)
  xml2::url_absolute(src, paste0(site, "/"))
}

# --- 2. tournaments ---------------------------------------------------------

api_key <- "da2-gsrx5bibzbb4njvhl7t37wqyl4"
schedule_query <- function(season) sprintf(
  '{ schedule(tourCode: "R", year: "%d") { completed { tournaments {
     id tournamentName tournamentLogo } } upcoming { tournaments {
     id tournamentName tournamentLogo } } } }',
  season
)
pga_schedule <- bind_rows(lapply(2012:this_year, function(season) {
  txt <- fetch("https://orchestrator.pgatour.com/graphql", "pgatour", "json",
               headers = list("Content-Type" = "application/json", "x-api-key" = api_key),
               body = toJSON(list(query = schedule_query(season)), auto_unbox = TRUE), delay = 2)
  s <- fromJSON(txt, simplifyVector = FALSE)$data$schedule
  months <- c(s$completed, s$upcoming)
  bind_rows(lapply(months, function(m) bind_rows(lapply(m$tournaments, function(t) {
    tibble(pga_tournament_id = t$id, name = t$tournamentName, logo_url = t$tournamentLogo %||% NA_character_)
  })))) |>
    mutate(season = season)
})) |>
  distinct(pga_tournament_id, .keep_all = TRUE) |>
  filter(!is.na(logo_url))

# The CDN serves the r000 placeholder when an event has no logo of its own
default_logo <- "https://res.cloudinary.com/pgatour-prod/tournaments/logos/r000.png"
n_events <- nrow(pga_schedule)
pga_checks <- check_urls(c(default_logo, pga_schedule$logo_url))
default_md5 <- pga_checks$md5[1]
pga_schedule <- pga_schedule |>
  mutate(md5 = pga_checks$md5[-1]) |>
  filter(!grepl("/r000\\.png$", logo_url), is.na(md5) | md5 != default_md5) |>
  select(-md5)

event_ids <- readRDS(gzcon(url("https://github.com/array-carpenter/golfastr/releases/download/ids/event_ids.rds")))
pga_tournaments <- pga_schedule |>
  left_join(distinct(event_ids, pga_tournament_id, espn_tournament_id), by = "pga_tournament_id") |>
  transmute(logo_type = "tournament", name, logo_url, logo_source = "pgatour",
            source_page = paste0("https://www.pgatour.com/schedule/", season),
            tour = "pga", pga_tournament_id, espn_tournament_id)
message(nrow(pga_tournaments), " PGA Tour event logos (", n_events, " events checked)")

# --- 3. courses -------------------------------------------------------------

course_files <- list.files(shotlink_out, "^tee_times_\\d{4}\\.rds$", full.names = TRUE)
tee <- if (length(course_files)) {
  bind_rows(lapply(course_files, function(f) readRDS(f)[, c("season", "tournament_id", "course_id", "course_name")]))
} else {
  bind_rows(lapply(2012:this_year, function(y) {
    readRDS(gzcon(url(sprintf("https://github.com/array-carpenter/golfastr/releases/download/tee_times/tee_times_%d.rds", y))))
  }))
}
courses <- tee |>
  filter(!is.na(course_name)) |>
  distinct(season, tournament_id, course_id, course_name) |>
  arrange(desc(season)) |>
  distinct(course_id, course_name, .keep_all = TRUE) |>
  select(course_id, course_name, season)

# --- 5. matching ------------------------------------------------------------

# "TPC Sawgrass (THE PLAYERS Stadium Course)" -> "tpc sawgrass"
match_key <- function(x) {
  x <- tolower(iconv(x, to = "ASCII//TRANSLIT"))
  x <- sub("\\s*\\(.*\\)\\s*$", "", x)
  x <- sub("\\s+-\\s+.*$", "", x)
  x <- gsub("\\b(stadium|champion|north|south|east|west|blue|black|old|plantation|seaside) course\\b", "", x)
  x <- gsub("&", " and ", x)
  x <- gsub("\\bg and cc\\b", "golf and country club", x)
  x <- gsub("\\bgc\\b", "golf club", x)
  x <- gsub("\\bcc\\b", "country club", x)
  x <- gsub("[^a-z0-9 ]", "", x)
  w <- setdiff(strsplit(x, "\\s+")[[1]], c("", "the", "golf", "country", "club", "links", "course",
                                            "resort", "and", "at", "lodge", "spa", "gc", "cc"))
  if (!length(w)) return(trimws(x))
  paste(w, collapse = " ")
}

match_clubs <- function(names, club_names) {
  keys <- vapply(names, match_key, "", USE.NAMES = FALSE)
  club_keys <- vapply(club_names, match_key, "", USE.NAMES = FALSE)
  vapply(keys, function(k) {
    hit <- which(club_keys == k)
    if (!length(hit)) {
      # every word of the club key appears in the course key, or the reverse
      kw <- strsplit(k, " ")[[1]]
      overlap <- vapply(strsplit(club_keys, " "), function(cw) {
        if (all(cw %in% kw) || all(kw %in% cw)) length(intersect(cw, kw)) else 0L
      }, 1L)
      hit <- which(overlap > 0 & overlap == max(overlap))
    }
    if (!length(hit)) {
      d <- utils::adist(k, club_keys) / pmax(nchar(k), nchar(club_keys))
      hit <- which(d <= 0.15)
    }
    if (length(hit) == 1) club_names[hit] else NA_character_
  }, "")
}

# Courses that match no host club: search Wikipedia for the club
search_club <- function(name) {
  r <- wiki(action = "query", list = "search", srsearch = paste(sub("\\s*\\(.*\\)$", "", name), "golf"),
            srlimit = 3)
  hits <- vapply(r$query$search %||% list(), function(x) x$title, "")
  hits <- hits[!grepl("^List", hits) & vapply(hits, function(h) any(name_tokens(h) %in% name_tokens(name)), TRUE)]
  if (!length(hits)) return(NA_character_)
  cats <- page_categories(hits)$cats
  ok <- hits[vapply(hits, function(h) is_club(cats[[h]] %||% character()), TRUE)]
  if (length(ok)) ok[1] else NA_character_
}

# --- overrides --------------------------------------------------------------

# logo_type, key, club_title, website, logo_url, logo_source, source_page,
# primary_color, secondary_color, note.
#   course: key = ShotLink course_name, club_title = the club's Wikipedia
#           title (or a club added below), "none" for no club
#   club:   key = Wikipedia title (or a new club name)
#   logo_url "none" removes a wrong logo
#   tour:   key = tour code;  tournament: key = name
overrides <- utils::read.csv("data-raw/logo_overrides.csv", colClasses = "character", na.strings = "")
ov <- function(type) overrides[overrides$logo_type == type, ]

apply_overrides <- function(df, type, key) {
  o <- ov(type)
  cols <- intersect(c("logo_url", "logo_source", "source_page", "primary_color", "secondary_color"), names(o))
  for (i in seq_len(nrow(o))) {
    hit <- df[[key]] == o$key[i]
    if (!any(hit, na.rm = TRUE)) {
      message("override not used: ", type, " / ", o$key[i])
      next
    }
    for (col in cols) if (!is.na(o[[col]][i])) df[[col]][which(hit)] <- o[[col]][i]
  }
  df$logo_url[df$logo_url %in% "none"] <- NA
  df
}

# --- clubs ------------------------------------------------------------------

course_map <- ov("course")
host_titles <- unique(hosts$title)
courses$club_title <- match_clubs(courses$course_name, host_titles)
unmatched <- is.na(courses$club_title) & !courses$course_name %in% course_map$key
courses$club_title[unmatched] <- vapply(courses$course_name[unmatched], search_club, "")
hit <- match(courses$course_name, course_map$key)
courses$club_title[!is.na(hit)] <- course_map$club_title[hit[!is.na(hit)]]
courses$club_title[courses$club_title %in% "none"] <- NA

club_names <- unique(c(host_titles, courses$club_title[!is.na(courses$club_title)], ov("club")$key))
club_wiki <- wiki_logos(club_names, sites = TRUE)
clubs <- tibble(title = club_names) |>
  left_join(distinct(club_wiki, title, .keep_all = TRUE), by = "title") |>
  mutate(name = sub("\\s*\\(.*\\)$", "", title),
         logo_source = ifelse(is.na(logo_url), NA, "wikimedia"))

club_ov <- ov("club")
site_hit <- match(clubs$title, club_ov$key)
use_site <- !is.na(site_hit) & !is.na(club_ov$website[site_hit])
clubs$website[use_site] <- club_ov$website[site_hit[use_site]]

# Club site for clubs with no file on Wikimedia
need_site <- which(is.na(clubs$logo_url) & !is.na(clubs$website))
for (i in need_site) {
  u <- site_logo(clubs$website[i])
  if (!is.na(u) && image_ok(u)) {
    clubs$logo_url[i] <- u
    clubs$logo_source[i] <- "club_site"
    clubs$source_page[i] <- clubs$website[i]
  }
}

slug <- function(x) {
  s <- tolower(iconv(x, to = "ASCII//TRANSLIT"))
  s <- gsub("[^a-z0-9& -]", "", s)
  s <- gsub("\\s+", " ", gsub("&", " and ", s))
  short <- sub("\\s+(golf (and|&) country club|golf club|country club|golf links|golf course|golf resort|club)$", "", s)
  short <- sub("\\s+and$", "", short)
  short <- ifelse(nchar(short) < 3 | short == "the", s, short)
  gsub("(^-|-$)", "", gsub("[^a-z0-9]+", "-", short))
}
clubs <- clubs |>
  mutate(club_id = slug(name),
         club_id = ifelse(duplicated(club_id) | duplicated(club_id, fromLast = TRUE), slug(title), club_id),
         primary_color = NA_character_, secondary_color = NA_character_) |>
  apply_overrides("club", "title")

# --- tours and Wikimedia tournaments ----------------------------------------

tour_wiki <- wiki_logos(tours$title)
tour_rows <- tours |>
  left_join(distinct(tour_wiki, title, .keep_all = TRUE) |> select(title, logo_url, source_page), by = "title") |>
  mutate(logo_source = ifelse(is.na(logo_url), NA, "wikimedia"))
for (i in which(is.na(tour_rows$logo_url))) {
  u <- site_logo(tour_rows$site[i])
  if (!is.na(u) && image_ok(u)) {
    tour_rows$logo_url[i] <- u
    tour_rows$logo_source[i] <- "tour_site"
    tour_rows$source_page[i] <- tour_rows$site[i]
  }
}
tour_rows <- tour_rows |>
  transmute(logo_type = "tour", key = tour, name, logo_url, logo_source, source_page, tour,
            primary_color = NA_character_, secondary_color = NA_character_) |>
  apply_overrides("tour", "key") |>
  select(-key)

category_members <- function(cat) {
  out <- character()
  cont <- list()
  repeat {
    r <- do.call(wiki, c(list(action = "query", list = "categorymembers", cmtitle = cat,
                              cmnamespace = 0, cmlimit = "max"), cont))
    out <- c(out, vapply(r$query$categorymembers, function(x) x$title, ""))
    if (is.null(r$continue)) break
    cont <- r$continue
  }
  out
}
event_titles <- bind_rows(
  tibble(title = c("Masters Tournament", "U.S. Open (golf)", "PGA Championship", "The Open Championship",
                   "Ryder Cup", "Presidents Cup"), tour = "pga"),
  tibble(title = c("U.S. Women's Open", "Women's PGA Championship", "Chevron Championship",
                   "Women's British Open", "Evian Championship", "Solheim Cup"), tour = "lpga"),
  tibble(title = c(category_members("Category:PGA Tour events"),
                   category_members("Category:Former PGA Tour events")), tour = "pga"),
  tibble(title = c(category_members("Category:LPGA Tour events"),
                   category_members("Category:Former LPGA Tour events")), tour = "lpga")
) |>
  distinct(title, .keep_all = TRUE)
event_wiki <- wiki_logos(event_titles$title)
wiki_tournaments <- event_titles |>
  left_join(distinct(event_wiki, title, .keep_all = TRUE) |> select(title, logo_url, source_page), by = "title") |>
  filter(!is.na(logo_url)) |>
  transmute(logo_type = "tournament", name = sub("\\s*\\(.*\\)$", "", title), logo_url,
            logo_source = "wikimedia", source_page, tour,
            primary_color = NA_character_, secondary_color = NA_character_) |>
  apply_overrides("tournament", "name") |>
  filter(!is.na(logo_url))
message(nrow(wiki_tournaments), " tournament logos from Wikimedia (", nrow(event_titles), " event pages)")

# --- assemble ---------------------------------------------------------------

club_rows <- clubs |>
  filter(!is.na(logo_url)) |>
  transmute(logo_type = "club", name, logo_url, logo_source, source_page, club_id,
            primary_color, secondary_color)

course_rows <- courses |>
  inner_join(select(clubs, title, club_id, logo_url, logo_source, source_page, primary_color, secondary_color) |>
               filter(!is.na(logo_url)),
             by = c("club_title" = "title")) |>
  arrange(course_id, club_id, desc(season)) |>
  distinct(course_id, club_id, .keep_all = TRUE) |>
  transmute(logo_type = "course", name = course_name, logo_url, logo_source, source_page,
            course_id, club_id, primary_color, secondary_color)

logos <- bind_rows(tour_rows, pga_tournaments, wiki_tournaments, course_rows, club_rows) |>
  filter(!is.na(logo_url))
ck <- check_urls(logos$logo_url)
ok <- !is.na(ck$status) & ck$status == 200 & grepl("^image/", ck$type)
if (any(!ok)) {
  message(sum(!ok), " logo URLs failed the check and were dropped:")
  print(as.data.frame(cbind(logos[!ok, c("logo_type", "name", "logo_url")], status = ck$status[!ok])))
}
logos$checked_at <- ck$checked_at
cols <- c("logo_type", "name", "logo_url", "logo_source", "source_page", "tour",
          "pga_tournament_id", "espn_tournament_id", "course_id", "club_id",
          "primary_color", "secondary_color", "checked_at")
logos <- logos[ok, ]
for (col in setdiff(cols, names(logos))) logos[[col]] <- NA_character_
logos <- logos[, cols] |>
  arrange(match(logo_type, c("tour", "tournament", "course", "club")), name)

missing_clubs <- clubs |>
  filter(!club_id %in% logos$club_id[logos$logo_type == "club"]) |>
  transmute(logo_type = "club", name, course_id = NA_character_, course_name = NA_character_,
            club_title = title, website)
missing_courses <- courses |>
  filter(!course_id %in% logos$course_id) |>
  distinct(course_id, .keep_all = TRUE) |>
  transmute(logo_type = "course", name = course_name, course_id, course_name, club_title,
            website = NA_character_)
missing <- bind_rows(missing_clubs, missing_courses)
utils::write.csv(missing, "data-raw/logos_missing.csv", row.names = FALSE, na = "")

message(nrow(logos), " logos: ", paste(names(table(logos$logo_type)), table(logos$logo_type), collapse = ", "),
        "; ", nrow(missing_clubs), " clubs and ", nrow(missing_courses), " courses without a logo")
saveRDS(logos, file.path(cache_dir, "logos.rds"))

if ("--upload" %in% commandArgs(trailingOnly = TRUE)) {
  out <- file.path(tempdir(), "logos")
  dir.create(out, showWarnings = FALSE)
  base <- file.path(out, "logos")
  saveRDS(logos, paste0(base, ".rds"))
  utils::write.csv(logos, paste0(base, ".csv"), row.names = FALSE)
  arrow::write_parquet(logos, paste0(base, ".parquet"))
  repo <- "array-carpenter/golfastr"
  if (!"logos" %in% piggyback::pb_releases(repo)$tag_name) {
    piggyback::pb_release_create(repo, "logos", name = "Logos",
                                 body = "Logo URLs for tours, tournaments, courses and clubs (load_logos()).")
  }
  for (f in list.files(out, full.names = TRUE)) {
    piggyback::pb_upload(f, repo = repo, tag = "logos", overwrite = TRUE)
  }
}
