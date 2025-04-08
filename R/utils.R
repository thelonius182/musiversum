salsa_git_version <- function(qfn_repo) {

  repo <- repository(qfn_repo)
  branch <- repository_head(repo)$name
  latest_commit <- commits(repo, n = 1)[[1]]
  commit_author <- latest_commit$author$name
  commit_date <- latest_commit$author$when
  fmt_commit_date <- format(with_tz(commit_date, tzone = "Europe/Amsterdam"), "%a %Y-%m-%d, %H:%M")

  return(list(git_branch = branch, ts = fmt_commit_date, by = commit_author, path = repo$path))
}

get_entity_matches <- function(name, lang = "nl", fallback_lang = "en") {

  try_search <- function(language) {
    resp <- request("https://www.wikidata.org/w/api.php") |>
      req_url_query(
        action = "wbsearchentities",
        search = name,
        language = language,
        format = "json"
      ) |> req_perform()

    if (resp_status(resp) != 200) return(NULL)

    data <- resp_body_json(resp)
    matches <- data$search

    if (length(matches) == 0) return(NULL)

    tibble(
      wikidata_id  = map_chr(matches, ~ .x$id %||% NA_character_),
      label        = map_chr(matches, ~ .x$label %||% NA_character_),
      description  = map_chr(matches, ~ .x$description %||% NA_character_),
      match_score  = map_dbl(matches, ~ .x$match$score %||% 0),
      language     = language
    )
  }

  matches <- try_search(lang)
  if (is.null(matches) && lang != fallback_lang) {
    matches <- try_search(fallback_lang)
  }

  matches
}

# Get Wikipedia summary (Dutch or English)
get_summary <- function(title, lang) {
  if (is.na(title)) return(NA_character_)

  summary_url <- glue("https://{lang}.wikipedia.org/api/rest_v1/page/summary/{URLencode(title)}")

  resp <- request(summary_url) |> req_perform()

  if (resp_status(resp) != 200) return(NA_character_)

  resp_body_json(resp)$extract %||% NA_character_
}

# Retrieve Wikipedia URLs and summaries for a given Wikidata ID
get_wikipedia_urls <- function(wikidata_id) {
  url <- glue("https://www.wikidata.org/wiki/Special:EntityData/{wikidata_id}.json")

  resp <- request(url) |> req_perform()

  if (resp_status(resp) != 200) {
    return(tibble(
      wikidata_id   = wikidata_id,
      wikipedia_nl  = NA_character_,
      wikipedia_en  = NA_character_,
      title_nl      = NA_character_,
      title_en      = NA_character_,
      summary_nl    = NA_character_,
      summary_en    = NA_character_
    ))
  }

  data <- resp_body_json(resp)
  sitelinks <- data$entities[[wikidata_id]]$sitelinks

  title_nl <- sitelinks$nlwiki$title %||% NA_character_
  title_en <- sitelinks$enwiki$title %||% NA_character_

  tibble(
    wikidata_id   = wikidata_id,
    wikipedia_nl  = if (!is.na(title_nl)) {
      glue("https://nl.wikipedia.org/wiki/{URLencode(title_nl)}")
    } else {
      NA_character_
    },
    wikipedia_en  = if (!is.na(title_en)) {
      glue("https://en.wikipedia.org/wiki/{URLencode(title_en)}")
    } else {
      NA_character_
    },
    title_nl      = title_nl,
    title_en      = title_en,
    summary_nl    = get_summary(title_nl, "nl"),
    summary_en    = get_summary(title_en, "en")
  )
}
