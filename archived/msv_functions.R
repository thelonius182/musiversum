salsa_git_version <- function(qfn_repo) {

  repo <- git2r::repository(qfn_repo)
  branch <- git2r::repository_head(repo)$name
  latest_commit <- git2r::commits(repo, n = 1)[[1]]
  commit_author <- latest_commit$author$name
  commit_date <- latest_commit$author$when
  fmt_commit_date <- format(lubridate::with_tz(commit_date, tzone = "Europe/Amsterdam"), "%a %Y-%m-%d, %H:%M")

  return(list(git_branch = branch, ts = fmt_commit_date, by = commit_author, path = repo$path))
}

search_wikidata <- function(name, lang) {

  api_response <- GET("https://www.wikidata.org/w/api.php",
                      query = list(action = "wbsearchentities",
                                   search = name,
                                   language = lang,
                                   format = "json")
  )

  # Parse JSON response
  result <- content(response, as = "parsed")

  # No match found
  if (length(result$search) == 0) return("Niet gevonden")

  # Get first matched entity ID
  entity_id <- result$search[[1]]$id

  # Fetch entity details to get the Wikipedia link
  entity_url <- str_glue("https://www.wikidata.org/wiki/Special:EntityData/{entity_id}.json")
  entity_response <- fromJSON(entity_url)

  # Extract Dutch Wikipedia link
  nlwiki <- entity_response$entities[[entity_id]]$sitelinks$nlwiki$title

  if (is.null(nlwiki)) return("Niet gevonden")

  # Construct Wikipedia URL
  str_glue("https://nl.wikipedia.org/wiki/{URLencode(nlwiki)}")
}

get_wikipedia_url <- function(name, lang = "nl") {
  # Define Wikidata API endpoint
  base_url <- "https://www.wikidata.org/w/api.php"

  # Create and perform search request
  search_req <- request(base_url) |>
    req_url_query(
      action = "wbsearchentities",
      search = name,
      language = lang,
      format = "json"
    )

  search_resp <- search_req |> req_perform()
  search_data <- search_resp |> resp_body_json()

  if (is.null(search_data$success) || search_data$success != 1) {
    flog.error(glue("API failed to respond >> no results for {name}"), name = config$log_slug)
    return(NA)
  }

    if (length(search_data$search) == 0) {
    return("Niet gevonden")
  }

  # Get the entity ID of the first search result
  entity_id <- search_data$search[[1]]$id

  # Build URL for the full entity data in JSON format
  entity_url <- glue("https://www.wikidata.org/wiki/Special:EntityData/{entity_id}.json")

  # Fetch and parse entity data
  entity_req <- request(entity_url)
  entity_resp <- entity_req |> req_perform()
  entity_data <- entity_resp |> resp_body_json()

  # Attempt to extract the Dutch Wikipedia title
  nlwiki <- entity_data$entities[[entity_id]]$sitelinks$nlwiki$title

  if (is.null(nlwiki)) {
    return("Niet gevonden")
  }

  # Construct full Wikipedia URL
  glue("https://nl.wikipedia.org/wiki/{URLencode(nlwiki)}")
}
