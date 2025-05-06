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

    resp <- tryCatch(
      {
        request("https://www.wikidata.org/w/api.php") |>
          req_url_query(
            action = "wbsearchentities",
            search = name,
            language = language,
            format = "json"
          ) |> req_perform()
      }, error = function(e) {
        flog.warn(str_glue("wikidata: {name}, {conditionMessage(e)}"), name = config$log_slug)
        return(NA_character_)
      }
    )

    if (length(resp) == 1 || resp_status(resp) != 200) return(NULL)  # Name not found

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

  if (is.null(matches)) {
    parts <- str_split(name, "[- ]", simplify = TRUE)

    if (ncol(parts) > 1) {
      name <- paste(parts[1, 1:min(3, ncol(parts))], collapse = " ")
      matches <- try_search(lang)

      if (is.null(matches) && ncol(parts) > 2) {
        name <- paste(parts[1, 1], parts[1,3], collapse = " ")
        matches <- try_search(lang)
      }
    }
  }

  matches
}

# Get Wikipedia summary (Dutch or English)
get_summary <- function(title, lang) {
  if (is.na(title)) return(NA_character_)

  summary_url <- glue("https://{lang}.wikipedia.org/api/rest_v1/page/summary/{URLencode(title)}")

  resp <- tryCatch(
    {
      request(summary_url) |> req_perform()
    }, error = function(e) {
      flog.warn(str_glue("wikipedia.{lang}: {title}, {conditionMessage(e)}"), name = config$log_slug)
      return(NA_character_)
    }
  )

  if (length(resp) == 1 || resp_status(resp) != 200) return(NA_character_)  # Summary not found

  resp_body_json(resp)$extract %||% NA_character_
}

# Retrieve Wikipedia URLs and summaries for a given Wikidata ID
get_wikipedia_urls <- function(wikidata_id) {
  url <- glue("https://www.wikidata.org/wiki/Special:EntityData/{wikidata_id}.json")

  resp <- tryCatch(
    {
      request(url) |> req_perform()
    }, error = function(e) {
      flog.warn(str_glue("wikidata: {wikidata_id}, {conditionMessage(e)}"), name = config$log_slug)
      return(NA_character_)
    }
  )

  if (length(resp) == 1 || resp_status(resp) != 200) {
    return(tibble(
      wikidata_id   = wikidata_id,
      wikipedia_nl  = NA_character_,
      wikipedia_en  = NA_character_,
      title_nl      = NA_character_,
      title_en      = NA_character_,
      summary_nl    = NA_character_,
      summary_en    = NA_character_,
      img_url       = NA_character_
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
    title_nl   = title_nl,
    title_en   = title_en,
    summary_nl = get_summary(title_nl, "nl"),
    summary_en = get_summary(title_en, "en"),
    img_url    = get_commons_url(data$entities[[wikidata_id]])
  )
}

get_commons_url <- function(entity, width = 250) {
  image_info <- entity$claims$P18

  if (length(image_info) == 0) return(NA_character_)  # No image metadata found

  filename <- image_info[[1]]$mainsnak$datavalue$value
  filename_encoded <- URLencode(gsub(" ", "_", filename), reserved = TRUE)
  img_url <- str_glue("https://commons.wikimedia.org/wiki/Special:FilePath/{filename_encoded}")

  # check image file exists
  head_res <- tryCatch(
    {
      request(img_url) |> req_method("HEAD") |> req_options(followlocation = TRUE) |> req_perform()
    }, error = function(e) {
      flog.warn(str_glue("wikimedia: {img_url}, {conditionMessage(e)}"), name = config$log_slug)
      return(NA_character_)
    }
  )

  if (length(head_res) == 1 || resp_status(head_res) != 200) return(NA_character_)  # No image file found

  return(img_url)
}

# TO VALIDATE CONN:
# sqlstmt <- "show variables like 'character_set_client'"
# result <- dbGetQuery(conn = wp_conn, statement = sqlstmt)
get_wp_conn <- function() {

  grh_conn <- tryCatch(
    {
      dbConnect(drv = MySQL(), user = db_user, password = db_password,
                dbname = db_name, host = db_host, port = 3306)
    },
    error = function(cond) {
      return("connection-error")
    }
  )

  return(grh_conn)
}
