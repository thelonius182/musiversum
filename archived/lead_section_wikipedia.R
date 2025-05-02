pacman::p_load(httr2, jsonlite, readr, stringr, futile.logger, dplyr, purrr, glue, tidyr)

# Configure logging
flog.appender(appender.file("c:/Users/nipper/Logs/dev/wikipedia_fetch.log"), name = "wiki_fetch")
flog.threshold(INFO, name = "wiki_fetch")

# Read artist names from a TSV file (assumes a column "artist")
artists <- read_tsv("C:/Users/nipper/Documents/artists.tsv", col_types = cols())

# Function to fetch Wikipedia lead section in a given language
get_wikipedia_summary <- function(artist, lang = "en") {
  article_title <- str_replace_all(artist, " ", "_")
  url <- glue("https://{lang}.wikipedia.org/api/rest_v1/page/summary/{article_title}")

  # Create the request
  req <- request(url) |> req_error(is_error = \(x) resp_status(x) >= 400)
  df_na <- tibble(summary = NA, wikipedia = NA, wikidata_item = NA, artist_img = NA)

  # Perform the request
  tryCatch({
    response <- req |> req_perform()

    # Extract JSON content
    data <- response |> resp_body_json(simplifyVector = TRUE)

    # Check if summary exists
    if (!"extract" %in% names(data)) {
      flog.warn(glue("[{lang}] No summary found for '{artist}'"), name = "wiki_fetch")
      return(df_na)
    }

    # Extract required fields
    summary <- data$extract
    wikipedia <- data$content_urls$desktop$page %||% NA
    wikidata_item <- data$wikibase_item %||% NA
    if (!is.na(wikidata_item)) {
      wikidata_item <- paste0("https://www.wikidata.org/wiki/", wikidata_item)
    }
    artist_img <- data$originalimage$source %||% NA

    flog.info(glue("[{lang}] Fetched items for '{artist}'"), name = "wiki_fetch")
    return(tibble(summary, wikipedia, wikidata_item, artist_img))
  }, error = function(e) {
    flog.error(glue("[{lang}] Error processing '{artist}': {conditionMessage(e)}"), name = "wiki_fetch")
    return(df_na)
  })
}

# Fetch Wikipedia items
results <- artists |>
  mutate(
    en_data = map(artist, get_wikipedia_summary, lang = "en"),
    nl_data = map(artist, get_wikipedia_summary, lang = "nl")
  ) |>
  unnest_wider(en_data, names_sep = "_en") |>
  unnest_wider(nl_data, names_sep = "_nl") |>
  select(artist, artist_slug, cz_artist_id = artist_id,
         summary_nl = nl_data_nlsummary,
         summary_en = en_data_ensummary,
         wikipedia_nl = nl_data_nlwikipedia,
         wikipedia_en = en_data_enwikipedia,
         wikidata = en_data_enwikidata_item,
         artist_img = nl_data_nlartist_img
  )

# Save results to a TSV file
write_tsv(results, "C:/Users/nipper/Downloads/cz_downloads/artist_summaries.tsv")

flog.info("Completed. Summaries saved to 'artist_summaries.tsv'.", name = "wiki_fetch")
