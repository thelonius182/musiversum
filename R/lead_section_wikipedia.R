pacman::p_load(httr, jsonlite, readr, stringr, futile.logger, dplyr, purrr, glue)

# Configure logging
flog.appender(appender.file("c:/Users/nipper/Logs/dev/wikipedia_fetch.log"), name = "wiki_fetch")
flog.threshold(INFO, name = "wiki_fetch")

# Read artist names from a TSV file (assumes a column "artist")
artists <- read_tsv("C:/Users/nipper/Documents/artists.tsv", col_types = cols())

# Function to fetch Wikipedia lead section in a given language
get_wikipedia_summary <- function(artist, lang = "en") {
  article_title <- str_replace_all(artist, " ", "_")
  url <- glue("https://{lang}.wikipedia.org/api/rest_v1/page/summary/{article_title}")

  tryCatch({
    response <- GET(url)
    if (http_error(response)) {
      flog.warn(glue("[{lang}] HTTP error for '{artist}': {status_code(response)}"), name = "wiki_fetch")
      return(NA)
    }

    data <- fromJSON(content(response, "text"))
    if (!"extract" %in% names(data)) {
      flog.warn(glue("[{lang}] No summary found for '{artist}'"), name = "wiki_fetch")
      return(NA)
    }

    flog.info(glue("[{lang}] Fetched summary for '{artist}'"), name = "wiki_fetch")
    return(data$extract)
  }, error = function(e) {
    flog.error(glue("[{lang}] Error processing '{artist}': {conditionMessage(e)}"), name = "wiki_fetch")
    return(NA)
  })
}

# Fetch Wikipedia summaries
results <- artists %>%
  mutate(
    summary_en = map_chr(artist, get_wikipedia_summary, lang = "en"),
    summary_nl = map_chr(artist, get_wikipedia_summary, lang = "nl")
  )

# Save results to a TSV file
write_tsv(results, "artist_summaries.tsv")

flog.info("Completed. Summaries saved to 'artist_summaries.tsv'.", name = "wiki_fetch")
