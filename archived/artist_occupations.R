pacman::p_load(httr2, jsonlite, readr, tidyr, dplyr, purrr, fs, conflicted)
conflict_prefer("filter", "dplyr", quiet = TRUE)
conflict_prefer("pull", "dplyr", quiet = TRUE)

# Load Q-IDs from a TSV file
# The file should have a column called 'occupation' (e.g., "Q639669")
occupation_ids <- read_tsv(file = "/mnt/muw/cz_artists_parts/artist_occupations/occupations.tsv",
                           col_types = cols(.default = "c")) |> pull(occupation) |> unique()

# Create VALUES clause for SPARQL
sparql_values <- paste0("wd:", occupation_ids, collapse = " ")

# Set batch size and max offset (adjust based on expected size)
batch_size <- 500
max_offset <- 50000  # up to ~40 batches; adjust based on estimated count

# Output folder for caching
wikidata_batches_dir <- "/mnt/muw/cz_artists_parts/artist_occupations/wikidata_batches"
dir_create(wikidata_batches_dir, showWarnings = FALSE)

# SPARQL query template
sparql_template <- 'SELECT ?musician ?musicianLabel ?occupation ?occupationLabel WHERE {
  ?musician wdt:P106 ?occupation.
  VALUES ?occupation { wd:%s }
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}
LIMIT %d
OFFSET %d'

# Loop over Q-ID's
for (qid in occupation_ids) {
  # Loop over batches
  for (offset in seq(0, max_offset, by = batch_size)) {
    batch_file <- str_glue("{wikidata_batches_dir}/batch_{qid}_{offset}.rds")

    if (file_exists(batch_file)) {
      message(str_glue("Skipping offset {qid}_{offset} (cached)."))
      next
    }

    query <- sprintf(sparql_template, qid, batch_size, offset)

    Sys.sleep(1)  # Be kind to Wikidata

    message(glue("Fetching OFFSET = {qid}_{offset}..."))

    batch_result <- tryCatch({
      request("https://query.wikidata.org/sparql") |>
        req_url_query(query = query, format = "json") |>
        req_user_agent("R httr2 client - occupation search w/cache") |>
        req_perform() |>
        resp_body_json() |>
        pluck("results", "bindings") |>
        map_dfr(~ tibble(
          musician = .x$musician$value,
          label = .x$musicianLabel$value,
          occupation_id = .x$occupation$value,
          occupation_label = .x$occupationLabel$value
        ))
    }, error = function(e) {
      message("Error fetching offset ", qid, "_", offset, ": ", conditionMessage(e))
      return(NULL)
    })

    if (is.null(batch_result) || nrow(batch_result) == 0) break

    saveRDS(batch_result, batch_file)
  }
}

# Combine all batch files
all_batches <- list.files(wikidata_batches_dir, full.names = TRUE, pattern = "\\.rds$")
musician_df <- map_dfr(all_batches, readRDS) |>
  distinct()

# Save final combined result
saveRDS(musician_df, str_glue("{wikidata_batches_dir}/musicians_combined.rds"))
write_tsv(musician_df, str_glue("{wikidata_batches_dir}/musicians_combined.tsv"))

# Summary
cat("Total musicians retrieved:", nrow(musician_df), "\n")
