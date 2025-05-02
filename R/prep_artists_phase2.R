
ls_resolved_artists <- dir_ls(path = "h:/artist_resolver/resources/", type = "file", regexp = "resolved_artists_d0")
combined_data <- map_dfr(ls_resolved_artists, ~ read_tsv(.x, col_types = cols(.default = "c")))
artist_reviews_dir <- "h:/artist_resolver/resources/reviews/"

resolved_artists_notfound <- combined_data |> filter(wikidata_id == "Not Found")
write_tsv(resolved_artists_notfound, file = str_glue("{artist_reviews_dir}resolved_artists_notfound.tsv", na = ""))

resolved_artists_review <- combined_data |> filter(wikidata_id == "Needs Review")
write_tsv(resolved_artists_review, file = str_glue("{artist_reviews_dir}resolved_artists_review.tsv", na = ""))

resolved_artists_NL_only <- combined_data |> filter(str_detect(wikidata_id, "^Q") & is.na(wikipedia_en) & !is.na(wikipedia_nl))
write_tsv(resolved_artists_NL_only, file = str_glue("{artist_reviews_dir}resolved_artists_NL_only.tsv", na = ""))

resolved_artists_EN_only <- combined_data |> filter(str_detect(wikidata_id, "^Q") & !is.na(wikipedia_en) & is.na(wikipedia_nl))
write_tsv(resolved_artists_EN_only, file = str_glue("{artist_reviews_dir}resolved_artists_EN_only.tsv", na = ""))

resolved_artists_ID_only <- combined_data |> filter(str_detect(wikidata_id, "^Q") & is.na(wikipedia_en) & is.na(wikipedia_nl))
write_tsv(resolved_artists_ID_only, file = str_glue("{artist_reviews_dir}resolved_artists_ID_only.tsv", na = ""))

resolved_artists_full_summary <- combined_data |>
  filter(str_detect(wikidata_id, "^Q") & !is.na(wikipedia_en) & !is.na(wikipedia_nl))
write_tsv(resolved_artists_full_summary, file = str_glue("{artist_reviews_dir}resolved_artists_NL_EN.tsv", na = ""))

resolved_artists_duplicate_ids <- combined_data |> filter(str_detect(wikidata_id, "^Q")) |> group_by(wikidata_id) |>
  summarise(n = n()) |> filter(n > 1)
resolved_artists_duplicates <- combined_data |> filter(wikidata_id %in% resolved_artists_duplicate_ids$wikidata_id) |>
  arrange(wikidata_id)
write_tsv(resolved_artists_duplicates, file = str_glue("{artist_reviews_dir}resolved_artists_duplicates.tsv", na = ""))
