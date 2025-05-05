# Combine all results from batch-resolver job
ls_wd_artists_200 <- dir_ls(path = "h:/artist_resolver/sts_200/", type = "file")
combined_data_200 <- map_dfr(ls_wd_artists_200, ~ read_rds(.x))

# combine them with the existing sts_300 objects
dir_300 <- "h:/artist_resolver/sts_300/"
ls_wd_artists_300 <- dir_ls(path = dir_300, type = "file", regexp = "\\.RDS$")

if (length(ls_wd_artists_300) > 0) {
  combined_data_300 <- map_dfr(ls_wd_artists_300, ~ read_rds(.x))
  combined_data_200 <- bind_rows(combined_data_200, combined_data_300) |> distinct()
}

# classify the result
# . duplicate Q-id's
wd_artists_200_duplicate_ids <- combined_data_200 |> filter(str_detect(wikidata_id, "^Q")) |> group_by(wikidata_id) |>
  summarise(n = n()) |> filter(n > 1)

if (nrow(wd_artists_200_duplicate_ids) > 0) {
  wd_artists_dup_qid <- combined_data_200 |> filter(wikidata_id %in% wd_artists_200_duplicate_ids$wikidata_id) |>
    arrange(wikidata_id)
  write_rds(wd_artists_dup_qid, file = str_glue("{dir_300}wd_artists_dup_qid.RDS"))
  write_tsv(wd_artists_dup_qid, file = str_glue("{dir_300}wd_artists_dup_qid.tsv", na = ""))
}

# . not found ----
# ((remove dups first)
combined_data_200 <- combined_data_200 |> filter(!wikidata_id %in% wd_artists_200_duplicate_ids$wikidata_id)

wd_artists_notfound <- combined_data_200 |> filter(wikidata_id == "Not Found")
write_rds(wd_artists_notfound, file = str_glue("{dir_300}wd_artists_notfound.RDS"))
write_tsv(wd_artists_notfound, file = str_glue("{dir_300}wd_artists_notfound.tsv", na = ""))

# . review ----
# ((remove not-founds first)
combined_data_200 <- combined_data_200 |> filter(!wikidata_id %in% wd_artists_notfound$wikidata_id)

wd_artists_review <- combined_data_200 |>
  filter(wikidata_id == "Needs Review" | (str_detect(wikidata_id, "^Q") & is.na(wikipedia_en) & is.na(wikipedia_nl)))
write_rds(wd_artists_review, file = str_glue("{dir_300}wd_artists_review.RDS"))

# . resolved ----
# ((remove reviews first)
combined_data_200 <- combined_data_200 |> filter(!wikidata_id %in% wd_artists_review$wikidata_id)

wd_artists_resolved <- combined_data_200 |> filter(str_detect(wikidata_id, "^Q"))
write_rds(wd_artists_resolved, file = str_glue("{dir_300}wd_artists_resolved.RDS"))
