pacman::p_load(
  conflicted,
  dplyr,
  fs,
  glue,
  purrr,
  readr,
  stringr,
  tidyr,
  yaml
)
conflicts_prefer(dplyr::filter, dplyr::pull, .quiet = TRUE)

# Get command-line arguments
# args <- commandArgs(trailingOnly = TRUE)
# qfn_review_results <- args[1]

while (TRUE) {

  # review_results <- read_rds(qfn_review_results)
  review_results <- read_rds("h:/artist_resolver/sts_400/wd_artists_reviewed.RDS")

  if (nrow(review_results) == 0) break

  # turn "rejected" into "not found"
  review_results <- review_results |> select(-modify) |>
    mutate(wikidata_id = if_else(wikidata_id == "Rejected", "Not Found", wikidata_id))

  # combine them with the existing sts_300 objects
  dir_300 <- "h:/artist_resolver/sts_300/"
  ls_wd_artists_300 <- dir_ls(path = dir_300, type = "file", regexp = "\\.RDS$")

  if (length(ls_wd_artists_300) > 0) {
    # remove review results
    combined_data_300 <- map_dfr(ls_wd_artists_300, ~ read_rds(.x)) |>
      filter(!artist_czid %in% review_results$artist_czid)
    review_results <- bind_rows(review_results, combined_data_300) |> distinct()
  }

  # classify the result
  # . duplicate Q-id's
  review_results_duplicate_ids <- review_results |> filter(str_detect(wikidata_id, "^Q")) |> group_by(wikidata_id) |>
    summarise(n = n()) |> filter(n > 1)

  if (nrow(review_results_duplicate_ids) > 0) {
    wd_artists_dup_qid <- review_results |> filter(wikidata_id %in% review_results_duplicate_ids$wikidata_id) |>
      arrange(wikidata_id)
    write_rds(wd_artists_dup_qid, file = str_glue("{dir_300}wd_artists_dup_qid.RDS"))
    write_tsv(wd_artists_dup_qid, file = str_glue("{dir_300}wd_artists_dup_qid.tsv", na = ""))
    # remove dups
    review_results <- review_results |> filter(!wikidata_id %in% review_results_duplicate_ids$wikidata_id)
  }

  # . not found ----
  wd_artists_notfound <- review_results |> filter(wikidata_id == "Not Found")
  write_rds(wd_artists_notfound, file = str_glue("{dir_300}wd_artists_notfound.RDS"))
  write_tsv(wd_artists_notfound, file = str_glue("{dir_300}wd_artists_notfound.tsv", na = ""))

  # . review ----
  # ((remove not-founds first)
  review_results <- review_results |> filter(!wikidata_id %in% wd_artists_notfound$wikidata_id)

  wd_artists_review <- review_results |> filter(wikidata_id == "Needs Review")
  write_rds(wd_artists_review, file = str_glue("{dir_300}wd_artists_review.RDS"))

  # . resolved ----
  # ((remove reviews first)
  review_results <- review_results |> filter(!wikidata_id %in% wd_artists_review$wikidata_id)

  wd_artists_resolved <- review_results |> filter(str_detect(wikidata_id, "^Q"))
  write_rds(wd_artists_resolved, file = str_glue("{dir_300}wd_artists_resolved.RDS"))

  break
}
