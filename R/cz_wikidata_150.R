pacman::p_load(readr, tidyr, dplyr, purrr)

# remove previous sts-150 results ----
cur_150s <- dir_ls(path = "h:/artist_resolver/sts_150/", type = "file")

for (qfn in cur_150s) {
  file_delete(qfn)
}

# remove previous sts-200 results ----
cur_200s <- dir_ls(path = "h:/artist_resolver/sts_200/", type = "file")

for (qfn in cur_200s) {
  file_delete(qfn)
}

# split artists in 500-row chunks ----
cz_artists_100 <- read_rds("h:/artist_resolver/sts_100/cz_artists_4p.RDS")

rows_per_file <- 500
splits <- split(cz_artists_100, (seq_len(nrow(cz_artists_100)) - 1) %/% rows_per_file)

walk2(splits, seq_along(splits), ~ write_rds(.x, paste0("h:/artist_resolver/sts_150/cz_artists_", .y, ".RDS")))

