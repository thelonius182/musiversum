pacman::p_load(readr, tidyr, dplyr, purrr)

cz_artists_100 <- read_rds("h:/artist_resolver/sts_100/cz_artists_4p.RDS")

rows_per_file <- 500
splits <- split(cz_artists_100, (seq_len(nrow(cz_artists_100)) - 1) %/% rows_per_file)

walk2(splits, seq_along(splits), ~ write_rds(.x, paste0("h:/artist_resolver/sts_150/cz_artists_", .y, ".RDS")))
