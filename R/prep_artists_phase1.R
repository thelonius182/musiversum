pacman::p_load(readr, tidyr, dplyr, purrr)

cz_artists_full_raw <- read_tsv("/mnt/muw/cz_artists_full.tsv", col_types = cols(.default = "c"))
cz_artists_full_raw <- read_tsv("/mnt/muw/cz_artists_parts/artists_wp_4plus/wp_4plus_artists.tsv", col_types = cols(.default = "c"))
names(cz_artists_full_raw) <- c("artist_name", "artist_id")

rows_per_file <- 500
splits <- split(cz_artists_full_raw, (seq_len(nrow(cz_artists_full_raw)) - 1) %/% rows_per_file)

# Write each chunk to a .tsv file
walk2(splits, seq_along(splits), ~ write_tsv(.x, paste0("/mnt/muw/cz_artists_parts/cz_artists_4p_chunk_", .y, ".tsv")))
