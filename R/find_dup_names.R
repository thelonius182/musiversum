find_duplicate_musicians_dt <- function(names_list, max_distance = 0.15) {

  dt <- data.table(name = names_list)
  dt[, id := .I]  # Add row numbers

  # Self-join with id constraints to avoid duplicate/self pairs
  pairs <- dt[dt, on = .(id < id), allow.cartesian = TRUE, nomatch = NULL]

  # Compute similarity
  pairs[, similarity := 1 - stringdist(name, i.name, method = "jw")]

  # Filter matches above threshold
  result <- pairs[similarity >= (1 - max_distance), .(
    name1 = name,
    name2 = i.name,
    similarity
  )][order(-similarity)]

  return(result)
}

names_vector <- combined_data |> pull(artist_name)
dups <- find_duplicate_musicians_dt(names_vector) |> filter(similarity >= 0.9)
dups_w_detail <- dups |> left_join(combined_data, by = join_by("name1" == "artist_name")) |>
  select(name1, name2, artist_czid_n1 = artist_czid, wikidata_id_n1 = wikidata_id) |>
  left_join(combined_data, by = join_by("name2" == "artist_name")) |>
  select(name1, name2, artist_czid_n1, artist_czid_n2 = artist_czid, wikidata_id_n1, wikidata_id_n2 = wikidata_id)

artist_reviews_dir <- "/mnt/muw/cz_artists_parts/artist_reviews/"
write_tsv(dups_w_detail, file = str_glue("{artist_reviews_dir}possible_duplicates.tsv", na = ""))

