pacman::p_load(httr2, jsonlite, purrr, tidyr, dplyr, readr, stringr)

# load the Neo4j connection details
neo_cred <- read_rds("/home/lon/Documents/neo_cred.rds")

# NODES ----
# Create the Cypher query payload
query <- list(statements = list(list(statement = "
MATCH (c:CalendarDate)
RETURN c
limit 5;"
)))

# run the cypher
response <- request(neo_cred$url) |> req_auth_basic(neo_cred$usr, neo_cred$pwd) |>
  req_body_json(query) |> req_perform()

# create a tibble from the result
resp_tib <- response |> resp_body_json(simplifyVector = TRUE) |>
  pluck("results", "data", 1) |> unnest(col = row) |> select(-meta)

# NODE PROPS ----
# Create the Cypher query payload
query <- list(statements = list(list(statement = "
MATCH (c:BroadcastWeek)
RETURN 'Uitzendweek' as object_type,
       c.year as jaar,
       c.weekNumber as week_vh_jaar,
       c.startDate as van,
       c.endDate as tem
limit 5;"
)))

# run the cypher
response <- request(neo_cred$url) |> req_auth_basic(neo_cred$usr, neo_cred$pwd) |>
  req_body_json(query) |> req_perform()

# create a tibble from the result
resp_tib <- response |> resp_body_json(simplifyVector = TRUE) |>
  pluck("results", "data", 1) |> unnest_wider(col = row, names_sep = "_") |> select(-meta)
# assign the original column names
resp_cols <- response |> resp_body_json(simplifyVector = TRUE) |> pluck("results", 1) |> unlist()
colnames(resp_tib) <- resp_cols

# RELATIONSHIPS ----
query <- list(statements = list(list(statement = "
MATCH (a:BroadcastWeek)-[:CONTAINS]-(b)
where a.weekNumber = 2 and a.year = 2025
RETURN b
")))

# run the cypher
response <- request(neo_cred$url) |> req_auth_basic(neo_cred$usr, neo_cred$pwd) |>
  req_body_json(query) |> req_perform()

resp_tib <- response |> resp_body_json(simplifyVector = TRUE) |>
  pluck("results", "data", 1) |> unnest(cols = row) |>
  mutate(object_type = "CONTAINS") |>
  select(object_type, everything(), -meta)

resp_tib <- response |> resp_body_json()
