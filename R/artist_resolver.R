library(shiny)
library(readr)
library(dplyr)
library(httr2)
library(jsonlite)
library(purrr)
library(tibble)
library(glue)

# -------------- Helper Functions --------------

# Query Wikidata for matches
get_entity_matches <- function(name, lang = "nl") {
  req <- request("https://www.wikidata.org/w/api.php") |>
    req_url_query(
      action = "wbsearchentities",
      search = name,
      language = lang,
      format = "json"
    ) |>
    req_perform()

  result <- req |> resp_body_json()

  if (length(result$search) == 0) return(NULL)

  map_dfr(result$search, ~ {
    tibble(
      id          = .x$id %||% NA_character_,
      label       = .x$label %||% NA_character_,
      description = .x$description %||% NA_character_,
      match_score = .x$match$score %||% NA_real_,
      display     = paste0(.x$label, " — ", .x$description)
    )
  })
}

# Fetch Wikipedia URL for a given entity ID
get_wikipedia_url <- function(entity_id, lang = "nl") {
  entity_url <- glue("https://www.wikidata.org/wiki/Special:EntityData/{entity_id}.json")

  req <- request(entity_url) |> req_perform()
  entity_data <- req |> resp_body_json()

  title <- entity_data$entities[[entity_id]]$sitelinks[[paste0(lang, "wiki")]]$title

  if (is.null(title)) return(NA_character_)
  glue("https://{lang}.wikipedia.org/wiki/{URLencode(title)}")
}

# -------------- Shiny App --------------

ui <- fluidPage(
  titlePanel("Batch Artist Resolver: Wikidata to Wikipedia"),
  sidebarLayout(
    sidebarPanel(
      fileInput("tsv_file", "Upload .tsv-file with artist names"),
      actionButton("start_btn", "Start Processing"),
      uiOutput("selection_ui"),
      actionButton("confirm_btn", "Confirm Selection"),
      textOutput("status")
    ),
    mainPanel(
      tableOutput("results_tbl")
    )
  )
)

server <- function(input, output, session) {
  artist_queue <- reactiveVal(NULL)
  current_index <- reactiveVal(1)
  current_matches <- reactiveVal(NULL)
  current_artist <- reactiveVal(NULL)
  processing_active <- reactiveVal(FALSE)

  resolved_results <- reactiveVal(tibble(
    artist_name = character(),
    wikidata_id = character(),
    wikipedia_url = character()
  ))

  observeEvent(input$start_btn, {
    req(input$tsv_file)
    df <- read_tsv(input$tsv_file$datapath, col_types = cols(.default = "c"))
    artist_queue(df$artist_name)
    current_index(1)
    processing_active(TRUE)
  })

  observe({
    req(processing_active(), artist_queue(), current_index())
    queue <- artist_queue()
    i <- current_index()

    if (i > length(queue)) {
      current_artist(NULL)
      current_matches(NULL)
      processing_active(FALSE)  # 🔑 Stop further processing
      return()
    }

    # if (i > length(queue)) return()

    name <- queue[[i]]
    current_artist(name)

    matches <- get_entity_matches(name)

    # If no matches at all, record as Not Found
    if (is.null(matches) || nrow(matches) == 0) {
      resolved_results(bind_rows(
        resolved_results(),
        tibble(
          artist_name = name,
          wikidata_id = "Not Found",
          wikipedia_url = NA_character_
        )
      ))

      current_index(i + 1)
      return()  # Skip further processing
    }

    # Check if we can auto-resolve
    auto_resolve <- FALSE

    if (!is.null(matches)) {
      if (nrow(matches) == 1) {
        auto_resolve <- TRUE
      } else {
        # Auto-resolve if the top match looks like a composer, etc.
        desc <- matches$description[1]
        if (!is.na(desc)
            && str_detect(desc, regex("composer|conductor|musician|singer|pianist|guitarist|trumpeter", ignore_case = TRUE))) {
          auto_resolve <- TRUE
        }
      }
    }

    if (auto_resolve) {
      id <- matches$id[1]
      url <- get_wikipedia_url(id)

      resolved_results(bind_rows(
        resolved_results(),
        tibble(
          artist_name = name,
          wikidata_id = id,
          wikipedia_url = url
        )
      ))

      current_index(i + 1)
    } else {
      # Manual review needed
      if (!is.null(matches) && nrow(matches) > 0) {
        current_matches(matches |> arrange(desc(match_score)))
      } else {
        current_matches(NULL)
      }
    }
  })

  output$selection_ui <- renderUI({
    matches <- current_matches()
    if (is.null(matches)) return(NULL)

    selectInput("selected_id", "Select a match:",
                choices = setNames(matches$id, matches$display))
  })

  observeEvent(input$confirm_btn, {
    req(input$selected_id)

    id <- input$selected_id
    url <- get_wikipedia_url(id)

    resolved_results(bind_rows(
      resolved_results(),
      tibble(
        artist_name = current_artist(),
        wikidata_id = id,
        wikipedia_url = url
      )
    ))

    current_matches(NULL)
    current_index(current_index() + 1)
  })

  output$results_tbl <- renderTable({
    resolved_results()
  })

  output$status <- renderText({
    queue <- artist_queue()
    i <- current_index()

    if (is.null(queue)) {
      return("📥 Upload a file to begin.")
    }

    if (!processing_active()) {
      return("✅ All artists processed!")
    }

    glue("🔄 Processing artist {i} of {length(queue)}...")
  })
}

shinyApp(ui, server)
