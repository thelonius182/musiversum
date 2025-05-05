# -------------- Shiny App --------------

ui <- fluidPage(
  titlePanel("Wikidata Artist Resolver"),
  sidebarLayout(
    sidebarPanel(
      fileInput("rds_file", "Select .RDS-file with artist names"),
      actionButton("start_btn", "Start Processing"),
      uiOutput("download_ui"),
      div(
        style = "margin-top: 20px;",
        textOutput("status")
      )
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
  artist_rgx <- "(?<!\\w)artist(?!\\s+discography)\\b|band|composer|conductor|guitarist|musician|percussionist|pianist|saxophonist|singer|trumpeter"

  resolved_results <- reactiveVal(tibble(
    artist_name = character(),
    artist_czid = character(),
    wikidata_id = character(),
    wikipedia_nl = character(),
    wikipedia_en = character(),
    title_nl = character(),
    title_en = character(),
    summary_nl = character(),
    summary_en = character(),
    img_url = character()
  ))

  # store result ----
  observeEvent(input$store_btn, {
    req(input$rds_file)
    on.exit({
      session$onFlushed(function() {
        stopApp()
      }, once = TRUE)
    })

    qfn <- paste0("h:/artist_resolver/sts_200/", str_replace(input$rds_file$name, "cz", "wd"))
    write_rds(resolved_results(), qfn)
  })

  output$download_ui <- renderUI({
    if (!processing_active() && !is.null(artist_queue())) {
      actionButton("store_btn", "Store Results", class = "btn-success")
    }
  })

  # load list of artists ----
  observeEvent(input$start_btn, {
    req(input$rds_file)
    df <- read_rds(input$rds_file$datapath) |> mutate(artist_id = as.character(artist_id))
    artist_queue(df)
    current_index(1)
    processing_active(TRUE)
  })

  observe({
    req(processing_active(), artist_queue(), current_index())
    queue <- artist_queue()
    i <- current_index()

    if (i > nrow(queue)) {
      current_artist(NULL)
      current_matches(NULL)
      processing_active(FALSE)  # 🔑 Stop further processing
      return()
    }

    if (i == 1) {

      # resolve artists ----
      withProgress(message = "Resolving artists...", value = 0, {

        for (j in seq(i, nrow(queue))) {
          current_index(j)
          artist_row <- queue[j, ]
          name <- artist_row$artist_name
          flog.info(name, name = config$log_slug)
          czid <- artist_row$artist_id
          current_artist(name)
          matches <- get_entity_matches(name)

          # no matches found ----
          if (is.null(matches) || nrow(matches) == 0) {
            resolved_results(bind_rows(
              resolved_results(),
              tibble(
                artist_name = name,
                artist_czid = czid,
                wikidata_id = "Not Found",
                wikipedia_nl = NA_character_,
                wikipedia_en = NA_character_,
                title_nl = NA_character_,
                title_en = NA_character_,
                summary_nl = NA_character_,
                summary_en = NA_character_,
                img_url = NA_character_
              )
            ))

            next
          }

          # review? ----
          match_idx <- which(str_detect(matches$description, pattern = regex(artist_rgx, ignore_case = TRUE)))

          if (length(match_idx) == 1) {
            # . get_wikipedia_urls ----
            urls_tib <- get_wikipedia_urls(matches$wikidata_id[match_idx])
            urls_tib$summary_nl[[1]] <- urls_tib$summary_nl[[1]] |>
              str_remove_all("(\r)?\n") |> str_replace_all("\t", " ")
            urls_tib$summary_en[[1]] <- urls_tib$summary_en[[1]] |>
              str_remove_all("(\r)?\n") |> str_replace_all("\t", " ")
            resolved_results(bind_rows(
              resolved_results(),
              tibble(
                artist_name = name,
                artist_czid = czid
              ) |> bind_cols(urls_tib)
            ))

          } else {

            # mark for review ----
            resolved_results(bind_rows(
              resolved_results(),
              tibble(
                artist_name = name,
                artist_czid = czid,
                wikidata_id = "Needs Review",
                wikipedia_nl = NA_character_,
                wikipedia_en = NA_character_,
                title_nl = NA_character_,
                title_en = NA_character_,
                summary_nl = NA_character_,
                summary_en = NA_character_,
                img_url = NA_character_
              )))
          }

          incProgress(1 / nrow(queue))
        }

        processing_active(FALSE)
      })
    }
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
      return("✅  All artists processed.")
    }

    glue("🔄 Processing artist {i} of {nrow(queue)}")
  })
}

shinyApp(ui, server)
