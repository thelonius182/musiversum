# -------------- Shiny App --------------

ui <- fluidPage(
  titlePanel("Wikidata Artist Resolver"),
  sidebarLayout(
    sidebarPanel(
      fileInput("tsv_file", "Upload .tsv-file with artist names"),
      actionButton("start_btn", "Start Processing"),
      uiOutput("selection_ui"),
      actionButton("confirm_btn", "Confirm Selection"),
      uiOutput("download_ui"),
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
    artist_czid = character(),
    wikidata_id = character(),
    wikipedia_nl = character(),
    wikipedia_en = character(),
    summary_nl = character(),
    summary_en = character()
  ))

  output$download_results <- downloadHandler(
    filename = function() {
      paste0("resolved_artists_", Sys.Date(), ".tsv")
    },
    content = function(file) {
      write_tsv(resolved_results(), file)
    }
  )

  output$download_ui <- renderUI({
    if (!processing_active() && !is.null(artist_queue())) {
      downloadButton("download_results", "Download Results", class = "btn-success")
    }
  })

  observeEvent(input$start_btn, {
    req(input$tsv_file)
    df <- read_tsv(input$tsv_file$datapath, col_types = cols(.default = "c"))
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

    artist_row <- queue[i, ]
    name <- artist_row$artist_name
    czid <- artist_row$artist_id
    current_artist(name)

    matches <- get_entity_matches(name)

    # If no matches at all, record as Not Found
    if (is.null(matches) || nrow(matches) == 0) {
      resolved_results(bind_rows(
        resolved_results(),
        tibble(
          artist_name = name,
          artist_czid = czid,
          wikidata_id = "Not Found",
          wikipedia_nl = NA_character_,
          wikipedia_en = NA_character_,
          summary_nl = NA_character_,
          summary_en = NA_character_
        )
      ))

      current_index(i + 1)
      return()  # Skip further processing
    }

    # Check if auto-resolve is possible
    auto_resolve <- FALSE

    if (nrow(matches) == 1) {
      auto_resolve <- TRUE
    } else {
      # Auto-resolve if the top match looks like a composer, etc.
      desc <- matches$description[1]
      if (!is.na(desc) && str_detect(desc,
                                     regex("composer|conductor|musician|singer|pianist|guitarist|trumpeter",
                                           ignore_case = TRUE))) {
        auto_resolve <- TRUE
      }
    }

    if (auto_resolve) {
      urls_tib <- get_wikipedia_urls(matches$wikidata_id[1])

      resolved_results(bind_rows(
        resolved_results(),
        tibble(
          artist_name = name,
          artist_czid = czid
        ) |> bind_cols(urls_tib)
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
                choices = setNames(matches$wikidata_id,
                                   paste0(matches$label, " - ", matches$description)))
  })

  observeEvent(input$confirm_btn, {
    req(input$selected_id)
    urls_tib <- get_wikipedia_urls(input$selected_id)

    resolved_results(bind_rows(
      resolved_results(),
      tibble(
        artist_name = current_artist(),
        artist_czid = artist_queue()[current_index(), ]$artist_id
      ) |> bind_cols(urls_tib)
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

    glue("🔄 Processing artist {i} of {nrow(queue)}")
  })
}

shinyApp(ui, server)
