# -------------- Shiny App --------------

ui <- fluidPage(
  titlePanel("Wikidata Artist Reviewer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(
        style = "margin-top: 20px;",
        textOutput("status")
      ),
      div(
        style = "margin-top: 20px;",
        fileInput("tsv_file", "Upload .tsv-file with artist names")
      ),
      div(
        style = "margin-top: 20px;",
        div(
          style = "display: flex; gap: 10px;",
          actionButton("start_btn", "Start"),
          actionButton("stop_btn", "Stop"),
          uiOutput("download_ui")
        )
      ),
      div(
        style = "margin-top: 20px;",
        uiOutput("selection_ui")
      ),
      div(
        style = "margin-top: 20px;",
        div(
          style = "display: flex; gap: 10px;",
          actionButton("accept_btn", "Accept"),
          actionButton("reject_btn", "Reject"),
          actionButton("prev_btn", "<-"),
          actionButton("next_btn", "->")
        )
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

  resolved_results <- reactiveVal(tibble(
    artist_name = character(),
    artist_czid = character(),
    wikidata_id = character(),
    wikipedia_nl = character(),
    wikipedia_en = character(),
    summary_nl = character(),
    summary_en = character()
  ))

  # download_results ----
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("artists_review_completed_", Sys.Date(), ".tsv")
    },
    content = function(file) {
      on.exit({
        session$onFlushed(function() {
          stopApp()
        }, once = TRUE)
      })

      write_tsv(resolved_results(), file)
    }
  )

  output$download_ui <- renderUI({
    if (!processing_active() && !is.null(artist_queue())) {
      downloadButton("download_results", "Download Results", class = "btn-success")
    }
  })

  # load list of artists ----
  # /mnt/muw/cz_artists_parts/artist_reviews/resolved_artists_review.tsv
  observeEvent(input$start_btn, {
    req(input$tsv_file)
    df <- read_tsv(input$tsv_file$datapath, col_types = cols(.default = "c")) |> rename(artist_id = artist_czid)
    artist_queue(df)
    current_index(1)
    processing_active(TRUE)
  })

  # match artists ----
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

    # no matches ----
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

    # check auto-resolve ----
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

    # . auto-resolve ----
    if (auto_resolve) {
      urls_tib <- get_wikipedia_urls(matches$wikidata_id[1])
      urls_tib$summary_nl[[1]] <- urls_tib$summary_nl[[1]] |> str_remove_all("(\r)?\n") |> str_replace_all("\t", " ")
      urls_tib$summary_en[[1]] <- urls_tib$summary_en[[1]] |> str_remove_all("(\r)?\n") |> str_replace_all("\t", " ")

      resolved_results(bind_rows(
        resolved_results(),
        tibble(
          artist_name = name,
          artist_czid = czid
        ) |> bind_cols(urls_tib)
      ))

      current_index(i + 1)
    } else {
      # . review needed ----
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

  # accept_btn ----
  observeEvent(input$accept_btn, {
    req(input$selected_id)
    urls_tib <- get_wikipedia_urls(input$selected_id)
    urls_tib$summary_nl[[1]] <- urls_tib$summary_nl[[1]] |> str_remove_all("(\r)?\n") |> str_replace_all("\t", " ")
    urls_tib$summary_en[[1]] <- urls_tib$summary_en[[1]] |> str_remove_all("(\r)?\n") |> str_replace_all("\t", " ")

    resolved_results(bind_rows(
      tibble(
        artist_name = current_artist(),
        artist_czid = artist_queue()[current_index(), ]$artist_id
      ) |> bind_cols(urls_tib),
      resolved_results()
    ))

    current_matches(NULL)
    current_index(current_index() + 1)
  })

  # reject_btn ----
  observeEvent(input$reject_btn, {
    req(artist_queue())
    queue <- artist_queue()
    i <- current_index()
    artist_row <- queue[i, ]
    name <- artist_row$artist_name
    czid <- artist_row$artist_id

    resolved_results(bind_rows(
      tibble(
        artist_name = name,
        artist_czid = czid,
        wikidata_id = "Rejected",
        wikipedia_nl = NA_character_,
        wikipedia_en = NA_character_,
        summary_nl = NA_character_,
        summary_en = NA_character_
      ),
      resolved_results()
    ))

    current_index(i + 1)
  })

  # prev_btn ----
  observeEvent(input$prev_btn, {
    req(artist_queue())
    queue <- artist_queue()
    current_index(current_index() - 1)
    artist_row <- queue[current_index(), ]
    name <- artist_row$artist_name
    czid <- artist_row$artist_id
  })

  # next_btn ----
  observeEvent(input$next_btn, {
    req(artist_queue())
    queue <- artist_queue()
    current_index(current_index() + 1)
    artist_row <- queue[current_index(), ]
    name <- artist_row$artist_name
    czid <- artist_row$artist_id
  })

  # stop_btn ----
  observeEvent(input$stop_btn, {
    current_artist(NULL)
    current_matches(NULL)
    processing_active(FALSE)  # 🔑 Stop further processing
    return()
  })

  # output results ----
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
      if (i > nrow(queue)) return("✅ Review completed")
      return("✅ Exit review session")
    }

    glue("🔄 Processing artist {i} of {nrow(queue)}")
  })
}

shinyApp(ui, server)
