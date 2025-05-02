# Load project-specific .Rprofile if not already loaded
if (!exists(".rprofile_loaded")) {
  rprofile_path <- file.path(getwd(), ".Rprofile")
  if (file.exists(rprofile_path)) {
    source(rprofile_path)
  }
}

# User Interface ----
ui <- page_fluid(
  theme = bs_theme(
    bootswatch = "flatly",
    base_font = font_google("Roboto")
  ),
  useShinyjs(),
  # Inject extra custom CSS safely here
  shiny::tags$style(HTML("
    .modal-body textarea.form-control {
      width: 70%;
      text-align: left;
      vertical-align: top;
      resize: both;
    }
    .modal-dialog {
      max-width: 90%;
    }
  ")),
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
          uiOutput("download_ui"),
          actionButton("merge_artists_btn", "Merge artists", style = "display: none;")
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
      DTOutput("results_tbl")
    )
  )
)

# Server ----
server <- function(input, output, session) {
  artist_queue <- reactiveVal(NULL)
  current_index <- reactiveVal(1)
  current_matches <- reactiveVal(NULL)
  current_artist <- reactiveVal(NULL)
  processing_active <- reactiveVal(FALSE)
  merge_artists_done <- reactiveVal(FALSE)
  merge_process <- reactiveVal(NULL)
  app_msg <- reactiveVal(NULL)

  #define results ----
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
    img_url = character(),
    modify = "blank"
  ))

  # download_results ----
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("artists_review_completed_", Sys.Date(), ".tsv")
    },
    content = function(file) {
      on.exit({
        session$onFlushed(function() {
          shinyjs::show("merge_artists_btn")
          app_msg("📥 Download completed. Ready to merge.")
        }, once = TRUE)
      })

      write_tsv(resolved_results(), file)
    }
  )

  # merge artists ----
  observeEvent(input$merge_artists_btn, {
    if (!is.null(merge_process()) && merge_process()$is_alive()) {
      showNotification("Merging already in progress...", type = "warning")
      return()  # Don't start another process
    }
    app_msg("🔄 Merging artists, please wait...")
    tmp_resolved_results <- tempfile(fileext = ".rds")
    write_rds(resolved_results(), tmp_resolved_results)

    # Start the external merge process
    process <- callr::r_bg(
      function(script, args) {
        system2("Rscript", c(script, args))
      },
      args = list("merge_artists.R", tmp_resolved_results)
    )

    merge_process(process)  # Store the process object
  })

  # merge finished? ----
  observe({
    invalidateLater(1000, session)  # Check every second

    proc <- merge_process()
    if (is.null(proc) || proc$is_alive()) return()

    # Process finished
    if (!merge_artists_done()) {
      merge_artists_done(TRUE)  # Only set done once
    }
  })

  # stop app ----
  observeEvent(merge_artists_done(), {
    if (merge_artists_done()) {
      app_msg("✅ Merging finished. Closing app...")
      stopApp()
    }
  })

  # show download-btn ----
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
          title_nl = NA_character_,
          title_en = NA_character_,
          summary_nl = NA_character_,
          summary_en = NA_character_,
          img_url = NA_character_,
          modify = "not_found"

        )
      ))

      current_index(i + 1)
      return()  # Skip further processing
    }

    # check auto-resolve ----
    # auto_resolve <- FALSE
    #
    # if (nrow(matches) == 1) {
    #   auto_resolve <- TRUE
    # } else {
    #   # Auto-resolve if the top match looks like a composer, etc.
    #   desc <- matches$description[1]
    #   if (!is.na(desc) && str_detect(desc,
    #                                  regex("composer|conductor|musician|singer|pianist|guitarist|trumpeter",
    #                                        ignore_case = TRUE))) {
    #     auto_resolve <- TRUE
    #   }
    # }
    #
    # # . auto-resolve ----
    # if (auto_resolve) {
    #   urls_tib <- get_wikipedia_urls(matches$wikidata_id[1])
    #   urls_tib$summary_nl[[1]] <- urls_tib$summary_nl[[1]] |> str_remove_all("(\r)?\n") |> str_replace_all("\t", " ")
    #   urls_tib$summary_en[[1]] <- urls_tib$summary_en[[1]] |> str_remove_all("(\r)?\n") |> str_replace_all("\t", " ")
    #
    #   resolved_results(bind_rows(
    #     resolved_results(),
    #     tibble(
    #       artist_name = name,
    #       artist_czid = czid
    #     ) |> bind_cols(urls_tib)
    #   ))
    #
    #   current_index(i + 1)
    # } else {
    #   # . review needed ----
    #   if (!is.null(matches) && nrow(matches) > 0) {
    #     current_matches(matches |> arrange(desc(match_score)))
    #   } else {
    #     current_matches(NULL)
    #   }
    # }

    current_matches(matches |> arrange(desc(match_score)))
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
      ) |> bind_cols(urls_tib) |> bind_cols(modify = "accepted"),
      resolved_results()
    ))

    proxy <- dataTableProxy("results_tbl")
    replaceData(proxy, resolved_results(), resetPaging = FALSE)  # optional
    proxy |> selectRows(NULL)
    proxy |> selectCells(NULL)
    proxy |> clearSearch()

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
        title_nl = NA_character_,
        title_en = NA_character_,
        summary_nl = NA_character_,
        summary_en = NA_character_,
        img_url = NA_character_,
        modify = "rejected"
      ),
      resolved_results()
    ))

    proxy <- dataTableProxy("results_tbl")
    replaceData(proxy, resolved_results(), resetPaging = FALSE)  # optional
    proxy |> selectRows(NULL)
    proxy |> selectCells(NULL)
    proxy |> clearSearch()

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
  output$results_tbl <- renderDT({
    datatable(
      resolved_results(),
      escape = FALSE,  # allow HTML inside table
      selection = "none",
      options = list(
        columnDefs = list(
          list(visible = FALSE, targets = c(4, 5, 6, 7, 10)),
          list(
            targets = 11,
            searching = FALSE,
            render = JS(
              "function(data, type, row, meta) {",
              "return '<button class=\"edit_btn\">Edit</button>';",
              "}"
            )
          )
        )
      ),
      callback = JS("
      table.on('dblclick', 'td', function() {
        var cellText = $(this).text();
        navigator.clipboard.writeText(cellText);
        Shiny.setInputValue('cell_copied', cellText);
      });
    ")
    ) |>
      formatStyle(columns = 1:11, cursor = "pointer")
  })

  observeEvent(input$cell_copied, {
    showNotification(
      paste("📋 Copied:", input$cell_copied),
      type = "message"
    )
  })

  # show message ----
  output$status <- renderText({
    # If a system message exists, show it first
    if (!is.null(app_msg())) return(app_msg())

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

  # edit summary ----
  observeEvent(input$results_tbl_cell_clicked, {
    click <- input$results_tbl_cell_clicked

    if (is.null(click$value) || click$col != 11) return()  # Only if Edit button clicked

    row_id <- click$row
    current_data <- resolved_results()[row_id, ]

    showModal(modalDialog(
      title = "Edit a Summary",
      textAreaInput("summary_nl", "Nederlands", value = current_data$summary_nl, width = "50%", rows = 5),
      textAreaInput("summary_en", "English", value = current_data$summary_en, width = "50%", rows = 5),
      # size = "l",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_edit", "Save Changes")
      )
    ))

    # save changes ----
    observeEvent(input$save_edit, {
      new_data <- resolved_results()
      new_data[row_id, "summary_nl"] <- list(input$summary_nl %||% NA_character_)
      new_data[row_id, "summary_en"] <- list(input$summary_en %||% NA_character_)
      resolved_results(new_data)
      removeModal()
    }, once = TRUE)
  })
}

options(shiny.launch.browser = TRUE)
runApp(
  list(ui = ui, server = server),
  launch.browser = TRUE
)
