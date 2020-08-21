function(input, output, session) {
  # ----------------------------------------------------------------------------
  #' Reactive values
  # ----------------------------------------------------------------------------
  rv <- reactiveValues(
    dat = data.frame(),
    dat_filtered = data.frame(),
    filters_on_close = list()
  )

  # ----------------------------------------------------------------------------
  #' When actionButton is clicked, open modal for data selection
  # ----------------------------------------------------------------------------
  observeEvent(input$pick_data_button, {
    showModal(modalDialog(
      title = "Please pick one of the available datasets",
      radioGroupButtons(
        inputId = "data_button",
        label = NULL,
        choices = config$data_choices,
        selected = "",
        individual = TRUE
      )
    ))
  })

  # ----------------------------------------------------------------------------
  #' When actionButton is clicked, hide langing page, show main page
  # ----------------------------------------------------------------------------
  w <- Waiter$new(html = span("Initialising"))
  observeEvent(input$data_button, {
    w$show()
    dat_path <- paste0("data/", input$data_button)
    rv$dat <- read_dat(dat_path)
    rv$dat_filtered <- rv$dat
    rv$dat_title <- tools::file_path_sans_ext(input$data_button)
  })

  # ----------------------------------------------------------------------------
  #' When actionButton is hit, hide landing page, show main page, remove modal
  # ----------------------------------------------------------------------------
  observeEvent(rv$dat, {
    shinyjs::hide("ui_landing")
    shinyjs::show("ui_main")
    removeModal()
    w$hide()
  }, ignoreInit = TRUE)

  # ----------------------------------------------------------------------------
  #' When the change dataset button is click, open a modal
  # ----------------------------------------------------------------------------
  observeEvent(input$change_data_button, {
    showModal(modalDialog(
      title = "Please pick one of the available datasets",
      radioGroupButtons(
        inputId = "data_button",
        label = NULL,
        choices = config$data_choices,
        selected = "",
        individual = TRUE
      )
    ))
  })

  # ----------------------------------------------------------------------------
  #' When the apply filters button is clicked, open a modal
  # ----------------------------------------------------------------------------
  observeEvent(input$apply_filters_button, {
    showModal(modalDialog(
      title = "Filter controls",
      selectInput(
        inputId = "filter_select",
        label = "Available filters",
        choices = names(rv$dat),
        multiple = TRUE,
        width = "100%",
        selected = {
          if (is.null(input$filter_select))
            NULL
          else
            input$filter_select
        }
      ),
      actionButton(
        inputId = "render_filters_button",
        label = "Render filters",
        icon = icon("rocket"),
        width = "100%"
      ),
      br(), br(),
      uiOutput("filter_render"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_filter_modal", "Confirm")
      )
    ))
  })

  output$filter_render <- renderUI({
    lapply(names(rv$dat), function(x) {
      shinyjs::hidden(
        div(
          id = paste0("dd_", x),
          selectInput(
            inputId = x,
            label = gsub("[.]", " ", x),
            choices = NULL,
            width = "100%",
            multiple = TRUE
          )
        )
      )
    })
  })

  # w_filters <- Waiter$new(id = "filter_render", html = spin_google(), color = transparent(.5))
  observeEvent(input$render_filters_button, {
    # w_filters$show()
    for (i in names(rv$dat)) {
      if (i %in% input$filter_select) {
        if (is.null(input[[i]])) {
          updateSelectInput(
            session = session,
            inputId = i,
            choices = sort(unique(rv$dat[[i]]))
          )
          shinyjs::show(paste0("dd_", i))
        }
      } else {
        shinyjs::hide(paste0("dd_", i))
      }
    }
    # w_filters$hide()
  })

  # ----------------------------------------------------------------------------
  #' When apply filters button is clicked, we check for previous selections and
  #' restore if possible
  # ----------------------------------------------------------------------------
  observeEvent(input$apply_filters_button, {
    if (length(rv$filters_on_close) > 0) {
      lapply(rv$filters_on_close, function(x) {
        col <- x$col
        selected_vals <- x$selected_vals
        all_vals <- x$all_vals
        updateSelectInput(session, col, choices = all_vals, selected = selected_vals)
        shinyjs::show(paste0("dd_", col))
      })
    }
  })

  # ----------------------------------------------------------------------------
  #' Reactive filter summary
  # ----------------------------------------------------------------------------
  dat_filter_summary <- reactive({
    out <- lapply(input$filter_select, function(x) {
      if (is.null(input[[x]])) {
        NULL
      } else {
        data.frame(
          Filter = x,
          Filter.Value = input[[x]]
        )
      }
    })

    dplyr::bind_rows(Filter(Negate(is.null), out))
  })

  # ----------------------------------------------------------------------------
  #' When the filter modal is confirmed "hits okay", we store the filters on
  #' close so that we can restore them in the future. We also update the
  #' reactable table
  # ----------------------------------------------------------------------------
  observeEvent(input$confirm_filter_modal, {
    removeModal()

    # preserve selections
    rv$filters_on_close <- lapply(input$filter_select, function(x) {
      if (!is.null(x))
        list(
          "col" = x,
          "selected_vals" = input[[x]],
          "all_vals" = sort(unique(rv$dat[[x]]))
        )
    })

    # filter the data for the reactable table
    df_summary <- dat_filter_summary()
    df_summary_split <- split(df_summary, df_summary$Filter)

    df <- rv$dat
    for (i in seq_along(df_summary_split)) {
      x <- df_summary_split[[i]]
      i_col <- unique(x$Filter)
      i_val <- unique(x$Filter.Value)
      df <- df[df[[i_col]] %in% i_val, ]
    }

    rv$dat_filtered <- df
  })

  # ----------------------------------------------------------------------------
  #' Data table output
  # ----------------------------------------------------------------------------
  output$data_table <- renderReactable({
    reactable_data(rv$dat_filtered)
  })

  # ----------------------------------------------------------------------------
  #' Filter summary table
  # ----------------------------------------------------------------------------
  output$filter_summary_table <- renderReactable({
    if (nrow(dat_filter_summary()) > 0) {
      reactable_filter_summary(dat_filter_summary())
    } else {
      reactable_filter_summary_alt()
    }
  })
}
