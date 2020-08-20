function(input, output, session) {
  # ----------------------------------------------------------------------------
  #' Reactive values
  # ----------------------------------------------------------------------------
  rv <- reactiveValues(
    dat = data.frame(),
    dat_stats = list(),
    dat_title = character()
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
    rv$dat_title <- tools::file_path_sans_ext(input$data_button)
    rv$dat_stats <- list(
      nrow = scales::comma(nrow(rv$dat)),
      ncol = scales::comma(ncol(rv$dat)),
      extension = tools::file_ext(input$data_button),
      size = paste0(round(file.size(dat_path) * 1e-6, 2), "MB")
    )
  })

  # ----------------------------------------------------------------------------
  #' When actionButton is hit, hide landing page, show main page, remove modal
  # ----------------------------------------------------------------------------
  observeEvent(rv$dat, {
    shinyjs::hide("ui_landing")
    shinyjs::show("ui_main")
    removeModal()
    output$dat_title <- renderUI({
      h1(paste0("[", rv$dat_title, "]"), align = "center")
    })
    output$dat_stats <- renderUI({
      tagList(
        shiny_container(
          "Data information",
          column(width = 3, style = "padding-left: 0px;padding-right: 0px;", shiny_card("Rows", "", rv$dat_stats$nrow)),
          column(width = 3, style = "padding-left: 0px;padding-right: 0px;", shiny_card("Columns", "", rv$dat_stats$ncol)),
          column(width = 3, style = "padding-left: 0px;padding-right: 0px;", shiny_card("Type", "", rv$dat_stats$extension)),
          column(width = 3, style = "padding-left: 0px;padding-right: 0px;", shiny_card("Size", "", rv$dat_stats$size))
        )
      )
    })
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
        width = "100%"
      ),
      actionButton(
        inputId = "confirm_filters_button",
        label = "Confirm filters",
        width = "100%"
      ),
      br(), br(),
      uiOutput("filter_render")
    ))
  })

  observeEvent(input$confirm_filters_button, {
    output$filter_render <- renderUI({
      lapply(isolate(input$filter_select), function(x) {
        selectInput(
          inputId = x,
          label = x,
          choices = sort(unique(rv$dat[[x]])),
          width = "100%",
          multiple = TRUE
        )
      })
    })
  })

  # ----------------------------------------------------------------------------
  #' Data table output
  # ----------------------------------------------------------------------------
  output$data_table <- renderReactable({
    reactable(
      data = rv$dat,
      filterable = TRUE,
      minRows = 10,
      bordered = TRUE,
      highlight = TRUE,
      height = 500,
      defaultColDef = colDef(
        header = function(value) gsub(".", " ", value, fixed = TRUE),
        align = "center",
        maxWidth = 200,
        headerStyle = list(background = "#f7f7f8")
      )
    )
  })
}
