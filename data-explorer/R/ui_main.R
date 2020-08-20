ui_main <- function() {
  navbarPage(
    title = "Main Page",
    theme = shinytheme("lumen"),
    tabPanel(
      "Data explorer",
      uiOutput("dat_title"),
      column(
        width = 8,
        uiOutput("dat_stats")
      ),
      column(
        width = 4,
        shiny_container(
          header = "Actions",
          actionButton("change_data_button", "Change dataset", width = "100%"),
          actionButton("apply_filters_button", "Apply filters", width = "100%"),
          actionButton("download_button", "Download", width = "100%")
        )
      ),
      column(
        width = 8,
        shiny_container(
          "Data",
          reactableOutput("data_table")
        )
      ),
      column(
        width = 4,
        shiny_container(
          "Filter Summary",
          reactableOutput("filter_summary_table")
        )
      )
    ),
    tabPanel(
      "About this app"
    )
  )
}
