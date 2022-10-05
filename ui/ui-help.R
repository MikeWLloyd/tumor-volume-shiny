source(file.path("ui", "interface_variables.R"), local = TRUE)

# Generate Navigation Page
tabPanel(
  title = title_help,
  icon = icon("question-circle"),

  br(),

  fluidRow(column(
    width = 10, offset = 1,

    shinydashboard::box(
      width = 12,
      title = "Help", status = "primary", solidHeader = TRUE,

      fluidRow(column(
        width = 10, offset = 1,
        includeMarkdown("documentation/help.md")
      )),

      br()
    )

  )),

  br(),
  closeAlert()
)
