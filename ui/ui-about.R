source(file.path("ui", "interface_variables.R"), local = TRUE)

# Generate Navigation Page
tabPanel(
  title = title_about,
  icon = icon("info", verify_fa = FALSE),

  br(),

  fluidRow(column(
    width = 10, offset = 1,

    shinydashboard::box(
      width = 12,
      title = "About This Tool", status = "primary", solidHeader = TRUE,

      fluidRow(column(
        width = 10, offset = 1,
        includeMarkdown("documentation/about.md")
      )),

      h2('Session Info'),

      tags$div(
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 10px;",
        htmlOutput("sessionInfo")
      ),

      br()
    )

  )),

  br(),
  closeAlert()
)
