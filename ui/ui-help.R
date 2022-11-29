source(file.path("ui", "interface_variables.R"), local = TRUE)

addResourcePath("tmpuser", getwd())

# Generate Navigation Page
tabPanel(
  title = title_help,
  icon = icon("question-circle", verify_fa = FALSE),

  br(),

  fluidRow(column(
    width = 10, offset = 1,

    shinydashboard::box(
      width = 12,
      title = "Help", status = "primary", solidHeader = TRUE,

      tags$iframe(
            src = "tmpuser/documentation/help.html", 
            width = "100%",
            style="height: 100vh;",
            scrolling = 'yes',
            frameborder="0"
        ), # https://stackoverflow.com/a/66421428/18557826
      br()
    )

  )),

  br(),
  closeAlert()
)

