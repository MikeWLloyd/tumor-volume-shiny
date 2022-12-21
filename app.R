suppressPackageStartupMessages({
  library("shiny")
  library("plotly")
  library("dygraphs")
  library("shinyWidgets")
  library("plyr")
  library("dplyr")
  library("zoo")
  library("ggpubr")
  library("grid")
  library("gridExtra")
  library("gtable")
  library("shinydashboard")
  library("reactable")
  library("bcrypt")
  library("shinyBS")
  library("shinyjs")
  library("shinyFeedback")
  library("shinycssloaders")
  library("shinyAce")
  library("jsonlite")
  library("magrittr")
  library("knitr")
  library("DT")
  library("readxl")
  library("survival")
  library("survminer")
  library("validate")
  library("multcomp")
  library("validate")
  library("purrr")
  library("stringr")
  library("shinyalert")
  library("kableExtra")
  library("ipc")
  library("promises")
  library("future")
})

  plan(multisession)

# Add variable to control whether login page will be allowed
source(file.path("ui", "interface_variables.R"), local = TRUE)

# Define User Interface variable
ui <- uiOutput("ui")

server <- function(input, output, session) {
  source(file.path("server", "timeout.R"), local = TRUE)

  # ui
  output$ui <- renderUI({
    shinyalert(title = 'Loading...', "Please be patient while the application loads.", type = "info", showConfirmButton = F, animation = F)
    source(file.path("ui", "ui-main.R"), local = TRUE)$value
  })

  source(file.path("server", "logic-validate.R"), local = TRUE)$value
  source(file.path("server", "query-tv.R"), local = TRUE)$value
  source(file.path("server", "logic-plots.R"), local = TRUE)$value
  source(file.path("server", "logic-tv.R"), local = TRUE)$value
  source(file.path("server", "report.R"), local = TRUE)$value

}

shinyApp(ui, server)
