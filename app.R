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

options(shiny.fullstacktrace = FALSE)

plan(multisession)

# Add variable to control whether login page will be allowed
source(file.path("ui", "interface_variables.R"), local = TRUE)

# Define User Interface variable
ui <- uiOutput("ui")


css <- "
.nav li a.disabled {
background-color: #ffffff !important;
color: #333 !important;
pointer-events: none;
text-decoration: line-through;
}"
## This css is used to disable the 'Body Weight Analysis' tab, when no body weight data is provided. 
# border-color: #aaa !important;
# https://stackoverflow.com/a/64324799/18557826

js <- HTML('
$(document).on("shiny:busy", function() {
  var inputs = document.getElementsByTagName("a");
  for (var i = 0; i < inputs.length; i++) {
    if (inputs[i]) {
      inputs[i].classList.add("disabled");
    }
  }
});

$(document).on("shiny:idle", function() {
  var inputs = document.getElementsByTagName("a");
  for (var i = 0; i < inputs.length; i++) {
    if (inputs[i]) {
      inputs[i].classList.remove("disabled");
    }
  }
});
')


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
