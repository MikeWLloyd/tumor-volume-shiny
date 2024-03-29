navbarPage(
  id = nav_id,

  windowTitle = "PVA",

  title = div(
    img(src = "logo-new.png", height='40', style = "margin:-10px -10px -10px -10px;"),
    br(), HTML("&emsp;&emsp;&emsp;&emsp;&emsp;")
  ),
  header = tags$script(inactivity),
  # Add a session timeout.
  source(file.path("ui", "ui-home.R"), local = TRUE)$value,
  source(file.path("ui", "ui-validate.R"), local = TRUE)$value,
  source(file.path("ui", "ui-tv.R"), local = TRUE)$value,
  source(file.path("ui", "ui-help.R"), local = TRUE)$value,
  source(file.path("ui", "ui-about.R"), local = TRUE)$value
)