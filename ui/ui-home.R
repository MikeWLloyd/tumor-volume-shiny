# Add variable to control whether login page will be allowed
source(file.path("ui", "interface_variables.R"), local = TRUE)

# Generate Navigation Page
tabPanel(
  title = title_home, icon = icon("home"),
  tags$head(HTML(
    '<link rel="apple-touch-icon" sizes="180x180" href="/favicon.png">
  <link rel="icon" type="image/png" sizes="32x32" href="/favicon.png">
  <link rel="icon" type="image/png" sizes="16x16" href="/favicon.png">
  <meta name="theme-color" content="#ffffff">')),
  includeCSS(path = "www/custom.css"),
  includeCSS(path = "css/AdminLTE.css"),
  includeCSS(path = "css/shinydashboard.css"),
  includeCSS(path = "css/sbgprogress.css"),

  useShinyFeedback(),

  useShinyjs(),
  div(
    class = "jumbotron",

    br(), br(),

    span(h1("Tumor Volume Suite", style = "color:#FFF; text-shadow: rgba(0, 0, 0, 0.3) 0px 0px 15px; text-align: center; font-weight: 700;") , align = "center"),

    br(), br(),

    span(h4("A tool for tumor volume analysis and plotting from your input volume data based on the given template."), style = "color:#FFF; text-shadow: rgba(0, 0, 0, 0.7) 0px 0px 15px; text-align: center;", align = "center"),

    br(), br(),

    div(
      align = "center",
      actionButton(
        "btn_nav_tv", "Tumor Volume Analysis",
        icon("arrow-circle-o-right"),
        class = "btn btn-lg", style = "margin-left: 25px; background-color: rgb(14, 104, 96); border-color: rgb(14, 104, 96);"
      ),
      actionButton(
        "btn_nav_val", "Data Validation",
        icon("arrow-circle-o-right"),
        class = "btn btn-lg", style = "margin-left: 25px; background-color: rgb(14, 104, 96); border-color: rgb(14, 104, 96);"
      ),
      actionButton(
        "btn_nav_help", "Need Help?",
        icon("book-open"),
        class = "btn btn-lg", style = "margin-left: 25px; background-color: rgb(3, 111, 173); border-color: rgb(3, 111, 173);"
      )
    )
  ),

  tags$style(
    type = "text/css",
    HTML('.jumbotron {width: 100vw; height: calc(100vh - 43px);
           border-radius:0px !important;
           margin-top:-30px; margin-left:-15px; margin-bottom:0px;
           background-image: url("bg.svg");
           background-size:cover; background-position:center;
           background-repeat:no-repeat;}')
  )
)
