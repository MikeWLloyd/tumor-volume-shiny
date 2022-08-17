tabPanel(
  title = title_validate, icon = icon("fa-solid fa-check"),

  fluidRow(column(
    width = 10, offset = 1,
      shinydashboard::box(
        width=12,
        title="Data Upload", status="primary",solidHeader = T,
        column(width=12,
          column(width = 7,
                 fluidRow(fileInput('user_tv_data_valid', 'Upload Your Tumor Volume',
                                    accept=c('.csv','.xls','.xlsx')))
          )#,
          # column(width = 4,
          #        style = "margin-left: 2px; margin-right: 1px; margin-top: 25px;",
          #        fluidRow(actionButton("user_tv_upload_valid", "Upload", class = "btn btn-block", icon = icon("upload")))
          # )
          # NOTE: this button would break the app when clicked without a data file present. Removed it for now. 
        )
      )
    )
  ),

  fluidRow(column(
    width = 10, offset = 1,

    shinydashboard::box(
      width = 12,
      title = "Uploaded Tumor Volume File Check Report", status = "primary", solidHeader = TRUE,

      div(
        span(verbatimTextOutput("tv_text_upload_valid"), style="color:red")
      ),
      br(),
        DTOutput("table_tv_validate_user"),
      hr()
    )
  )),

  fluidRow(column(
    width = 10, offset = 1,

    shinydashboard::box(
      width = 12,
      title = "Example Expected Tumor Volume File Formatting", status = "primary", solidHeader = TRUE,

      withSpinner(
        DTOutput("table_tv_validate_default"),
        proxy.height = "100px", color="#0273B7"
      ),
      hr()
    )
  )),

  br()
)
