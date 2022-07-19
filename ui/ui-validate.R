tabPanel(
  title = title_validate, icon = icon("fa-solid fa-check"),

  fluidRow(column(
    width = 10, offset = 1,
      shinydashboard::box(
        width=12,
        title="Data Upload", status="primary",solidHeader = T,
        column(width=12,
          column(width = 7,
                 fluidRow(fileInput('user_tv_data_vald', 'Upload Your Tumor Volume',
                                    accept=c('.csv','.xls','.xlsx')))
          ),
          column(width = 4,
                 style = "margin-left: 2px; margin-right: 1px; margin-top: 25px;",
                 fluidRow(actionButton("user_tv_upload_vald", "Upload", class = "btn btn-block", icon = icon("upload")))
          )
        )
      )
    )
  ),

  fluidRow(column(
    width = 10, offset = 1,

    shinydashboard::box(
      width = 12,
      title = "Uploaded Tumor Volume File", status = "primary", solidHeader = TRUE,

      div(
        span(textOutput("tv_text_upload_vald"), style="color:red")
      ),
      br(),
      withSpinner(
        DTOutput("table_tv_validate_user"),
        proxy.height = "100px", color="#0273B7"
      ),
      hr()
    )
  )),

  fluidRow(column(
    width = 10, offset = 1,

    shinydashboard::box(
      width = 12,
      title = "Expected Tumor Volume File", status = "primary", solidHeader = TRUE,

      withSpinner(
        DTOutput("table_tv_validate_default"),
        proxy.height = "100px", color="#0273B7"
      ),
      hr()
    )
  )),

  fluidRow(
    HTML('<h6 align="center">&copy; 2022 Seven Bridges &nbsp; &middot; &nbsp; <a href="https://www.sbgenomics.com/privacy-policy/" target="_blank">Privacy</a> &nbsp;&nbsp; <a href="https://www.sbgenomics.com/copyright-policy/" target="_blank">Copyright</a> &nbsp;&nbsp; <a href="https://www.sbgenomics.com/terms-of-service/" target="_blank">Terms</a> &nbsp;&nbsp; <a href="https://www.sbgenomics.com/offices/" target="_blank">Contact</a></h6>')
  )
)
