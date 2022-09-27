tabPanel(
  title = title_validate, icon = icon("fa-solid fa-check", verify_fa = FALSE),

  fluidRow(column(
    width = 10, offset = 1,
      shinydashboard::box(
        width=12,
        title="Data Upload", status="primary",solidHeader = T,
        column(width=12,
          column(width = 12,
                 fluidRow(fileInput('user_tv_data', 'Upload Your Tumor Volume',
                                    accept=c('.csv','.xls','.xlsx')))
          ),
          hr(),
          fluidRow(
            column(
              width = 10, offset = 1,
              textAreaInput("tv_user_return_msg", label = "Upload Validation Message: ", resize = "none", height = "250px")
            )
          ),
          hr(),
          fluidRow(
              column(
                width = 10, offset = 1,
                div(
                  span(textOutput("tv_text_continue"), style="color:green"),
                  span(textOutput("tv_text_stop"), style="color:red"),
                  span(textOutput("tv_text_guide"))
                )
              )
            )
        )
      )
    )
  ),

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
