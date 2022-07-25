tabPanel(
  title = title_tumor_volume, icon = icon("fa-solid fa-chart-line"),
  shinydashboard::box(
    width=12,
    title="Data and Query", status="primary",solidHeader = T,

    shinydashboard::box(
      width=4,
      title="Data Upload", status="primary",solidHeader = T,
      column(width = 12,
             fluidRow(fileInput('user_tv_data', 'Upload Your Tumor Volume',
                                accept=c('.csv','.xls','.xlsx'))),
             div(
               span(textOutput("tv_text_upload"), style="color:red")
             ),

             fluidRow(
               column(width=5,
                      fluidRow(actionButton("user_tv_upload", "Upload", class = "btn btn-block", icon = icon("upload"))),
               ),
               column(width=5,offset = 2,
                      fluidRow(actionButton("user_tv_upload_default", "Load Example", class = "btn btn-block", icon = icon("download")))
               )
             )
      )
    ),

    shinydashboard::box(
      width = 8,
      title = "Data Query", status = "primary", solidHeader = TRUE,

      fluidRow(
        id = "input_group_tv", # for reset button
        column(
          width = 12,
          column(width = 12,
            fluidRow(
              column(
                width = 4,
                # wellPanel(
                  checkboxGroupInput(
                    "tv_contributor", "Contributor",
                    inline = TRUE,
                    choices = get_tv_contributor(),
                    selected = get_tv_contributor()[1]
                  )
                # )
              ),
              column(
                width = 4,
                # wellPanel(
                  pickerInput("tv_treatment", "Treatment",
                              choices = get_tv_treatment(),
                              selected = get_tv_treatment()[1:3],
                              options = pickerOptions(actionsBox = TRUE, style = 'btn-light',
                                                      showContent = TRUE),multiple = T)
                # )
              ),

              column(
                width = 4,
                pickerInput("tv_disease_type", "Disease Type",
                            choices = get_tv_disease(),
                            selected = get_tv_disease()[1],
                            options = pickerOptions(actionsBox = TRUE, style = 'btn-light',
                                                    showContent = TRUE),multiple = T)
              )
            ),
            br(),
            fluidRow(
              style = "margin-left: 2px; margin-right: 2px; margin-bottom: 10px;",
              column(
                width = 6,
                actionButton("tv_submit_query", "Submit Query", class = "btn btn-block", icon = icon("paper-plane"))
              ),
              column(
                width = 6,
                actionButton("tv_reset_query", "Reset Query", class = "btn btn-block btn-primary", icon = icon("undo"))
              ),
              # responsive button text for smaller screens https://stackoverflow.com/questions/19284153/
              tags$style(type = "text/css", ".btn { white-space: normal; }")
            )
          )
        )
      )
    )
  ),
  br(),
  hr(),
  br(),
  shinydashboard::box(
    width = 12,
    title = "Results", status="primary",solidHeader = TRUE,

    shinydashboard::box(
      width = 12,
      title = "Matched Tumor Volumes - Summary", status="primary",solidHeader = TRUE,

      div(
        progressBar(
          id = "pbar_tv", value = 100,
          total = 100, display_pct = TRUE,
          title = "Number of Studies"
        )
      ),

      fluidRow(
        column(offset = 0, width = 12,
          shinydashboard::valueBoxOutput("card_tv_volumes", width = 2),
          shinydashboard::valueBoxOutput("card_tv_treatment", width = 2),
          shinydashboard::valueBoxOutput("card_tv_disease", width = 2),
          shinydashboard::valueBoxOutput("card_tv_models", width = 2),
          shinydashboard::valueBoxOutput("card_tv_studies", width = 2),
          shinydashboard::valueBoxOutput("card_tv_contributor", width = 2)
        )
      )
    ),

    shinydashboard::box(
      
      width = 12,
      title = "Matched Tumor Volume - Visual Analytics", status="primary",solidHeader = TRUE,

      tabsetPanel(type = "tabs",
                  tabPanel("Tumor Volume Analytics",

                    fluidRow(

                      column(

                        width = 12,

                        div(

                          fluidRow(

                            column(

                              width = 12,

                              div(
                                column(
                                  width = 2,
                                  pickerInput("tv_all_plot_type", "Plot Type",
                                              choices = c("Study Plot", "Treatment Plot"),
                                              selected = c("Study Plot"),
                                              options = pickerOptions(actionsBox = FALSE, style = 'btn-light',
                                                                      showContent = TRUE),multiple = FALSE)
                                ),

                                column(
                                  width = 2,
                                  pickerInput("tv_all_plot_style", "Plot Style",
                                              choices = c("Study Average", "Individual Animal"),
                                              selected = c("Study Average"),
                                              options = pickerOptions(actionsBox = FALSE, style = 'btn-light',
                                                                      showContent = TRUE),multiple = FALSE)
                                ),

                                column(
                                  offset = 2,
                                  width = 1,
                                  checkboxInput("tv_all_scale", "Scaled Plot", FALSE),
                                  checkboxInput("tv_all_interpolate", "Interpolation", FALSE)
                                ),

                                column(
                                  width = 2,
                                  div(id="tv_div_all_scale_picker",
                                      pickerInput("tv_all_scaleby", "Scale Plot By",
                                                  choices = c("Growth Factor", "Volume"),
                                                  selected = c("Growth Factor"),
                                                  options = pickerOptions(actionsBox = FALSE, style = 'btn-light',
                                                                          showContent = TRUE),multiple = FALSE))
                                ),
                                column(
                                  width = 2,
                                  div(id="tv_div_all_endpoint",
                                      numericInput("tv_all_endpoint_scale", "Endpoint Scaling", value = 4, min = 1, max = 15000))
                                )

                              )
                            )),

                          hr(),

                          fluidRow(
                            column(
                              width = 10, offset = 1,
                              div(
                                textOutput("tv_all_text_scaled")
                              )
                            )
                          ),

                          fluidRow(
                            div(
                              withSpinner(
                                plotlyOutput("plot_tumorvol", width = "100%", height = "750px", inline = F),
                                proxy.height = "100px", color="#0273B7"
                              )
                            )
                          )
                        )
                      ))
        ),
        tabPanel("Tumor Volume Data",
          shinydashboard::box(
            width = 12,
            title = "", solidHeader = TRUE,

            fluidRow(
              column(
                width = 12,
                tags$script(
                  "Shiny.addCustomMessageHandler('resetInputValue', function(variableNameTask){Shiny.onInputChange(variableNameTask, null);});"
                ),

                textOutput("tbl_msg_all"),

                # table with css spinner
                HTML("<br>"),
                withSpinner(
                  DTOutput("tbl_tv_all"),
                  proxy.height = "100px", color="#0273B7"
                ),

                hr()
              )
            )
          )
        )
      )
    ),

    shinydashboard::box(

      width = 12,
      title = "Matched Study Focused Tumor Volume - Visual Analytics", status="primary",solidHeader = TRUE,
      # shinydashboard::box(
      #   width = 12,
      #   title = "Matched Tumor Volumes - Summary", status="primary",solidHeader = TRUE,
        fluidRow(
                                   column(
                                     width = 2,
                                     pickerInput("tv_study_picker", "Study",
                                                 choices = get_tv_study(),
                                                 selected = get_tv_study()[1],
                                                 options = pickerOptions(actionsBox = FALSE, style = 'btn-light',
                                                                         showContent = TRUE),multiple = FALSE),
                                   )
        ),
      # ),
      tabsetPanel(type = "tabs",
                  tabPanel("Objective Response (RECIST)",
                           fluidRow(
                             column(
                               width = 12,
                               div(
                                 fluidRow(
                                   column(
                                     width = 2,
                                     numericInput("tv_recist", "RECIST Day",
                                                  value = 24,
                                                  min = 0, max = 100),
                                   ),
                                   column(
                                     offset = 0,
                                     width = 1,
                                     checkboxInput("tv_interpolate", "Interpolation", FALSE)
                                   )
                                 )
                               )
                             )),

                           hr(),

                           fluidRow(
                             column(
                               width = 10, offset = 1,
                               div(
                                 textOutput("tv_text_scaled_study")
                               )
                             )
                           ),

                           fluidRow(
                             column(
                               width = 6, offset = 0,
                               withSpinner(
                                 plotlyOutput("plot_tumorvol_study", width = "100%", inline=TRUE, height = '500px'),
                                 proxy.height = "100px", color="#0273B7"
                               )),
                             column(
                               width = 6,
                               HTML("<br>"),
                               withSpinner(
                                 DTOutput("dt_dr_table"),
                                 proxy.height = "100px", color="#0273B7"
                               )
                             )
                           )
                  ),
                  tabPanel("Waterfall Plots",
                          fluidRow(
                             column(
                               width = 12,
                               div(
                                 fluidRow(
                                   column(
                                     width = 2,
                                     pickerInput("waterfall_metric", "Waterfall Metric",
                                                 choices = c('dVt', 'AUC.Filtered.Measures', 'AUC.All.Measures'),
                                                 selected = 'dVt',
                                                 options = pickerOptions(actionsBox = FALSE, style = 'btn-light',
                                                                         showContent = TRUE),multiple = FALSE)
                                   ),
                                   column(
                                     width = 2,
                                     numericInput("tv_AUC.day.waterfall", "AUC Day",
                                                  value = 24,
                                                  min = 0, max = 100), ### MAKE THIS SO IT ONLY TURNS ON WHEN AUC LAST DAY IS PICKED. 
                                   ),
                                   column(
                                     offset = 0,
                                     width = 1,
                                     checkboxInput("tv_waterfall_interpolate", "Interpolation", FALSE)
                                   )
                                 )
                               )
                             )),

                           hr(),

                           fluidRow(
                             column(
                               width = 8, offset = 2,
                               withSpinner(
                                 plotlyOutput("tv_plot_waterfall", width = "100%", height = '500px'),
                                 proxy.height = "100px", color="#0273B7"
                               ))
                           )                           
                  ),
                  tabPanel("Event Free Survival Plots",
                          fluidRow(
                             column(
                               width = 12,
                               div(
                                 fluidRow(
                                   column(
                                     width = 2,
                                     numericInput("tv_PercChange_EventSize", "Event Size", ### NOTE THIS MAY CHANGE TO REFLECT MORE THAN % INCREASE
                                                  value = 100,
                                                  min = 0, max = 99999),
                                   )
                                 )
                               )
                             )),

                           hr(),

                           fluidRow(
                             column(
                               width = 8, offset = 2,
                               withSpinner(
                                 plotlyOutput("tv_plot_EFS", width = "100%", height = '500px'),
                                 proxy.height = "100px", color="#0273B7"
                               ))
                           )                           
                  )
      )
    )
  ),

  fluidRow(column(width = 12)),

  fluidRow(
    HTML('<p align="center">&copy; 2022 Seven Bridges &nbsp; &middot; &nbsp; <a href="https://www.sevenbridges.com/privacy-policy/" target="_blank">Privacy</a> &nbsp;&nbsp; <a href="https://www.sevenbridges.com/copyright-policy/" target="_blank">Copyright</a> &nbsp;&nbsp; <a href="https://www.sevenbridges.com/terms-of-service/" target="_blank">Terms</a> &nbsp;&nbsp; <a href="https://www.sevenbridges.com/contact/" target="_blank">Contact</a></p>')
  )
)
