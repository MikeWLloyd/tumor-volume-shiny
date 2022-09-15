parameter_tabs <- tabsetPanel(
  id = "scale_options",
  type = "hidden",
  tabPanel("Scaled",
    pickerInput("tv_all_scaleby", "Scale Plot By",
                choices = c("Growth Factor", "Volume"),
                selected = c("Growth Factor"),
                options = pickerOptions(actionsBox = FALSE, style = 'btn-light',
                                        showContent = TRUE),multiple = FALSE),
    numericInput("tv_all_endpoint_scale", "Endpoint Scaling", value = 4, min = 1, max = 15000)
  ),
  tabPanel("Volume",
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
    # used to keep spacing the same, so the UI doesn't jump as noticeably. 
  ),
  tabPanel("Percent Change",
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
  ),
  tabPanel("Semi-Log",
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
  )
)
# This is a dynamic tab set used when 'scaled' is selected in the main plot function. 
# There are three parts to make dynamic UI work. 
# 1. The above code that sets tabPanels by input provided to the 'function'
# 2. calling 'parameter_tabs' in the main UI body below: 
    # column(
    #   width = 3,
    #   parameter_tabs
    # )
# 3. Observing the plot type pick list on the server, and triggering updates to the UI when changes are made to the list.
    # observeEvent(input$tv_all_plotType, {
    #   updateTabsetPanel(inputId = "scale_options", selected = input$tv_all_plotType)
    # }) 

parameter_tabs_waterfall <- tabsetPanel(
  id = "waterfall_options",
  type = "hidden",
  tabPanel("AUC.Filtered.Measures",
    numericInput("tv_AUC.day.waterfall", "AUC Calc. Day", value = 21, min = 0, max = 500)
  ),
  tabPanel("dVt",
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
    # used to keep spacing the same, so the UI doesn't jump as noticeably. 
  ),
  tabPanel("AUC.All.Measures",
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
  )
)
#### DOUBLE CHECK THE INDIVIDUAL WATERFALL CODE.

#### FIX THE VALIDATION, SO THAT IF BODY_WEIGHTS IS PROVIDED BUT THE ENTIRE COLUMN IS EMPTY THE TAB IS ALSO TURNED OFF. 



css <- "
.nav li a.disabled {
background-color: #aaa !important;
color: #333 !important;
cursor: not-allowed !important;
border-color: #aaa !important;
}"
## This css is used to disable the 'Body Weight Analysis' tab, when no body weight data is provided. 

tabPanel(
  title = title_tumor_volume, icon = icon("fa-solid fa-chart-line"),
  shinyjs::inlineCSS(css),

  shinydashboard::box(
    width=12,
    title="Query and Data Summary", status="primary",solidHeader = T,

    
    shinydashboard::box(
      width = 12,
      title = "Data Selection and Filtering", status = "primary", solidHeader = TRUE,

      fluidRow(
        id = "input_group_tv", # for reset button
        column(
          width = 12,
          column(width = 12,
            fluidRow(
              column(
                width = 3,
                # wellPanel(
                  pickerInput("tv_contributor", "Contributor",
                              choices = get_tv_contributor(),
                              selected = get_tv_contributor()[1],
                              options = pickerOptions(actionsBox = TRUE, style = 'btn-light',
                                                      showContent = TRUE),multiple = T)
              ),
              column(
                width = 3,
                  pickerInput("tv_treatment", "Treatment Arm",
                              choices = get_tv_treatment(),
                              selected = c('Control', get_tv_treatment()[1:3]),
                              options = pickerOptions(actionsBox = TRUE, style = 'btn-light',
                                                      showContent = TRUE),multiple = T)
              ),
              column(
                width = 3,
                pickerInput("tv_study", "Study",
                            choices = get_tv_study(),
                            selected = get_tv_study()[1:length(get_tv_study())],
                            options = pickerOptions(actionsBox = TRUE, style = 'btn-light',
                                                    showContent = TRUE),multiple = T)
              ),
               column(
                width = 3,
                pickerInput("tv_disease_type", "Disease Type",
                            choices = get_tv_disease(),
                            selected = get_tv_disease()[1],
                            options = pickerOptions(actionsBox = TRUE, style = 'btn-light',
                                                    showContent = TRUE),multiple = T)
              )
            )
          )
        )
      )
    ),
    br(),
    shinydashboard::box(
      width = 12,
      title = "Selected Volume Data - Summary", status="primary",solidHeader = TRUE,

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
  ),
  br(),
  hr(),
  br(),
  shinydashboard::tabBox(
    width = 12,
    title = "",
    id = "main_tabset", height = "250px",
    tabPanel("Cross Study Plots & Analysis",
      tabsetPanel(type = "tabs",
          tabPanel("Response Plot",
            br(),
            fluidRow(
              column(
                width = 12,
                div(
                  fluidRow(
                    column(
                      width = 12,
                      div(
                        column(
                          width = 3,
                          tabPanel("tv_plot",
                            pickerInput("tv_all_plot_type", "Plot Facet Type",
                                        choices = c("Study Plot", "Treatment Plot"),
                                        selected = c("Study Plot"),
                                        options = pickerOptions(actionsBox = FALSE, style = 'btn-light',
                                                                showContent = TRUE),multiple = FALSE),
                            checkboxInput("tv_all_interpolate", "Interpolate Data", FALSE)
                          )
                        ),
                        column(
                          width = 3,
                          pickerInput("tv_all_plot_style", "Plot Style",
                                      choices = c("Study Average", "Individual Animal"),
                                      selected = c("Study Average"),
                                      options = pickerOptions(actionsBox = FALSE, style = 'btn-light',
                                                              showContent = TRUE),multiple = FALSE)
                        ),
                        column(
                          width = 3,
                          pickerInput("tv_all_plotType", "Plot Type",
                                            choices = c("Volume", "Scaled", "Percent Change", "Semi-Log"),
                                            selected = c("Volume"),
                                            options = pickerOptions(actionsBox = FALSE, style = 'btn-light',
                                                                    showContent = TRUE),multiple = FALSE)
                        ),
                        column(
                          width = 3,
                          parameter_tabs
                        )
                      )
                    )
                  ),

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
              )
            )
          ),

      tabPanel("Average Volume Plot",
        br(),
        fluidRow(
          column(
            width = 12,
            div(
              fluidRow(
                column(
                  width = 2,
                  numericInput("main_avgplot.day", "Avg Measure Calc. Day",
                                value = 21,
                                min = 0, max = 500),
                ),
                column(
                  offset = 0,
                  width = 2,
                  checkboxInput("main_avgplot_interpolate", "Interpolate Data\nFor Calculation", FALSE)
                ),
                column(
                  width = 12,
                  div(
                    withSpinner(
                      plotlyOutput("avg_growth_plot", width = "100%", height = "750px", inline = F),
                      proxy.height = "100px", color="#0273B7"
                    )
                  )
                )
              )
            )
          )
        )
      ),

      tabPanel("Log2 Fold Change Plot",
        br(),
        fluidRow(
          column(
            width = 12,
            div(
              fluidRow(
                column(
                  offset = 0,
                  width = 2,
                  checkboxInput("main_log2_interpolate", "Interpolate Data For Calculation", FALSE)
                ),
                column(
                  width = 12,
                  div(
                    withSpinner(
                      plotlyOutput("log2_foldchange", width = "100%", height = "750px", inline = F),
                      proxy.height = "100px", color="#0273B7"
                    )
                  )
                )
              )
            )
          )
        )
      ),

      tabPanel("Hybrid Waterfall Plot",
        br(),
        fluidRow(
          column(
            width = 12,
            div(
              fluidRow(
                column(
                  width = 2,
                  numericInput("main_TC.day", "Waterfall Calc. Day",
                                value = 21,
                                min = 0, max = 500),
                ),
                column(
                  offset = 0,
                  width = 2,
                  checkboxInput("main_tc_interpolate", "Interpolate Data For Calculation", FALSE)
                ),
                column(
                  width = 12,
                  div(
                    withSpinner(
                      plotlyOutput("hybrid_waterfall", width = "100%", height = "750px", inline = F),
                      proxy.height = "100px", color="#0273B7"
                    )
                  )
                )
              )
            )
          )
        )
      ),

      tabPanel("Tumor Growth Inhibition",
        br(),
        fluidRow(
          column(
            width = 12,
            div(
              fluidRow(
                column(
                  width = 2,
                  numericInput("main_TGI.day", "TGI Calc. Day",
                                value = 21,
                                min = 0, max = 500),
                ),
                column(
                  offset = 0,
                  width = 2,
                  checkboxInput("main_tgi_interpolate", "Interpolate Data For Calculation", FALSE)
                )
              ),

              hr(),

              fluidRow(
                column(
                  width = 6, offset = 0,
                  withSpinner(
                    plotlyOutput("main_tgi", width = "100%", height = '500px'),
                    proxy.height = "100px", color="#0273B7"
                  )
                ),
                column(
                  width = 6,
                  HTML("<br>"),
                  withSpinner(
                    DTOutput("main_tc_table"),
                    proxy.height = "100px", color="#0273B7"
                  )
                )
              )                          
            )
          )
        )
      ),

      tabPanel("Stacked Objective Response Plot",
        br(),
        fluidRow(
          column(
            width = 12,
            div(
              fluidRow(
                column(
                  width = 2,
                  numericInput("main_orc.day", "RECIST Calc. Day",
                                value = 21,
                                min = 0, max = 500),
                ),
                column(
                  offset = 0,
                  width = 2,
                  checkboxInput("main_stackedorc_interpolate", "Interpolate Data For Calculation", FALSE)
                ),
                column(
                  width = 12,
                  div(
                    withSpinner(
                      plotlyOutput("main_orc_plot", width = "100%", height = "750px", inline = F),
                      proxy.height = "100px", color="#0273B7"
                    )
                  )
                )
              )
            )
          )
        )
      ) 
    )
  ),

  tabPanel("Individual Study Plots & Analysis",
    fluidRow(
      column(
        width = 2,
        pickerInput("tv_study_filtered", "Study",
                    choices = get_tv_study(),
                    options = pickerOptions(actionsBox = FALSE, style = 'btn-light',
                                            showContent = TRUE),multiple = FALSE),
      )
    ),

    tabsetPanel(type = "tabs",
      tabPanel("Objective Response (RECIST)",
        br(),
        fluidRow(
          column(
            width = 12,
            div(
              fluidRow(
                column(
                  width = 2,
                  numericInput("tv_recist", "RECIST Calc. Day",
                              value = 21,
                              min = 0, max = 500),
                ),
                column(
                  offset = 0,
                  width = 2,
                  checkboxInput("tv_interpolate", "Interpolate Data For Calculation", FALSE)
                )
              )
            )
          )
        ),

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
              plotlyOutput("plot_tumorvol_study", width = "100%", inline=F, height = '500px'),
              proxy.height = "100px", color="#0273B7"
            )
          ),

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

      tabPanel("Waterfall Plot",
        br(),
        fluidRow(
          column(
            width = 12,
            div(
              fluidRow(
                column(
                  width = 12,
                  div(
                    column(
                      width = 3,
                      tabPanel("waterfall_plot",
                        pickerInput("waterfall_metric", "Waterfall Metric",
                                    choices = c('dVt', 'AUC.All.Measures', 'AUC.Filtered.Measures'),
                                    selected = 'dVt',
                                    options = pickerOptions(actionsBox = FALSE, style = 'btn-light',
                                                            showContent = TRUE),multiple = FALSE),
                        checkboxInput("tv_waterfall_interpolate", "Interpolate Data For Calculation", FALSE)
                      )
                    ),
                    column(
                      width = 3,
                      parameter_tabs_waterfall
                    )
                  )
                )
              ),

              hr(),

              fluidRow(
                div(
                  withSpinner(
                    plotlyOutput("tv_plot_waterfall", width = "100%", height = "750px", inline = F),
                    proxy.height = "100px", color="#0273B7"
                  )
                )
              )
            )
          )
        )
      ),

      tabPanel("Event Free Survival",
        br(),
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
          )
        ),

        hr(),

        fluidRow(
          column(
            width = 8, offset = 2,
            withSpinner(
              plotlyOutput("tv_plot_EFS", width = "100%", height = '500px'),
              proxy.height = "100px", color="#0273B7"
            )
          )
        )
      ),

      tabPanel("ANOVA",
        br(),
        fluidRow(
          column(
            width = 12,
            div(
              fluidRow(
                column(
                  width = 2,
                  numericInput("anova_Measure_Day", "ANOVA Calc. Day",
                              value = 21,
                              min = 0, max = 99999),
                ),
                column(
                  offset = 0,
                  width = 2,
                  checkboxInput("main_anova_interpolate", "Interpolate Data For Analysis", FALSE)
                )
              )
            )
          )
        ),

        hr(),

        fluidRow(
          column(
            width = 4, offset = 1,
            withSpinner(
              DTOutput("dt_anova_table"),
              proxy.height = "100px", color="#0273B7"
            )
          ),
          column(
            width = 6, offset = 0,
            withSpinner(
              DTOutput("dt_tukey_table"),
              proxy.height = "100px", color="#0273B7"
            )
          )
        )
      )
    )
  ),
    tabPanel("Body Weight Analysis", value = 'weight_tab',
      fluidRow(
        column(
          width = 12,
          fluidRow(
            column(
              width = 12,
              div(
                column(
                  width = 3,
                    pickerInput("tv_weight_plot_type", "Plot Facet Type",
                                choices = c("Study Plot", "Treatment Plot"),
                                selected = c("Study Plot"),
                                options = pickerOptions(actionsBox = FALSE, style = 'btn-light',
                                                        showContent = TRUE),multiple = FALSE)
                ),
                column(
                  width = 3,
                  pickerInput("tv_weight_plot_style", "Plot Style",
                              choices = c("Study Average", "Individual Animal"),
                              selected = c("Study Average"),
                              options = pickerOptions(actionsBox = FALSE, style = 'btn-light',
                                                      showContent = TRUE),multiple = FALSE)
                ),
                column(
                  width = 3,
                  pickerInput("tv_weight_plotType", "Plot Type",
                                    choices = c("Weight", "Percent Change"),
                                    selected = c("Weight"),
                                    options = pickerOptions(actionsBox = FALSE, style = 'btn-light',
                                                            showContent = TRUE),multiple = FALSE)
                )
              )
            )
          )
        )
      ),
      hr(),
      fluidRow(
        column(
          width = 10, offset = 1,
          div(
            withSpinner(
              plotlyOutput("plot_mouseweight", width = "100%", height = "750px", inline = F),
              proxy.height = "100px", color="#0273B7"
            )
          )
        )
      )
    ),
    tabPanel("Current Data Table",
      fluidRow(
        column(
          width = 12,
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
  ),
  fluidRow(column(width = 12))
)



