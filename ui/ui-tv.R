tabPanel(
  title = title_tumor_volume, icon = icon("fa-solid fa-chart-line"),
  
  shinydashboard::box(
    width=12,
    title="Query and Data Summary", status="primary",solidHeader = T,

    
    shinydashboard::box(
      width = 12,
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
                  pickerInput("tv_contributor", "Contributor",
                              choices = get_tv_contributor(),
                              selected = get_tv_contributor()[1],
                              options = pickerOptions(actionsBox = TRUE, style = 'btn-light',
                                                      showContent = TRUE),multiple = T)
                  # checkboxGroupInput(
                  #   "tv_contributor", "Contributor",
                  #   inline = TRUE,
                  #   choices = get_tv_contributor(),
                  #   selected = get_tv_contributor()[1]
                  # )
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
            # fluidRow(
            #   style = "margin-left: 2px; margin-right: 2px; margin-bottom: 10px;",
            #   column(
            #     width = 6,
            #     actionButton("tv_submit_query", "Submit Query", class = "btn btn-block btn-primary", icon = icon("paper-plane"))
            #   ),
            #   column(
            #     width = 6,
            #     actionButton("tv_reset_query", "Reset Query", class = "btn btn-block", icon = icon("undo"))
            #   ),
              
              # responsive button text for smaller screens https://stackoverflow.com/questions/19284153/
              # tags$style(type = "text/css", ".btn { white-space: normal; }")
            # )
          )
        )
      )
    ),
    br(),
    shinydashboard::box(
      width = 12,
      title = "Selected Volume Data - Summary", status="primary",solidHeader = TRUE,

      # div(
      #   fluidRow(
      #     column(offset = 0, width = 3,
      #       progressBar(
      #         id = "pbar_tv", value = 100,
      #         total = 100, display_pct = TRUE,
      #         title = "Selected Number of Studies"
      #       )
      #     )
      #   ) 
      # ), 
      # MWL NOTE: At present we do not have a way to select studies. This bar confused me the first few times I looked at it. The data presented is also redundant with 'card_tv_studies'. Removing for now. 

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
    # tags$style(".nav-tabs {
    # background-color: #142A44;
    # }

    # .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
    # background-color: transparent;
    # border-color: transparent;
    # }

    # .nav-tabs-custom .nav-tabs li.active {
    #     border-top-color: #273D54;
    # }"),
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
                                  pickerInput("tv_all_plot_type", "Plot Facet Type",
                                              choices = c("Study Plot", "Treatment Plot"),
                                              selected = c("Study Plot"),
                                              options = pickerOptions(actionsBox = FALSE, style = 'btn-light',
                                                                      showContent = TRUE),multiple = FALSE)
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
                                  div(id="tv_div_all_scale_picker",
                                      pickerInput("tv_all_scaleby", "Scale Plot By",
                                                  choices = c("Growth Factor", "Volume"),
                                                  selected = c("Growth Factor"),
                                                  options = pickerOptions(actionsBox = FALSE, style = 'btn-light',
                                                                          showContent = TRUE),multiple = FALSE))
                                ),
                                column(
                                  width = 3,
                                  div(id="tv_div_all_endpoint",
                                      numericInput("tv_all_endpoint_scale", "Endpoint Scaling", value = 4, min = 1, max = 15000))
                                )

                              )
                            )),

                          fluidRow(
                            column(
                              width = 12,
                              column(
                                width = 2,
                                checkboxInput("tv_all_semi.log", "Semi Log Plot", FALSE)
                              ),
                              column(
                                width = 2,
                                checkboxInput("tv_all_interpolate", "Interpolation", FALSE)
                              ),
                              column(
                                width = 2,
                                offset = 3,
                                checkboxInput("tv_all_scale", "Scaled Plot", FALSE)
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
                      ))
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
                                     numericInput("main_avgplot.day", "Avg Measure Study Day",
                                                  value = 21,
                                                  min = 0, max = 500),
                            ),
                            column(
                                     offset = 0,
                                     width = 2,
                                     checkboxInput("main_avgplot_interpolate", "Interpolate Data For Calculation", FALSE)
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
                                     numericInput("main_TC.day", "T/C Date",
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
                                     numericInput("main_TGI.day", "TGI Date",
                                                  value = 21,
                                                  min = 0, max = 500),
                            ),
                            column(
                                     offset = 0,
                                     width = 2,
                                     checkboxInput("main_tgi_interpolate", "Interpolate Data For Calculation", FALSE)
                            )),

                            hr(),

                             fluidRow(
                             column(
                               width = 6, offset = 0,
                               withSpinner(
                                 plotlyOutput("main_tgi", width = "100%", height = '500px'),
                                 proxy.height = "100px", color="#0273B7"
                               )),
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
                                     numericInput("main_orc.day", "RECIST Calculation Day",
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
        ),
        
      )
    
    
    
    ),
    tabPanel("Individual Study Plots & Analysis",
      fluidRow(
          column(
            width = 2,
            pickerInput("tv_study_picker", "Study",
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
                                     numericInput("tv_recist", "RECIST Day",
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
                  tabPanel("Waterfall Plot",
                           br(),
                          fluidRow(
                             column(
                               width = 12,
                               div(
                                 fluidRow(
                                   column(
                                     width = 2,
                                     pickerInput("waterfall_metric", "Waterfall Metric",
                                                 choices = c('dVt', 'AUC.All.Measures', 'AUC.Filtered.Measures'),
                                                 selected = 'dVt',
                                                 options = pickerOptions(actionsBox = FALSE, style = 'btn-light',
                                                                         showContent = TRUE),multiple = FALSE)
                                   ),
                                   column(
                                     width = 2,
                                     numericInput("tv_AUC.day.waterfall", "AUC Day",
                                                  value = 21,
                                                  min = 0, max = 500),
                                   ),
                                   column(
                                     offset = 0,
                                     width = 2,
                                     checkboxInput("tv_waterfall_interpolate", "Interpolate Data For Calculation", FALSE)
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
                  # tabPanel("Tumor Growth Inhibition",
                  #          br(),
                  #         fluidRow(
                  #            column(
                  #              width = 12,
                  #              div(
                  #                fluidRow(
                  #                    column(
                  #                    width = 2,
                  #                    numericInput("tv_TC.day", "T/C Date",
                  #                                 value = 21,
                  #                                 min = 0, max = 500),
                  #                  ),
                  #                  column(
                  #                    offset = 0,
                  #                    width = 2,
                  #                    checkboxInput("tv_TC_interpolate", "Interpolate Data For Calculation", FALSE)
                  #                  )
                  #                )
                  #              )
                  #            )),

                  #          hr(),

                  #          fluidRow(
                  #            column(
                  #              width = 6, offset = 0,
                  #              withSpinner(
                  #                plotlyOutput("tv_plot_tc", width = "100%", height = '500px'),
                  #                proxy.height = "100px", color="#0273B7"
                  #              )),
                  #             column(
                  #              width = 6,
                  #              HTML("<br>"),
                  #              withSpinner(
                  #                DTOutput("dt_tc_table"),
                  #                proxy.height = "100px", color="#0273B7"
                  #              )
                  #            )
                  #          )
                  # ),
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
                                     numericInput("anova_Measure_Day", "ANOVA Calculation Day",
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
                             )),

                           hr(),

                           fluidRow(
                             column(
                               width = 4, offset = 1,
                               withSpinner(
                                 DTOutput("dt_anova_table"),
                                 proxy.height = "100px", color="#0273B7"
                               )),
                               column(
                               width = 6, offset = 0,
                               withSpinner(
                                 DTOutput("dt_tukey_table"),
                                 proxy.height = "100px", color="#0273B7"
                               ))
                           )
                  )
                  # anova_Measure_Day box for input
                  # main_anova_interpolate for interpolate.
      
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
