### If the App idles for more than 4 min, this will warn the user a shutdown is coming. 
observeEvent(input$warnTimeOut, { 
    #print(paste0("Session (", session$token, ") approaching time out at: ", Sys.time()))
    
    showModal(modalDialog(
        title = "Timeout",
        HTML(paste('Session will time out due to inactivity in 60 seconds.<br>Click continue to maintain your session.')),
        footer = actionButton("dismiss_modal", label = "Continue")
    ))
})
####

observeEvent(input$dismiss_modal, {
  removeModal()
})

### If the App idles for more than 5 min, this will close the session and stop the server. 
observeEvent(input$timeOut, { 
    #print(paste0("Session (", session$token, ") timed out at: ", Sys.time()))
    
    showModal(modalDialog(
        title = "Timeout",
        HTML(paste("Session timed out due to", round((as.numeric(input$timeOut) / 60), 2) , "min of inactivity at -", Sys.time(), '<br>Refresh the page to reconnect to the Shiny server, and start a new session.')),
        footer = NULL
    ))

    session$close()
    
    #stopApp()
})
####

### NOTE: 
### 
### The factors: Contributor, Study, Tumor, and Arms must be accounted for by every plot / metric. 

# DATA
## load example data. 
data <- try(as.data.frame(readRDS("include/tv-test.rds")), silent = T) #%>% withSpinner(color="#0dc5c1")
rownames(data) <- NULL

# data <- try(as.data.frame(read.csv("include/unit_test.csv", header = TRUE)), silent = T)
# rownames(data) <- NULL

# GET methods
  get_tv_contributor <- function() {
    if (length(unique(data$Contributor)) < 1) {
      unique(data$Contributor)
    } else {
      sort(unique(data$Contributor), na.last = T,)
    }
  }

  get_tv_treatment <- function() {
    
    filtered.df <- base::subset(
      data, Contributor %in% unique(sort(data$Contributor))[1]
    ) # filter list by what contributor is first picked.
    
    if (length(unique(filtered.df$Arms)) < 2) {
      unique(filtered.df$Arms)
    } else {
      agents <- factor(unique(filtered.df$Arms))
      agents <- relevel(agents, 'Control')
      levels(agents)
    }
  }

  get_tv_disease <- function() {

    filtered.df <- base::subset(
      data, Contributor %in% unique(sort(data$Contributor))[1]
    ) # filter list by what contributor is first picked.
    

    if (length(unique(filtered.df$Disease_Type)) < 2) {
      list(unique(filtered.df$Disease_Type))
    } else {
      sort(unique(filtered.df$Disease_Type), na.last = T)
    }
  }

  get_tv_study <- function() {

    filtered.df <- base::subset(
      data, Contributor %in% unique(sort(data$Contributor))[1]
    ) # filter list by what contributor is first picked.
    

    if (length(unique(filtered.df$Study)) < 2) {
      unique(sort(filtered.df$Study))
    } else {
      unique(sort(filtered.df$Study, na.last = T))
    }
  }

  get_tv_tumor <- function() {

    filtered.df <- base::subset(
      data, Contributor %in% unique(sort(data$Contributor))[1]
    ) # filter list by what contributor is first picked.
    
    if (length(unique(filtered.df$Tumor)) < 2) {
      unique(sort(filtered.df$Tumor))
    } else {
      unique(sort(filtered.df$Tumor, na.last = T))
    }
  }

## ## ## ## ##

# CARDS
  get_tv_contributor_count <- function(df) ifelse(is.null(df), 0, length(unique(df$"Contributor")))

  output$card_tv_contributor <- shinydashboard::renderValueBox({
    ret <- get_query_tv()$"df"
    shinydashboard::valueBox(
      get_tv_contributor_count(ret), "Contributors",
      icon = icon("hospital", verify_fa = FALSE), color = "light-blue"
    )
  })

  get_tv_volumes_count <- function(df) ifelse(is.null(df), 0, length(unique(df$"ID")))

  output$card_tv_volumes <- shinydashboard::renderValueBox({
    ret <- get_query_tv()$"df"
    shinydashboard::valueBox(
      get_tv_volumes_count(ret), "Unique Mouse IDs",
      icon = icon("file-medical", verify_fa = FALSE), color = "green"
    )
  })

  get_tv_studies_count <- function(df) ifelse(is.null(df), 0, length(unique(df$"Study")))

  output$card_tv_studies <- shinydashboard::renderValueBox({
    ret <- get_query_tv()$"df"
    shinydashboard::valueBox(
      get_tv_studies_count(ret), "Studies",
      icon = icon("chart-area", verify_fa = FALSE), color = "yellow"
    )
  })

  get_tv_models_count <- function(df) ifelse(is.null(df), 0, length(unique(df$"Tumor")))

  output$card_tv_models <- shinydashboard::renderValueBox({
    ret <- get_query_tv()$"df"
    shinydashboard::valueBox(
      get_tv_models_count(ret), "Models",
      icon = icon("paw", verify_fa = FALSE), color = "blue"
    )
  })

  get_tv_disease_count <- function(df) ifelse(is.null(df), 0, length(unique(df$"Disease_Type")))

  output$card_tv_disease <- shinydashboard::renderValueBox({
    ret <- get_query_tv()$"df"
    shinydashboard::valueBox(
      get_tv_disease_count(ret), "Disease Types",
      icon = icon("disease", verify_fa = FALSE), color = "red"
    )
  })

  get_tv_treatment_count <- function(df) ifelse(is.null(df), 0, length(unique(df$"Arms")))

  output$card_tv_treatment <- shinydashboard::renderValueBox({
    ret <- get_query_tv()$"df"
    shinydashboard::valueBox(
      get_tv_treatment_count(ret), "Treatment Arms",
      icon = icon("pills", verify_fa = FALSE), color = "maroon"
    )
  })

## ## ## ## ## ##

# UPLOAD
  output$tv_text_upload <- renderText({
      paste0("Example Tumor Volume Data is Preloaded. \nClick Upload Button to Load Your Tumor Volume Data!")
  })

  observeEvent(input$user_tv_upload, {
    output$tv_text_upload <- renderText({
      paste0("Data Upload is Unsuccessful. Please check the Validation Tab for Input Data Conversion to the Expected Format!")
    })
  })

  observeEvent(input$user_tv_load_default, {
    output$tv_text_upload <- renderText({
      paste0("Example Tumor Volume Data is Preloaded. \nClick Upload Button to Load Your Tumor Volume Data!")
    })
  })

## ## ## 

# QUERY
  ## Track is data are uploaded or not. 
  values <- reactiveValues(
    upload_state = "default"
  )

  ## Track if data are loaded or not
  check_state <- reactiveValues(
    loaded_data = FALSE
  )

  # Two states are needed to track if data is uploaded, and if data in general is loaded. 
  # The first is to check what data to load to the app, the second is to allow for checks to be done on selected items
  # Without checking if any data is loaded, the app will display invalid messages to the splash page on instantiation of the app. 

  ## holdover code from prior 'reset' button
  # observeEvent(input$user_tv_load_default_btn, {
  #   values$upload_state <- 'reset'
  #   output$tv_text_upload <- renderText({
  #     paste0("Example Tumor Volume Data Still Active!")
  #   })
  # })
  ## Need to reimplement the 'RESET' button. 

  ## Main data import reactive function. 
  get_data <- reactive({

      curr_data <- data

      if (values$upload_state == 'uploaded') {
        
        check_state$loaded_data = FALSE

        input_file_user <- input$user_tv_data

        if (is.null(input_file_user) | flag_user_data$flag != 1) {
            # catch when a file is uploaded, but the file is improperly formatted. 

            curr_data <- data
            
            shinyjs::enable(selector = '.nav-tabs a[data-value="weight_tab"')

        } else{
          # else there is a data file and it passed validation, and we should load it in.
          removeNotification(id = 'bad_import_format')
          ext <- tools::file_ext(input_file_user$datapath)

          if (ext == "csv") {
            curr_data <- read.csv(input_file_user$datapath, header = TRUE)
          } else if (ext %in% c("xlsx","xls")) {
            curr_data <- read_excel(input_file_user$datapath)
          } else {
            # should not get here, as users are only able to pick valid extensions. 
            showNotification(id = 'bad_import_format', "Expected import formats are CSV and Excel (xlsx or xls). Please upload correct format.", type = c("error"), duration = NULL, closeButton = FALSE)
            curr_data <- data
            shinyjs::enable(selector = '.nav-tabs a[data-value="weight_tab"')
          }
          # files <- list.files(outputDir, full.names = TRUE)
          # data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
          # Concatenate all data together into one data.frame. Example commented code is untested.
        }

        ## With the new data, update all pick lists. 
        updatePickerInput(session, "tv_contributor",
                          choices = unique(sort(curr_data$Contributor)),
                          selected = unique(sort(curr_data$Contributor))[1])

        ## Make the remaining pick lists relative to the selected 'contributor'
        filtered.df <- base::subset(
          curr_data, Contributor %in% unique(sort(curr_data$Contributor))[1]
        )

        n_unique_arms <- length(unique(filtered.df$Arms))

        agents <- factor(unique(filtered.df$Arms))
        agents <- levels(relevel(agents, 'Control'))

        n_studies <- length(unique(sort(filtered.df$Study)))

        updatePickerInput(session, "tv_treatment",
                      choices = agents,
                      selected = c('Control', unique(filtered.df$Arms)[1:min(10,n_unique_arms)]))

        updatePickerInput(session, "tv_disease_type",
                      choices = unique(sort(filtered.df$Disease_Type)),
                      selected = unique(sort(filtered.df$Disease_Type))[1])

        updatePickerInput(session, "tv_study",
                      choices = unique(sort(filtered.df$Study)),
                      selected = unique(sort(filtered.df$Study))[1:n_studies])

        updatePickerInput(session, inputId = "tv_study_filtered",
                      choices = unique(sort(filtered.df$Study)))

         # Note: the pick lists are updated in two places. here, and in the above 'get_tv_NAME' functions.
         #       they are first populated to prevent errors when the app is first loaded and when the example data first loads in.

      } else if (values$upload_state == 'reset') {
        curr_data <- data
        ## Need to reimplement the 'RESET' button. 

      } else {
        curr_data <- data
      }
      # This is where the data that is imported is either loaded or not. When an upload is triggered, it is tested and if valid passed in. 
      # If the data is not valid, the base dataset is loaded in. 

      # There was an issue with the data loaded, and then 'reloaded' when the app was first instantiated. This was because of the logic block above.
      # The app loads, then would 'updatePickerInput' for all pick lists. This only needs to be done if the app changes data.
      # The updatePickerInput was moved to a block when a new upload is done. If 'RESET' is reimplemented, the pick lists will need to be updated in that block as well. 

      # Note that Shiny is 'smart' enough to know when data was loaded and when/if it should run the same statement again. 
      # The csv file gets loaded once, and then the 'curr_data' are queried in the get_query_tv statement. Because get_data is reactive, this block is only run once per change/update to inputs$user_tv_data. 

      tumor_list <- curr_data %>% dplyr::filter(Study == curr_data$Study[1]) %>% dplyr::select(Tumor) %>% unique()

      updatePickerInput(session, inputId = "tv_tumor_filtered",
                        choices = tumor_list$Tumor)
      ## Update the Tumor list used in ANOVA. Done here regardless of the data loaded.

      check_state$loaded_data = TRUE
      # set check_state to true, showing we have data and observeevents below should update pick lists based on logic checks. 

      return(curr_data)
  })

  # Main data query and filter reactive function.   button
  get_query_tv <- reactive( {

    # if(input$query_button == 0) {
    #   return(0)
    # } 
    # The above would supress a first load of the data, until 'query_button' was clicked for the first time. 
    # If on load we no longer want the data loaded, uncomment this code. 

    input$query_button
    # make the code dependent on the input button

    isolate({
      # This statement isolates the code that follows from 'seeing' updates to the pick lists. Until the 'button' is clicked. 
      # It breaks the dependency of the other inputs until input$button triggers. 

      withProgress(
        message = "Querying Tumor Volume data:",
        value = 0, {
          for (i in 1:10) {
            incProgress(0.025, detail = "[Status] Sending your query to server...")
            Sys.sleep(0.03)
          }

          for (i in 1:10) {
            incProgress(0.025, detail = "[Status] Executing query, please wait...")
            Sys.sleep(0.03)
          }
          curr_data <- get_data()
          # note that 'get_data' is held by shiny as a set variable until the reactive value inputs$user_tv_data is changed. 
          # once a data file is loaded, this is a constant. 

          df_query <- query_tv(curr_data,
                              input$tv_contributor,
                              input$tv_treatment,
                              input$tv_study,
                              input$tv_disease_type)

          updateProgressBar(
            session = session,
            id = "pbar_tv",
            value = length(unique((df_query$df)$Study)),
            total = length(unique(curr_data$Study))
          )

          for (i in 1:10) {
            incProgress(0.025, detail = "[Status] Finishing up...")
            Sys.sleep(0.02)
          }

          incProgress(0.15, detail = "[Status] Rendering data...")
        }
      )

      choices <- input$tv_study

      updatePickerInput(
        session,
        inputId = "tv_study_filtered",
        choices = choices
      )
      # update the sub-list in 'Individual Study'

      df_query

    })

  })

# OBSERVE EVENTS

  # Watch to see if study is empty.  
  observeEvent(input$tv_contributor, {
    if (check_state$loaded_data) {
      if( is.null(input$tv_contributor) ) {
        showNotification(id = 'missing_contrib', "You must select a contributor from the contributor list", type = c("error"), duration = NULL, closeButton = FALSE)
        shinyjs::disable('query_button')
      } else {
        removeNotification(id = 'missing_contrib')
        shinyjs::enable('query_button')
      }
    }
  }, ignoreNULL = F)

  # Watch to see if any treatments are picked, and if 'Control' is or is not selected in treatment Arms. Notify user that a treatment AND 'Control' must be selected. 
  observeEvent(input$tv_treatment, {
    if (check_state$loaded_data) {
      if( !('Control' %in% input$tv_treatment)) {
        showNotification(id = 'missing_control', "You must select 'Control' in the treatment arm list", type = c("error"), duration = NULL, closeButton = FALSE)
        shinyjs::disable('query_button')
      } else {
        removeNotification(id = 'missing_control')
        shinyjs::enable('query_button')
      }

      if( length(input$tv_treatment) > 25) {
        showNotification(id = 'too_many_arms', "You must select fewer treatment arms. Plots support a maximum of 25 arms.", type = c("error"), duration = NULL, closeButton = FALSE)
        shinyjs::disable('query_button')
      } else {
        removeNotification(id = 'missing_control')
        shinyjs::enable('query_button')
      }

    }
  }, ignoreNULL = F)


  # Watch to see if study is empty.  
  observeEvent(input$tv_study, {
    if (check_state$loaded_data) {
      if(is.null(input$tv_study) ) {
        showNotification(id = 'missing_study', "You must select a study from the study list", type = c("error"), duration = NULL, closeButton = FALSE)
        shinyjs::disable('query_button')
      } else {
        removeNotification(id = 'missing_study')
        shinyjs::enable('query_button')
      }
    }
  }, ignoreNULL = F)

  # Watch to see if study is empty.  
  observeEvent(input$tv_disease_type, {
    if (check_state$loaded_data) {
      if( is.null(input$tv_disease_type) ) {
        showNotification(id = 'missing_disease', "You must select a disease type from the disease type list", type = c("error"), duration = NULL, closeButton = FALSE)
        shinyjs::disable('query_button')
      } else {
        removeNotification(id = 'missing_disease')
        shinyjs::enable('query_button')
      }
    }
  }, ignoreNULL = F)

  # Watch is the study selector in the 'individual study' section changes, and update what is available in ANOVA tumor selector.
  observeEvent(input$tv_study_filtered, {
    if (check_state$loaded_data) {
      
      #tv_study_filtered
      df <- base::subset(
        get_query_tv()$"df", Study %in% c(input$tv_study_filtered)
      )
      
      df <- droplevels(df)

      updatePickerInput(session, inputId = "tv_tumor_filtered",
              choices = unique(sort(levels(as.factor(df$Tumor)))))

    }
  },ignoreNULL = F)

  # # Watch is the contributor selector changes, and update what is available in study / treatment / disease type.
  observeEvent(input$tv_contributor, {
    if (check_state$loaded_data) {

      # get full data, and then filter to what is selected in contrib. 
      df <- get_data()

      filtered.df <- base::subset(
        df, Contributor %in% input$tv_contributor
      )

      n_unique_arms <- length(unique(filtered.df$Arms))

      agents <- factor(unique(filtered.df$Arms))
      agents <- levels(relevel(agents, 'Control'))

      n_studies <- length(unique(sort(filtered.df$Study)))

      updatePickerInput(session, "tv_study",
                    choices = unique(sort(filtered.df$Study)),
                    selected = unique(sort(filtered.df$Study))[1:n_studies])

      updatePickerInput(session, "tv_treatment",
                    choices = agents,
                    selected = c('Control', unique(filtered.df$Arms)[1:min(10,n_unique_arms)]))

      updatePickerInput(session, "tv_disease_type",
                    choices = unique(sort(filtered.df$Disease_Type)),
                    selected = unique(sort(filtered.df$Disease_Type))[1])

      updatePickerInput(session, inputId = "tv_study_filtered",
                    choices = unique(sort(filtered.df$Study)))
    } 
  },ignoreNULL = T) #

  # # Watch is the study selector changes, and update what is available in treatment / disease type.
  observeEvent(input$tv_study, {
    if (check_state$loaded_data) {

      # get full data, and then filter to what is selected in contrib. 
      df <- get_data()

      filtered.df <- df %>% dplyr::filter(Contributor %in% input$tv_contributor & Study %in% input$tv_study) 

      n_unique_arms <- length(unique(filtered.df$Arms))

      agents <- factor(unique(filtered.df$Arms))
      agents <- levels(relevel(agents, 'Control'))

      n_studies <- length(unique(sort(filtered.df$Study)))

      updatePickerInput(session, "tv_treatment",
                    choices = agents,
                    selected = c('Control', unique(filtered.df$Arms)[1:min(10,n_unique_arms)]))

      updatePickerInput(session, "tv_disease_type",
                    choices = unique(sort(filtered.df$Disease_Type)),
                    selected = unique(sort(filtered.df$Disease_Type))[1])

    } 
  },ignoreNULL = T)

 # # Watch for download report click, and open modal. 
  observeEvent(input$report_modal, {
    showModal(modalDialog(
      title = "Selection Report Options",
        HTML("Generate a report capturing the currently selected options across all tabs.<br><br>"),
        radioButtons("report_type", "Output File Type:",
               c("HTML" = "html",
                 "PDF" = "pdf")),
        HTML("<br>NOTE: Report compile time can be ~1-5min.<br>Please be patient while the report is generated."),
      footer = tagList(
        downloadButton(outputId = "report", "Generate Report"),
        modalButton("Cancel")
      ),
      easyClose = TRUE
    ))
  })

## ## ## ## ##

# PLOTS & METRICS

  ## Tumor volume plot - cross study

    ### Get scale by information for 'scaled plot'
    observeEvent(input$tv_all_scaleby,{
      if(input$tv_all_scaleby == "Volume") {
        updateNumericInput(session, "tv_all_endpoint_scale",
                          label = paste("Endpoint Scaling (Volume mm3)"),
                          value = 1200, min = 0, max = 10000)
      } else {
        updateNumericInput(session, "tv_all_endpoint_scale",
                          label = paste("Endpoint Scaling (Growth Factor)"),
                          value = 4, min = 0, max = 10000)
      }
    })

    ### get plot type, used as trigger for turning 'scale options' on in the UI. 
    observeEvent(input$tv_all_plotType, {
      updateTabsetPanel(inputId = "scale_options", selected = input$tv_all_plotType)
    }) 

    ### generate the plot. 
    output$plot_tumorvol <- renderPlotly({
      # Data
      s.data <- get_query_tv()$"df"

      if (is.null(s.data) | (nrow(s.data) == 0)) {
        plot_ly()
      } else {

        # Get Level Type Input by User

        if (input$tv_all_plot_type == "Treatment Plot") {
          pattern_type <- "Treatment"
        } else if (input$tv_all_plot_type == "Study Plot") {
          pattern_type <- "Study"
        }

        if (input$tv_all_plot_style == "Study Average") {
          level_type <- "Arm"
        } else if (input$tv_all_plot_style == "Individual Animal") {
          level_type <- "Animal"
        }

        # interpolate the data if asked for.
        if (input$tv_all_interpolate){
            s.data = get_interpolated_pdx_data(data = s.data)
            s.data$Volume <- s.data$Interpolated_Volume
        }

          #Adjust the data to semi-log if asked for.
        if (input$tv_all_plotType == 'Log2(Volume)'){
            s.data$Volume <- log2(1 + s.data$Volume)

          # Adjust the data to percent change the data if asked for
        } else if (input$tv_all_plotType == 'Percent Change') {
            s.data <- s.data %>%
              dplyr::arrange(Contributor, Study, Tumor, Arms, ID, Times) %>%
              dplyr::group_by(Contributor, Study, Tumor, Arms, ID) %>%
              dplyr::mutate(dVt = (((Volume - Volume[1]) / Volume[1] ) * 100))
            s.data$Volume <- s.data$dVt

          # Adjust to semi-log of prop change if asked for
        } else if (input$tv_all_plotType == 'Log2(Proportion Volume Change)') {
            s.data <- s.data %>%
              dplyr::arrange(Contributor, Study, Tumor, Arms, ID, Times) %>%
              dplyr::group_by(Contributor, Study, Tumor, Arms, ID) %>%
              dplyr::mutate(log2.dVt = log2(1 + ((Volume - Volume[1]) / Volume[1])))
            s.data$Volume <- s.data$log2.dVt
        }

        # Call plot
        if (input$tv_all_plotType == 'Scaled') {
          if(input$tv_all_scaleby == "Volume") {
            scale_by_volume_all <- TRUE

            output$tv_all_text_scaled <- renderText({
              paste0("Plots are scaled by Volume. Y-axis ranges from 100% with endpoint scaling of ",
                    input$tv_all_endpoint_scale,
                    "mm3 to -100% (total regression)")
            })

          } else {
            scale_by_volume_all <- FALSE

            output$tv_all_text_scaled <- renderText({
              paste0("Plots are scaled by Relative Growth. Y-axis ranges from 100% with endpoint scaling of ",
                    input$tv_all_endpoint_scale,
                    "x starting growth to -100% (total regression)")
            })
          }

          endpoint_scale_all <- input$tv_all_endpoint_scale

          get_plot_scaled(data = s.data, position.dodge = 0.2,  title = NULL, scale.factor = endpoint_scale_all, scale.by.volume = scale_by_volume_all, level = level_type, pattern = pattern_type)

        } else {

          # Reset Info Text of the Scaled Plot
          output$tv_all_text_scaled <- renderText({""})

          get_tv_plot(data = s.data, level = level_type, pattern = pattern_type, position.dodge = 0.2, tv_all_plotType = input$tv_all_plotType)
        }
      }
    })

  ## Average growth plot

    ### Get day for calculation 
    avgplot.study.day <- reactive({
      return(input$main_avgplot.day)
    })

    ### Generate the plot
    output$avg_growth_plot <- renderPlotly({

      s.data <- get_query_tv()$"df"

      if (is.null(s.data) | (nrow(s.data) == 0)) {
        plot_ly()
      } else {

        if (input$main_avgplot_interpolate){
          s.data <- get_interpolated_pdx_data(data = s.data)
          s.data$Volume <- s.data$Interpolated_Volume
        }

        tc_ratios <- T.C_ratio(s.data, last.measure.day = avgplot.study.day())

        p1 <- ggplotly(plotAvgGrowthBar(tc_ratios))

        p1 %>% layout(legend = list(
                      itemclick = FALSE,
                      itemdoubleclick = FALSE,
                      groupclick = FALSE
                      )) # turn off plotly legend click
      }
    })


  ## Log2Fold change plot

    
    ### Get Calculation day
    log2fold.study.day <- reactive({
      return(input$main_log2fold.day)
    })

    ### Generate the plot
    output$log2_foldchange <- renderPlotly({

      s.data <- get_query_tv()$"df"

      if (is.null(s.data) | (nrow(s.data) == 0)) {
        plot_ly()
      } else {

        if (input$main_log2_interpolate){
          s.data <- get_interpolated_pdx_data(data = s.data)
          s.data$Volume <- s.data$Interpolated_Volume
        }

        vc_change <- IndividualMouseResponse(s.data, last.measure.day = log2fold.study.day())

        p1 <- ggplotly(log2FoldPlot(vc_change, caption_text_on = F))

        p1 %>% layout(legend = list(
                itemclick = FALSE,
                itemdoubleclick = FALSE,
                groupclick = FALSE
                ))  # https://stackoverflow.com/questions/56195584/r-plotly-disable-legend-click-and-legend-double-click

      }

    })

  ## Hybrid waterfall plot

    ### Get calculation day
    mainTC.study.day <- reactive({
      return(input$main_TC.day)
    })

    ### Generate the plot
    output$hybrid_waterfall <- renderPlotly({

      s.data <- get_query_tv()$"df"

      if (is.null(s.data) | (nrow(s.data) == 0)) {
        plot_ly()
      } else {

        if (input$main_tc_interpolate){
          s.data <- get_interpolated_pdx_data(data = s.data)
          s.data$Volume <- s.data$Interpolated_Volume
        }

        tc_ratios <- T.C_ratio(s.data, last.measure.day = mainTC.study.day())

        p1 <- ggplotly(WaterfallPlot_Hybrid(tc_ratios))

        for (i in 1:length(p1$x$data)){
          if (!is.null(p1$x$data[[i]]$name)){
            p1$x$data[[i]]$name = gsub('^\\(|,\\d+\\)$', '', p1$x$data[[i]]$name)
          }
        }
        # remove '(*,1)' from the legends
        clean_pltly_legend(p1) %>% layout(legend = list(
                                          itemclick = FALSE,
                                          itemdoubleclick = FALSE,
                                          groupclick = FALSE
                                          )) # turn off the legend click function. 
      }
    })

  ## TGI plot

    ### Get calc day
    mainTGI.study.day <- reactive({
      return(input$main_TGI.day)
    })

    ### Generate the plot
    output$main_tgi <- renderPlotly({

      df <- get_query_tv()$"df"

      if (input$main_tgi_interpolate){
        df = get_interpolated_pdx_data(data = df)
        df$Volume <- df$Interpolated_Volume
      }

      tc_ratios <- T.C_ratio(df, last.measure.day = mainTGI.study.day())

      #plot_measure = c('TC.ratio', 'aov.TC.ratio')
      p1 <- ggplotly(plotTC.ratio(tc_ratios, plot_measure = 'aov.TC.ratio' ))
      # NOTE: aov.TC.ratio is used in manuscript.

      p1 %>% layout(legend = list(
              itemclick = FALSE,
              itemdoubleclick = FALSE,
              groupclick = FALSE
              )) 
    })

  ## TGI table

    ## Render table call
    output$main_tc_table <- DT::renderDataTable(
      main_tc_table()
    )

    ## Make the table
    main_tc_table <- reactive({

      df <- get_query_tv()$"df"

      if (input$main_tgi_interpolate){
          df = get_interpolated_pdx_data(data = df)
          df$Volume <- df$Interpolated_Volume
      }

      response_list = list()
      response_list <- T.C_ratio(df, last.measure.day = mainTGI.study.day())

      response_list <- response_list %>% dplyr::select(-mean.TVratio,	-var.TVratio,	-mean.dVt, -TC.ratio) %>%
                                        dplyr::select(Contributor, Study, Tumor, Arms, TC.CalcDay, n.TVratio, aov.TC.ratio, se_TC.ratio, Contrast.pValue) %>%
                                        dplyr::rename(pValue = Contrast.pValue)

      tab.df <- DT::datatable(response_list[order(response_list$Arms),],
                style = "bootstrap",
                escape = FALSE,
                filter = 'none',
                rownames= FALSE,
                class = "cell-border stripe",
                extensions = "Buttons",
                options = list(
                  dom = "Blrtip", scrollX = TRUE, ordering = F, autoWidth = TRUE, keys = TRUE, lengthMenu = list(c(5, 20, 50, -1), c('5', '20', '50', 'All')), pageLength = 20, paging = T,
                  buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Download")))) %>%
                formatRound(c('aov.TC.ratio', 'se_TC.ratio'), digits = 2) %>%
                formatSignif(c('pValue'), 2)
      tab.df
    })

  ## ORC stacked plot

    ### Get calc day
    mainORC.day <- reactive({
      return(input$main_orc.day)
    })

    ### Generate the plot
    output$main_orc_plot <- renderPlotly({

      s.data <- get_query_tv()$"df"

      if (is.null(s.data) | (nrow(s.data) == 0)) {
        plot_ly()
      } else {

        if (input$main_stackedorc_interpolate){
          s.data <- get_interpolated_pdx_data(data = s.data)
          s.data$Volume <- s.data$Interpolated_Volume
        }

        vc_change <- IndividualMouseResponse(s.data, last.measure.day = mainORC.day())

        p1 <- ggplotly(plotStackedORC(vc_change))

        p1 %>% layout(legend = list(
                      itemclick = FALSE,
                      itemdoubleclick = FALSE,
                      groupclick = FALSE
                      ))
      }
    })




##
  ## Individual study RECIST
    
    ### Get calculation day
    last.study.day <- reactive({
      return(input$tv_recist)
    })

    ### Generate the plot
    output$plot_tumorvol_study <- renderPlotly({
      df <- base::subset(
        get_query_tv()$"df", Study %in% c(input$tv_study_filtered)
      )

      df <- droplevels(df)

      if (input$tv_interpolate){
          df = get_interpolated_pdx_data(data = df)
          df$Volume <- df$Interpolated_Volume
      }

      study <- unlist(levels(factor(df$Study)))[1]

      filtered.df <- df %>%
        filter(Study == study) %>% droplevels()

      p1 <- study_volume_plot(filtered.df, position.dodge = 0.2, title = paste('Study:', study))

      p1 <- p1 + geom_vline(xintercept = last.study.day(), linetype="dashed",
                            color = "black", size=1.2)

      p1 <- p1 + xlab("Time (d)") + ylab("Tumor Volume (mm3)")

      p1

    })
    ### Get Table
    
    #### Table render call
    output$dt_ocr_cohort_table <- DT::renderDataTable(
      ocr_cohort_table()
    )

    #### Render the table
    ocr_cohort_table <- reactive({
      df <- base::subset(
        get_query_tv()$"df", Study %in% c(input$tv_study_filtered)
      )

      if (input$tv_interpolate){
          df = get_interpolated_pdx_data(data = df)
          df$Volume <- df$Interpolated_Volume
      }

      study <- unlist(levels(factor(df$Study)))[1]

      filtered.df <- df %>%
        filter(Study == study) %>% droplevels()

      response_list = list()
      response_list[[study]] <- get_response_level(filtered.df, last.measure.day = last.study.day())$Response.Level
      response_list[[study]]$Study <- study


      if ('Control'%in%unique(response_list[[study]]$Arms)){
        response_list[[study]]$Arms <- relevel(factor(response_list[[study]]$Arms), 'Control')
      }

      response_list[[study]]$Response.Level <- factor(response_list[[study]]$Response.Level)

      response_list[[study]] <- response_list[[study]] %>% dplyr::select(-Study)

      tab.df <- DT::datatable(response_list[[study]][order(response_list[[study]]$Arms),],
                style = "bootstrap",
                escape = FALSE,
                filter = list(position = "top", clear = T),
                rownames= FALSE,
                class = "cell-border stripe",
                extensions = "Buttons",
                options = list(
                  dom = "Blrtip", scrollX = TRUE, ordering = F, autoWidth = TRUE, keys = TRUE, lengthMenu = list(c(5, 20, 50, -1), c('5', '20', '50', 'All')), pageLength = 20, paging = T,
                  buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Download")))) %>%
                formatRound(c('Best.Response', 'Avg.Response'), digits = 2)
      tab.df
    })


  ## Individual study waterfall plot
    ### Get waterfall metric to use
    waterfall_metric <- reactive({
      return(input$waterfall_metric)
    })

    ### Get waterfall metric, this is used as a trigger to update the UI with waterfall options when needed. 
    observeEvent(input$waterfall_metric, {
      updateTabsetPanel(inputId = "waterfall_options", selected = input$waterfall_metric)
    }) 

    ### Get calculation day
    AUC.study.day <- reactive({
      return(input$tv_AUC.day.waterfall)
    })

    #### Make the plot
    output$tv_plot_waterfall <- renderPlotly({

      df <- base::subset(
        get_query_tv()$"df", Study %in% c(input$tv_study_filtered)
      )
      
      df <- droplevels(df)

      study <- unlist(levels(factor(df$Study)))[1]

      if (waterfall_metric() == 'AUC.Filtered.Measures') {
          shinyjs::enable("tv_AUC.day.waterfall")
      } else {
            # Turn off Scaled Plot I/Os
            shinyjs::disable("tv_AUC.day.waterfall")
      }

      filtered.df <- df %>%
        filter(Study == study) %>% droplevels()

      if (input$tv_waterfall_interpolate){
        filtered.df = get_interpolated_pdx_data(data = filtered.df)
        filtered.df$Volume <- filtered.df$Interpolated_Volume
      }

      resp <- IndividualMouseResponse(filtered.df, last.measure.day = AUC.study.day())

      p1 <- ggplotly(WaterfallPlot_PDX(resp, waterfall_metric()))

      for (i in 1:length(p1$x$data)){
        if (!is.null(p1$x$data[[i]]$name)){
          p1$x$data[[i]]$name = gsub('^\\(|,\\d+\\)$', '', p1$x$data[[i]]$name)
        }
      }
      # remove '(*,1)' from the legends

      for (i in 1:length(p1$x$data)){
        p1$x$data[[i]]$text <- c(p1$x$data[[i]]$text, "") 
      }

      clean_pltly_legend(p1) %>% layout(legend = list(
                                          itemclick = FALSE,
                                          itemdoubleclick = FALSE,
                                          groupclick = FALSE
                                          ))

    })

  ## Individual study EFS

    ### Get the calculation threshold
    PercChange_EventSize <- reactive({
      return(input$tv_PercChange_EventSize)
    })

    ### Generate the plot
    output$tv_plot_EFS <- renderPlotly({

      df <- base::subset(
        get_query_tv()$"df", Study %in% c(input$tv_study_filtered)
      )
      
      df <- droplevels(df)

      study <- unlist(levels(factor(df$Study)))[1]

      filtered.df <- df %>%
        filter(Study == study) %>% droplevels()

      p1 <- EFSplot(filtered.df, PercChange_EventSize())

      s <- subplot(p1[[1]], p1[[2]], heights = c(0.75, 0.25), margin = 0.05, nrows=2, shareX = T, titleY = T)

      for(i in 1:length(s$x$data)) {
        if (i <= length(levels(as.factor(p1$data.survtable$strata)))) {
          s$x$data[[i]]$showlegend <- TRUE
        } else {
          s$x$data[[i]]$showlegend <- FALSE
        }
      }
      for (i in 1:length(s$x$data)){
        if (!is.null(s$x$data[[i]]$name)){
          s$x$data[[i]]$name = gsub('^\\(|,\\d+\\)$', '', s$x$data[[i]]$name)
        }
      }
      # remove '(*,1)' from the legends

      s

    })

  ## Individual study ANOVA
    ### Get calculation day
    anova_measure_day <- reactive({
      return(input$anova_Measure_Day)
    })
    ### Get ANOVA table
    output$dt_anova_table <- DT::renderDataTable(
      response_analysis(get_query_tv()$"df", method = 'endpoint.ANOVA', last.measure.day = anova_measure_day(), multi_test_anova = FALSE, tv_study_filtered = input$tv_study_filtered, tv_tumor_filtered = input$tv_tumor_filtered, main_anova_interpolate = input$main_anova_interpolate)
    )

    ### Get Tukey table
    output$dt_tukey_table <- DT::renderDataTable(
      response_analysis(get_query_tv()$"df", method = 'endpoint.ANOVA', last.measure.day = anova_measure_day(), multi_test_anova = TRUE, tv_study_filtered = input$tv_study_filtered, tv_tumor_filtered = input$tv_tumor_filtered, main_anova_interpolate = input$main_anova_interpolate)
    )

##
  ## Body weight plot
    output$plot_mouseweight <- renderPlotly({
      # Data
      s.data <- get_query_tv()$"df"

      if (is.null(s.data) | (nrow(s.data) == 0)) {
        plot_ly()
      } else {

      #   # Get Level Type Input by User
        if (input$tv_weight_plot_type == "Treatment Plot") {
          pattern_type <- "Treatment"
        } else if (input$tv_weight_plot_type == "Study Plot") {
          pattern_type <- "Study"
        }

        if (input$tv_weight_plot_style == "Study Average") {
          level_type <- "Arm"
        } else if (input$tv_weight_plot_style == "Individual Animal") {
          level_type <- "Animal"
        }
        
        if (input$tv_weight_plotType == 'Percent Change'){
            s.data <- s.data %>%
              dplyr::arrange(Study, Tumor, Arms, ID, Times) %>%
              dplyr::group_by(Study, Tumor, Arms, ID) %>%
              dplyr::mutate(dWeight = (((Body_Weight - Body_Weight[1]) / Body_Weight[1] ) * 100))

            s.data$Body_Weight <- s.data$dWeight
        }
      #   # Call plot
        get_weight_plot(data = s.data, level = level_type, pattern = pattern_type, position.dodge = 0.2, tv_weight_plotType = input$tv_weight_plotType)
        
      }
    })


## example empty plot / placeholder
  # output$generic_plot_start <- renderPlotly({

  #   temp <- data.frame(X = c(1,2,3,4), Y = c(2,3,4,5))
  #   p <- ggplot(temp, aes(x=X, y=Y)) + geom_point(size=3)
  #   return(p)

  # })

##

## Current Data Tab
  output$tbl_tv_all <- DT::renderDataTable(
    get_query_tv()$"df",
    style = "bootstrap",
    escape = FALSE,
    filter = list(position = "top", clear = T),
    class = "cell-border stripe",
    extensions = "Buttons",
    options = list(
      dom = "Bflrtip", scrollX = TRUE, autoWidth = TRUE, keys = TRUE, pageLength = 5, lengthMenu = list(c(5, 20, 50, 100, 500, -1), c('5', '20', '50', '100','500', 'All')),paging = T,
      buttons = list(I("colvis"), list(extend = 'copy', title = NULL), list(extend = "collection", buttons = list('csv', list(extend = 'excel', title = NULL)), title = NULL, text = "Download"))
    )
  )
  # Render selected data for "Current Data Table" tab. 


## landing page buttons
observeEvent(input$btn_nav_val, updateNavlistPanel(session, "nav_bco", selected = title_validate))
observeEvent(input$btn_nav_tv, updateNavlistPanel(session, "nav_bco", selected = title_tumor_volume))
observeEvent(input$btn_nav_help, updateNavlistPanel(session, "nav_bco", selected = title_help))
observeEvent(input$btn_nav_about, updateNavlistPanel(session, "nav_bco", selected = title_about))

  output$sessionInfo <- renderPrint({
     capture.output(sessionInfo())
  })

