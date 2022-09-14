# DATA

data <- try(as.data.frame(readRDS("include/tv-test.rds")), silent = T)
rownames(data) <- NULL

# GET methods
get_tv_contributor <- function() {
  if (length(unique(data$Contributor)) < 1) {
    unique(data$Contributor)
  } else {
    sort(unique(data$Contributor), na.last = T,)
  }
}

get_tv_treatment <- function() {
  if (length(unique(data$Arms)) < 2) {
    unique(data$Arms)
  } else {
    agents <- factor(unique(data$Arms))
    agents <- relevel(agents, 'Control')
    levels(agents)
  }
}

get_tv_disease <- function() {
  if (length(unique(data$Disease_Type)) < 2) {
    list(unique(data$Disease_Type))
  } else {
    sort(unique(data$Disease_Type), na.last = T)
  }
}

get_tv_study <- function() {
  if (length(unique(data$Study)) < 2) {
    unique(sort(data$Study))
  } else {
    unique(sort(data$Study, na.last = T))
  }
}

# CARDS
get_tv_contributor_count <- function(df) ifelse(is.null(df), 0, length(unique(df$"Contributor")))

output$card_tv_contributor <- shinydashboard::renderValueBox({
  ret <- get_query_tv()$"df"
  shinydashboard::valueBox(
    get_tv_contributor_count(ret), "Contributors",
    icon = icon("hospital"), color = "light-blue"
  )
})

get_tv_volumes_count <- function(df) ifelse(is.null(df), 0, length(unique(df$"ID")))

output$card_tv_volumes <- shinydashboard::renderValueBox({
  ret <- get_query_tv()$"df"
  shinydashboard::valueBox(
    get_tv_volumes_count(ret), "Unique Mouse IDs",
    icon = icon("file-medical"), color = "green"
  )
})

get_tv_studies_count <- function(df) ifelse(is.null(df), 0, length(unique(df$"Study")))

output$card_tv_studies <- shinydashboard::renderValueBox({
  ret <- get_query_tv()$"df"
  shinydashboard::valueBox(
    get_tv_studies_count(ret), "Studies",
    icon = icon("chart-area"), color = "yellow"
  )
})

get_tv_models_count <- function(df) ifelse(is.null(df), 0, length(unique(df$"Tumor")))

output$card_tv_models <- shinydashboard::renderValueBox({
  ret <- get_query_tv()$"df"
  shinydashboard::valueBox(
    get_tv_models_count(ret), "Models",
    icon = icon("paw"), color = "blue"
  )
})

get_tv_disease_count <- function(df) ifelse(is.null(df), 0, length(unique(df$"Disease_Type")))

output$card_tv_disease <- shinydashboard::renderValueBox({
  ret <- get_query_tv()$"df"
  shinydashboard::valueBox(
    get_tv_disease_count(ret), "Disease Types",
    icon = icon("disease"), color = "red"
  )
})

get_tv_treatment_count <- function(df) ifelse(is.null(df), 0, length(unique(df$"Arms")))

output$card_tv_treatment <- shinydashboard::renderValueBox({
  ret <- get_query_tv()$"df"
  shinydashboard::valueBox(
    get_tv_treatment_count(ret), "Treatment Arms",
    icon = icon("pills"), color = "maroon"
  )
})

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

# QUERY

values <- reactiveValues(
  upload_state = "default"
)

check_state <- reactiveValues(
  loaded_data = FALSE
)

# Two states are needed to track if data is uploaded, and if data in general is loaded. 
# The first is to check what data to load to the app, the second is to allow for checks to be done on selected items
# Without checking if any data is loaded, the app will display invalid messages to the splash page on instantiation of the app. 

observeEvent(input$user_tv_data, {
  values$upload_state <- 'uploaded'
})

observeEvent(input$user_tv_load_default_btn, {
  values$upload_state <- 'reset'
  output$tv_text_upload <- renderText({
    paste0("Example Tumor Volume Data Still Active!")
  })
})

get_data <- reactive({

    curr_data <- data

    if (values$upload_state == 'uploaded') {
      input_file_user <- input$user_tv_data
      if (is.null(input_file_user) | flag_user_data$flag != 1) {
          curr_data <- data
      }else{
          curr_data <- read.csv(input_file_user$datapath, header = TRUE)
      }


    n_unique_arms <- length(unique(curr_data$Arms))

    agents <- factor(unique(curr_data$Arms))
    agents <- levels(relevel(agents, 'Control'))

    n_studies <- length(unique(sort(curr_data$Study)))

    updatePickerInput(session, "tv_contributor",
                      choices = unique(sort(curr_data$Contributor)),
                      selected = unique(sort(curr_data$Contributor))[1])

    updatePickerInput(session, "tv_treatment",
                  choices = agents,
                  selected = c('Control', unique(curr_data$Arms)[1:min(3,n_unique_arms)]))
    # Note: the pick lists are updated in two places. here, and in the above 'get_tv_treatment' function. 

    updatePickerInput(session, "tv_disease_type",
                  choices = unique(sort(curr_data$Disease_Type)),
                  selected = unique(sort(curr_data$Disease_Type))[1])

    updatePickerInput(session, "tv_study",
                  choices = unique(sort(curr_data$Study)),
                  selected = unique(sort(curr_data$Study))[1:n_studies])

    updatePickerInput(session, inputId = "tv_study_filtered",
        choices = unique(sort(curr_data$Study)))

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

    check_state$loaded_data = TRUE

    return(curr_data)
})

  observeEvent(input$tv_study, {
    choices <- input$tv_study

    updatePickerInput(
      session,
      inputId = "tv_study_filtered",
      choices = choices
    )

  })
  # Watch the selected study list, and pass that list to the study pick list for the study specific metrics. 

  observeEvent(input$tv_treatment, {
    if (check_state$loaded_data) {
      if( !('Control' %in% input$tv_treatment)) {
        showNotification(id = 'missing_control', "You must select 'Control' in the treatment arm list", type = c("error"), duration = NULL, closeButton = FALSE)
      } else {
        removeNotification(id = 'missing_control')
      }
    }
  },ignoreNULL = F)
  # Watch to see if 'Control' is or is not selected in treatment Arms. Notify user that 'Control' must be selected. 

get_query_tv <- reactive( {

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

  df_query
})
# This function controls the data selection from the filters. The function `query_tv` subsets the data based on selected pick lists.  
# Notifications will report out if user has deselected entire lists. 

# DATA TABLE
# query_all_submit <- reactiveValues(counter = 0L)
#
# observeEvent(input$tv_submit_query, {
#   query_all_submit$counter <- input$tv_submit_query
# })
#
# output$tbl_msg_all <- renderText(
#   if (query_all_submit$counter == 0L) {
#     "Please submit the query."
#   } else {
#     get_query_tv()$"msg"
#   }
# )
# Hold over code from prior 'query' button implementation. 


output$tbl_tv_all <- DT::renderDataTable(
  get_query_tv()$"df",
  style = "bootstrap",
  escape = FALSE,
  filter = list(position = "top", clear = T),
  class = "cell-border stripe",
  extensions = "Buttons",
  options = list(
    dom = "Bflrtip", scrollX = TRUE, autoWidth = TRUE, keys = TRUE, pageLength = 5, lengthMenu = list(c(5, 20, 50, 100, 500, -1), c('5', '20', '50', '100','500', 'All')),paging = T,
    buttons = list(I("colvis"), "copy", "print", list(extend = "collection", buttons = c("csv", "excel"), text = "Download"))
  )
)
# Render selected data for "Current Data Table" tab. 


# PLOT

# PLOT - ALL
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


observeEvent(input$tv_all_plotType, {
  updateTabsetPanel(inputId = "scale_options", selected = input$tv_all_plotType)
}) 


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
    if (input$tv_all_plotType == 'Semi-Log'){
        s.data$Volume <- log(s.data$Volume)

      # Adjust the data to percent change the data if asked for
    } else if (input$tv_all_plotType == 'Percent Change'){
        s.data <- s.data %>%
          dplyr::arrange(Study, Tumor, Arms, ID, Times) %>%
          dplyr::group_by(Study, Tumor, Arms, ID) %>%
          dplyr::mutate(dVt = (((Volume - Volume[1]) / Volume[1] ) * 100))
        s.data$Volume <- s.data$dVt
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

      get_plot_scaled(data = s.data, position.dodge = 0.5,  title = NULL, scale.factor = endpoint_scale_all, scale.by.volume = scale_by_volume_all, level = level_type, pattern = pattern_type)

    } else {

      # Reset Info Text of the Scaled Plot
      output$tv_all_text_scaled <- renderText({""})

      get_tv_plot(data = s.data, level = level_type, pattern = pattern_type, position.dodge = 0.99)
    }
  }
})

# PLOT - Study

last.study.day <- reactive({
  return(input$tv_recist)
})

avgplot.study.day <- reactive({
  return(input$main_avgplot.day)
})

AUC.study.day <- reactive({
  return(input$tv_AUC.day.waterfall)
})

mainORC.day <- reactive({
  return(input$main_orc.day)
})

mainTC.study.day <- reactive({
  return(input$main_TC.day)
})

TC.study.day <- reactive({
  return(input$tv_TC.day)
})

mainTGI.study.day <- reactive({
  return(input$main_TGI.day)
})

anova_measure_day <- reactive({
  return(input$anova_Measure_Day)
})

PercChange_EventSize <- reactive({
  return(input$tv_PercChange_EventSize)
})

waterfall_metric <- reactive({
  return(input$waterfall_metric)
})

output$plot_tumorvol_study <- renderPlotly({

  df <- base::subset(
    get_query_tv()$"df", Study %in% c(input$tv_study_filtered)
  )
  
  if (input$tv_interpolate){
      df = get_interpolated_pdx_data(data = df)
      df$Volume <- df$Interpolated_Volume
  }

  study <- unlist(levels(factor(df$Study)))[1]

  one_A_N_DRAP <- df %>%
    filter(Study == study) %>% droplevels()

  p1 <- study_volume_plot(one_A_N_DRAP, level = 'Arm',
                                position.dodge = 0.2,
                                title = paste('Study:', study), plot_on = FALSE )

  p1 <- p1 + geom_vline(xintercept = last.study.day(), linetype="dashed",
                        color = "black", size=1.2)


  p1 <- p1 + xlab("Time (d)") + ylab("Tumor Volume (mm3)")

  p1

})

output$dt_dr_table <- DT::renderDataTable(
  dr_table()
)

dr_table <- reactive({
  
  df <- base::subset(
    get_query_tv()$"df", Study %in% c(input$tv_study_filtered)
  )
  
  if (input$tv_interpolate){
      df = get_interpolated_pdx_data(data = df)
      df$Volume <- df$Interpolated_Volume
  }

  study <- unlist(levels(factor(df$Study)))[1]

  one_A_N_DRAP <- df %>%
    filter(Study == study) %>% droplevels()

  response_list = list()
  response_list[[study]] <- get_DRLevel(one_A_N_DRAP, neg.control = 'Control', last.measure.day = last.study.day())$Response.Level
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

output$tv_plot_EFS <- renderPlotly({

  df <- base::subset(
    get_query_tv()$"df", Study %in% c(input$tv_study_filtered)
  )
  
  study <- unlist(levels(factor(df$Study)))[1]

  one_A_N_DRAP <- df %>%
    filter(Study == study) %>% droplevels()

  p1 <- EFSplot(one_A_N_DRAP, PercChange_EventSize())

  s <- subplot(p1[[1]], p1[[2]], heights = c(0.75, 0.25), margin = 0.05, nrows=2, shareX = T, titleY = T)

  for(i in 1:length(s$x$data)) {
    if (i <= length(levels(as.factor(p1$data.survtable$Arms)))) {
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

output$tv_plot_waterfall <- renderPlotly({

  df <- base::subset(
    get_query_tv()$"df", Study %in% c(input$tv_study_filtered)
  )
  
  study <- unlist(levels(factor(df$Study)))[1]

  if (waterfall_metric() == 'AUC.Filtered.Measures') {
      shinyjs::enable("tv_AUC.day.waterfall")
  } else {
        # Turn off Scaled Plot I/Os
        shinyjs::disable("tv_AUC.day.waterfall")
  }
  one_A_N_DRAP <- df %>%
    filter(Study == study) %>% droplevels()

  if (input$tv_waterfall_interpolate){
    one_A_N_DRAP = get_interpolated_pdx_data(data = one_A_N_DRAP)
    one_A_N_DRAP$Volume <- one_A_N_DRAP$Interpolated_Volume
  }

  resp <- IndividualMouseReponse(one_A_N_DRAP, last.measure.day = AUC.study.day())

  p1 <- ggplotly(WaterfallPlot_PDX(resp, waterfall_metric()))

  for (i in 1:length(p1$x$data)){
    if (!is.null(p1$x$data[[i]]$name)){
      p1$x$data[[i]]$name = gsub('^\\(|,\\d+\\)$', '', p1$x$data[[i]]$name)
    }
  }
  # remove '(*,1)' from the legends

  p1

})

output$tv_plot_tc <- renderPlotly({

  df <- base::subset(
    get_query_tv()$"df", Study %in% c(input$tv_study_filtered)
  )
  
  study <- unlist(levels(factor(df$Study)))[1]

  one_A_N_DRAP <- df %>%
    filter(Study == study) %>% droplevels()

  if (input$tv_TC_interpolate){
    one_A_N_DRAP = get_interpolated_pdx_data(data = one_A_N_DRAP)
    one_A_N_DRAP$Volume <- one_A_N_DRAP$Interpolated_Volume
  }

  tc_ratios <- T.C_ratio(one_A_N_DRAP, last.measure.day = TC.study.day())

  #plot_measure = c('TC.ratio', 'aov.TC.ratio')

  plotTC.ratio(tc_ratios, plot_measure = 'aov.TC.ratio' )
  # NOTE: aov.TC.ratio is used in manuscript.
})

output$dt_tc_table <- DT::renderDataTable(
  tc_table()
)

tc_table <- reactive({

  df <- base::subset(
    get_query_tv()$"df", Study %in% c(input$tv_study_filtered)
  )

  if (input$tv_TC_interpolate){
      df = get_interpolated_pdx_data(data = df)
      df$Volume <- df$Interpolated_Volume
  }

  study <- unlist(levels(factor(df$Study)))[1]

  one_A_N_DRAP <- df %>%
    filter(Study == study) %>% droplevels()

  response_list = list()
  response_list <- T.C_ratio(one_A_N_DRAP, last.measure.day = TC.study.day())

  # response_list[[study]]$Response.Level <- factor(response_list[[study]]$Response.Level)

  response_list <- response_list %>% dplyr::select(-Tumor, -mean.TVratio,	-var.TVratio,	-mean.dVt, -TC.ratio) %>%
                                     dplyr::select(Arms, TC.CalcDay, n.TVratio, aov.TC.ratio, se_TC.ratio, Contrast.pValue) %>%
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


output$main_tc_table <- DT::renderDataTable(
  main_tc_table()
)



output$main_tgi <- renderPlotly({

  df <- get_query_tv()$"df"

  if (input$main_tgi_interpolate){
    df = get_interpolated_pdx_data(data = df)
    df$Volume <- df$Interpolated_Volume
  }

  tc_ratios <- T.C_ratio(df, last.measure.day = mainTGI.study.day())

  #plot_measure = c('TC.ratio', 'aov.TC.ratio')

  plotTC.ratio(tc_ratios, plot_measure = 'aov.TC.ratio' )
  # NOTE: aov.TC.ratio is used in manuscript.
})

main_tc_table <- reactive({

  df <- get_query_tv()$"df"

  if (input$main_tgi_interpolate){
      df = get_interpolated_pdx_data(data = df)
      df$Volume <- df$Interpolated_Volume
  }

  response_list = list()
  response_list <- T.C_ratio(df, last.measure.day = mainTGI.study.day())

  # response_list[[study]]$Response.Level <- factor(response_list[[study]]$Response.Level)

  response_list <- response_list %>% dplyr::select(-Tumor, -mean.TVratio,	-var.TVratio,	-mean.dVt, -TC.ratio) %>%
                                     dplyr::select(Arms, TC.CalcDay, n.TVratio, aov.TC.ratio, se_TC.ratio, Contrast.pValue) %>%
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

output$log2_foldchange <- renderPlotly({

  s.data <- get_query_tv()$"df"

  if (is.null(s.data) | (nrow(s.data) == 0)) {
    plot_ly()
  } else {

    if (input$main_log2_interpolate){
      s.data <- get_interpolated_pdx_data(data = s.data)
      s.data$Volume <- s.data$Interpolated_Volume
    }
    vc_change <- IndividualMouseReponse(s.data)

    log2FoldPlot(vc_change, caption_text_on = F)

  }

})



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

    plotAvgGrowthBar(tc_ratios)

  }

})


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

    p1

  }

})



output$main_orc_plot <- renderPlotly({

  s.data <- get_query_tv()$"df"

  if (is.null(s.data) | (nrow(s.data) == 0)) {
    plot_ly()
  } else {

    if (input$main_stackedorc_interpolate){
      s.data <- get_interpolated_pdx_data(data = s.data)
      s.data$Volume <- s.data$Interpolated_Volume
    }

    vc_change <- IndividualMouseReponse(s.data, last.measure.day = mainORC.day())

    plotStackedORC(vc_change)

  }

})


response_analysis <- function(method=c('endpoint.ANOVA','endpoint.KW','mixed.ANOVA','LMM'), last.measure.day = NULL, multi_test_anova = FALSE) {

  ## NOTE: 'Volume' is used here, but dVt could potentially be used.

  df <- base::subset(
    get_query_tv()$"df", Study %in% c(input$tv_study_filtered)
  )

  study <- unlist(levels(factor(df$Study)))[1]

  df <- df %>%
    filter(Study == study) %>% droplevels()

  if(inherits(df, "data.frame")){
    df<-as.data.frame(df)
  }

  if (input$main_anova_interpolate){
    df <- get_interpolated_pdx_data(data = df)
    df$Volume <- df$Interpolated_Volume
  }

  if (!is.null(last.measure.day)) {
    end_day_index = match(last.measure.day,df$Times)
    if (is.na(end_day_index)) {
      end_day_index = which.min(abs(df$Times - last.measure.day))
    }
    df <- subset(df, Times <= df$Times[end_day_index])
  }

  df$Arms <- relevel(as.factor(df$Arms), 'Control')

  Volume <- df[,'Volume']

  df <- subset(df,Volume != 0)

  #get endpoint data
  endpoint.data <- data[df$Times == max(df$Times),]
  endpoint.data <- endpoint.data[,c('Arms','Volume')]
  endpoint.data$Arms=factor(endpoint.data$Arms)

  #get mean growth rate
  ID <- unique(as.character(data$ID))


    dra.res <- switch (method,
                        endpoint.ANOVA = summary(aov(Volume ~ Arms, data = endpoint.data)),
                        endpoint.KW    = kruskal.test(Volume ~ Arms, data = endpoint.data),
                        mixed.ANOVA    = summary(aov(Volume ~ Arms + Error(ID/Times), data = df)),
                        LMM            = summary(lme(Volume ~ Arms, random = ~1|ID/Times, data = df))[[20]]
    )

    dra.res.full <- switch (method,
                        endpoint.ANOVA = aov(Volume ~ Arms, data = endpoint.data),
                        endpoint.KW    = kruskal.test(Volume ~ Arms, data = endpoint.data),
                        mixed.ANOVA    = aov(Volume ~ Arms + Error(ID/Times), data = df),
                        LMM            = lme(Volume ~ Arms, random = ~1|ID/Times, data = df)
    )

    multiple_comp_test <- TukeyHSD(dra.res.full)
  if (!multi_test_anova) {
    tab.df <- DT::datatable(dra.res[[1]],
              style = "bootstrap",
              escape = FALSE,
              filter = 'none',
              rownames= TRUE,
              class = "cell-border stripe",
              extensions = "Buttons",
              options = list(
                dom = "Blrti", info = FALSE, scrollX = TRUE, ordering = F, autoWidth = TRUE, keys = TRUE, pageLength = 20, paging = F,
                buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Download")))) %>%
              formatSignif(c('Sum Sq', 'Mean Sq', 'F value', 'Pr(>F)'), 4)
    return(tab.df)
  } else {

    mct <- as.data.frame(multiple_comp_test[['Arms']])
    mct$'diff' <- round(mct$'diff', digits=2)
    mct$'lwr' <- round(mct$'lwr', digits=2)
    mct$'upr' <- round(mct$'upr', digits=2)
    mct$'p adj' <- round(mct$'p adj', digits=4)


    tab.df <- DT::datatable(mct,
              style = "bootstrap",
              escape = FALSE,
              filter = list(position = "top", clear = T),
              rownames= TRUE,
              class = "cell-border stripe",
              extensions = "Buttons",
              options = list(
                dom = "Blrtip", scrollX = TRUE, ordering = F, autoWidth = TRUE, keys = TRUE, lengthMenu = list(c(5, 20, 50, -1), c('5', '20', '50', 'All')), pageLength = 20, paging = T,
                buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Download"))))
    return(tab.df)
  }
}

output$dt_anova_table <- DT::renderDataTable(
  response_analysis(method = 'endpoint.ANOVA', last.measure.day = anova_measure_day(), multi_test_anova = FALSE)
)

output$dt_tukey_table <- DT::renderDataTable(
  response_analysis(method = 'endpoint.ANOVA', last.measure.day = anova_measure_day(), multi_test_anova = TRUE)
)

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
    get_weight_plot(data = s.data, level = level_type, pattern = pattern_type, position.dodge = 0.99)
    
  }
})

output$user_tv_download_default_btn <- downloadHandler(
  filename = function() {
    paste("tv_suite-sample-input.csv", sep="")
  },
  content = function(file) {
    write.csv(get_data(), file)
  })


# output$generic_plot_start <- renderPlotly({

#   temp <- data.frame(X = c(1,2,3,4), Y = c(2,3,4,5))
#   p <- ggplot(temp, aes(x=X, y=Y)) + geom_point(size=3)
#   return(p)

# })



# landing page buttons
observeEvent(input$btn_nav_val, updateNavlistPanel(session, "nav_bco", selected = title_validate))
observeEvent(input$btn_nav_tv, updateNavlistPanel(session, "nav_bco", selected = title_tumor_volume))
observeEvent(input$btn_nav_help, updateNavlistPanel(session, "nav_bco", selected = title_help))