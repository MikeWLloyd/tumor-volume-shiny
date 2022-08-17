# DATA

data <- try(as.data.frame(readRDS("include/tv-test-new.rds")), silent = T)
rownames(data) <- NULL

# NOTE: This will likely need to be set as a reactive variable: 
#/   diamond <- reactiveValues( df=NULL )
  
#  ###Initial setting
#  observe({diamond$df <- diamonds})
#  
#  observeEvent(input$read, {
#      df1 <- diamonds
#      ...
#/#
# I tried a few different things to get this worked but couldn't figure it out. Note that the 'upload' button may have the same issues with breaking things as it did on the validate page, and might not be needed. 


    

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
    unique(data$Study)
  } else {
    sort(unique(data$Study), na.last = T)
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
    get_tv_treatment_count(ret), "Treatments",
    icon = icon("pills"), color = "maroon"
  )
})

# UPLOAD
output$tv_text_upload <- renderText({
  paste0("Example Tumor Volume Data is Preloaded. Select Your Tumor Volume File and Click Upload Button!")
})

observeEvent(input$user_tv_upload, {
  output$tv_text_upload <- renderText({
    paste0("Data Upload is Unsuccessful. Please check the Validation Tab for Input Data Conversion to the Expected Format!")
  })
})

observeEvent(input$user_tv_upload_default, {
  output$tv_text_upload <- renderText({
    paste0("Example Tumor Volume Data is Preloaded. Select Your Tumor Volume File and Click Upload Button!")
  })
})


# QUERY

get_data <- reactive({

    curr_data <- data

    if (input$add_user_tv_btn) {
      input_file_user <- input$user_tv_data
      if (is.null(input_file_user)) {
        curr_data <- data
      }else{
        print("In Upload Button")
        curr_data <- read.csv(input_file_user$datapath, header = TRUE)
      }
    }

    if (input$user_tv_load_default_btn) {
      print("In Load Button")
      curr_data <- data
    }


    n_unique_arms <- length(unique(curr_data$Arms))
    updatePickerInput(session, "tv_contributor", 
                      choices = unique(curr_data$Contributor), 
                      selected = unique(curr_data$Contributor)[1])

    updatePickerInput(session, "tv_treatment", 
                  choices = unique(curr_data$Arms), 
                  selected = unique(curr_data$Arms)[1:min(3,n_unique_arms)])
    
    updatePickerInput(session, "tv_disease_type", 
                  choices = unique(curr_data$Disease_Type), 
                  selected = unique(curr_data$Disease_Type)[1])

    updatePickerInput(session, "tv_study_picker", 
                  choices = unique(curr_data$Study), 
                  selected = unique(curr_data$Study)[1])

    return(curr_data)
})

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

# DATA TABLE
query_all_submit <- reactiveValues(counter = 0L)

observeEvent(input$tv_submit_query, {
  query_all_submit$counter <- input$tv_submit_query
})

output$tbl_msg_all <- renderText(
  if (query_all_submit$counter == 0L) {
    "Please submit the query."
  } else {
    get_query_tv()$"msg"
  }
)

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

    #semi-log the data if asked for. 
    

    if (input$tv_all_semi.log){
        s.data$Volume <- log(s.data$Volume)
        updateCheckboxInput(session, 'tv_all_scale', value = FALSE)
        shinyjs::disable("tv_all_scale")
        #input$ = FALSE
    } else {
      shinyjs::enable("tv_all_scale")
    }
    

    # Call plot
    if (input$tv_all_scale) {

      shinyjs::enable("tv_div_all_endpoint")
      shinyjs::enable("tv_div_all_scale_picker")

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
      # Turn off Scaled Plot I/Os
      shinyjs::disable("tv_div_all_endpoint")
      shinyjs::disable("tv_div_all_scale_picker")

      # Reset Info Text of the Scaled Plot
      output$tv_all_text_scaled <- renderText({""})

      get_tv_plot(data = s.data, level = level_type, pattern = pattern_type, position.dodge = 0.99) #MWL: THIS IS FIXED!
    }
  }
})

# PLOT - Study

# Get Data for Study Default
gen_data_filter_study <- eventReactive(c(input$tv_study_picker, input$tv_submit_query), {
  data_study <- base::subset(
    get_query_tv()$"df", Study %in% c(input$tv_study_picker)
  )
  data_study
}, ignoreNULL = F)

last.study.day <- reactive({
  return(input$tv_recist)
})

AUC.study.day <- reactive({
  return(input$tv_AUC.day.waterfall)
})

TCmain.study.day <- reactive({
  return(input$main_TC.day)
})

TC.study.day <- reactive({
  return(input$tv_TC.day)
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

  if (input$tv_interpolate){

    # Data

    Data_Response <- gen_data_filter_study()

    #print(Data_Response)

    study <- unlist(levels(factor(Data_Response$Study)))[1]

    one_A_N_DRAP <- Data_Response %>%
      filter(Study == study) %>% droplevels()


    p1 <- get_plot_interpolated(data = get_interpolated_pdx_data(data = one_A_N_DRAP), position.dodge = 0.5,  title = study)

    p1 <- p1 + geom_vline(xintercept = last.study.day(), linetype="dashed",
                          color = "black", size=1.2)

    return(p1)

  } else {

    df <- gen_data_filter_study()

    study <- unlist(levels(factor(df$Study)))[1]

    one_A_N_DRAP <- df %>%
      filter(Study == study) %>% droplevels()

    p1 <- get_plot_volumeGC_alt(one_A_N_DRAP, level = 'Arm',
                                position.dodge = 0.2,
                                title = paste('Study:', study), plot_on = FALSE )

    p1 <- p1 + geom_vline(xintercept = last.study.day(), linetype="dashed",
                          color = "black", size=1.2)


    p1 <- p1 + xlab("Time (d)") + ylab("Tumor Volume (mm3)")

    return(p1)

  }
})

output$dt_dr_table <- DT::renderDataTable(
  dr_table()
)

dr_table <- reactive({

  df <- gen_data_filter_study()

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

  df <- gen_data_filter_study()
  study <- unlist(levels(factor(df$Study)))[1]

  one_A_N_DRAP <- df %>%
    filter(Study == study) %>% droplevels()

  p1 <- EFSplot(one_A_N_DRAP, PercChange_EventSize())

  s <- subplot(p1[[1]], p1[[2]], heights = c(0.75, 0.25), margin = 0.05, nrows=2, shareX = T, titleY = T)

  for(i in 1:length(s$x$data)) {
    if (i <= length(levels(as.factor(p1$data.survtable$Arms)))) {
      s$x$data[[i]]$showlegend <- TRUE
      #s$x$data[[i]]$name <- levels(relevel(as.factor(p1$data.survtable$Arms), 'Control')) [[i]]
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

  df <- gen_data_filter_study()
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

  df <- gen_data_filter_study()
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

  df <- gen_data_filter_study()

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

output$hybrid_waterfall <- renderPlotly({


  s.data <- get_query_tv()$"df"

  if (is.null(s.data) | (nrow(s.data) == 0)) {
    plot_ly()
  } else {
    
    if (input$main_tc_interpolate){
      s.data <- get_interpolated_pdx_data(data = s.data)
      s.data$Volume <- s.data$Interpolated_Volume
    }
    tc_ratios <- T.C_ratio(s.data, last.measure.day = TCmain.study.day())



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


response_analysis <- function(method=c('endpoint.ANOVA','endpoint.KW','mixed.ANOVA','LMM'), last.measure.day = NULL, multi_test_anova = FALSE) {
  
  ## NOTE: 'Volume' is used here, but dVt could potentially be used. 
  
  data <- get_query_tv()$"df"

  if(inherits(data, "data.frame")){
    data<-as.data.frame(data)
  }
  
  if (input$main_anova_interpolate){
    data <- get_interpolated_pdx_data(data = data)
    data$Volume <- data$Interpolated_Volume
  }

  if (!is.null(last.measure.day)) {
    end_day_index = match(last.measure.day,data$Times)
    if (is.na(end_day_index)) {
      end_day_index = which.min(abs(data$Times - last.measure.day))
    }   
    data <- subset(data, Times <= data$Times[end_day_index])
  }
  
  data$Arms <- relevel(as.factor(data$Arms), 'Control')
  
  Volume <- data[,'Volume']
  
  data <- subset(data,Volume != 0)

  #get endpoint data
  endpoint.data <- data[data$Times == max(data$Times),]
  endpoint.data <- endpoint.data[,c('Arms','Volume')]
  endpoint.data$Arms=factor(endpoint.data$Arms)
  
  #get mean growth rate
  ID <- unique(as.character(data$ID))
  
  
    dra.res <- switch (method,
                        endpoint.ANOVA = summary(aov(Volume ~ Arms, data = endpoint.data)),
                        endpoint.KW    = kruskal.test(Volume ~ Arms, data = endpoint.data),
                        mixed.ANOVA    = summary(aov(Volume ~ Arms + Error(ID/Times), data = data)),
                        LMM            = summary(lme(Volume ~ Arms, random = ~1|ID/Times,data = data))[[20]]
    )

    dra.res.full <- switch (method,
                        endpoint.ANOVA = aov(Volume ~ Arms, data = endpoint.data),
                        endpoint.KW    = kruskal.test(Volume ~ Arms, data = endpoint.data),
                        mixed.ANOVA    = aov(Volume ~ Arms + Error(ID/Times), data = data),
                        LMM            = lme(Volume ~ Arms, random = ~1|ID/Times,data = data)
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

# output$generic_plot_start <- renderPlotly({

#   temp <- data.frame(X = c(1,2,3,4), Y = c(2,3,4,5))
#   p <- ggplot(temp, aes(x=X, y=Y)) + geom_point(size=3)
#   return(p)

# })

output$user_tv_download_default_btn <- downloadHandler(
  filename = function() { 
    paste("tv_suite-sample-input.csv", sep="")
  },
  content = function(file) {
    write.csv(get_data(), file)
  })

# landing page buttons
observeEvent(input$btn_nav_tv, updateNavlistPanel(session, "nav_bco", selected = title_tumor_volume))
observeEvent(input$btn_nav_val, updateNavlistPanel(session, "nav_bco", selected = title_validate))
observeEvent(input$btn_nav_help, updateNavlistPanel(session, "nav_bco", selected = title_help))



