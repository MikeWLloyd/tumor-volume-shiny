# DATA
data <- try(as.data.frame(readRDS("include/tv-test-new.rds")), silent = T)
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
get_query_tv <- eventReactive(input$tv_submit_query, {

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

      df_query <- query_tv(data,
                          input$tv_contributor,
                          input$tv_treatment,
                          input$tv_disease_type)

      updateProgressBar(
        session = session,
        id = "pbar_tv",
        value = length(unique((df_query$df)$Study)),
        total = length(unique(data$Study))
      )


      for (i in 1:10) {
        incProgress(0.025, detail = "[Status] Finishing up...")
        Sys.sleep(0.02)
      }

      incProgress(0.15, detail = "[Status] Rendering data...")
    }
  )

  df_query
}, ignoreNULL = FALSE)

# DATA TABLE
query_all_submit <- reactiveValues(counter = 0L)

observeEvent(input$tv_submit_query, {
  query_all_submit$counter <- input$tv_submit_query
})

output$tbl_msg_all <- renderText(
  if (query_all_submit$counter == 0L) {
    "Please submit the query, then the magic will happen."
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

    # Call plot
    if (input$tv_all_scale) {

      shinyjs::enable("tv_all_interpolate")
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

      interpolate_all <- input$tv_all_interpolate
      endpoint_scale_all <- input$tv_all_endpoint_scale

      if(input$tv_all_interpolate) {
        get_plot_scaled_interpolated(data = get_interpolated_pdx_data(s.data), position.dodge = 0.5,  title = NULL, scale.factor = endpoint_scale_all, scale.by.volume = scale_by_volume_all, level = level_type, pattern = pattern_type)
      } else {
        get_plot_scaled(data = s.data, position.dodge = 0.5,  title = NULL, scale.factor = endpoint_scale_all, scale.by.volume = scale_by_volume_all, level = level_type, pattern = pattern_type)
      } # MWL: THESE NEED TO BE FIXED! 

    } else {
      # Turn off Scaled Plot I/Os
      shinyjs::disable("tv_all_interpolate")
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
  return(input$tv_resist)
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

    output$tv_text_scaled_study <- renderText({""})

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

  output$tv_text_scaled_study <- renderText({""})

  df <- gen_data_filter_study()

  if (input$tv_interpolate){
      df = get_interpolated_pdx_data(data = df)
      print(df)
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

  tab.df <- DT::datatable(response_list[[study]][order(response_list[[study]]$Arms),],
            style = "bootstrap",
            escape = FALSE,
            filter = list(position = "top", clear = T),
            rownames= FALSE,
            class = "cell-border stripe",
            extensions = "Buttons",
            options = list(
              dom = "Blrtip", scrollX = TRUE, ordering = F, autoWidth = TRUE, keys = TRUE, pageLength = 20, paging = T,
              buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Download")))) %>%
            formatRound(c('Best.Response', 'Avg.Response'), digits = 2)
  tab.df
})




# landing page buttons
observeEvent(input$btn_nav_tv, updateNavlistPanel(session, "nav_bco", selected = title_tumor_volume))
observeEvent(input$btn_nav_val, updateNavlistPanel(session, "nav_bco", selected = title_validate))
observeEvent(input$btn_nav_help, updateNavlistPanel(session, "nav_bco", selected = title_help))



