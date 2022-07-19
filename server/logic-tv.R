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
  if (length(unique(data$AgentName)) < 2) {
    unique(data$AgentName)
  } else {
    agents <- factor(unique(data$AgentName))
    agents <- relevel(agents, 'Control')
    levels(agents)
  }
}

get_tv_disease <- function() {
  if (length(unique(data$SDC_Diagnosis_Description)) < 2) {
    list(unique(data$SDC_Diagnosis_Description))
  } else {
    sort(unique(data$SDC_Diagnosis_Description), na.last = T)
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
    get_tv_volumes_count(ret), "Volume Records",
    icon = icon("file-medical"), color = "green"
  )
})

get_tv_studies_count <- function(df) ifelse(is.null(df), 0, length(unique(df$"Study")))

output$card_tv_models <- shinydashboard::renderValueBox({
  ret <- get_query_tv()$"df"
  shinydashboard::valueBox(
    get_tv_studies_count(ret), "Studies",
    icon = icon("vial"), color = "yellow"
  )
})

get_tv_patients_count <- function(df) ifelse(is.null(df), 0, length(unique(df$"Patient_ID")))

output$card_tv_patients <- shinydashboard::renderValueBox({
  ret <- get_query_tv()$"df"
  shinydashboard::valueBox(
    get_tv_patients_count(ret), "Patients",
    icon = icon("users"), color = "blue"
  )
})

get_tv_disease_count <- function(df) ifelse(is.null(df), 0, length(unique(df$"SDC_Diagnosis_Description")))

output$card_tv_disease <- shinydashboard::renderValueBox({
  ret <- get_query_tv()$"df"
  shinydashboard::valueBox(
    get_tv_disease_count(ret), "Disease Types",
    icon = icon("virus"), color = "red"
  )
})

get_tv_treatment_count <- function(df) ifelse(is.null(df), 0, length(unique(df$"AgentName")))

output$card_tv_treatment <- shinydashboard::renderValueBox({
  ret <- get_query_tv()$"df"
  shinydashboard::valueBox(
    get_tv_treatment_count(ret), "Treatments",
    icon = icon("medkit"), color = "maroon"
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
  df <- get_query_tv()$"df"

  if (is.null(df) | (nrow(df) == 0)) {
    plot_ly()
  } else {

    # Get Level Type Input by User
    level_type <- "Animal"
    if (input$tv_all_plot_type == "Treatment Plot") {
      level_type <- "Arm"
    }
    pattern_type <- "TAN"

    # Call plot
    if (input$tv_all_scale) {
      shinyjs::enable("tv_all_interpolate")
      shinyjs::enable("tv_div_all_endpoint")
      shinyjs::enable("tv_div_all_scale_picker")

      s.data <- get_data_summary_scaled(df, measure.var = "Volume",
                                        group.vars = c("Study", "Times", "Arms", "Model_ID", "Type"))
      # print(s.data)
      s.data <- subset(s.data, select = !(names(s.data) %in% c("ID")))
      s.data <- s.data %>% dplyr::rename(ID = Study)

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
      }

    } else {
      # Turn off Scaled Plot I/Os
      shinyjs::disable("tv_all_interpolate")
      shinyjs::disable("tv_div_all_endpoint")
      shinyjs::disable("tv_div_all_scale_picker")

      # Reset Info Text of the Scaled Plot
      output$tv_all_text_scaled <- renderText({""})

      get_tv_plot(data = df, level = level_type,pattern = pattern_type,position.dodge = 0.99)
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

gen_data_processed <- reactive({
  data_processed <-
    gen_data_filter_study() %>%
    dplyr::group_by(Model_ID) %>%
    dplyr::mutate(Days=OBS_DAY) %>%
    dplyr::ungroup() %>%
    dplyr::rename(Arms = AgentName, Tumor = Model_ID, Times = Days, Volume = TUMOR_WT)%>%#, Type = SDC_Diagnosis_Description) %>%
    mutate(Arms=recode(Arms, 'Untreated'='Control')) %>%
    dplyr::select(Arms, Tumor, Times, Volume, ID, Type) %>%
    dplyr::filter(Arms != '')

  data_processed
})

last.study.day <- reactive({
  return(input$tv_resist)
})

observeEvent(input$tv_scale_picker_study,{
  if(input$tv_scale_picker_study == "Volume") {
    updateNumericInput(session, "tv_endpoint.scale",
                       label = paste("Endpoint Scaling (Volume mm3)"),
                       value = 1200, min = 0, max = 10000)
  } else {
    updateNumericInput(session, "tv_endpoint.scale",
                       label = paste("Endpoint Scaling (Growth Factor)"),
                       value = 4, min = 0, max = 10000)
  }
})

output$plot_tumorvol_study <- renderPlot({

  if (input$tv_checkbox){

    # Data
    shinyjs::enable("tv_interpolate")
    shinyjs::enable("div_tv_endpoint.scale")
    shinyjs::enable("div_tv_scale_picker_study")

    if(input$tv_scale_picker_study == "Volume") {
      scale.by.volume <- TRUE

      output$tv_text_scaled_study <- renderText({
        paste0("Plots are scaled by Volume. Y-axis ranges from 100% with endpoint scaling of ",
               input$tv_endpoint.scale,
               "mm3 to -100% (total regression)")
      })

    } else {
      scale.by.volume <- FALSE

      output$tv_text_scaled_study <- renderText({
        paste0("Plots are scaled by Relative Growth. Y-axis ranges from 100% with endpoint scaling of ",
               input$tv_endpoint.scale,
               "x starting growth to -100% (total regression)")
      })
    }

    Data_Response <- get_data_summary_scaled(gen_data_filter_study(), measure.var = "Volume",
                                             group.vars = c("Study", "Times", "Arms", "ID", "Type"))
    study <- unlist(levels(factor(Data_Response$Tumor)))[1]

    one_A_N_DRAP <- Data_Response %>%
      filter(Tumor == study) %>% droplevels()

    if (input$tv_interpolate){
      p1 <- get_plot_scaled_interpolated_study(data = get_interpolated_pdx_data(data = one_A_N_DRAP), position.dodge = 0.5,  title = study, scale.factor = input$tv_endpoint.scale, scale.by.volume = scale.by.volume )
    }else{
      p1 <- get_plot_scaled_study(data = one_A_N_DRAP, position.dodge = 0.5,  title = study, scale.factor = input$tv_endpoint.scale, scale.by.volume = scale.by.volume )
    }

    p1 <- p1 + geom_vline(xintercept = last.study.day(), linetype="dashed",
                          color = "black", size=1.2)

    mytheme <- gridExtra::ttheme_default(
      core = list(fg_params=list(cex = 0.8)),
      colhead = list(fg_params=list(cex = 0.8)),
      rowhead = list(fg_params=list(cex = 1.0)))

    response_list = list()

    response_list[[study]] <- get_DRLevel(one_A_N_DRAP, neg.control = 'Control', last.measure.day = last.study.day())$Response.Level
    response_list[[study]]$Study <- study

    t1 <- gridExtra::tableGrob(response_list[[study]][order(relevel(factor(response_list[[study]]$Arms), 'Control')),] , theme=mytheme, rows=NULL)  # transform into a tableGrob
    #
    title <- textGrob(paste('RECIST at', last.study.day(), 'days'),gp=gpar(fontsize=12))
    padding <- unit(5,"mm")
    table <- gtable_add_rows(
      t1,
      heights = grobHeight(title) + padding,
      pos = 0)
    table <- gtable_add_grob(
      table,
      title,
      1, 1, 1, ncol(table))

    return(grid.arrange(p1, table, nrow=1))

    # return(p1)

  } else {

    shinyjs::disable("tv_interpolate")
    shinyjs::disable("div_tv_endpoint.scale")
    shinyjs::disable("div_tv_scale_picker_study")

    output$tv_text_scaled_study <- renderText({""})

    df <- gen_data_processed()

    study <- unlist(levels(factor(df$Tumor)))[1]

    one_A_N_DRAP <- df %>%
      filter(Tumor == study) %>% droplevels()


    p1 <- get_plot_volumeGC_alt(one_A_N_DRAP, level = 'Arm',
                                position.dodge = 0.2,
                                title = paste('Study:', study), plot_on = FALSE )

    p1 <- p1 + geom_vline(xintercept = last.study.day(), linetype="dashed",
                          color = "black", size=1.2)


    p1 <- p1 + xlab("Time (d)") + ylab("Tumor Volume (mm3)")

    mytheme <- gridExtra::ttheme_default(
      core = list(fg_params=list(cex = 0.8)),
      colhead = list(fg_params=list(cex = 0.8)),
      rowhead = list(fg_params=list(cex = 1.0)))

    response_list = list()
    response_list[[study]] <- get_DRLevel(one_A_N_DRAP, neg.control = 'Control', last.measure.day = last.study.day())$Response.Level
    response_list[[study]]$Study <- study

    if ('Control'%in%unique(response_list[[study]]$Arms)){
      response_list[[study]]$Arms <- relevel(factor(response_list[[study]]$Arms), 'Control')
    }

    t1 <- gridExtra::tableGrob(response_list[[study]][order(response_list[[study]]$Arms),] , theme=mytheme, rows=NULL)  # transform into a tableGrob

    title <- textGrob(paste('RECIST at', last.study.day(), 'days'),gp=gpar(fontsize=12))
    padding <- unit(5,"mm")
    table <- gtable_add_rows(
      t1,
      heights = grobHeight(title) + padding,
      pos = 0)
    table <- gtable_add_grob(
      table,
      title,
      1, 1, 1, ncol(table))

    return(grid.arrange(p1, table, nrow=1))

  }
})

# landing page buttons
observeEvent(input$btn_nav_tv, updateNavlistPanel(session, "nav_bco", selected = title_tumor_volume))
observeEvent(input$btn_nav_val, updateNavlistPanel(session, "nav_bco", selected = title_validate))
observeEvent(input$btn_nav_help, updateNavlistPanel(session, "nav_bco", selected = title_help))



