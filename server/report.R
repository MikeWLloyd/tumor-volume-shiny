output$report <- downloadHandler(

  filename = function(){
        if(input$report_type == 'html'){ 
          paste0('PVA_Report_Generated_',format(Sys.time(), "%Y-%m-%d_%Hh%Mm"), '.html')
        } else {
          paste0('PVA_Report_Generated_',format(Sys.time(), "%Y-%m-%d_%Hh%Mm"), '.pdf')
        }
        },

  content = function(file) {
    
    on.exit(removeModal())

    # # Copy the report file to a temporary directory before processing it, in
    # # case we don't have write permissions to the current working dir (which
    # # can happen when deployed).
    # tempReport <- file.path(tempdir(), "report.Rmd")

    # file.copy("report.Rmd", tempReport, overwrite = TRUE)

    # Set up parameters to pass to Rmd document
    params <- list(data_subset = get_query_tv()$"df",
                  tv_all_plot_type = input$tv_all_plot_type,
                  tv_all_plot_style = input$tv_all_plot_style,
                  tv_all_interpolate = input$tv_all_interpolate,
                  tv_all_plotType = input$tv_all_plotType,
                  tv_all_scaleby = input$tv_all_scaleby,
                  tv_all_endpoint_scale = input$tv_all_endpoint_scale,
                  main_avgplot.day = input$main_avgplot.day,
                  main_avgplot_interpolate = input$main_avgplot_interpolate,
                  main_log2fold.day = input$main_log2fold.day,
                  main_log2_interpolate = input$main_log2_interpolate,
                  main_TC.day = input$main_TC.day,
                  main_tc_interpolate = input$main_tc_interpolate,
                  main_TGI.day = input$main_TGI.day,
                  main_tgi_interpolate = input$main_tgi_interpolate,
                  main_orc.day = input$main_orc.day,
                  main_stackedorc_interpolate = input$main_stackedorc_interpolate,
                  tv_recist.day = input$tv_recist,
                  tv_study_filtered = input$tv_study_filtered,
                  tv_interpolate = input$tv_interpolate,
                  waterfall_metric = input$waterfall_metric,
                  tv_AUC.day.waterfall = input$tv_AUC.day.waterfall,
                  tv_waterfall_interpolate = input$tv_waterfall_interpolate,
                  tv_PercChange_EventSize = input$tv_PercChange_EventSize,
                  anova_Measure_Day = input$anova_Measure_Day,
                  tv_tumor_filtered = input$tv_tumor_filtered,
                  main_anova_interpolate = input$main_anova_interpolate,
                  tv_weight_plot_type = input$tv_weight_plot_type,
                  tv_weight_plot_style = input$tv_weight_plot_style,
                  tv_weight_plotType = input$tv_weight_plotType,
                  script_location = file.path(getwd(), "server/logic-plots.R"))

    # NOTE: If a new param is added, it must also be listed in the 'params' statement of the markdown document. 
    #       The params are then accessed via `params$` e.g., params$PARAM_NAME

    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).

    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("report/report.Rmd", tempReport, overwrite = TRUE)
    # https://resources.symbolix.com.au/2020/10/28/downloadable-reports-shiny/
    
    if(input$report_type == 'html'){ 

      withProgress(message = 'Rendering report, please wait.', {
        rmarkdown::render(tempReport, 
          output_file = file,
          output_format = 'html_document',
          params = params,
          envir = new.env(parent = globalenv()),
          clean = TRUE
        )
      })

    } else {
      
      withProgress(message = 'Rendering report, please wait.', {
        rmarkdown::render(tempReport, output_file = file,
        output_format = 'pdf_document',
          params = params,
          envir = new.env(parent = globalenv()),
          clean = TRUE
        )
      })
    }
  }
)