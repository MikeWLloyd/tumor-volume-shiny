output$report <- downloadHandler(

  filename = function(){
        if(input$report_type == 'html'){ 
          paste0('PVA_Report_Generated_',format(Sys.time(), "%Y-%m-%d_%Hh%Mm"), '.html')
        } else {
          paste0('PVA_Report_Generated_',format(Sys.time(), "%Y-%m-%d_%Hh%Mm"), '.pdf')
        }
        },

  content = function(file) {
    
    # # Copy the report file to a temporary directory before processing it, in
    # # case we don't have write permissions to the current working dir (which
    # # can happen when deployed).
    # tempReport <- file.path(tempdir(), "report.Rmd")

    # file.copy("report.Rmd", tempReport, overwrite = TRUE)
    
    on.exit(removeModal())

    progress <- AsyncProgress$new(message="Rendering report, please wait.")
    # http://htmlpreview.github.io/?https://github.com/fellstat/ipc/blob/master/inst/doc/shinymp.html 
    
    # Set up parameters to pass to Rmd document

    if(!input$override_day) {
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
                    script_location = file.path(getwd(), "server/logic-plots.R"),
                    progress = progress)
    } else if (input$override_day) {
      params <- list(data_subset = get_query_tv()$"df",
                    tv_all_plot_type = input$tv_all_plot_type,
                    tv_all_plot_style = input$tv_all_plot_style,
                    tv_all_interpolate = input$tv_all_interpolate,
                    tv_all_plotType = input$tv_all_plotType,
                    tv_all_scaleby = input$tv_all_scaleby,
                    tv_all_endpoint_scale = input$tv_all_endpoint_scale,
                    main_avgplot.day = input$report_day,
                    main_avgplot_interpolate = input$main_avgplot_interpolate,
                    main_log2fold.day = input$report_day,
                    main_log2_interpolate = input$main_log2_interpolate,
                    main_TC.day = input$report_day,
                    main_tc_interpolate = input$main_tc_interpolate,
                    main_TGI.day = input$report_day,
                    main_tgi_interpolate = input$main_tgi_interpolate,
                    main_orc.day = input$report_day,
                    main_stackedorc_interpolate = input$main_stackedorc_interpolate,
                    tv_recist.day = input$report_day,
                    tv_study_filtered = input$tv_study_filtered,
                    tv_interpolate = input$tv_interpolate,
                    waterfall_metric = input$waterfall_metric,
                    tv_AUC.day.waterfall = input$report_day,
                    tv_waterfall_interpolate = input$tv_waterfall_interpolate,
                    tv_PercChange_EventSize = input$tv_PercChange_EventSize,
                    anova_Measure_Day = input$report_day,
                    tv_tumor_filtered = input$tv_tumor_filtered,
                    main_anova_interpolate = input$main_anova_interpolate,
                    tv_weight_plot_type = input$tv_weight_plot_type,
                    tv_weight_plot_style = input$tv_weight_plot_style,
                    tv_weight_plotType = input$tv_weight_plotType,
                    script_location = file.path(getwd(), "server/logic-plots.R"),
                    progress = progress)
    }


    # NOTE: If a new param is added, it must also be listed in the 'params' statement of the markdown document. 
    #       The params are then accessed via `params$` e.g., params$PARAM_NAME

    # NOTE: Due to the async future_promise command, and the behavior of tempdir(),
    #       it is possible to have concurrent users building reports in the same temp directory space. 
    #       This is because `tempdir()` uses the same directory for all members of a session, and
    #       sessions are often concurrent based on the cloud server setup. 
    #       The default behavior of R markdown is to build intermediate files in the directory the *.Rmd file is contained in. 
    #       Using either `intermediates_dir` or placing the Rmd in a specific folder overcome this conflict. 
    #       The `file` variable set by `downloadHandler`` is placed in `tempdir`; 
    #       however, tempfile is used to generate a random temp name for that file. 
    #       If conflicts are still seen, more explicit use of `intermediates_dir`, `output_dir` can be explored here. 

    random_string <- function(n = 5000) {
      a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
      paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
    }

    # this will create a folder within our temp folder, with a name of our choice:
    ReportDir <- paste0(tempdir(), "/", random_string(1))
    
    dir.create(path = ReportDir)

    tempReport <- file.path(ReportDir, "report.Rmd") 
    # If conflict still occurs, consider also adding random_string(1) to the report name. 

    file.copy("report/report.Rmd", tempReport, overwrite = TRUE)
    # https://resources.symbolix.com.au/2020/10/28/downloadable-reports-shiny/

    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).

    # The knit is done with `future_promise` to build the reports asynchronously. 
    # The async behavior requires minor changes within the report.
    # Note that the 'progress' bar from above can be incremented within the report. 

    if(input$report_type == 'html'){
      future_promise({
            rmarkdown::render(tempReport, 
              output_file = file,
              output_format = 'html_document',
              params = params,
              envir = new.env(parent = globalenv()),
              clean = TRUE
            )
          progress$close()
      }, seed = NULL)
    } else {
      future_promise({
          rmarkdown::render(tempReport, 
            output_file = file,
            output_format = 'pdf_document',
            params = params,
            envir = new.env(parent = globalenv()),
            clean = TRUE
          )
          progress$close()
      }, seed = NULL)
    }
  }
)