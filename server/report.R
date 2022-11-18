
output$report <- downloadHandler(

  filename = paste0('PVA_Report_Generated_',format(Sys.time(), "%Y-%m-%d_%Hh%Mm"), '.html'),

  content = function(file) {
    
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <- file.path(tempdir(), "report.Rmd")

    file.copy("report.Rmd", tempReport, overwrite = TRUE)

    # Set up parameters to pass to Rmd document
    params <- list(data_subset = get_query_tv()$"df",
                  tv_all_plot_type = input$tv_all_plot_type,
                  tv_all_plot_style = input$tv_all_plot_style,
                  tv_all_interpolate = input$tv_all_interpolate,
                  tv_all_plotType = input$tv_all_plotType,
                  tv_all_scaleby = input$tv_all_scaleby,
                  tv_all_endpoint_scale = input$tv_all_endpoint_scale,
                  tv_weight_plot_type = input$tv_weight_plot_type,
                  tv_weight_plot_style = input$tv_weight_plot_style,
                  tv_weight_plotType = input$tv_weight_plotType)

    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    withProgress(message = 'Rendering report, please wait.', {
    rmarkdown::render('report.Rmd', output_file = file,
      params = params,
      envir = new.env(parent = globalenv())
    )
    })
  }
)

