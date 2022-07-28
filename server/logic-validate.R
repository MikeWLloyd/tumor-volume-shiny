

observeEvent(input$user_tv_upload_vald, {

  file <- input$user_tv_data_vald
  if (is.null(file)) {
    output$tv_text_upload_vald <- renderText({
      paste0("Please first select your input file!")
    })
  }else{
    ext <- tools::file_ext(file$datapath)

    if (ext == "csv") {
      df <- read.csv(file$datapath)
    }else if (ext %in% c("xlsx","xls")) {
      df <- read_excel(file$datapath)
    }else{
      validate(need(ext == "csv", "Please upload a csv file"))
      df <- NULL
    }

    if (colnames(data)%in%colnames(df)){
      output$tv_text_upload_vald <- renderText({
        paste0("Uploaded data is in the Expected Format!")
      })
    } else {
      output$tv_text_upload_vald <- renderText({
        paste0("Uploaded Data is not in the Expected Format. Your Table is Live Editable.
             Please take the below table as a reference to edit your Tumor Volume Data!")
      })
    }

    output$table_tv_validate_user <- DT::renderDataTable(
      df,
      editable = TRUE,
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
  }

})

observeEvent(input$user_tv_upload_default, {

})


output$table_tv_validate_default <- DT::renderDataTable(
  data,
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
