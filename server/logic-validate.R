observeEvent(input$user_tv_data_valid, {
  # changed above from user_tv_upload_valid. The button was breaking the app when clicked without data present. 
  
  file <- input$user_tv_data_valid
  ext <- tools::file_ext(file$datapath)

  if (ext == "csv") {
    df <- read.csv(file$datapath)
  }else if (ext %in% c("xlsx","xls")) {
    df <- read_excel(file$datapath)
  }else{
    validate(need(ext == "csv", "Please upload a csv file"))
    df <- NULL
  }

  rules <- validator(
    is.character(Contributor), 
    is.character(Arms), 
    is.numeric(Times) | is.integer(Times),
    is.numeric(Volume) | is.integer(Volume),
    is.character(Study),
    is.character(ID),
    is.character(Model_ID) | is.numeric(Model_ID) | is.integer(Model_ID),
    is.character(Tumor) | is.numeric(Tumor) | is.integer(Tumor),
    is.character((Disease_Type)),
    Volume >= 0
  )

  check1 <- 0
  check2 <- 0

  out <- confront(df, rules)
  test_results <- as.data.frame(summary(out))
  test_results$ColumnCheck <- c("Contributor", "Arms", "Times", "Volume", "Study", "ID", "Model_ID", "Tumor", "Disease_Type", "VolnotNeg")
  test_results$ExpectedType <- c('Character', 'Character', 'Numeric or Integer', 'Numeric or Integer', 'Character', 'Character', 'Character or Numeric or Integer', 'Character or Numeric or Integer', 'Character', 'Volume >= 0')

  error_check_string <- ' COLUMN NAME ERRORS: \n'
  if(nrow(test_results %>% dplyr::filter(error == 'TRUE') > 0)) {

    for(error in errors(out)) {
      error <- gsub('object', 'Column', error)
      error_check_string <- paste(error_check_string,' ERROR: ', error)
      error_check_string <- paste(error_check_string, '', sep = '\n')
    }
  } else {
    error_check_string <- paste(error_check_string, ' NONE\n')
    check1 <-1
  }

  failures <- as.data.frame(test_results %>% dplyr::filter(fails == 1 & name != 'V10'))

  if(nrow(failures > 0)) {
    error_check_string <- paste(error_check_string, 'DATA TYPE ERRORS:\n')
    for(error in (1:nrow(failures))) {
      
      error_check_string <- paste0(error_check_string, "  Invalid data type in column: '", failures[error,]$ColumnCheck, "' expected data type: ", failures[error,]$ExpectedType)
      error_check_string <- paste(error_check_string, '', sep = '\n')
    }
  } else {
    error_check_string <- paste(error_check_string, 'DATA TYPE ERRORS:\n  NONE\n')
    check2 <- 1
  }

  failed_volume <- test_results %>% dplyr::filter(fails != 0 & name == 'V10')

  if(nrow(failed_volume > 0)) {
    error_check_string <- paste(error_check_string, 'IMPORT WARNINGS: \n  There are', failed_volume$fails, 'volume measures below 0. Check these data points are valid.\n') 

    output$table_tv_validate_user <- DT::renderDataTable(
      as.data.frame(violating(df, out[10])),
      editable = FALSE,
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
  } else {
    error_check_string <- paste(error_check_string, 'IMPORT WARNINGS:\n  NONE\n')
  }

  if (check1 == 1 && check2 == 1) {
    error_check_string <- paste(error_check_string, '\n NO ISSUES! IMPORT TO MAIN TAB POSSIBLE.')
  } else if (check1 != 1 || check2 != 1) {
     error_check_string <- paste(error_check_string, '\nImported data are not in expected format for reasons above.\nCorrect issues in local file and re-upload to test. Example valid input data is provided below for reference.')
  }

  output$tv_text_upload_valid <- renderText({
    paste(error_check_string)
  })
})


observeEvent(input$user_tv_data, {
  # changed above from user_tv_upload_valid. The button was breaking the app when clicked without data present. 
  
  file <- input$user_tv_data
  ext <- tools::file_ext(file$datapath)

  if (ext == "csv") {
    df <- read.csv(file$datapath)
  }else if (ext %in% c("xlsx","xls")) {
    df <- read_excel(file$datapath)
  }else{
    validate(need(ext == "csv", "Please upload a csv file"))
    df <- NULL
  }

  rules <- validator(
    is.character(Contributor), 
    is.character(Arms), 
    is.numeric(Times) | is.integer(Times),
    is.numeric(Volume) | is.integer(Volume),
    is.character(Study),
    is.character(ID),
    is.character(Model_ID) | is.numeric(Model_ID) | is.integer(Model_ID),
    is.character(Tumor) | is.numeric(Tumor) | is.integer(Tumor),
    is.character((Disease_Type)),
    Volume >= 0
  )

  check1 <- 0
  check2 <- 0

  out <- confront(df, rules)
  test_results <- as.data.frame(summary(out))
  test_results$ColumnCheck <- c("Contributor", "Arms", "Times", "Volume", "Study", "ID", "Model_ID", "Tumor", "Disease_Type", "VolnotNeg")
  test_results$ExpectedType <- c('Character', 'Character', 'Numeric or Integer', 'Numeric or Integer', 'Character', 'Character', 'Character or Numeric or Integer', 'Character or Numeric or Integer', 'Character', 'Volume >= 0')

  error_check_string <- ' COLUMN NAME ERRORS: \n'
  if(nrow(test_results %>% dplyr::filter(error == 'TRUE') > 0)) {

    for(error in errors(out)) {
      error <- gsub('object', 'Column', error)
      error_check_string <- paste(error_check_string,' ERROR: ', error)
      error_check_string <- paste(error_check_string, '', sep = '\n')
    }
  } else {
    error_check_string <- paste(error_check_string, ' NONE\n')
    check1 <-1
  }

  failures <- as.data.frame(test_results %>% dplyr::filter(fails == 1 & name != 'V10'))

  if(nrow(failures > 0)) {
    error_check_string <- paste(error_check_string, 'DATA TYPE ERRORS:\n')
    for(error in (1:nrow(failures))) {
      
      error_check_string <- paste0(error_check_string, "  Invalid data type in column: '", failures[error,]$ColumnCheck, "' expected data type: ", failures[error,]$ExpectedType)
      error_check_string <- paste(error_check_string, '', sep = '\n')
    }
  } else {
    error_check_string <- paste(error_check_string, 'DATA TYPE ERRORS:\n  NONE\n')
    check2 <- 1
  }

  failed_volume <- test_results %>% dplyr::filter(fails != 0 & name == 'V10')

  if(nrow(failed_volume > 0)) {
    error_check_string <- paste(error_check_string, 'IMPORT WARNINGS: \n  There are', failed_volume$fails, 'volume measures below 0. Check these data points are valid.\n') 
  } else {
    error_check_string <- paste(error_check_string, 'IMPORT WARNINGS:\n  NONE\n')
  }

  if (check1 == 1 && check2 == 1) {
    error_check_string <- paste(error_check_string, '\n NO ISSUES! IMPORT TO MAIN TAB POSSIBLE.')
  } else if (check1 != 1 || check2 != 1) {
     error_check_string <- paste(error_check_string, '\nImported data are not in expected format for reasons above.\nCorrect issues in local file and re-upload to test. Example valid input data is provided below for reference.')
  }

  updateTextAreaInput(session, "tv_user_return_msg", value = paste(error_check_string))
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
