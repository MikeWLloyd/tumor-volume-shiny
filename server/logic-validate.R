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
    is.character(Tumor) | is.numeric(Tumor) | is.integer(Tumor),
    is.character((Disease_Type)),
    Volume >= 0,
    contains_at_least(keys = data.frame(Arms = 'Control'))
  )

  check1 <- 0
  check2 <- 0
  check3 <- 0

  out <- confront(df, rules)
  test_results <- as.data.frame(summary(out))
  test_results$ColumnCheck <- c("Contributor", "Arms", "Times", "Volume", "Study", "ID", "Tumor", "Disease_Type", "VolnotNeg", "ControlPresent")
  test_results$ExpectedType <- c('Character', 'Character', 'Numeric or Integer', 'Numeric or Integer', 'Character', 'Character', 'Character or Numeric or Integer', 'Character', 'Volume >= 0', 'Arms contains "Control"')

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

  failures <- as.data.frame(test_results %>% dplyr::filter(fails == 1 & name != 'V10' | fails == 1 & name != 'V11'))

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

  failed_control <- test_results %>% dplyr::filter(fails != 0 & name == 'V11')

  if(nrow(failed_control > 0)) {
    error_check_string <- paste(error_check_string, 'IMPORT ERRORS:\n  "Control" is not present in treatment arms. Control arm must be named "Control"\n') 
  } else {
    error_check_string <- paste(error_check_string, 'IMPORT ERRORS:\n  NONE\n')
    check3 <- 1
  }

  failed_volume <- test_results %>% dplyr::filter(fails != 0 & name == 'V10')

  if(nrow(failed_volume > 0)) {
    error_check_string <- paste(error_check_string, 'IMPORT WARNINGS: \n  There are', failed_volume$fails, 'volume measures below 0. Check these data points are valid.\n') 
  } else {
    error_check_string <- paste(error_check_string, 'IMPORT WARNINGS:\n  NONE\n')
  }

  if (check1 == 1 && check2 == 1 && check3 == 1) {
    error_check_string <- paste(error_check_string, '\n NO ISSUES!')
  } else if (check1 != 1 || check2 != 1 || check3 != 3) {
     error_check_string <- paste(error_check_string, '\nImported data are not in expected format for reasons above.\nCorrect issues in local file and re-upload to test. Example valid input data is provided below for reference.')
  }

  output$tv_text_upload_valid <- renderText({
    paste(error_check_string)
  })
})

flag_user_data <- reactiveValues(flag = 0L)

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
    is.character(Tumor) | is.numeric(Tumor) | is.integer(Tumor),
    is.character((Disease_Type)),
    Volume >= 0,
    contains_at_least(keys = data.frame(Arms = 'Control'))
  )

  check1 <- 0
  check2 <- 0
  check3 <- 0

  out <- confront(df, rules)
  test_results <- as.data.frame(summary(out))
  test_results$ColumnCheck <- c("Contributor", "Arms", "Times", "Volume", "Study", "ID", "Tumor", "Disease_Type", "VolnotNeg", "ControlPresent")
  test_results$ExpectedType <- c('Character', 'Character', 'Numeric or Integer', 'Numeric or Integer', 'Character', 'Character', 'Character or Numeric or Integer', 'Character', 'Volume >= 0', 'Arms contains "Control"')

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

  failures <- as.data.frame(test_results %>% dplyr::filter(fails == 1 & name != 'V10' | fails == 1 & name != 'V11'))

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

  failed_control <- test_results %>% dplyr::filter(fails != 0 & name == 'V11')

  if(nrow(failed_control > 0)) {
    error_check_string <- paste(error_check_string, 'IMPORT ERRORS:\n  "Control" is not present in treatment arms. Control arm must be named "Control"\n') 
  } else {
    error_check_string <- paste(error_check_string, 'IMPORT ERRORS:\n  NONE\n')
    check3 <- 1
  }

  failed_volume <- test_results %>% dplyr::filter(fails != 0 & name == 'V10')

  if(nrow(failed_volume > 0)) {
    error_check_string <- paste(error_check_string, 'IMPORT WARNINGS: \n  There are', failed_volume$fails, 'volume measures below 0. Check these data points are valid.\n') 
  } else {
    error_check_string <- paste(error_check_string, 'IMPORT WARNINGS:\n  NONE\n')
  }

  if (check1 == 1 && check2 == 1 && check3 == 1) {
    error_check_string <- paste(error_check_string, '\n NO ISSUES!')
    shinyjs::enable("add_user_tv_btn")

    flag_user_data$flag <- 1

    output$tv_text_continue <- renderText({
      paste0("Successfully Validated Your Tumor Volume Data!")
    })

    output$tv_text_upload <- renderText({
      paste0("Loaded Your Tumor Volume Data!")
    })

    output$tv_text_stop <- renderText({
      paste0("")
    })

    output$tv_text_guide <- renderText({
      paste0("")
    })

  } else if (check1 != 1 || check2 != 1 || check3 != 1) {

    flag_user_data$flag <- 0

    shinyjs::disable("add_user_tv_btn")

    output$tv_text_stop <- renderText({
      paste0("Failed Validation!")
    })

    output$tv_text_guide <- renderText({
      paste0("Edit Your Tumor Volume Data According to the Template under the Validation Tab!")
    })

    output$tv_text_continue <- renderText({
      paste0("")
    })

    output$tv_text_upload <- renderText({
      paste0("Example Tumor Volume Data Still Active!")
    })

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
