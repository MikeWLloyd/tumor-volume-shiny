library(dplyr)
temp <- readRDS(file = "include/tv-test.rds")

data.filtered <- temp %>%
  dplyr::group_by(Model_ID) %>%
  dplyr::mutate(MouseID = paste0(Patient_ID, "_", ID)) %>%
  dplyr::mutate(ModelName = paste0(Patient_ID, "_", Model_ID)) %>%
  dplyr::mutate(Volume =  TUMOR_WT) %>%
  dplyr::ungroup() %>%
  dplyr::rename(ID2 = ID) %>%
  dplyr::rename(Arms = AgentName, Tumor = ModelName, Times = OBS_DAY, ID = MouseID, Model_ID=Model_ID, Disease_Type = SDC_Diagnosis_Description)%>%#, Study = Model_ID) %>%
  dplyr::select(Contributor, Arms, Times, Volume, Study, ID, Model_ID, Tumor, Disease_Type)

write.table(data.filtered, file = 'include/example_import.csv', quote = F, row.names = F, sep = ',')

write.table(data.filtered %>% select(-Tumor, -Model_ID), file = 'include/example_import_missingCol.csv', quote = F, row.names = F, sep = ',')

saveRDS(data.filtered, file = "include/tv-test-new.rds")

###

#install.packages("validate")

library(validate)

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

data.filtered %>% select(-Tumor)

out <- confront(data.filtered %>% select(-Tumor, -Model_ID), rules)
summary(out)
plot(out)

errors(out)

test_results <- as.data.frame(summary(out))

test_results$ColumnCheck <- c("Contributor", "Arms", "Times", "Volume", "Study", "ID", "Model_ID", "Tumor", "Disease_Type", "VolnotNeg")
test_results$ExpectedType <- c('Character', 'Character', 'Numeric or Integer', 'Numeric or Integer', 'Character', 'Character', 'Character or Numeric or Integer', 'Character or Numeric or Integer', 'Character', 'Volume >= 0')



test_results %>% dplyr::filter(fails != 0 & name != 'V10')
test_results %>% dplyr::filter(fails != 0 & name == 'V10')

#########

out <- confront(read.csv(file = 'include/example_import_missingCol.csv'), rules)
summary(out)
errors(out)

out <- confront(read.csv(file = 'include/example_import_invalidData.csv'), rules)

error_check_string <- 'IMPORT ERROR: \n'
if(nrow(test_results %>% dplyr::filter(error == 'TRUE') > 0)) {
  for(error in errors(out)) {
    error <- gsub('object', 'Column', error)
    error_check_string <- paste(error_check_string,'ERROR: ', error)
    error_check_string <- paste(error_check_string, '', sep = '\n')
  }
}
cat(error_check_string)

test_results <- as.data.frame(summary(out))

test_results$ColumnCheck <- c("Contributor", "Arms", "Times", "Volume", "Study", "ID", "Model_ID", "Tumor", "Disease_Type", "VolnotNeg")
test_results$ExpectedType <- c('Character', 'Character', 'Numeric or Integer', 'Numeric or Integer', 'Character', 'Character', 'Character or Numeric or Integer', 'Character or Numeric or Integer', 'Character', 'Volume >= 0')

failures <- as.data.frame(test_results %>% dplyr::filter(fails == 1 & name != 'V10'))

failure_check_string <- ''
if(nrow(failures > 0)) {
  for(error in (1:nrow(failures))) {
    error <- gsub('object', 'Column', error)
    failure_check_string <- paste0(failure_check_string, "Invalid data type in column: '", failures[error,]$ColumnCheck, ".' expected data type: ", failures[error,]$ExpectedType)
    failure_check_string <- paste(failure_check_string, '', sep = '\n')
  }
}
cat(failure_check_string)

failed_volume <- test_results %>% dplyr::filter(fails != 0 & name == 'V10')

if(nrow(failed_volume > 0)) {
  print(paste('There are', failed_volume$fails, 'volume measures below 0. Check these data points are valid'))
  as.data.frame(violating(read.csv(file = 'include/example_import_invalidData.csv'), out[10]))
}

as.data.frame(out) %>% dplyr::filter(name == 'V10', 'value ')

