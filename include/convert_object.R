temp <- readRDS(data.filtered, file = "include/tv-test.rds")

data.filtered <- temp %>%
  dplyr::group_by(Model_ID) %>%
  dplyr::mutate(MouseID = paste0(Patient_ID, "_", ID)) %>%
  dplyr::mutate(ModelName = paste0(Patient_ID, "_", Model_ID)) %>%
  dplyr::mutate(Volume =  TUMOR_WT) %>%
  dplyr::ungroup() %>%
  dplyr::rename(ID2 = ID) %>%
  dplyr::rename(Arms = AgentName, Tumor = ModelName, Times = OBS_DAY, ID = MouseID, Model_ID=Model_ID, Disease_Type = SDC_Diagnosis_Description)%>%#, Study = Model_ID) %>%
  dplyr::select(Contributor, Arms, Times, Volume, Study, ID, Model_ID, Tumor, Disease_Type)

saveRDS(data.filtered, file = "include/tv-test-new.rds")
