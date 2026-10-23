suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(zoo)
})

timeout_clock <- new.env()
timeout_clock$now <- as.POSIXct("2026-10-16 12:00:00", tz = "UTC")
timeout_server_env <- new.env(parent = asNamespace("shiny"))
timeout_server_env$Sys.time <- function() timeout_clock$now
timeout_server <- function(input, output, session) {
  source(file.path("server", "timeout.R"), local = TRUE)
}
environment(timeout_server) <- timeout_server_env

shiny::testServer(timeout_server, {
  session$flushReact()
  stopifnot(!session$env$idle_warning_shown, !session$isClosed())
  timeout_clock$now <- timeout_clock$now + 539
  session$elapse(5000)
  stopifnot(!session$env$idle_warning_shown)
  timeout_clock$now <- timeout_clock$now + 1
  session$elapse(5000)
  stopifnot(session$env$idle_warning_shown, !session$isClosed())

  session$setInputs(analyzer_continue_session = 1)
  stopifnot(!session$env$idle_warning_shown)
  stopifnot(identical(session$env$last_activity, timeout_clock$now))
  timeout_clock$now <- timeout_clock$now + 540
  session$elapse(5000)
  stopifnot(session$env$idle_warning_shown)

  session$setInputs(analyzer_activity = 1)
  stopifnot(!session$env$idle_warning_shown)
  stopifnot(identical(session$env$last_activity, timeout_clock$now))
  timeout_clock$now <- timeout_clock$now + 599
  session$elapse(5000)
  stopifnot(!session$isClosed())
  timeout_clock$now <- timeout_clock$now + 1
  session$elapse(5000)
  stopifnot(session$isClosed())
})

shiny::testServer(timeout_server, {
  session$flushReact()
  timeout_clock$now <- timeout_clock$now + 600
  session$elapse(5000)
  stopifnot(session$isClosed())
})

query_env <- new.env(parent = globalenv())
query_env$showNotification <- function(...) invisible(NULL)
query_env$removeNotification <- function(...) invisible(NULL)
source(file.path("server", "query-tv.R"), local = query_env)
source_function_definitions <- function(path, function_names, envir = query_env) {
  expressions <- parse(path)
  assignments <- Filter(function(expression) {
    is.call(expression) &&
      identical(expression[[1L]], as.name("<-")) &&
      length(expression) >= 3L &&
      is.symbol(expression[[2L]]) &&
      as.character(expression[[2L]]) %in% function_names &&
      is.call(expression[[3L]]) &&
      identical(expression[[3L]][[1L]], as.name("function"))
  }, expressions)
  found_names <- vapply(assignments, function(expression) as.character(expression[[2L]]), character(1))
  missing_names <- setdiff(function_names, found_names)
  if (length(missing_names) > 0L) {
    stop("Function definitions not found in ", path, ": ", paste(missing_names, collapse = ", "))
  }
  for (expression in assignments) eval(expression, envir = envir)
}
source_function_definitions(
  file.path("server", "logic-plots.R"),
  c(
    "get_data_summary",
    "get_data_summary_study",
    "get_interpolated_pdx_data",
    "auc",
    "get_plot_scaled",
    "get_endpoint_measurements",
    "T.C_ratio",
    "get_TC_pairwise_contrasts",
    "response_analysis",
    "get_response_level",
    "IndividualMouseResponse"
  )
)
source_function_definitions(
  file.path("server", "logic-tv.R"),
  "load_default_tv_data"
)

volume_data <- read.csv(file.path("include", "unit_test.csv"), stringsAsFactors = FALSE)
interaction_example <- read.csv(
  file.path("include", "example_combination_interaction.csv"),
  stringsAsFactors = FALSE
)
query_env$colorblind_palette <- rep(c("#0072B2", "#D55E00", "#009E73", "#CC79A7"), 5)
query_env$shape_palette <- rep(c(16, 17, 15, 18), 5)
scaled_interaction_plot <- query_env$get_plot_scaled(
  interaction_example,
  position.dodge = 0.2,
  scale.factor = 4,
  scale.by.volume = FALSE,
  level = "Arm",
  pattern = "Study"
)
stopifnot(inherits(scaled_interaction_plot, "ggplot"))
required_example_columns <- c(
  "Contributor", "Arms", "Times", "Volume", "Study", "ID", "Tumor", "Disease_Type"
)
stopifnot(all(required_example_columns %in% names(interaction_example)))
stopifnot(nrow(interaction_example) == 96L)
stopifnot(all(table(interaction_example$Arms) == 24L))
stopifnot(length(unique(interaction_example$ID)) == 24L)
stopifnot(all(interaction_example$Volume >= 0))
stopifnot(all(interaction_example$Drug_A_Present == ifelse(
  interaction_example$Arms %in% c("Drug A", "Drug A + Drug B"), "Yes", "No"
)))
stopifnot(all(interaction_example$Drug_B_Present == ifelse(
  interaction_example$Arms %in% c("Drug B", "Drug A + Drug B"), "Yes", "No"
)))

default_data <- query_env$load_default_tv_data()
stopifnot(setequal(unique(default_data$Contributor), c("Example_Set_1", "Example_Set_2")))
stopifnot(identical(
  sort(unique(default_data$Contributor)),
  c("Example_Set_1", "Example_Set_2")
))
stopifnot(identical(
  query_env$select_available_choice("INV_1", "Combination_Interaction_Demo"),
  "Combination_Interaction_Demo"
))
stopifnot(identical(
  query_env$select_available_choice("Combination_Interaction_Demo", "Combination_Interaction_Demo"),
  "Combination_Interaction_Demo"
))
stopifnot(is.null(query_env$select_available_choice("INV_1", character())))
stopifnot(sum(default_data$Contributor == "Example_Set_2") == nrow(interaction_example))
second_contributor_query <- query_env$query_tv(
  default_data,
  "Example_Set_2",
  c("Control", "Drug A", "Drug B", "Drug A + Drug B"),
  "Combination_Interaction_Demo",
  "Synthetic model"
)
stopifnot(second_contributor_query$hits_total == nrow(interaction_example))
selected_study <- query_env$select_available_choice(
  "INV_1",
  unique(sort(default_data$Study[default_data$Contributor == "Example_Set_2"]))
)
study_query <- query_env$query_tv(
  default_data,
  "Example_Set_2",
  c("Control", "Drug A", "Drug B", "Drug A + Drug B"),
  selected_study,
  "Synthetic model"
)
tumor_choices <- sort(unique(as.character(study_query$df$Tumor)))
stopifnot(identical(selected_study, "Combination_Interaction_Demo"))
stopifnot(identical(query_env$select_available_choice("PDX1", tumor_choices), "DEMO_001"))
stopifnot(identical(
  levels(second_contributor_query$df$Arms),
  c("Control", "Drug A", "Drug B", "Drug A + Drug B")
))
interaction_anova <- query_env$response_analysis(
  default_data,
  last.measure.day = 21,
  tv_study_filtered = "Combination_Interaction_Demo",
  tv_tumor_filtered = "DEMO_001",
  main_anova_interpolate = FALSE,
  report = TRUE
)
stopifnot(nrow(interaction_anova$mct$Arms) == 6L)

interaction_endpoints <- interaction_example[interaction_example$Times == 21, ]
baseline_volumes <- interaction_example[
  interaction_example$Times == 0, c("ID", "Volume"), drop = FALSE
]
names(baseline_volumes)[2] <- "Baseline.Volume"
interaction_endpoints <- merge(interaction_endpoints, baseline_volumes, by = "ID")
interaction_endpoints$Log2.Fold.Change <- log2(
  interaction_endpoints$Volume / interaction_endpoints$Baseline.Volume
)
interaction_endpoints$Drug_A_Present <- factor(
  interaction_endpoints$Drug_A_Present, levels = c("No", "Yes")
)
interaction_endpoints$Drug_B_Present <- factor(
  interaction_endpoints$Drug_B_Present, levels = c("No", "Yes")
)
interaction_model <- stats::lm(
  Log2.Fold.Change ~ Drug_A_Present * Drug_B_Present,
  data = interaction_endpoints
)
interaction_term <- stats::coef(interaction_model)["Drug_A_PresentYes:Drug_B_PresentYes"]
interaction_p_value <- summary(interaction_model)$coefficients[
  "Drug_A_PresentYes:Drug_B_PresentYes", "Pr(>|t|)"
]
stopifnot(interaction_term < -0.9, interaction_p_value < 1e-6)
stopifnot(isTRUE(all.equal(unname(2^interaction_term), 0.5, tolerance = 0.02)))

query_result <- query_env$query_tv(
  volume_data,
  "Example_Set_1",
  "Control",
  "Study_1",
  "Example Models"
)
expected <- subset(
  volume_data,
  Contributor == "Example_Set_1" & Arms == "Control" &
    Study == "Study_1" & Disease_Type == "Example Models"
)
stopifnot(query_result$hits_total == nrow(expected))
stopifnot(identical(as.character(query_result$df$ID), as.character(expected$ID)))
stopifnot(grepl(paste0("Total hits: ", nrow(expected)), query_result$msg, fixed = TRUE))
stopifnot(grepl('Contributor %in% c("Example_Set_1")', query_result$query_text, fixed = TRUE))
stopifnot(is.factor(query_result$df$Contributor))

multiple_selections <- query_env$query_tv(
  volume_data,
  c("Example_Set_1", "Another Contributor"),
  "Control",
  "Study_1",
  "Example Models"
)
stopifnot(multiple_selections$hits_total == nrow(expected))

missing_contributor <- query_env$query_tv(volume_data, character(), "Control", "Study_1", "Example Models")
missing_treatment <- query_env$query_tv(volume_data, "Example_Set_1", character(), "Study_1", "Example Models")
missing_study <- query_env$query_tv(volume_data, "Example_Set_1", "Control", character(), "Example Models")
missing_disease <- query_env$query_tv(volume_data, "Example_Set_1", "Control", "Study_1", character())
stopifnot(identical(missing_contributor$msg, "Please select the contributor to query from."))
stopifnot(identical(missing_treatment$msg, "Please select at least one Treatment Arm."))
stopifnot(identical(missing_study$msg, "Please select at least one Study"))
stopifnot(identical(missing_disease$msg, "Please select at least one Disease Type."))
stopifnot(all(vapply(
  list(missing_contributor, missing_treatment, missing_study, missing_disease),
  function(result) is.null(result$df),
  logical(1)
)))

no_match <- query_env$query_tv(volume_data, "unknown", "Control", "Study_1", "Example Models")
stopifnot(is.null(no_match$df))
stopifnot(identical(no_match$msg, "No matches found. Please try another query."))

query_env$plot_ly <- function(...) NULL
interpolation_data <- data.frame(
  Contributor = c("C1", "C1"),
  Study = c("S1", "S1"),
  Tumor = c("T1", "T1"),
  Arms = c("Control", "Control"),
  ID = c("M1", "M1"),
  Times = c(0, 2),
  Volume = c(10, 30)
)
interpolated <- query_env$get_interpolated_pdx_data(interpolation_data)
missing_day <- interpolated[interpolated$Times == 1, , drop = FALSE]
stopifnot(nrow(missing_day) == 1L)
stopifnot(isTRUE(all.equal(missing_day$Interpolated_Volume, 20)))
stopifnot(identical(as.character(missing_day$Arms), "Control"))

late_start_data <- transform(interpolation_data, Times = c(5, 7))
late_start <- query_env$get_interpolated_pdx_data(late_start_data)
late_middle <- late_start[late_start$Times == 6, , drop = FALSE]
stopifnot(identical(late_start$Times, c(5, 6, 7)))
stopifnot(nrow(late_middle) == 1L)
stopifnot(isTRUE(all.equal(late_middle$Interpolated_Volume, 20)))

one_day_data <- interpolation_data[1, , drop = FALSE]
one_day_result <- query_env$get_interpolated_pdx_data(one_day_data)
stopifnot(nrow(one_day_result) == 1L)
stopifnot(isTRUE(all.equal(one_day_result$Interpolated_Volume, 10)))
stopifnot(is.null(query_env$get_interpolated_pdx_data(NULL)))
stopifnot(is.null(query_env$get_interpolated_pdx_data(data.frame())))

mixed_animal_data <- rbind(
  interpolation_data,
  transform(one_day_data, ID = "M2", Volume = 7)
)
mixed_interpolation <- query_env$get_interpolated_pdx_data(mixed_animal_data)
single_animal <- mixed_interpolation[mixed_interpolation$ID == "M2", , drop = FALSE]
stopifnot(nrow(single_animal) == 1L)
stopifnot(isTRUE(all.equal(single_animal$Interpolated_Volume, 7)))

stopifnot(isTRUE(all.equal(query_env$auc(c(0, 1, 2), c(0, 1, 0), type = "linear"), 1)))
stopifnot(isTRUE(all.equal(query_env$auc(c(0, 1, 2), c(0, 1, 0), from = 0, to = 1), 0.5)))
stopifnot(isTRUE(all.equal(query_env$auc(c(0, 2), c(-1, 1), type = "linear", absolutearea = TRUE), 1)))
stopifnot(is.na(query_env$auc(c(2), c(10))))
stopifnot(inherits(tryCatch(query_env$auc(c(0, 1), 1), error = identity), "error"))

response_data <- data.frame(
  Contributor = rep("C1", 8),
  Study = rep("S1", 8),
  Tumor = rep("T1", 8),
  Arms = rep(c("CR", "PR", "SD", "PD"), each = 2),
  ID = rep(c("MCR", "MPR", "MSD", "MPD"), each = 2),
  Times = rep(c(0, 10), 4),
  Volume = c(100, 5, 100, 30, 100, 110, 100, 150)
)
response <- query_env$get_response_level(response_data, 9)
response_by_arm <- setNames(response$Response.Level$Response.Level, response$Response.Level$Arms)
stopifnot(identical(unname(response_by_arm[c("CR", "PR", "SD", "PD")]), c("CR", "PR", "SD", "PD")))
stopifnot(all(response$mouse.info$Nearest.Measure.Day.Avail == 10))

individual_response <- query_env$IndividualMouseResponse(response_data, 9)
individual_by_arm <- setNames(individual_response$ORC, individual_response$Arms)
stopifnot(identical(unname(individual_by_arm[c("CR", "PR", "SD", "PD")]), c("CR", "PR", "SD", "PD")))
stopifnot(all(individual_response$VC.LastDay == 10))
stopifnot(all(is.finite(individual_response$AUC.All.Measures)))

single_arm <- response_data[response_data$Arms == "SD", , drop = FALSE]
t_c_error <- tryCatch(query_env$T.C_ratio(single_arm), error = identity)
stopifnot(inherits(t_c_error, "error"))
stopifnot(grepl("only 1 group", conditionMessage(t_c_error), fixed = TRUE))
no_control <- response_data[response_data$Arms %in% c("PR", "SD"), , drop = FALSE]
no_control_error <- tryCatch(query_env$T.C_ratio(no_control), error = identity)
stopifnot(inherits(no_control_error, "error"))
stopifnot(grepl("without a Control group", conditionMessage(no_control_error), fixed = TRUE))

summary_schema_error <- tryCatch(
  query_env$get_data_summary_study(data.frame(Volume = 10), "Volume", "Arms"),
  error = identity
)
stopifnot(inherits(summary_schema_error, "error"))
stopifnot(grepl("must include at least four columns", conditionMessage(summary_schema_error), fixed = TRUE))

if (requireNamespace("plyr", quietly = TRUE)) {
  query_env$ddply <- plyr::ddply
  cross_study_summary <- query_env$get_data_summary(
    response_data,
    "normal",
    "Volume",
    c("Arms")
  )
  stopifnot(nrow(cross_study_summary) == 4L)
  stopifnot(all(c("N", "Volume", "SD", "SE") %in% names(cross_study_summary)))
  raw_result <- query_env$get_data_summary(
    response_data,
    "raw",
    "Volume",
    c("Arms")
  )
  stopifnot(identical(raw_result, response_data))
  summary <- query_env$get_data_summary_study(response_data, "Volume", c("Arms"))
  stopifnot(nrow(summary) == 4L)
  stopifnot(all(c("N", "Volume", "SD", "SE") %in% names(summary)))
} else {
  cat("SKIP: grouped volume summaries require the app's plyr dependency.\n")
}

combination_data <- expand.grid(
  Times = c(0, 5, 10),
  animal = seq_len(4),
  Arms = c("Control", "B", "C", "B+C"),
  stringsAsFactors = FALSE
)
combination_data$Contributor <- "C1"
combination_data$Study <- "S1"
combination_data$Tumor <- "T1"
combination_data$ID <- paste(combination_data$Arms, combination_data$animal)
arm_endpoint_ratio <- c(Control = 1.8, B = 1.2, C = 0.9, `B+C` = 0.5)
animal_offset <- combination_data$animal * 0.08
combination_data$Volume <- (100 + combination_data$animal * 5) * (
  1 + (arm_endpoint_ratio[combination_data$Arms] + animal_offset - 1) * combination_data$Times / 10
)
combination_data$Volume[
  combination_data$Arms == "B+C" & combination_data$animal == 1 & combination_data$Times == 10
] <- 0
combination_data <- combination_data[
  !(combination_data$Arms == "B+C" & combination_data$animal == 4 & combination_data$Times == 10),
]
combination_data <- rbind(
  combination_data,
  data.frame(
    Times = c(0, 10), animal = 5, Arms = "B", Contributor = "C1", Study = "S1",
    Tumor = "T1", ID = "B_zero_baseline", Volume = c(0, 30)
  )
)

endpoint_measurements <- query_env$get_endpoint_measurements(
  combination_data, c("Arms", "ID"), "Times", "Volume", target_day = 10
)
stopifnot(all(endpoint_measurements$Endpoint.Day == 10))
stopifnot(!"B_zero_baseline" %in% endpoint_measurements$ID)
stopifnot(any(endpoint_measurements$Arms == "B+C" & endpoint_measurements$TV.ratio == 0))
stopifnot(sum(endpoint_measurements$Arms == "B+C") == 3L)

interpolated_endpoint <- query_env$get_endpoint_measurements(
  combination_data, c("Arms", "ID"), "Times", "Volume", target_day = 7
)
stopifnot(all(interpolated_endpoint$Endpoint.Day == 7))
stopifnot(isTRUE(all.equal(
  interpolated_endpoint$TV.ratio[interpolated_endpoint$ID == "B 2"],
  (1 + (arm_endpoint_ratio[["B"]] + 2 * 0.08 - 1) * 0.7)
)))
stopifnot(nrow(query_env$get_endpoint_measurements(
  combination_data, c("Arms", "ID"), "Times", "Volume", target_day = 11
)) == 0L)
stopifnot(nrow(query_env$get_endpoint_measurements(
  combination_data, c("Arms", "ID"), "Times", "Volume", target_day = NA_real_
)) == 0L)
default_endpoint <- query_env$get_endpoint_measurements(
  combination_data, c("Arms", "ID"), "Times", "Volume"
)
stopifnot(all(default_endpoint$Endpoint.Day == 5))

tc_result <- query_env$T.C_ratio(combination_data, last.measure.day = 10)
stopifnot(setequal(as.character(tc_result$Arms), c("B", "C", "B+C")))
stopifnot(identical(levels(tc_result$Arms), c("B", "C", "B+C")))
stopifnot(all(tc_result$TC.CalcDay == 10))
stopifnot(tc_result$n.TVratio[tc_result$Arms == "B+C"] == 3L)
stopifnot(isTRUE(all.equal(
  tc_result$mean.TVratio[tc_result$Arms == "B+C"],
  mean(endpoint_measurements$TV.ratio[endpoint_measurements$Arms == "B+C"])
)))
stopifnot(is.finite(tc_result$aov.TC.ratio[tc_result$Arms == "B+C"]))
stopifnot(all(is.finite(tc_result$se_aov.TC.ratio)))

control_endpoints <- endpoint_measurements[endpoint_measurements$Arms == "Control", ]
b_endpoints <- endpoint_measurements[endpoint_measurements$Arms == "B", ]
b_result <- tc_result[tc_result$Arms == "B", ]
expected_b_ratio <- mean(b_endpoints$TV.ratio) / mean(control_endpoints$TV.ratio)
expected_b_se <- sqrt(
  stats::var(b_endpoints$TV.ratio) /
    (nrow(b_endpoints) * mean(control_endpoints$TV.ratio)^2) +
    mean(b_endpoints$TV.ratio)^2 * stats::var(control_endpoints$TV.ratio) /
      (nrow(control_endpoints) * mean(control_endpoints$TV.ratio)^4)
)
stopifnot(isTRUE(all.equal(b_result$TC.ratio, expected_b_ratio)))
stopifnot(isTRUE(all.equal(b_result$se_TC.ratio, expected_b_se)))

endpoint_measurements$Arms <- factor(
  endpoint_measurements$Arms,
  levels = query_env$order_treatment_arms(endpoint_measurements$Arms)
)
endpoint_measurements$log2.TV.ratio <- log2(pmax(endpoint_measurements$TV.ratio, 0.001))
ratio_model <- stats::lm(log2.TV.ratio ~ Arms, data = endpoint_measurements)
ratio_model_summary <- summary(ratio_model)$coefficients
ratio_model_tukey <- stats::TukeyHSD(stats::aov(ratio_model))$Arms
for (arm in c("B", "C", "B+C")) {
  row <- tc_result[tc_result$Arms == arm, ]
  arm_endpoints <- endpoint_measurements[endpoint_measurements$Arms == arm, ]
  coefficient_name <- paste0("Arms", arm)
  stopifnot(isTRUE(all.equal(
    as.numeric(row$mean.TVratio),
    mean(arm_endpoints$TV.ratio)
  )))
  stopifnot(isTRUE(all.equal(
    as.numeric(row$var.TVratio),
    stats::var(arm_endpoints$TV.ratio)
  )))
  stopifnot(row$n.TVratio == nrow(arm_endpoints))
  stopifnot(isTRUE(all.equal(as.numeric(row$mean.dVt), mean(arm_endpoints$dVt))))
  stopifnot(isTRUE(all.equal(
    as.numeric(row$SE.dVt), stats::sd(arm_endpoints$dVt) / sqrt(nrow(arm_endpoints))
  )))
  stopifnot(isTRUE(all.equal(
    as.numeric(row$mean.log2FC), mean(log2(pmax(arm_endpoints$TV.ratio, 0.001)))
  )))
  stopifnot(isTRUE(all.equal(
    as.numeric(row$TC.ratio),
    mean(arm_endpoints$TV.ratio) / mean(control_endpoints$TV.ratio)
  )))
  stopifnot(isTRUE(all.equal(
    as.numeric(row$aov.TC.ratio),
    as.numeric(2^stats::coef(ratio_model)[coefficient_name])
  )))
  stopifnot(isTRUE(all.equal(
    as.numeric(row$se_aov.TC.ratio),
    as.numeric(row$aov.TC.ratio * log(2) * ratio_model_summary[coefficient_name, "Std. Error"])
  )))
  stopifnot(isTRUE(all.equal(
    as.numeric(row$Contrast.pValue),
    as.numeric(ratio_model_tukey[paste0(arm, "-Control"), "p adj"])
  )))
}

pairwise_result <- query_env$get_TC_pairwise_contrasts(combination_data, 10)
stopifnot(nrow(pairwise_result) == 6L)
stopifnot(identical(
  pairwise_result$Contrast,
  c("Control / B", "Control / C", "Control / B+C", "B / C", "B / B+C", "C / B+C")
))
pairwise_keys <- paste(
  pmin(pairwise_result$Arm.1, pairwise_result$Arm.2),
  pmax(pairwise_result$Arm.1, pairwise_result$Arm.2),
  sep = "|"
)
stopifnot(all(c("B|B+C", "B+C|C") %in% pairwise_keys))
stopifnot(all(pairwise_result$TC.CalcDay == 10))
for (i in seq_len(nrow(pairwise_result))) {
  contrast <- pairwise_result[i, ]
  tukey_row <- paste(contrast$Arm.2, contrast$Arm.1, sep = "-")
  expected <- ratio_model_tukey[tukey_row, ]
  stopifnot(isTRUE(all.equal(
    as.numeric(contrast$Fold.Change.Ratio), as.numeric(2^(-expected["diff"]))
  )))
  stopifnot(isTRUE(all.equal(
    as.numeric(contrast$CI.Lower), as.numeric(2^(-expected["upr"]))
  )))
  stopifnot(isTRUE(all.equal(
    as.numeric(contrast$CI.Upper), as.numeric(2^(-expected["lwr"]))
  )))
  stopifnot(isTRUE(all.equal(
    as.numeric(contrast$Adjusted.pValue), as.numeric(expected["p adj"])
  )))
}
for (arm in c("B", "C", "B+C")) {
  pairwise_control <- pairwise_result[
    pairwise_result$Arm.1 == "Control" & pairwise_result$Arm.2 == arm,
  ]
  summary_control <- tc_result[tc_result$Arms == arm, ]
  stopifnot(isTRUE(all.equal(
    as.numeric(pairwise_control$Adjusted.pValue),
    as.numeric(summary_control$Contrast.pValue)
  )))
}

anova_result <- query_env$response_analysis(
  combination_data,
  last.measure.day = 10,
  tv_study_filtered = "S1",
  tv_tumor_filtered = "T1",
  main_anova_interpolate = FALSE,
  report = TRUE
)
stopifnot(nrow(anova_result$mct$Arms) == 6L)
stopifnot(all(c("B-Control", "C-Control", "B+C-Control", "C-B", "B+C-B", "B+C-C") %in%
                rownames(anova_result$mct$Arms)))
anova_endpoints <- endpoint_measurements
anova_endpoints$Arms <- stats::relevel(factor(anova_endpoints$Arms), "Control")
expected_tukey <- stats::TukeyHSD(stats::aov(endpoint_volume ~ Arms, data = anova_endpoints))$Arms
stopifnot(isTRUE(all.equal(anova_result$mct$Arms, expected_tukey)))
