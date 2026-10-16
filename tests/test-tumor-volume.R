suppressPackageStartupMessages({
  library(dplyr)
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
  timeout_clock$now <- timeout_clock$now + 839
  session$elapse(5000)
  stopifnot(!session$env$idle_warning_shown)
  timeout_clock$now <- timeout_clock$now + 1
  session$elapse(5000)
  stopifnot(session$env$idle_warning_shown, !session$isClosed())

  session$setInputs(analyzer_continue_session = 1)
  stopifnot(!session$env$idle_warning_shown)
  stopifnot(identical(session$env$last_activity, timeout_clock$now))
  timeout_clock$now <- timeout_clock$now + 840
  session$elapse(5000)
  stopifnot(session$env$idle_warning_shown)

  session$setInputs(analyzer_activity = 1)
  stopifnot(!session$env$idle_warning_shown)
  stopifnot(identical(session$env$last_activity, timeout_clock$now))
  timeout_clock$now <- timeout_clock$now + 899
  session$elapse(5000)
  stopifnot(!session$isClosed())
  timeout_clock$now <- timeout_clock$now + 1
  session$elapse(5000)
  stopifnot(session$isClosed())
})

shiny::testServer(timeout_server, {
  session$flushReact()
  timeout_clock$now <- timeout_clock$now + 900
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
    "T.C_ratio",
    "get_response_level",
    "IndividualMouseResponse"
  )
)

volume_data <- read.csv(file.path("include", "unit_test.csv"), stringsAsFactors = FALSE)
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

if (requireNamespace("multcomp", quietly = TRUE)) {
  suppressPackageStartupMessages(library(multcomp))
  tc_data <- data.frame(
    Contributor = rep("C1", 8),
    Study = rep("S1", 8),
    Tumor = rep("T1", 8),
    Arms = rep(c("Control", "Drug"), each = 4),
    ID = rep(c("C1", "C2", "T1", "T2"), each = 2),
    Times = rep(c(0, 10), 4),
    Volume = c(100, 120, 110, 132, 100, 60, 90, 54)
  )
  tc_result <- query_env$T.C_ratio(tc_data, last.measure.day = 10)
  stopifnot(nrow(tc_result) == 1L)
  stopifnot(identical(as.character(tc_result$Arms), "Drug"))
  stopifnot(is.finite(tc_result$TC.ratio))
} else {
  cat("SKIP: T/C ratio significance analysis requires the app's multcomp dependency.\n")
}
