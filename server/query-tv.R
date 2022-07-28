
# Query TV Data
query_tv <- function(tv_df, tv_contributor, tv_treatment, tv_disease_type) {

  # 1.2 Contributor

  if (length(tv_contributor) < 0.5) {
    df <- NULL
    df_raw <- NULL
    msg <- "Please select the contributor to query from."
    return(list("df" = df, "msg" = msg, "query_text" = "N/A", "hits_total" = 0))
  }

  source_list <- tv_df$Contributor

  # 1.3 Treatment

  if (length(tv_treatment) < 0.5) {
    df <- NULL
    df_raw <- NULL
    msg <- "Please select at least one Treatment."
    return(list("df" = df,  "msg" = msg, "query_text" = "N/A", "hits_total" = 0))
  }

  treatment_list <- tv_df$Arms

  # 1.4 Disease Type

  if (length(tv_disease_type) < 0.5) {
    df <- NULL
    df_raw <- NULL
    msg <- "Please select at least one Disease Type."
    return(list("df" = df, "msg" = msg, "query_text" = "N/A", "hits_total" = 0))
  }

  disease_type <- tv_df$Disease_Type

  # 2. query the in-memory task log data frame

  df <- base::subset(
    tv_df, source_list %in% tv_contributor &
      treatment_list %in% tv_treatment &
      disease_type %in% tv_disease_type
  )

  rownames(df) <- NULL

  # total hits
  hits_tot <- nrow(df)

  if (hits_tot == 0L) {
    df <- NULL
    df_raw <- NULL
    msg <- "No matches found. Please try another query."
    return(list("df" = df,  "msg" = msg, "query_text" = "N/A", "hits_total" = 0))
  }

  # make factors so DT can have dropdowns for filter boxes
  df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)

  # message
  pretee <- function(x) formatC(as.numeric(x), format = "d", big.mark = ",")

  msg <- paste0(
    "Total hits: ", pretee(hits_tot), ". ",
    "Showing ", pretee(hits_tot), " hits."
  )

  # query string
  q_final <- paste0(
    "Contributor %in% ", paste0('c("', paste(tv_contributor, collapse = '", "'), '")'),
    " & Treatment %in% ", paste0('c("', paste(tv_treatment, collapse = '", "'), '")'),
    " & Disease Type %in% ", paste0('c("', paste(tv_disease_type, collapse = '", "'), '")')
  )

  list("df" = df, "msg" = msg, "query_text" = q_final, "hits_total" = hits_tot)
}
