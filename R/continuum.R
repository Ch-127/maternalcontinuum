#' Re-encode a binary variable
#'
#' @param x A vector containing binary survey responses.
#' @return A numeric vector where 1 is "Yes/Completed" and 0 is "No/Incomplete".
#' @export
recode_binary <- function(x) {
  z <- trimws(tolower(as.character(x)))
  yes_values <- c("yes", "y", "1", "true", "completed", "complete")
  no_values  <- c("no", "n", "0", "false", "not completed", "incomplete")

  out <- rep(NA_integer_, length(z))
  out[z %in% yes_values] <- 1L
  out[z %in% no_values] <- 0L
  out
}

#' Calculate the Maternal Healthcare Continuum (CoC)
#'
#' @param data A data frame containing the ANC, SBA, and PNC variables.
#' @param anc_var The name of the ANC variable (as a string).
#' @param sba_var The name of the SBA variable (as a string).
#' @param pnc_var The name of the PNC variable (as a string).
#'
#' @return The original data frame with three new columns (ANC_completed, SBA_completed, PNC_completed) and a final CoC_completion column.
#' @export
calculate_continuum <- function(data, anc_var, sba_var, pnc_var) {

  if (!anc_var %in% names(data)) stop(paste("Column", anc_var, "not found in data."))
  if (!sba_var %in% names(data)) stop(paste("Column", sba_var, "not found in data."))
  if (!pnc_var %in% names(data)) stop(paste("Column", pnc_var, "not found in data."))

  data$ANC_completed <- recode_binary(data[[anc_var]])
  data$SBA_completed <- recode_binary(data[[sba_var]])
  data$PNC_completed <- recode_binary(data[[pnc_var]])

  data$CoC_completion <- ifelse(
    data$ANC_completed == 1 & data$SBA_completed == 1 & data$PNC_completed == 1, 1,
    ifelse(
      data$ANC_completed == 0 | data$SBA_completed == 0 | data$PNC_completed == 0, 0, NA
    )
  )

  return(data)
}
