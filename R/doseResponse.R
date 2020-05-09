# dose response slope

#' Methacholine Dose Response Curve
#'
#' @param data Takes a dataframe with columns c(Mouse, MCh, variable, value) as
#'   input
#' @param respVar A string containing the respiratory variable you want eg. 'Rn'
#' @export
#' @return A linear regression model.
#' @examples
#' if (requireNamespace("magrittr", quietly = TRUE)) {
#' summary(doseResp(df))
#' }
doseResp <- function(data, respVar) {
  data$Mouse <- data$Mouse %>% as.factor()
  data$MCh <- data$MCh  %>% as.factor()

  selection <- filter(data, variable == respVar)
  data$MCh <- data$MCh %>% as.numeric()
  linearMod <- lm(value ~ MCh, data = selection)
  return(linearMod)
}
