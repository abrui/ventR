# provocative concentration

#' Provocative Concentration
#'
#' Calculates the methacholine dose required to change a respiratory variable
#' past a given threshold.
#'
#' @param data Takes a dataframe with columns c(Mouse, MCh, variable, value) as
#'   input
#' @param respVar A string containing the respiratory variable you want eg. 'Rn'
#' @param threshold An integer value of the desired threshold. Eg. a PD20 would
#'   have a threshold of 1.2
#' @export
#' @return An integer value of the provocative concentration
#' @examples
#' provoConc(df, 'Rn', 1.2)
#' provoConc(df, 'Rn', 2.0)
#' @section Citation: \code{Sugget, J. A., Nagel, M. W., & Mitchell, J. P.
#'   (n.d.). The Methacholine Challenge Test for Reversible Airways Disease
#'   Assessment: A Practical Guide on How to Interpret New 2017 ERS Guidelines.
#'   Retrieved from https://www.trudellmed.com/node/227}
#'
provoConc <- function(data, respVar, threshold) {
  data$Mouse <- data$Mouse %>% as.factor()
  data$MCh <- data$MCh  %>% as.factor()

  if(!('0' %in% data$MCh)) stop('No saline condition for baseline')

  selection <- filter(data, variable == respVar)
  baseline <- mean(filter(data, MCh == '0')$value, na.rm = TRUE)


  for (i in seq_along(selection$value)) {
    if (selection$value[i] >= baseline * threshold) {
      # message(sprintf('threshold at %s', i))
      # dose at which threshold was breached
      D1 <- selection$MCh[i] %>% as.numeric()
      # dose before
      D2 <- levels(selection$MCh)[which(levels(selection$MCh) == D1) - 1] %>% as.numeric()

      #percentage above baseline for doses
      R1 <- (selection$value[i] / baseline) * 100
      R2 <- (max(filter(selection, MCh == D2)$value, na.rm = TRUE) / baseline) * 100
      break
    }
  }

  # Sugget, J. A., Nagel, M. W., & Mitchell, J. P. (n.d.). The Methacholine
  # Challenge Test for Reversible Airways Disease Assessment: A Practical Guide
  # on How to Interpret New 2017 ERS Guidelines. Retrieved from
  # https://www.trudellmed.com/node/227

  # if ()

  PD <- exp( (log(D1) + ((log(D2) - log(D1)) * (((threshold - 1) * 100) - R1)) / (R2 - R1)) )

  return(PD)
}
