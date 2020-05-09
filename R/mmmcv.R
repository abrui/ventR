# max min mean cv

#' Max Min Mean CV
#'
#' Calculates the maximum, minimum, mean, or percent coefficient of variance of
#' a respiratory variable value
#'
#' @param data Takes a dataframe with columns c(Mouse, MCh, variable, value) as
#'   input
#' @param measurements A vector containing one or more of the following:
#'   c('max', 'min', 'mean', 'cv')
#' @export
#' @return A dataframe in long form containing the selected measurements for
#'   each respiratory variable, mouse, and methacholine dose.
mmmcv <- function(data, measurements) {
  # measurements is a vector containing what measurements should be returned
  # for each MCh dosage for each mouse for each respiratory variable
  # eg c('max', 'min, 'mean', 'cv')

  #fixing the factors
  data$Mouse <- data$Mouse %>% as.factor()
  data$MCh <- data$MCh  %>% as.factor()

  respVars <- levels(data$variable)

  mice <- levels(data$Mouse)
  mouseCount <- length(mice)

  doses <- levels(data$MCh)
  doseCount <- length(doses)


  result <- as.data.frame(matrix(ncol=5, nrow=0))
  colnames(result) <- c('Mouse', 'MCh', 'respVar', 'variable', 'value')

  for (mouse in mice) {

    # initializing result df for individual mouse
    mouse.df <- as.data.frame(matrix(ncol=5, nrow=0))
    colnames(mouse.df) <- c('Mouse', 'MCh', 'respVar', 'variable', 'value')

    for (selectRespVar in respVars) {
      # constructing result df for indiv respvar of indiv mouse

      selectedVar <- filter(data, variable == selectRespVar)


      var.df <- as.data.frame(matrix(ncol=5, nrow=0))
      colnames(var.df) <- c('Mouse', 'MCh', 'respVar', 'variable', 'value')

      for (measurement in measurements) {
        # collecting individual measurements, then appending them to the var.df

        if (measurement == 'max') {

          measure.df <- data.frame(Mouse = mouse,
                                   MCh = doses,
                                   respVar = selectRespVar,
                                   variable = 'Max',
                                   value = by(selectedVar,
                                              selectedVar$MCh,
                                              function(x) max(x$value, na.rm = TRUE)) %>% as.vector()
          )


        } else if (measurement == 'min') {

          measure.df <- data.frame(Mouse = mouse,
                                   MCh = doses,
                                   respVar = selectRespVar,
                                   variable = 'Min',
                                   value = by(selectedVar,
                                              selectedVar$MCh,
                                              function(x) min(x$value, na.rm = TRUE)) %>% as.vector()
          )

        } else if (measurement == 'mean') {

          measure.df <- data.frame(Mouse = mouse,
                                   MCh = doses,
                                   respVar = selectRespVar,
                                   variable = 'Mean',
                                   value = by(selectedVar,
                                              selectedVar$MCh,
                                              function(x) mean(x$value, na.rm = TRUE)) %>% as.vector()
          )

        } else if (measurement == 'cv') {

          measure.df <- data.frame(Mouse = mouse,
                                   MCh = doses,
                                   respVar = selectRespVar,
                                   variable = 'CV',
                                   value = by(selectedVar,
                                              selectedVar$MCh,
                                              function(x) sd(x$value, na.rm = TRUE) / mean(x$value, na.rm = TRUE) * 100) %>% as.vector()
          )

        } else {
          #throw an error
        }

        #appending measure results
        var.df <- rbind(var.df, measure.df)

      }


      # appending var.df to mouse.df
      mouse.df <- rbind(mouse.df, var.df)
    }

    #appending mouse df to result df

    result <- rbind(result, mouse.df)

  }


  return(result)

}
