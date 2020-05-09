# assistant function to to make stuff cleaner
pk2tr.assist <- function(data) {


  if(!all(is.na(data))) { # Has to have some values to compare

    pk <- max(data, na.rm=TRUE) # peak
    pkIndex <- which.max(data) # where peak is

    tr <- min(data[pkIndex:length(data)], na.rm=TRUE) # trough found after peak

    result <- (pk - tr) / pk * 100 # returns a percentage

  } else result <- NA # if no values to look at, return NA
  return(result)
}

#' Peak To Trough
#'
#' Finds the difference between the maximum of a respiratory value and the
#' minimum following the peak.
#'
#' @param data Takes a dataframe with columns c(Mouse, MCh, variable, value) as
#'   input
#' @export
#' @return A dataframe containing the percent decrease from the peak of a
#'   respiratory variable to the next minimum.
pk2tr <- function(data) {
  #fixing the factors
  data$Mouse <- data$Mouse %>% as.factor()
  data$MCh <- data$MCh  %>% as.factor()

  # melt into long form

  vars <- levels(data$variable)

  mice <- levels(data$Mouse)
  mouseCount <- length(mice)

  doses <- levels(data$MCh)
  doseCount <- length(doses)

  # #making result df
  # result <- as.data.frame(matrix(ncol=8, nrow=mouseCount*doseCount))
  # colnames(result) <- c('Mouse', 'MCh', 'Rrs.pk2tr', 'Crs.pk2tr', 'Ers.pk2tr',
  #                       'Rn.pk2tr', 'G.pk2tr', 'H.pk2tr')
  # result$MCh <- doses
  # result$Mouse <- sapply(mice, function(x) rep(x, each=6)) %>% as.vector()
  # result$Mouse <- mice
  #

  # ^ change this to use var list using paste (more general)

  #initializing result df

  result <- as.data.frame(matrix(ncol=4, nrow=0))
  colnames(result) <- c('Mouse', 'MCh', 'variable', 'value')

  for (mouse in mice) {

    # initializing result df for individual mouse
    mouse.df <- as.data.frame(matrix(ncol=4, nrow=0))
    colnames(mouse.df) <- c('Mouse', 'MCh', 'variable', 'value')

    for (respVar in vars) {

      selectedVar <- filter(data, variable == respVar)
      # by(selectedVar, selectedVar$Mouse , function(x) pk2tr.assist(x$value)) %>% as.vector()

      # constructing result df for indiv respvar of indiv mouse
      var.df <- data.frame(Mouse = mouse,
                           MCh = doses,
                           variable = respVar,
                           value = by(selectedVar,
                                      selectedVar$MCh,
                                      function(x) pk2tr.assist(x$value)) %>% as.vector()
      )

      # appending var.df to mouse.df
      mouse.df <- rbind(mouse.df, var.df)
    }

    #appending mouse df to result df

    result <- rbind(result, mouse.df)

  }

  return(result)

}


