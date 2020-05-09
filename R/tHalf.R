
# Assistance Functions ------------------------------------------
tHalfAssist <- function(data) {
  # does the modelling and error handling

  #setting up t
  t <- 1:(length(data$value))

  modelSuccess <- tryCatch(

    { # trying to make a nonlinear least squares self start
      fit <- nls(value ~ SSasymp(t, yf, y0, log_alpha), data = data)
      message('model success')
    },

    error=function(cond){ # modelSuccess is FALSE if an error is thrown
      # during the model fit

      message(cond)
      return(FALSE)

    }
  )

  #checking that there was a fit
  if (is.null(modelSuccess)) {

    #extracting model parameters and solving for THALF
    modelParameters <- summary(fit)[["parameters"]]

    yf <- modelParameters[1,1]
    y0 <- modelParameters[2,1]
    alpha <- exp(modelParameters[3,1])

    thalf <- log( (0.5-yf) /  (y0 - yf) ) / (-alpha) #solving

    result <- c(thalf, sigma(fit))
  } else {
    #outputting an NA if the model didn't work
    result <- c(NA,NA)
  }

  return(result)

}

tHalfNormalization <- function(data) {
  # normalizes resp var values within a Mch dosage
  # note: is index agnostic, ie the minimum can occur before the maximum
  # if that occurs, the model will almost certainly through an NA

  #set values as max value = 1.0, min = 0.0

  max <- max(data$value, na.rm = TRUE)
  min <- min(data$value, na.rm = TRUE)

  result <- sapply(data$value, function(x) (x-min)/(max-min))

  return(result)


}

# --------------------------------------------------

#' T-Half
#'
#' Calculates the time it takes a respiratory variable to go from a peak after a
#' methacholine insult to a minimum. Uses a self-start nonlinear regression
#' model. It will spit out a ton of stuff during use; don't worry about it.
#'
#' @param data Takes a dataframe with columns c(Mouse, MCh, variable, value) as
#'   input
#' @param timeInterval The time interval between observations.
#' @export
#' @return A dataframe containing the T-Half for each respiratory variable of
#'   each mouse of each methacholine insult.
tHalf <- function(data, timeInterval) {
  #fixing the factors
  data$Mouse <- data$Mouse %>% as.factor()
  data$MCh <- data$MCh  %>% as.factor()


  # grabbing parameters
  vars <- levels(data$variable)

  mice <- levels(data$Mouse)
  mouseCount <- length(mice)

  doses <- levels(data$MCh)
  doseCount <- length(doses)

  #initializing result df

  result <- as.data.frame(matrix(ncol=5, nrow=0))
  colnames(result) <- c('Mouse', 'MCh', 'variable', 'value', 'residual.std.error')

  for (mouse in mice) {
    for (respVar in vars) {
      for (dose in doses) {
        # selecting the data
        selection <- filter(data,
                            Mouse == mouse,
                            variable == respVar,
                            MCh == dose)
        # normalizing the values
        selection$value <- tHalfNormalization(selection)
        # finding the t Half
        newtHalf <- c(mouse, dose, respVar, tHalfAssist(selection)[1] * timeInterval, tHalfAssist(selection)[2]) %>% t() %>% as.data.frame()
        colnames(newtHalf) <- colnames(result)
        # had to convert newtHalf to df as rbinding vectors is weird
        result <- rbind(result, newtHalf)
      }

    }

  }
  return(result)
}

