#' Simulate rhythmic data
#'
#' Simulate rhythmic data using cosinor regression.
#'
#' @param time_series numeric vector for time (x-axis). If NULL (default), a [0, 24] hour time series will be created by `time_gap` steps
#' @param time_gap Difference between 2 time points. Used only when `time_series = NULL`. (default = 1)
#' @param period Period of rhythmic data (default = 24)
#' @param amplitude Amplitude of rhythmic data (default = 0.5)
#' @param acrophase Acrophase (peak time) of rhythmic data (default = 12)
#' @param mesor MESOR of rhythmic data (default = 1)
#' @param noise Add Gaussian noise to data (default = NULL). Passed to `sd` parameter of `rnorm`.
#' @param plot Boolean to plot of simulated data. (default = FALSE)
#' @param ... Further parameters for `plot`
#'
#' @return
#' Simulated rhythmic data (y-axis)
#'
#' @importFrom graphics abline
#' @export
#'
#' @examples
#'
#' set.seed(666)
#' simulate_rhythm()
#' simulate_rhythm(plot = TRUE)
#' simulate_rhythm(time_gap = 0.5, plot = TRUE)
#' simulate_rhythm(time_series = 1:48, plot = TRUE)
#' simulate_rhythm(time_series = 1:24, period = 12, plot = TRUE)
#' simulate_rhythm(time_series = 1:24, amplitude = 1, plot = TRUE)
#' simulate_rhythm(time_series = 1:24, acrophase = 3, plot = TRUE)
#' simulate_rhythm(time_series = 1:24, mesor = 10, plot = TRUE)
#'
#' par(mfrow = c(2,2))
#' simulate_rhythm(time_series = 1:24,
#'                 mesor = 10,
#'                 amplitude = 1,
#'                 acrophase = 6,
#'                 plot = TRUE,
#'                 main = "noise = 0")
#' simulate_rhythm(time_series = 1:24,
#'                 mesor = 10,
#'                 amplitude = 1,
#'                 acrophase = 6,
#'                 plot = TRUE,
#'                 noise = 0.1,
#'                 main = "noise = 0.1")
#' simulate_rhythm(time_series = 1:24,
#'                 mesor = 10,
#'                 amplitude = 1,
#'                 acrophase = 6,
#'                 plot = TRUE,
#'                 noise = 0.5,
#'                 main = "noise = 0.5")
#' simulate_rhythm(time_series = 1:24,
#'                 mesor = 10,
#'                 amplitude = 1,
#'                 acrophase = 6,
#'                 plot = TRUE,
#'                 noise = 1,
#'                 main = "noise = 1.0")


simulate_rhythm <- function(time_series = NULL,
                            time_gap = 1,
                            period = 24,
                            amplitude = 0.5,
                            acrophase = 12,
                            mesor = 1,
                            noise = NULL,
                            plot = FALSE,
                            ...){

  if(is.null(time_series)){
    time_series <- seq(from = 0, to = 24, by = time_gap)
  }

  if(!is.numeric(time_gap)){
    stop("'time_gap' must be numeric")
  }

  if(!is.numeric(period) | period <= 0 ){
    stop("'period' must be a positive numeric")
  }

  if(!is.numeric(amplitude) | amplitude <= 0 ){
    stop("'amplitude' must be a positive numeric")
  }

  if(!is.numeric(acrophase) | acrophase <= 0 ){
    stop("'acrophase' must be a positive numeric")
  }

  if(!is.numeric(mesor)){
    stop("'mesor' must be an numeric")
  }

  if(!(is.null(noise) | is.numeric(noise))){
    stop("'noise' must be an non-negative numeric or NULL")
  }

  if(is.numeric(noise)){
    if(noise < 0){
      stop("'noise' must be an non-negative numeric or NULL")
    }
  }

  if(!is.logical(plot)){
    stop("'plot' must be a logical")
  }



  y_base <- mesor + amplitude * cos(2*pi*(time_series-acrophase)/period)

  if(!is.null(noise)){
    y <- y_base + rnorm(n = length(time_series), mean = 0, sd = noise)
  }else{
    y <- y_base
  }

  if(plot){
    plot(time_series,
         y_base,
         type = "l",
         ylim = range(y),
         xlab = "x",
         ylab = "y",
         ...)
    points(time_series, y)
    abline(v = time_series[which(y_base == max(y_base))], col = 2)
    abline(h = mesor, col = 4)
  }

  return(y)
}
