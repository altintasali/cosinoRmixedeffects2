#' Create cosinor parameters using the time variable and the cosinor period
#'
#' @param time a vector of time variable name, with same units as that of the cosinor period
#' @param period the length of the cosinor period, eg.24
#' @param data the dataframe with the time and period information
#'
#' @return \code{data.frame} with cosinor parameters. \code{rrr} = cosine component, \code{sss} = sine component
#'
#' @importFrom dplyr mutate
#'
#' @export
#'
#' @examples
#' data(db.cosinor)
#' head(db.cosinor)
#' ## create cosinor paramenters needed for cosinor model
#' db.model<-create.cosinor.param(time="Hour_of_Day", period=24, data=db.cosinor)
create.cosinor.param <- function(time, period, data){
  db.cosinor <- dplyr::mutate(data,
                     t=data[,time],
                     rrr=cos(2*pi*t/period),
                     sss=sin(2*pi*t/period))
}
