#' Get the correct value of acrophase
#'
#' Given beta (b_rrr) and gamma (b_sss), get the correct value of acrophase.
#'
#' @param b_rrr is the beta coefficient for x (beta), which is cos(2πt/τ), τ is period
#' @param b_sss is the beta coefficient for z (gamma), which is sin(2πt/τ), τ is period
#' @param period period (τ). Default is 24
#'
#' @return correct value of acrophase
#' @export
#'
#' @examples
#'
correct.acrophase.msf <- function(b_rrr, b_sss, period = 24){
  acrophase <- atan2(b_sss, b_rrr)/(2 * pi/period)
  return(acrophase)
}
