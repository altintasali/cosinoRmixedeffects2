#' Bootstrap to get standard error of MESOR, amplitude and acrophase contrasts
#'
#' Bootstrap function takes in a fitted merMod object as input and returns the statistics (e.g. standard error) of pair-wise contrasts
#' of MESOR, amplitude and acrophase
#'
#' @param contrast.frm a string formula specifying the names of the predictors over which emmeans are desired.
#' @param pairwise If TRUE (default), test for differential rhythmicity (compare groups). If FALSE, test for rhythmicity only (detect rhythmicity)
#'
#' @return function input for [bootMer] calculating the mean of MESOR, Amplitude and Acrophase
#'
#' @import lme4
#' @importFrom doMC registerDoMC
#' @export
#'
#' @examples
#' data(db.cosinor)
#' head(db.cosinor)
#' ## create cosinor paramenters needed for cosinor model
#' db.model<-create.cosinor.param(time="Hour_of_Day", period=24, data=db.cosinor)
#'
#' ## examine the effects of gender on MESOR, acrophase, amplitude on hrv
#' f <- fit.cosinor.mixed(y = "hrv",
#'                        x = "gender",
#'                        random = "1|participant_id",
#'                        data = db.model)
#'
#' boot.cont <- lme4::bootMer(f,
#'                            FUN = create.boot.FUN.cont(contrast.frm='~gender'),
#'                            nsim = 50,
#'                            parallel = "multicore",
#'                            ncpus = 2)
create.boot.FUN.cont<-function(contrast.frm, pairwise = TRUE){
  boot.FUN.cont=function(.){just.get.contrasts.cosinor(., contrast.frm=contrast.frm, pairwise = pairwise)};
  return(boot.FUN.cont)}
