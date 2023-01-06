#' Bootstrap to get standard error of mean MESOR, amplitude and acrophase
#'
#' Bootstrap function takes in a fitted merMod object as input and returns the statistics (e.g. standard error) of mean
#' MESOR, amplitude and acrophase in each group.
#'
#' @param contrast.frm  a string formula specifying the names of the predictors over which emmeans are desired.
#' @param pairwise If TRUE (default), test for differential rhythmicity (compare groups). If FALSE, test for rhythmicity only (detect rhythmicity)
#'
#' @return function input for [bootMer] calculating the mean of MESOR, Amplitude and Acrophase
#'
#' @import lme4
#' @export
#'
#' @examples
#' \dontrun{
#' #' data(db.cosinor)
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
#' boot.mean <- lme4::bootMer(f,
#'                            FUN = create.boot.FUN.mean(contrast.frm='~gender'),
#'                            nsim = 500,
#'                            parallel = "multicore",
#'                            ncpus = 8)
#' }
create.boot.FUN.mean<-function(contrast.frm, pairwise = TRUE){
  boot.FUN.mean=function(.){just.get.means.cosinor(., contrast.frm=contrast.frm, pairwise = pairwise)};
  return(boot.FUN.mean)}

