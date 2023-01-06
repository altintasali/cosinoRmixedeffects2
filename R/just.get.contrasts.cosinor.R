#' Get the pairwise contrasts for MESOR, Amplitude, Acrophase
#'
#' Get the estimated marginal means for the specified factors or factor combinations in a linear model,
#' and transform to non-linear parameters MESOR, amplitude, and acrophase, and get the pairwise contrasts
#' for MESOR, amplitude and acrophase by the specified factors.
#'
#'
#' @param fit  the object from lmer()
#' @param contrast.frm  a string formula specifying the names of the predictors over which emmeans are desired.
#' @param pairwise If TRUE (default), test for differential rhythmicity (compare groups). If FALSE, test for rhythmicity only (detect rhythmicity)
#' @param ... other arguments passed on to methods
#'
#' @return MESOR, amplitude and acrophase estimates within contrasts
#'
#' @import stats emmeans
#'
#' @export
#'
#' @examples
#' require(lme4)
#'
#' db.model<-create.cosinor.param(time="Hour_of_Day", period=24, data=db.cosinor)
#'
#' f1.a<-lme4::lmer(hrv~gender+
#'                  gender*rrr+
#'                  gender*sss+(1|participant_id),
#'                  data=db.model, na.action = na.omit)
#'
#' # Differential rhythmicity stats (pairwise comparisons)
#' just.get.contrasts.cosinor(fit=f1.a, contrast.frm='~gender', pairwise = TRUE)
#'
#' # Rhytmicity stats (individual comparisons)
#' just.get.contrasts.cosinor(fit=f1.a, contrast.frm='~gender', pairwise = FALSE)
#'
just.get.contrasts.cosinor<- function(fit,
                                      contrast.frm,
                                      pairwise = TRUE,
                                      ...){
  #get the fitted contrasts and transform to Amp,Acr
  contrast.frm<-as.formula(contrast.frm)
  mf <- fit #object$fit

  groups.M<-emmeans(mf, contrast.frm)
  groups.rrr<-emtrends(mf, contrast.frm,'rrr')
  groups.sss<-emtrends(mf, contrast.frm,'sss')

  ## assemble summary matrix
  groups.rrr.db<-as.data.frame(groups.rrr)
  w<-which(colnames(groups.rrr.db)=='rrr.trend')-1
  groups.names<-as.vector(apply(groups.rrr.db,1,function(x){paste0(x[1:w],collapse=',')}))

  pars.raw.mesor<-groups.M@linfct %*% fixef(mf)
  pars.raw.rrr<-groups.rrr@linfct %*% fixef(mf)
  pars.raw.sss<-groups.sss@linfct %*% fixef(mf)

  pars.raw.mesor<-as.vector(pars.raw.mesor)
  names(pars.raw.mesor)<-paste0('MESOR_',groups.names)
  amp<-apply(pars.raw.rrr,1,function(x){sqrt(sum(x^2))})
  names(amp) <- paste0('Amplitude_',groups.names)

  acr<-apply(cbind(pars.raw.rrr,pars.raw.sss), 1,
             function(x){correct.acrophase.msf(b_rrr=x[1],b_sss=x[2])})
  names(acr) <- paste0('Acrophase_',groups.names)

  if (pairwise){
    out<-c(get.pairwise.diff(pars.raw.mesor),
           get.pairwise.diff(amp),
           get.pairwise.diff(acr))
  }else{
    out<-c(pars.raw.mesor, amp, acr)
    names(out) <- c("MESOR", "Amplitude", "Acrophase")
  }

  return(out)
}

