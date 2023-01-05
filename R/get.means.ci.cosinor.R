#' Get means and confidence interval for MESOR, Amplitude, Acrophase
#'
#' The function will takes in a fitted merMod object, transform the linear/raw coefficients to COSINOR non-linear/transformed coefficients
#' for MESOR, Amplitude and acrophase and estimated the standard error through bootstrapping to provide the
#' confidence interval.
#'
#' @param fit the object from lmer() function
#' @param contrast.frm a string formula specifying the names of the predictors over which emmeans are desired.
#' @param nsim the number of simulations, positive integer; the bootstrap B (or R) for bootMer function.Default is 500.
#' @param parallel the type of parallel operation to be used (if any) for bootMer function. The default is "multicore".
#' @param ncpus integer: number of processes to be used in parallel operation: typically one would choose this to be the number of available CPUs.Default is 8.
#' @param conftype A character string representing the type of interval required.The value must be one of "norm", "basic","perc".
#' @param pairwise If TRUE (default), test for differential rhythmicity (compare groups). If FALSE, test for rhythmicity only (detect rhythmicity)
#' @param seed argument to pass into 'seed' parameter in bootMer() function. Default NULL
#' @param conflevel The confidence level required, default is 0.95.
#' @param ... additional argument(s) for methods.
#'
#' @return
#' @export
#'
#' @examples
#'
#' f<-fit.cosinor.mixed(y="hrv", x="gender", random="1|participant_id", data=db.model)
#' summary(f)
#' get.means.ci.cosinor(fit=f, contrast.mean.frm="~gender")
#'
get.means.ci.cosinor<-function(fit, contrast.frm,
                               pairwise = TRUE,
                               nsim = 500,
                               parallel = "multicore",
                               ncpus = 8,
                               conftype = "norm",
                               conflevel = 0.95,
                               seed = NULL,
                               ...){

  ## get tarnsformed means of MESOR, amplitude and acrophase
  ModelCoefs<-just.get.means.cosinor(fit=fit, contrast.frm=contrast.frm, pairwise = pairwise)

  ##bootstrap to get the confidence interval
  boot.mean<-bootMer(fit,
                     FUN = create.boot.FUN.mean(contrast.frm=contrast.frm, pairwise = pairwise),
                     nsim = nsim,
                     parallel = parallel,
                     ncpus = ncpus,
                     seed = seed)

  ##get the results
  db.means<-cbind.data.frame(MEAN=boot.mean$t0,
                             confint(boot.mean, type=conftype, level=conflevel))

  if(pairwise){
    db.means$VALUE<-gsub(" ", "",rownames(db.means))

    db.means<-mutate(db.means,
                     Param=strsplit2(VALUE, "_")[,1],
                     contrast=gsub(" ","",strsplit2(VALUE, "_")[,2]))
    ##"|", "*" will appear when contrast.frm are interactions
    db.means<-plyr::rename(db.means, c("contrast" = mgsub(c("~","[|]","[*]"),c("","_","_"), contrast.frm)))
    db.means$Param<-factor(db.means$Param,levels=c("MESOR","Amplitude","Acrophase"))
  }else{
    db.means$Param<-factor(rownames(db.means),levels=c("MESOR","Amplitude","Acrophase"))
  }

  return(db.means)
}



