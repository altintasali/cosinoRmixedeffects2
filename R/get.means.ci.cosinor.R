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
#' @return Output with confidence intervals and means for MESOR, amplitude and acrophase
#'
#' @importFrom lme4 bootMer
#' @importFrom limma strsplit2
#' @importFrom plyr rename
#' @export
#'
#' @examples
#' data(db.cosinor)
#' head(db.cosinor)
#' db.model <- create.cosinor.param(time = "Hour_of_Day", period = 24, data = db.cosinor)
#'
#' # Rhythmicity stats (individual comparisons)
#' f1 <- lme4::lmer(hrv~rrr+sss+(1|participant_id),
#'                data=db.model, na.action = na.omit)
#'
#' get.means.ci.cosinor(fit = f1, contrast.frm = '~ rrr + sss', nsim = 50, ncpus = 2, pairwise = TRUE)
#'
#' # Differentiaal rhythmicity stats (pairwise comparisons)
#' f2 <- fit.cosinor.mixed(y = "hrv", x = "gender", random = "1|participant_id", data = db.model)
#' summary(f2)
#' get.means.ci.cosinor(fit = f2, contrast.frm = "~gender", nsim = 50, ncpus = 2, pairwise = FALSE)
#' get.means.ci.cosinor(fit = f2, contrast.frm = "~gender", nsim = 50, ncpus = 2, pairwise = TRUE)
#'
get.means.ci.cosinor <- function(fit,
                                 contrast.frm,
                                 pairwise = FALSE,
                                 nsim = 500,
                                 parallel = "multicore",
                                 ncpus = 8,
                                 conftype = "norm",
                                 conflevel = 0.95,
                                 seed = NULL,
                                 ...){

  ## Assign parameters to NULL to avoid devtools::check() NOTES
  VALUE <-  NULL

  # ## get tarnsformed means of MESOR, amplitude and acrophase
  # ModelCoefs <- just.get.means.cosinor(fit=fit, contrast.frm=contrast.frm, pairwise = pairwise)

  ## Check the rhythmicity logic
  parsed_formula_pars <- trimws(unlist(strsplit(x = as.character(as.formula(contrast.frm))[2], split = "[+]")))
  rhythmicity_logic <- all(parsed_formula_pars %in% c("rrr", "sss"))

  ##bootstrap to get the confidence interval
  boot.mean <- lme4::bootMer(fit,
                             FUN = create.boot.FUN.mean(contrast.frm=contrast.frm, pairwise = pairwise),
                             nsim = nsim,
                             parallel = parallel,
                             ncpus = ncpus,
                             seed = seed)

  ##get the results
  db.means<-cbind.data.frame(MEAN=boot.mean$t0,
                             confint(boot.mean, type=conftype, level=conflevel))

  if(rhythmicity_logic){
    db.means$Param <- factor(rownames(db.means), levels=c("MESOR","Amplitude","Acrophase"))
  }else{
    if(!pairwise){
      db.means$VALUE<-gsub(" ", "",rownames(db.means))

      db.means<-dplyr::mutate(db.means,
                       Param = limma::strsplit2(VALUE, "_")[,1],
                       contrast = gsub(" ", "", limma::strsplit2(VALUE, "_")[,2]))
      ##"|", "*" will appear when contrast.frm are interactions
      db.means <- plyr::rename(db.means, c("contrast" = mgsub(c("~","[|]","[*]"),c("","_","_"), contrast.frm)))
      db.means$Param <- factor(db.means$Param, levels=c("MESOR","Amplitude","Acrophase"))
    }else{
      db.means$VALUE<-gsub(" ", "",rownames(db.means))
    }
  }

  return(db.means)
}
