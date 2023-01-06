#' Get the stars for statistical test plots
#'
#' @param db.delta the output from function \link[cosinoRmixedeffects2]{get.contrasts.ci.cosinor}
#' @param db.means the output from function \link[cosinoRmixedeffects2]{get.means.ci.cosinor}
#' @param contrast.frm a string formula specifying the names of the predictors over which emmeans are desired.
#'
#' @return output for significance stats
#'
#' @import stats
#' @importFrom rstatix add_significance add_x_position add_y_position
#' @importFrom dplyr mutate
#' @importFrom limma strsplit2
#'
#' @export
#'
#' @examples
#' db.model<-create.cosinor.param(time="Hour_of_Day", period=24, data=db.cosinor)
#' f1<-fit.cosinor.mixed(y="hrv",x="gender",random="1|participant_id", data=db.model)
#' db.delta<-get.contrasts.ci.cosinor(f1,contrast.frm="~gender",nsim=100,ncpus=2)
#' db.means<-get.means.ci.cosinor(f1,contrast.frm="~gender",nsim=100,ncpus=2)
#' stat.test.stars(db.delta=db.delta, db.means=db.means, contrast.frm = "~gender")
#'
stat.test.stars <- function(db.delta, db.means, contrast.frm){

  ## Assign parameters to NULL to avoid devtools::check() NOTES
  Param <- group1 <- group2 <-  NULL

  ## Actual function
  db.delta<-as.data.frame(db.delta)
  db.means<-as.data.frame(db.means)

  stat.test<-data.frame(Param=factor(strsplit2(rownames(db.delta), "_")[,1]),
                      .y.="MEAN",
                      group1=limma::strsplit2(rownames(db.delta), "_")[,2],
                      group2=limma::strsplit2(rownames(db.delta), "_")[,5],
                      n1=10,
                      n2=10,
                      statistic=db.delta$boot.estimate,
                      df=db.delta$boot.SE,
                      p=db.delta$pvalue)

  stat.test <- rstatix::add_significance(stat.test, cutpoints = c(0, 1e-04, 0.001, 0.01, 0.05, 0.1, 1),
                                            symbols = c("****", "***", "**", "*", "+", "ns"))
  stat.test <- rstatix::add_x_position(stat.test)
  stat.test<-do.call("rbind.data.frame", lapply(c("MESOR", "Amplitude", "Acrophase"), function(L){
    db.temp<-subset(stat.test, Param==L)

    contrast.frm<-mgsub(c("[|]","[*]"),c("_","_"), contrast.frm) ## this is for interaction contrast

    db.temp2<-rstatix::add_y_position(db.temp, formula=as.formula(paste0("`97.5 %`",contrast.frm)), # need to specify variable here
                           data=subset(db.means, Param==L))
  }))

  stat.test<-dplyr::mutate(stat.test, contrast=paste0(group1, "_vs_",group2))

  stat.test
}


