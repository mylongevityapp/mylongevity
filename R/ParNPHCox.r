#' ParNPHCox
#' @param formula.scale The formula object defining the fields for time-to-failure (or timeof-start and time-to-failure) and for covariates influencing the proportional hazard term;
#' @param formula.shape The object defining the fields for covariates influencing the shape;
#' @param cluster The name of the covariate defining the random effect (is equal to NULL for the fixed-effect model);
#' @param dist  distribution of the time-to-failure (’Weibull’ or ’Gompertz’);
#' @param data  The data set of interest
#' This function calculates the estimates of unknown parameters, their standard errors, and other attributes.
#' @keywords The estimates of unknown parameters, their standard errors, and other attributes.
ParNPHCox=function(formula.scale, formula.shape,cluster,dist,data)
{
  #######This function calculates negloglikelihood for Weibull or Gompertz survival model
  nr=nrow(data)
  obsdata <- NULL
  if (length(formula.scale[[2]]) == 3) {
    obsdata$trunc <- c(rep(0,nrow(data)))
    obsdata$time <- eval(formula.scale[[2]][[2]], envir = data)
    obsdata$event <- eval(formula.scale[[2]][[3]], envir = data)
  }   else if (length(formula.scale[[2]]) == 4) {
    obsdata$trunc <- eval(formula.scale[[2]][[2]], envir = data)
    obsdata$time <- eval(formula.scale[[2]][[3]], envir = data)
    obsdata$event <- eval(formula.scale[[2]][[4]], envir = data)
  }
  if (!all(levels(as.factor(obsdata$event)) %in% 0:1)) {
    stop(paste("The status indicator 'event' in the Surv object",
               "in the left-hand side of the formula object", "must be either 0 (no event) or 1 (event)."))
  }
  obsdata$x <- as.data.frame(stats::model.matrix.lm(formula.scale, data = data,na.action='na.pass'))
  obsdata$xs <- as.data.frame(stats::model.matrix.lm(formula.shape, data = data,na.action='na.pass')) #factors for shape
  names(obsdata$x)=paste(names(obsdata$x),"scale",sep=".")
  names(obsdata$xs)=paste(names(obsdata$xs),"shape",sep=".")
  ind.x=which(is.na(c(apply(obsdata$x,1,sum))))
  ind.xs=which(is.na(c(apply(obsdata$xs,1,sum))))

  if (is.null(cluster)) {
    obsdata$ncl <- 0
    obsdata$di <- sum(obsdata$event)
    obsdata$cluster <- c(rep(0,nrow(data)))
    ind.cl <- as.numeric({})
  }   else {
    if (!cluster %in% names(data)) {
      stop(paste0("object '", cluster, "' not found"))
    }
    obsdata$cluster <- eval(as.name(cluster), envir = data)
    obsdata$ncl <- length(levels(as.factor(obsdata$cluster)))
    obsdata$di <- stats::aggregate(obsdata$event, by = list(obsdata$cluster),
                            FUN = sum)[, , drop = FALSE]
    cnames <- obsdata$di[, 1]
    obsdata$di <- as.vector(obsdata$di[, 2])
    names(obsdata$di) <- cnames
    ind.cl=which(is.na(obsdata$cl))
  }
  ind=sort(unique(c(ind.x,ind.xs,ind.cl)))
  nx=rep(0,nr)
  nx[ind]=1
  if (is.factor(obsdata$cluster)) obsdata$cluster=as.character(obsdata$cluster)
  obs=data.frame(obsdata$xs[-1],obsdata$x[-1],obsdata$event,obsdata$trunc,obsdata$time,obsdata$cluster)[nx!=1,]
  nr=nrow(obs)
  namesk=names(obsdata$xs)[-1]
  namesf=names(obsdata$x)[-1]
  names(obs)=c(names(obsdata$xs)[-1],names(obsdata$x)[-1],names(obsdata$event),"event","trunc","time","cluster")
  ncl=obsdata$ncl
  nk=length(namesk)
  nf=length(namesf)
  par0=c(0,0)
  Result=ucminf::ucminf(par=par0,LikGenNPH,gr=NULL,D=obs,nf=0,nk=0,ncl=0,dist=dist,hessian=1)
  if (ncl>0){
    par0=c(Result$par,rep(0,(1+nk+nf)))} else {
      par0=c(Result$par,rep(0,(nk+nf)))
    }
  Result=ucminf(par=par0,LikGenNPH,gr=NULL,D=obs,nf=nf,nk=nk,ncl=ncl,dist=dist,hessian=1)
  par=Result$par
  if (any(!is.finite(as.matrix(Result$hessian))))
    stop("infinite or missing values in hessian. It is not possible to calculate the matrix of covariates. \n  Change the model and try again.")
  if (any(suppressWarnings(diag(MASS::ginv(Result$hessian)))<0))
    stop("hessian cannot be correctly calculated. \n  Change the model and try again.")
  invHes=sqrt(diag(ginv(Result$hessian)))
  Lik=-Result$value
  Vnames1=c("a","b",namesk,namesf)
  Vnames={}
  if ((nk+nf)>0)    Vnames=paste("exp(",c(namesk,namesf),")",sep="")
  if (ncl>0) Vnames1=c(Vnames1,"Sigma2")

  if (ncl==0 & dist=="Weibull"){
    Names=c("Sample size","Number of non-censored","a","b",Vnames,"Loglik","AIC")} else if (ncl>0 & dist=="Weibull"){
      Names=c("Sample size","Number of non-censored","a  ","b",Vnames,"Sigma2","Loglik","AIC")} else if (ncl==0 & dist=="Gompertz"){
        Names=c("Sample size","Number of non-censored","1000a ","100b",Vnames,"Loglik","AIC")} else if (ncl>0 & dist=="Gompertz"){
          Names=c("Sample size","Number of non-censored","1000a ","100b",Vnames,"Sigma2","Loglik","AIC")
        }
  if (dist=='Weibull'){
    parval=c(as.character(nr),as.character(sum(obs$event)),round(exp(Result$par),3),round(-Result$value,2),round(2*(Result$value+length(par0)),2))} else if (dist=='Gompertz'){
      parval=c(as.character(nr),as.character(sum(obs$event)),round(exp(Result$par)[1],3),round(exp(Result$par)[2],3),round(-Result$value,2),round(2*(Result$value+length(par0)),2))
      if ((nk+nf)>0)  parval=c(as.character(nr),as.character(sum(obs$event)),round(exp(Result$par)[1],3),round(exp(Result$par)[2],3),round(exp(Result$par)[3:length(par0)],3),round(-Result$value,2),round(2*(Result$value+length(par0)),2))
    }
  if (dist=='Weibull'){
    parvalm=c("","",round(exp(Result$par-1.96*invHes),3),"","")} else if (dist=='Gompertz'){
      parvalm=c("","",round(exp(Result$par-1.96*invHes)[1],3),round(exp(Result$par-1.96*invHes)[2],3),"","")
      if ((nk+nf)>0)  parvalm=c("","",round(exp(Result$par-1.96*invHes)[1],3),round(exp(Result$par-1.96*invHes)[2],3),round(exp(Result$par-1.96*invHes)[3:length(par0)],3),"","")
    }
  if (dist=='Weibull'){
    parvalp=c("","",round(exp(Result$par+1.96*invHes),3),"","")} else if (dist=='Gompertz'){
      parvalp=c("","",round(exp(Result$par+1.96*invHes)[1],3),round(exp(Result$par+1.96*invHes)[2],3),"","")
      if ((nk+nf)>0)  parvalp=c("","",round(exp(Result$par+1.96*invHes)[1],3),round(exp(Result$par+1.96*invHes)[2],3),round(exp(Result$par+1.96*invHes)[3:length(par0)],3),"","")
    }
  CI=c("","",paste(parvalm[3:(2+length(par))],parvalp[3:(2+length(par))],sep=" - "),"","")
  pval=c("","",round(2*stats::pnorm(-abs(par[1:(length(par))]/invHes[1:(length(par))])),4),"","")
  Tab=data.frame(parval,CI,pval)
  colnames(Tab)=c("Estimates","CI","p-value")
  rownames(Tab)=Names
  capt=paste("Parameter estimates.",dist,"model.",sep=" ")
  print(xtable::xtable(Tab,caption=capt))
  list(par=par,se=invHes,LogLik=Lik,Tab=Tab,Names=Vnames1)
  save(obs, file = "ISobs.RData")
  N=data.frame(nk,nf,ncl)
  print(xtable(N))
  save(N, file = "N.RData")
  save(Tab,file="tabIS.RData")
  save(obs,file="obs.RData")
}
