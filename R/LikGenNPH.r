#' LikGenNPH
#' This function calculates the neglikelihood for parametrical survival model.
#' @param b shape parameters defining the hazard functions
#' @param D The data set as described in previous section;
#' @param nf The number of continuous and binary factors in the data set D corresponding to the covariates used in the Cox-regression for proportional hazard term;
#' @param nk The number of continuous and binary factors in the data set D corresponding to the covariates used in the Cox-regression for shape b;
#' @param ncl The number of clusters in the data set D corresponding to the covariate defining the shared frailty (is equal to 0 for the fixed-effect model);
#' @param dist distribution either Weibull or Gompertz
#' @keywords the neglikelihood for parametrical survival model
LikGenNPH=function(b,D,nf,nk,ncl,dist){
  #######This function calculates negloglikelihood for Weibull or Gompertz survival model
  if (dist=='Weibull'){
    lambda0=exp(b[1])
    k0=exp(b[2])} else if (dist=='Gompertz') {
      lambda0=1e-3*exp(b[1])
      k0=1e-2*exp(b[2])
    }
  Coxk=c(rep(0,nrow(D)))
  Cox=c(rep(0,nrow(D)))
  if (nk>0){
    for (i in 1:nk){
      Coxk=Coxk+D[,i]*b[2+i]
    }
  }
  if (nf>0){
    for (i in 1:nf){
      Cox=Cox+D[,(nk+i)]*b[2+nk+i]
    }
  }
  Coxk=exp(Coxk)
  LCox=Cox
  Cox=exp(Cox)
  if (ncl>0) {
    G2=exp(b[3+nk+nf])
    list=unique(D$cluster)
    nl=length(list)
  }
  k0=k0*Coxk
  Cens=D$event
  x1=D$time
  x0=D$trunc
  if (dist=='Weibull'){
    if (ncl>0){
      Lik=0
      for (i in 1:nl){
        ID=list[i]
        ind=which(D$cluster==ID)
        nn=length(ind)
        ic1=1*(Cens[ind]==1)
        nc1=sum(ic1)
        Hfull1= Cox[ind]*(x1[ind]/lambda0)^k0[ind]
        Hfull0= Cox[ind]*(x0[ind]/lambda0)^k0[ind]
        mufull1=Cox[ind]*k0[ind]*x1[ind]^(k0[ind]-1)/lambda0^k0[ind]#Weibull
        Lmufull1=LCox[ind]+((k0[ind]-1)*log(x1[ind]/lambda0)+log(k0[ind]/lambda0))
        if (nc1<=1) {
          ee1=0} else {
            ee1=sum(log(c(1:(nc1-1))*G2+1))
          }
        Lik=Lik+ee1+sum(Lmufull1*ic1)-(1/G2+nc1)*log(1+G2*sum(Hfull1))+(1/G2)*log(1+G2*sum(Hfull0))
      }
    } else {
      ic=1*(Cens==1)
      Hfull1= Cox*(x1/lambda0)^k0
      Hfull0= Cox*(x0/lambda0)^k0
      Lmufull1=LCox+((k0-1)*log(x1/lambda0)+log(k0/lambda0))
      Lik=sum(Lmufull1*ic)-sum(Hfull1)+sum(Hfull0)

    }
  }

  if (dist=='Gompertz'){
    if (ncl>0){
      Lik=0
      for (i in 1:nl){
        ID=list[i]
        ind=which(D$cluster==ID)
        nn=length(ind)
        ic1=1*(Cens[ind]==1)
        nc1=sum(ic1)
        Hfull1= Cox[ind]*(lambda0/k0[ind])*(exp(k0[ind]*x1[ind])-1)
        Hfull0= Cox[ind]*(lambda0/k0[ind])*(exp(k0[ind]*x0[ind])-1)
        mufull1=Cox[ind]*lambda0[ind]*exp(k0[ind]*x1[ind])
        Lmufull1=LCox[ind]+log(lambda0)+k0[ind]*x1[ind]
        if (nc1<=1) {
          ee1=0} else {
            ee1=sum(log(c(1:(nc1-1))*G2+1))
          }
        Lik=Lik+ee1+sum(Lmufull1*ic1)-(1/G2+nc1)*log(1+G2*sum(Hfull1))+(1/G2)*log(1+G2*sum(Hfull0))
      }
    } else {
      ic=1*(Cens==1)
      Hfull1= Cox*(lambda0/k0)*(exp(k0*x1)-1)
      Hfull0= Cox*(lambda0/k0)*(exp(k0*x0)-1)
      Lmufull1=LCox+log(lambda0)+k0*x1
      Lik=sum(Lmufull1*ic)-sum(Hfull1)+sum(Hfull0)

    }
  }

  Lik=-Lik
  return(Lik)
}
