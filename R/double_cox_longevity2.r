#' double_cox_longevity2
#'
#' This function loads a file as a data frame of clients and fits parametric survival model
#' @param data given data frame of clients
#' @param dist distribution either Weibull or Gompertz
#' @param cluster The name of the covariate defining the random effect (is equal to NULL for the fixed-effect model);
#' @param formula.scale The formula object defining the fields for time-to-failure (or time-of-start and time-to-failure) and for covariates influencing the proportional hazard term;
#' @param formula.shape The object defining the fields for covariates influencing the shape.
#' @param age_of_diagnosis age of diagnosis for a particular condition
#' @param time_past_from_diagnosis time past diagnosis
#' @param name_for_age_factor the name of continuous age covariate
#' @param working_directory the working directory for any data that is produced from the functions to be saved in a folder
#' @details This method fits the Double Cox regression to the provided data and using the hazard ratios from scale and shape parameters it calculates the life expectancies for all combination of given categorical variables
#' @keywords life_expectancy
#' @return data frame with life expectancies for given data frame of clients
#' @export
#' @examples
#' #set.seed(1)
#' #n<-10000
#' #clinic<-as.factor(round(runif(n, min = 1, max = 1000)))###suppose we have 200 clinic
#' #gender <- c(rep('M',times=n/2),rep('F',times=n/2))
#' #townsend <- as.factor(round(runif(n, min = 1, max = 5)))
#' #smokerCategory <- as.factor(round(runif(n, min = 1, max = 3)))
#' #HTN_diag_treat  <- as.factor(round(runif(n, min = 1, max = 3)))
#' #diabetes  <- as.factor(round(runif(n, min = 0, max = 1)))
#' #hypercholesterolaemia <- as.factor(round(runif(n, min = 0, max = 1)))
#' #bmiCategory <- as.factor(round(runif(n, min = 1, max = 3)))
#' #age_cat<- as.factor(round(runif(n, min = 1, max = 3)))
#' #birth_cohort<- as.factor(round(runif(n, min = 0, max = 3)))
#' #cvd_risk <- as.factor(round(runif(n, min = 0, max = 2)))
#' #statins <- as.factor(round(runif(n, min = 0, max = 1)))
#' #aspirin<-as.factor(round(runif(n, min = 0, max = 1)))
#' #age_c<-rnorm(n,mean=70, sd=10)
#' #Tstart<-rep(60, times=n)
#' #Tstop<-abs(rnorm(n,mean=70,sd= 6))
#' #death<-sample(c(0,1), replace=TRUE, size=n)
#' #T_start_indicator <- "Tstart"
#' #T_stop_indicator <-  "Tstop"
#' #status_indicator <-  "death"
#' #working_directory<-"E:/Documentation for R package/"
#' #data<-data.frame(clinic,statins,age_c,birth_cohort,cvd_risk,diabetes,HTN_diag_treat,hypercholesterolaemia,bmiCategory,age_cat,smokerCategory,townsend,aspirin,Tstart,Tstop,death)
#' #data <- subset(data, Tstop>Tstart) #removing observations where Tstop happened before Tstart
#' #age_of_diagnosis<-60 ##age at diagnosis
#' #time_past_from_diagnosis<-5 ## time from diagnosis
#' #cluster="clinic"
#' #dist="Weibull"
#' #formula.scale=as.formula("survival::Surv(Tstart,Tstop,death)~birth_cohort+age_c+HTN_diag_treat+smokerCategory")
#' #formula.shape=as.formula("survival::Surv(Tstart,Tstop, death) ~birth_cohort")
#' #double_cox_longevity2(data, dist, cluster, formula.shape, formula.scale,age_of_diagnosis,time_past_from_diagnosis,working_directory,name_for_age_factor="age_c")
double_cox_longevity2<-function(data,dist,cluster,formula.shape,formula.scale,age_of_diagnosis,time_past_from_diagnosis,working_directory,name_for_age_factor=NULL){
	if (missing(data))
		stop("Must specify a data via the 'data' argument.")
	if (missing(dist))
		stop("Must specify a data via the 'dist' argument.")
	if (missing(cluster))
		stop("Must specify a data via the 'cluster' argument.")
	if (missing(formula.shape))
		stop("Must specify a data via the 'formula.shape' argument.")
	if (missing(formula.scale))
		stop("Must specify a data via the 'formula.scale' argument.")
	if (missing(age_of_diagnosis))
		stop("Must specify a data via the 'age_of_diagnosis' argument.")
	if (missing(time_past_from_diagnosis))
		stop("Must specify a data via the 'time_past_from_diagnosis' argument.")
	if (missing(working_directory))
		stop("Must specify working directory via the 'working_directory' argument.")
	if (!is.numeric(age_of_diagnosis))
			stop("age_of_diagnosis argument is not numeric")

###################################
':=' <- function(lhs, rhs) {
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL))
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL))
}


':=' <- function(lhs, rhs) {
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL))
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL))
}

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
  obsdata$x <- as.data.frame(model.matrix.lm(formula.scale, data = data,na.action='na.pass'))
  obsdata$xs <- as.data.frame(model.matrix.lm(formula.shape, data = data,na.action='na.pass')) #factors for shape
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
    obsdata$di <- aggregate(obsdata$event, by = list(obsdata$cluster),
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
  Result=ucminf::ucminf(par=par0,LikGenNPH,gr=NULL,D=obs,nf=nf,nk=nk,ncl=ncl,dist=dist,hessian=1)
  par=Result$par
  if (any(!is.finite(as.matrix(Result$hessian))))
    stop("infinite or missing values in hessian. It is not possible to calculate the matrix of covariates. \n  Change the model and try again.")
  if (any(suppressWarnings(diag(MASS::ginv(Result$hessian)))<0))
    stop("hessian cannot be correctly calculated. \n  Change the model and try again.")
  invHes=sqrt(diag(MASS::ginv(Result$hessian)))
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
  pval=c("","",round(2*pnorm(-abs(par[1:(length(par))]/invHes[1:(length(par))])),4),"","")
  Tab=data.frame(parval,CI,pval)
  colnames(Tab)=c("Estimates","CI","p-value")
  rownames(Tab)=Names
  capt=paste("Parameter estimates.",dist,"model.",sep=" ")
  print(xtable::xtable(Tab,caption=capt))
  list(par=par,se=invHes,LogLik=Lik,Tab=Tab,Names=Vnames1)
  save(obs, file = "ISobs.RData")
  N=data.frame(nk,nf,ncl)
  print(xtable::xtable(N))

  data_with_life_expectancies<-data
  ###################################################################################################################
  ###Extracting all coefficients from the output Tab
vector_of_coefficients<-stringr::str_replace_all(rownames(Tab), stringr::fixed(" "), "")
vector_of_coefficients<-gsub("exp\\(", "", vector_of_coefficients)
vector_of_coefficients<-gsub("\\)", "", vector_of_coefficients)
HazardRatios<-as.character(Tab$Estimates)
####here we need to be careful with 'a' defined by Alexander and 'a' we are using for life expectancies.
####There are two different 'a'
if (dist=="Weibull") {
a<-as.numeric(as.character(Tab$Estimates[vector_of_coefficients=='a']))
b<-as.numeric(as.character(Tab$Estimates[vector_of_coefficients=='b']))
} else if (dist=="Gompertz"){
a<-as.numeric(as.character(Tab$Estimates[vector_of_coefficients=='1000a']))/1000
b<-as.numeric(as.character(Tab$Estimates[vector_of_coefficients=='100b']))/100
}#####################################################################################################
Sigma2<-as.numeric(as.character(Tab$Estimates[vector_of_coefficients=='Sigma2']))
### 1.1.1 Extraction of categorical variables from the model
######################################################################################################
### 1.1.1.1
vector_of_coefficients_shape<-vector_of_coefficients[!stringr::str_detect(vector_of_coefficients, ":")]
vector_of_coefficients_shape<-vector_of_coefficients_shape[stringr::str_detect(vector_of_coefficients_shape, ".shape")]
shape_paramaters<-gsub(".shape", "", vector_of_coefficients_shape)
shape_paramaters <- gsub('[[:digit:]]+', '', shape_paramaters)
unique_coefficients_shape<-unique(shape_paramaters)
#creating a table for unique categories
table_of_unique_coefficients_for_main_effects_shape <- NULL
#now we can go through these unique categories and idenity how many factors each variable has
if(!rlang::is_empty(unique_coefficients_shape)) {
for(coefficient_shape in 1:length(unique_coefficients_shape)){
		current_coefficient <- unique_coefficients_shape[coefficient_shape]
		#current_variable <- vector_of_coefficients_shape[stringr::str_detect(vector_of_coefficients_shape, current_coefficient)]
		current_variable<-vector_of_coefficients_shape[grep(paste("^",current_coefficient,"",sep=""),vector_of_coefficients_shape)]
		matches <- regmatches(current_variable, gregexpr("[[:digit:]]+", current_variable))
		categories <- as.numeric(unlist(matches))
		baseline_category_index <- (min(categories)-1)
		max_category_index <- max(categories)
		temporary_table_for_coefficient <- data.frame(unique_coefficients_shape[coefficient_shape],(length(current_variable)),baseline_category_index,max_category_index)
		names(temporary_table_for_coefficient) <- c("variable","count","baseline_category_index","max_category_index")
		table_of_unique_coefficients_for_main_effects_shape <- rbind(table_of_unique_coefficients_for_main_effects_shape,temporary_table_for_coefficient)
}
}
########################################################################################################
### 1.1.1.2 Extraction of interaction variables between categorical variables from the model
########################################################################################################
### lets check for interactions of shape parameters that has colon
interactions_shape<-vector_of_coefficients[stringr::str_detect(vector_of_coefficients, ":")]
#interactions_scale<-interactions_scale[!stringr::str_detect(interactions_scale, ".scale")]
interactions_effects_of_coefficients_shape<-interactions_shape[stringr::str_detect(interactions_shape, ".shape")]
shape_interaction_parameters<-gsub(".shape", "", interactions_effects_of_coefficients_shape)
#creating a table for unique categories
table_of_unique_coefficients_for_interactions_shape <- NULL
####at the moment there is no interaction for shape
if(!rlang::is_empty(shape_interaction_parameters)){
splitted_interactions_shape=stringr::str_split(shape_interaction_parameters, ":")
my_vector_shape<-c()
for(item_shape in 1:length(splitted_interactions_shape)){
for(i in 1:length(splitted_interactions_shape[[item_shape]])){
each_element_of_list_shape=splitted_interactions_shape[[item_shape]]
each_element_of_vector=each_element_of_list_shape[i]
my_vector_shape <- c(my_vector_shape, each_element_of_vector)
}
}
my_vector_shape<-unique(my_vector_shape)

my_vector_shape_parameters <- gsub('[[:digit:]]+', '', my_vector_shape)
my_vector_shape_parameters <-unique(my_vector_shape_parameters)

vector_of_coefficients_shape<-my_vector_shape
vector_of_coefficients_shape_paramaters <- gsub('[[:digit:]]+', '', vector_of_coefficients_shape)
unique_coefficients_for_interactions_shape<-unique(vector_of_coefficients_shape_paramaters)

#now we can go through these unique categories and idenity how many factors each variable has
for(coefficient_interaction_shape in 1:length(unique_coefficients_for_interactions_shape)){
		current_coefficient <- unique_coefficients_for_interactions_shape[coefficient_interaction_shape]
		#current_variable <- vector_of_coefficients_shape[stringr::str_detect(vector_of_coefficients_shape, current_coefficient)]
		current_variable<-vector_of_coefficients_shape[grep(paste("^",current_coefficient,"",sep=""),vector_of_coefficients_shape)]
		matches <- regmatches(current_variable, gregexpr("[[:digit:]]+", current_variable))
		categories <- as.numeric(unlist(matches))
		baseline_category_index <- (min(categories)-1)
		max_category_index <- max(categories)
		temporary_table_for_coefficient <- data.frame(unique_coefficients_for_interactions_shape[coefficient_interaction_shape],(length(current_variable)),baseline_category_index,max_category_index)
		names(temporary_table_for_coefficient) <- c("variable","count","baseline_category_index","max_category_index")
		table_of_unique_coefficients_for_interactions_shape <- rbind(table_of_unique_coefficients_for_interactions_shape,temporary_table_for_coefficient)
}
}
#### 2.1.1 Extraction of categorical variables in scale
##############################################################################################
###  2.1.1.1 Extraction of main categorical effects in scale
###First we need to make sure that we are extracting main effects and non-interaction effects
main_effects_scale<-vector_of_coefficients[!stringr::str_detect(vector_of_coefficients, ":")]
main_effects_of_coefficients_scale<-main_effects_scale[stringr::str_detect(main_effects_scale, ".scale")]
scale_parameters<-gsub(".scale", "", main_effects_of_coefficients_scale)
###separating continuous and categorical variables
continuous_scale_variables<- suppressWarnings(scale_parameters[is.na(readr::parse_number(scale_parameters))])
categorical_scale_variables<-suppressWarnings(scale_parameters[!is.na(readr::parse_number(scale_parameters))])
###Here are producing a table in order to understand the categories of categorical variables
categorical_scale_variables_no_digits <- gsub('[[:digit:]]+', '', categorical_scale_variables)
unique_main_effect_coefficients_scale<-unique(categorical_scale_variables_no_digits)
table_of_unique_coefficients_for_main_effects_scale <- NULL
if(!rlang::is_empty(unique_main_effect_coefficients_scale)){
#creating a table for unique categories
#now we can go through these unique categories and idenity how many factors each variable has
for(coefficient_scale in 1:length(unique_main_effect_coefficients_scale)){
		current_coefficient <- unique_main_effect_coefficients_scale[coefficient_scale]
		#current_variable_main_effect_scale <- main_effects_of_coefficients_scale[stringr::str_detect(main_effects_of_coefficients_scale, current_coefficient)]
		current_variable_main_effect_scale<-main_effects_of_coefficients_scale[grep(paste("^",current_coefficient,"",sep=""),main_effects_of_coefficients_scale)]
		matches <- regmatches(current_variable_main_effect_scale, gregexpr("[[:digit:]]+", current_variable_main_effect_scale))
		categories <- as.numeric(unlist(matches))
		baseline_category_index <- (min(categories)-1)
		max_category_index <- max(categories)
		temporary_table_for_coefficient <- data.frame(unique_main_effect_coefficients_scale[coefficient_scale],(length(current_variable_main_effect_scale)),baseline_category_index,max_category_index)
		names(temporary_table_for_coefficient) <- c("variable","count","baseline_category_index","max_category_index")
		table_of_unique_coefficients_for_main_effects_scale <- rbind(table_of_unique_coefficients_for_main_effects_scale,temporary_table_for_coefficient)
}
}
####################################################################################################
####################################################################################################
###  2.1.1.2 Extraction of interaction effects in scale between two categorical variables
interactions_scale<-vector_of_coefficients[stringr::str_detect(vector_of_coefficients, ":")]
interactions_scale<-interactions_scale[!stringr::str_detect(interactions_scale, ".shape")]
scale_interaction_parameters<-gsub(".scale", "", interactions_scale)
####################################################################################################
#table for creating a table for unique categories
table_of_unique_coefficients_for_interactions_scale <- NULL
if(!rlang::is_empty(scale_interaction_parameters)){
splitted_interactions_scale=stringr::str_split(scale_interaction_parameters, ":")
my_vector_scale<-c()
for(item_scale in 1:length(splitted_interactions_scale)){
for(i in 1:length(splitted_interactions_scale[[item_scale]])){
each_element_of_list_scale=splitted_interactions_scale[[item_scale]]
each_element_of_vector=each_element_of_list_scale[i]
my_vector_scale <- c(my_vector_scale, each_element_of_vector)
}
}
my_vector_scale<-unique(my_vector_scale)
my_vector_scale_parameters <- gsub('[[:digit:]]+', '', my_vector_scale)
my_vector_scale_parameters <-unique(my_vector_scale_parameters)
#################################################################################################
vector_of_coefficients_scale<-my_vector_scale
vector_of_coefficients_scale_paramaters <- gsub('[[:digit:]]+', '', vector_of_coefficients_scale)
unique_coefficients_for_interactions_scale<-unique(vector_of_coefficients_scale_paramaters)
#now we can go through these unique categories and idenity how many factors each variable has
for(coefficient_interaction_scale in 1:length(unique_coefficients_for_interactions_scale)){
		current_coefficient <- unique_coefficients_for_interactions_scale[coefficient_interaction_scale]
		#current_variable <- vector_of_coefficients_scale[stringr::str_detect(vector_of_coefficients_scale, current_coefficient)]
		current_variable<-vector_of_coefficients_scale[grep(paste("^",current_coefficient,"",sep=""),vector_of_coefficients_scale)]
		matches <- regmatches(current_variable, gregexpr("[[:digit:]]+", current_variable))
		categories <- as.numeric(unlist(matches))
		baseline_category_index <- (min(categories)-1)
		max_category_index <- max(categories)
		temporary_table_for_coefficient <- data.frame(unique_coefficients_for_interactions_scale[coefficient_interaction_scale],(length(current_variable)),baseline_category_index,max_category_index)
		names(temporary_table_for_coefficient) <- c("variable","count","baseline_category_index","max_category_index")
		table_of_unique_coefficients_for_interactions_scale <- rbind(table_of_unique_coefficients_for_interactions_scale,temporary_table_for_coefficient)
}

}
######################################################################################################
######################################################################################################
######################################################################################################
if(!is.null(table_of_unique_coefficients_for_interactions_shape))
{
combinated_table_of_unique_coefficients_shape<-merge(table_of_unique_coefficients_for_main_effects_shape,table_of_unique_coefficients_for_interactions_shape,all=TRUE)
} else {
combinated_table_of_unique_coefficients_shape<-table_of_unique_coefficients_for_main_effects_shape
}
#####Now we need to look at the union of two table and extract all variables for scale
if(!is.null(table_of_unique_coefficients_for_interactions_scale))
{
combinated_table_of_unique_coefficients_scale<-merge(table_of_unique_coefficients_for_main_effects_scale,
table_of_unique_coefficients_for_interactions_scale,all=TRUE)
} else {
combinated_table_of_unique_coefficients_scale<-table_of_unique_coefficients_for_main_effects_scale
}
######################################################################################################
#####Now we need to look at the union of two table and extract all variables for scale and shape combined
if(!is.null(combinated_table_of_unique_coefficients_scale)&!is.null(combinated_table_of_unique_coefficients_shape)){
combinated_table_of_unique_coefficients<-merge(combinated_table_of_unique_coefficients_scale,
combinated_table_of_unique_coefficients_shape,all=TRUE)
} else if (is.null(combinated_table_of_unique_coefficients_scale)){
combinated_table_of_unique_coefficients<-combinated_table_of_unique_coefficients_shape
} else if (is.null(combinated_table_of_unique_coefficients_shape)){
combinated_table_of_unique_coefficients<-combinated_table_of_unique_coefficients_scale
}

#data_with_life_expectancies<-data_with_life_expectancies[,combinated_table_of_unique_coefficients$variable]


  for(coefficient in 1:length(unique_coefficients_shape)){
		temporary_shape=unique_coefficients_shape[coefficient]
		#####here we are making sure we are extracting shape parameters
		temporary_no_digits<-gsub(".shape", "", temporary_shape)
		temporary_no_digits_shape <- gsub('[[:digit:]]+', '', temporary_no_digits)
		#current_table_of_unique_coefficients<-combinated_table_of_unique_coefficients[stringr::str_detect(combinated_table_of_unique_coefficients$variable, temporary_no_digits_shape),]
		current_table_of_unique_coefficients<-combinated_table_of_unique_coefficients[grep(paste("^",temporary_no_digits_shape,"$",sep=""), combinated_table_of_unique_coefficients$variable),]
		current_dimention<-dim(data_with_life_expectancies)[2]
		#baseline_category_index<-current_table_of_unique_coefficients$baseline_category_index
		#max_category_index<-current_table_of_unique_coefficients$max_category_index
		m=1
		for(subcategory in current_table_of_unique_coefficients$baseline_category_index:current_table_of_unique_coefficients$max_category_index){
		temp_variable<-paste(current_table_of_unique_coefficients$variable,subcategory,sep = "")
		data_with_life_expectancies$temp_variable<-0
		data_with_life_expectancies$temp_variable[data_with_life_expectancies[,temporary_shape]==subcategory]<-1
		names(data_with_life_expectancies)[current_dimention+m]<-temp_variable
		m=m+1
		}
}

########################################################################################################
log_hazard_ratio_for_all_coefficients <- suppressWarnings(log(as.numeric(HazardRatios)))
####Here we need to make sure that we are extracting non interaction hazard ratios
vector_of_coefficients_shape<-vector_of_coefficients[!stringr::str_detect(vector_of_coefficients, ":")]
LogHazardRatios_shape_main_effects<-log_hazard_ratio_for_all_coefficients[!stringr::str_detect(vector_of_coefficients, ":")]
##However here we still have continious variables inside
########################################################################################################
only_vector_of_coefficients_shape<-vector_of_coefficients_shape[stringr::str_detect(vector_of_coefficients_shape, ".shape")]
LogHazardRatios_main_effects_shape<-LogHazardRatios_shape_main_effects[stringr::str_detect(vector_of_coefficients_shape, ".shape")]

#Here we are adding log-hazard ratios to the table
current_dimension<-dim(data_with_life_expectancies)[2]
all_coefficient_shape<-rep(0,times=dim(data_with_life_expectancies)[1])
	for(term in 1:length(unique_coefficients_shape)){
		temporary=unique_coefficients_shape[term]
		current_LogHazardRatio<-c(0,LogHazardRatios_main_effects_shape[stringr::str_detect(only_vector_of_coefficients_shape, temporary)])
		temporary_no_digits<-gsub(".shape", "", temporary)
		temporary_no_digits_shape <- gsub('[[:digit:]]+', '', temporary_no_digits)

		#current_table_of_unique_coefficients<-combinated_table_of_unique_coefficients[stringr::str_detect(combinated_table_of_unique_coefficients$variable, temporary_no_digits_shape),]
		current_table_of_unique_coefficients<-combinated_table_of_unique_coefficients[grep(paste("^",temporary_no_digits_shape,"",sep=""), combinated_table_of_unique_coefficients$variable),]

		current_variables<-NULL
			for(j in current_table_of_unique_coefficients$baseline_category_index:current_table_of_unique_coefficients$max_category_index){
				temp_variable<-paste(current_table_of_unique_coefficients$variable,j,sep = "")
				current_variables<-c(current_variables,temp_variable)
				}

		mat_i=data_with_life_expectancies[,current_variables]
		temp_variable<-paste(temporary,"_estimates",sep = "")
		all_coefficient_shape<-all_coefficient_shape+t(t(mat_i))%*%t(t(current_LogHazardRatio))
	}

data_with_life_expectancies$Betas_shape_main_effect <- all_coefficient_shape
##################################################################################################
###############################################SCALE##############################################
##################################################################################################
###First we need to make sure that we are not extracting interaction variables
main_effects_scale<-vector_of_coefficients[!stringr::str_detect(vector_of_coefficients, ":")]
log_hazard_ratio_main_effects_scale<-log_hazard_ratio_for_all_coefficients[!stringr::str_detect(vector_of_coefficients, ":")]
####Then we need to make sure that we are extracting variables for scale
main_effects_of_coefficients_scale<-main_effects_scale[stringr::str_detect(main_effects_scale, ".scale")]
main_effects_log_hazard_ratio_scale<-log_hazard_ratio_main_effects_scale[stringr::str_detect(main_effects_scale, ".scale")]
#################################################################################################
###Now we need to separate categorical and continious variables
categorical_variables_log_hazard_ratios_scale<-suppressWarnings(main_effects_log_hazard_ratio_scale[!is.na(readr::parse_number(scale_parameters))])
continious_variables_log_hazard_ratios_scale<-suppressWarnings(main_effects_log_hazard_ratio_scale[is.na(readr::parse_number(scale_parameters))])
    for(coefficient in 1:length(unique_main_effect_coefficients_scale)){
		temporary=unique_main_effect_coefficients_scale[coefficient]
		#temporary_no_digits<-gsub(".shape", "", temporary)
		#temporary_no_digits <- gsub('[[:digit:]]+', '', temporary_no_digits)

		#current_table_of_unique_coefficients<-combinated_table_of_unique_coefficients[stringr::str_detect(combinated_table_of_unique_coefficients$variable, temporary),]
		current_table_of_unique_coefficients<-combinated_table_of_unique_coefficients[grep(paste("^",temporary,"$",sep=""), combinated_table_of_unique_coefficients$variable),]

		current_dimention<-dim(data_with_life_expectancies)[2]
		baseline_category_index<-current_table_of_unique_coefficients$baseline_category_index
		max_category_index<-current_table_of_unique_coefficients$max_category_index
		m=1
		for(sub_category in baseline_category_index:max_category_index){
		temp_variable<-paste(current_table_of_unique_coefficients$variable,sub_category,sep = "")
		data_with_life_expectancies$temp_variable<-0
		data_with_life_expectancies$temp_variable[data_with_life_expectancies[,temporary]==sub_category]<-1
		names(data_with_life_expectancies)[current_dimention+m]<-temp_variable
		m=m+1
		}
	}

#Here we are adding log-hazard ratios to the table
current_dimension<-dim(data_with_life_expectancies)[2]
all_coefficient_scale<-rep(0,times=dim(data_with_life_expectancies)[1])

	for(term in 1:length(unique_main_effect_coefficients_scale)){
		temporary=unique_main_effect_coefficients_scale[term]
		current_LogHazardRatio<-c(0,categorical_variables_log_hazard_ratios_scale[stringr::str_detect(categorical_scale_variables, temporary)])
		temporary_no_digits<-gsub(".shape", "", temporary)
		temporary_no_digits <- gsub('[[:digit:]]+', '', temporary_no_digits)

		#current_table_of_unique_coefficients<-combinated_table_of_unique_coefficients[stringr::str_detect(combinated_table_of_unique_coefficients$variable, temporary_no_digits),]
		current_table_of_unique_coefficients<-combinated_table_of_unique_coefficients[grep(paste("^",temporary_no_digits,"$",sep=""), combinated_table_of_unique_coefficients$variable),]

		baseline_category_index<-current_table_of_unique_coefficients$baseline_category_index
		max_category_index<-current_table_of_unique_coefficients$max_category_index
		current_variables<-NULL
			for(sub_category in baseline_category_index:max_category_index){
				temp_variable<-paste(current_table_of_unique_coefficients$variable,sub_category,sep = "")
				current_variables<-c(current_variables,temp_variable)
				}

		mat_i=data_with_life_expectancies[,current_variables]
		temp_variable<-paste(temporary,"_estimates",sep = "")
		all_coefficient_scale<-all_coefficient_scale+t(t(mat_i))%*%t(t(current_LogHazardRatio))


	}

data_with_life_expectancies$Betas_scale_main_effect <- all_coefficient_scale
#### 2.1.2 Extraction of continuous variables in scale
####################################################################################################
####################Lets add the effect of continuous variables into the table######################
if(!rlang::is_empty(continuous_scale_variables)) {
for (term in 1:length(continuous_scale_variables)){
temporary=continuous_scale_variables[term]
current_LogHazardRatio<-main_effects_log_hazard_ratio_scale[stringr::str_detect(main_effects_of_coefficients_scale, temporary)]
data_with_life_expectancies$variable<-current_LogHazardRatio
names(data_with_life_expectancies)[dim(data_with_life_expectancies)[2]]<-paste(temporary,"_continuous",sep="")
}
}


####################################################################################################
####################################################################################################
####################################################################################################
############################Accounting for interaction effect for scale#############################
if(!is.null(table_of_unique_coefficients_for_interactions_scale))
{
coefficients_scale_interaction_effects<-vector_of_coefficients[stringr::str_detect(vector_of_coefficients, ":")]
LogHazardRatios_scale_interaction_effects<-log_hazard_ratio_for_all_coefficients[stringr::str_detect(vector_of_coefficients, ":")]
####################################################################################################
vector_of_coefficients_scale<-coefficients_scale_interaction_effects[stringr::str_detect(coefficients_scale_interaction_effects, ".scale")]
LogHazardRatios_scale_main_effects<-LogHazardRatios_scale_interaction_effects[stringr::str_detect(coefficients_scale_interaction_effects, ".scale")]
###################################################################################################
vector_of_coefficients_scale<-gsub(".scale", "", vector_of_coefficients_scale)
data_with_life_expectancies$betaXinteraction_scale<-0
for (each_interaction_scale in 1: length(vector_of_coefficients_scale)){
interaction_item_scale<-vector_of_coefficients_scale[each_interaction_scale]
log_hazard_of_interaction_item_scale<-LogHazardRatios_scale_main_effects[each_interaction_scale]
my_string<-unlist(strsplit(interaction_item_scale, ":"))
        betaX_interaction_scale<-1
        for (variable in 1:(length(my_string)-1)){
		betaX_interaction_scale<-betaX_interaction_scale*data_with_life_expectancies[,my_string[variable]]
		}


		betaX_interaction_scale<-betaX_interaction_scale*data_with_life_expectancies[,my_string[length(my_string)]]
		betaX_interaction_scale<-betaX_interaction_scale*log_hazard_of_interaction_item_scale
		data_with_life_expectancies$betaXinteraction_scale<-data_with_life_expectancies$betaXinteraction_scale+betaX_interaction_scale
}
} else {
data_with_life_expectancies$betaXinteraction_scale<-0
}
######################################################################################################
######################################################################################################
################################Accounting for interaction effect for shape###########################
if(!is.null(table_of_unique_coefficients_for_interactions_shape))
{
coefficients_shape_interaction_effects<-vector_of_coefficients[stringr::str_detect(vector_of_coefficients, ":")]
LogHazardRatios_shape_interaction_effects<-log_hazard_ratio_for_all_coefficients[stringr::str_detect(vector_of_coefficients, ":")]
#####################################################################################################
only_vector_of_coefficients_shape<-coefficients_shape_interaction_effects[stringr::str_detect(coefficients_shape_interaction_effects, ".shape")]
LogHazardRatios_main_effects_shape<-LogHazardRatios_shape_interaction_effects[stringr::str_detect(coefficients_shape_interaction_effects, ".shape")]
#####################################################################################################
only_vector_of_coefficients_shape<-gsub(".shape", "", only_vector_of_coefficients_shape)
data_with_life_expectancies$betaXinteraction_shape<-0
for (each_interaction_shape in 1: length(only_vector_of_coefficients_shape)){
interaction_item_shape<-only_vector_of_coefficients_shape[each_interaction_shape]
log_hazard_of_interaction_item_shape<-LogHazardRatios_main_effects_shape[each_interaction_shape]
my_string<-unlist(strsplit(interaction_item_shape, ":"))
        betaX_interaction_shape<-1
        for (variable in 1:(length(my_string)-1)){
		betaX_interaction_shape<-betaX_interaction_shape*data_with_life_expectancies[,my_string[variable]]
		}
		betaX_interaction_shape<-betaX_interaction_shape*data_with_life_expectancies[,my_string[length(my_string)]]
		betaX_interaction_shape<-betaX_interaction_shape*log_hazard_of_interaction_item_shape
		data_with_life_expectancies$betaXinteraction_shape<-data_with_life_expectancies$betaXinteraction_shape+betaX_interaction_shape
}
} else {
data_with_life_expectancies$betaXinteraction_shape<-0
}
#####################################################################################################
#####################################################################################################
#####################################################################################################
data_with_life_expectancies$a<-a
data_with_life_expectancies$b<-b
data_with_life_expectancies$sigma2<-Sigma2
data_with_life_expectancies$betas_shape<-data_with_life_expectancies$Betas_shape_main_effect+data_with_life_expectancies$betaXinteraction_shape
data_with_life_expectancies$betas_scale<-data_with_life_expectancies$Betas_scale_main_effect+data_with_life_expectancies$betaXinteraction_scale
data_with_life_expectancies$time_past_from_diagnosis<-time_past_from_diagnosis
data_with_life_expectancies$age_of_diagnosis<-age_of_diagnosis



if(Sigma2!=0){

if(!is.null(name_for_age_factor)){
###Age is a continuous covariate in scale
if (dist=="Weibull") {
#############################################numerator###################################################
integral_of_Sx_Weibull<-function(time_to_event,a,b,beta_scaleU,beta_shapeU,sigma2){
	Sx<-(1+sigma2*(exp(beta_scaleU)*(time_to_event/a)^(b*exp(beta_shapeU))))^{-1/sigma2}
	return(Sx)
}
life_expectancy_Weibull<-function(time_past_from_diagnosis,age_of_diagnosis,age_continuous,a,b,beta_scaleU,beta_shapeU,sigma2){
integrate(integral_of_Sx_Weibull, lower=time_past_from_diagnosis, upper=Inf,a=a,b=b,beta_scaleU=beta_scaleU+age_continuous*age_of_diagnosis,beta_shapeU=beta_shapeU,sigma2=sigma2)$value[1]
}
v.life_expectancy<- Vectorize(life_expectancy_Weibull)
#########################################################################################################
cumH<-function(time_to_event,a,b,beta_scaleU,beta_shapeU){exp(beta_scaleU)*(time_to_event/a)^{b*exp(beta_shapeU)}}
#########################################################################################################
}

if (dist=="Gompertz"){
#############################################numerator###################################################
integral_of_Sx_Gompertz<-function(time_to_event,a,b,beta_scaleU,beta_shapeU,sigma2){
	Sx<-(1+sigma2*(exp(beta_scaleU)*(a/b)*(exp(b*exp(beta_shapeU)*time_to_event)-1)))^{-1/sigma2}
	return(Sx)
}

life_expectancy_Gompertz<-function(time_past_from_diagnosis,age_of_diagnosis,age_continuous,a,b,beta_scaleU,beta_shapeU,sigma2){
integrate(integral_of_Sx_Gompertz, lower=time_past_from_diagnosis, upper=Inf,a=a,b=b,beta_scaleU=beta_scaleU+age_continuous*age_of_diagnosis,beta_shapeU=beta_shapeU,sigma2=sigma2)$value[1]
}
v.life_expectancy<-Vectorize(life_expectancy_Gompertz)
#########################################################################################################
cumH<-function(time_to_event,a,b,beta_scaleU,beta_shapeU){exp(beta_scaleU)*(a/b)*(exp(b*exp(beta_shapeU)*time_to_event)-1)}
#########################################################################################################
}


####needs to change#####
name_for_age_factor<-paste(name_for_age_factor,"_continuous",sep="")
temporary_files_directory<-working_directory
####needs to change#####

fileConn3<-file(paste(temporary_files_directory,"table_with_life_expectancies.txt",sep = ""))
data_with_life_expectancies_name_for_age_factor<-paste("data_with_life_expectancies$",name_for_age_factor,sep="")
writeLines(c(paste("data_with_life_expectancies$cumH_at_t_time_past_from_diagnosis<-cumH(time_to_event=data_with_life_expectancies$time_past_from_diagnosis,a=data_with_life_expectancies$a,
b=data_with_life_expectancies$b,
beta_scaleU=(data_with_life_expectancies$betas_scale+",data_with_life_expectancies_name_for_age_factor,"*data_with_life_expectancies$age_of_diagnosis),
beta_shapeU=data_with_life_expectancies$betas_shape)",sep=""),
paste("data_with_life_expectancies$integral_of_Sx_numerator<-v.life_expectancy(time_past_from_diagnosis=data_with_life_expectancies$time_past_from_diagnosis,age_of_diagnosis=data_with_life_expectancies$age_of_diagnosis,age_continuous=",data_with_life_expectancies_name_for_age_factor,",a=data_with_life_expectancies$a,b=data_with_life_expectancies$b,
beta_scaleU=data_with_life_expectancies$betas_scale,beta_shapeU=data_with_life_expectancies$betas_shape,sigma2=data_with_life_expectancies$sigma2)"),
paste("data_with_life_expectancies$life_expectancy<-data_with_life_expectancies$integral_of_Sx_numerator/exp(-data_with_life_expectancies$cumH_at_t_time_past_from_diagnosis)")
),fileConn3)

source(paste(working_directory,"/table_with_life_expectancies.txt",sep=""),local=TRUE)


} else {

###Age is not a continuous covariate in scale, however is age is included in as a categorical variable in
### scale covariate

####TO DO
#Here is the life expectancy is calculated with age instead of time from diagnosis??????

if (dist=="Weibull") {
#############################################numerator###################################################
integral_of_Sx_Weibull<-function(time_to_event,a,b,beta_scaleU,beta_shapeU,sigma2){
	Sx<-(1+sigma2*(exp(beta_scaleU)*(time_to_event/a)^(b*exp(beta_shapeU))))^{-1/sigma2}
	return(Sx)
}
life_expectancy_Weibull<-function(time_past_from_diagnosis,age_of_diagnosis,a,b,beta_scaleU,beta_shapeU,sigma2){
integrate(integral_of_Sx_Weibull, lower=time_past_from_diagnosis, upper=Inf,a=a,b=b,beta_scaleU=beta_scaleU,beta_shapeU=beta_shapeU,sigma2=sigma2)$value[1]
}
v.life_expectancy<- Vectorize(life_expectancy_Weibull)
#######################################################################################################
cumH<-function(time_to_event,a,b,beta_scaleU,beta_shapeU){exp(beta_scaleU)*(time_to_event/a)^{b*exp(beta_shapeU)}}
#######################################################################################################
} else if (dist=="Gompertz"){
#############################################numerator#################################################
integral_of_Sx_Gompertz<-function(time_to_event,a,b,beta_scaleU,beta_shapeU,sigma2){
	Sx<-(1+sigma2*(exp(beta_scaleU)*(a/b)*(exp(b*exp(beta_shapeU)*time_to_event)-1)))^{-1/sigma2}
	return(Sx)
}
life_expectancy_Gompertz<-function(time_past_from_diagnosis,age_of_diagnosis,a,b,beta_scaleU,beta_shapeU,sigma2){
integrate(integral_of_Sx_Gompertz, lower=time_past_from_diagnosis, upper=Inf,a=a,b=b,beta_scaleU=beta_scaleU,beta_shapeU=beta_shapeU,sigma2=sigma2)$value[1]
}
v.life_expectancy<-Vectorize(life_expectancy_Gompertz)
######################################################################################################
cumH<-function(time_to_event,a,b,beta_scaleU,beta_shapeU){exp(beta_scaleU)*(a/b)*(exp(b*exp(beta_shapeU)*time_to_event)-1)}
######################################################################################################
}
data_with_life_expectancies$cumH_at_t_time_past_from_diagnosis<-
cumH(time_to_event=data_with_life_expectancies$time_past_from_diagnosis,a=data_with_life_expectancies$a,
b=data_with_life_expectancies$b,beta_scaleU=(data_with_life_expectancies$betas_scale),
beta_shapeU=data_with_life_expectancies$betas_shape)

data_with_life_expectancies$integral_of_Sx_numerator<-v.life_expectancy(time_past_from_diagnosis=data_with_life_expectancies$time_past_from_diagnosis,a=data_with_life_expectancies$a,b=data_with_life_expectancies$b,
beta_scaleU=data_with_life_expectancies$betas_scale,beta_shapeU=data_with_life_expectancies$betas_shape,sigma2=data_with_life_expectancies$sigma2)
data_with_life_expectancies$life_expectancy<-data_with_life_expectancies$integral_of_Sx_numerator/exp(-data_with_life_expectancies$cumH_at_t_time_past_from_diagnosis)

}

} else {
stop("The variance of frailty is zero, hence integral of survival function can not be calculated. Change the model and try again.")
}

return(data_with_life_expectancies)
}
