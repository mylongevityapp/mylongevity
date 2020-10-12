#' mylongevity_method3
#'
#' This function loads a file as a data frame of clients.
#' Using the argument 'indexes_of_variables', this function selects the columns of factors
#' Then using the argument 'list_of_variables' this functions selects the columns of interest to be matched
#' with coefficients (i.e log-hazard ratios) estimated in analysis
#' @param data given data frame of clients
#' @param indexes_of_variables indices for columns of interest
#' @param list_of_variables columns of interest to be matched to coefficients (i.e log-hazard ratios)
#' @param a intercept from Gompertz baseline hazards
#' @param b slope from Gompertz baseline hazards
#' @param age age of clients
#' @param T_start_indicator for right censored data, this is the follow up time. For interval data, the first argument is the starting time for the interval
#' @param T_stop_indicator ending time of the interval for interval censored or counting process data only. Intervals are assumed to be open on the left and closed on the right, (start, end]. For counting process data, event indicates whether an event occurred at the end of the interval
#' @param status_indicator the status indicator, normally 0=alive, 1=dead
#' @param working_directory the working directory for any data that is produced from the functions to be saved in a folder
#' @details This method fits the Cox regression to the provided data, eliminates non-significant terms, calculates weights for each combination of risk factors in the final Cox regression model and computes life expectancies based on a paper Kulinskaya et al. (2020b).  We recommend to include sex and some measure of deprivation in the list of risk factors to be explored. At the moment, we can only include additive terms to the model without interactions. Also in this method we can only include categorical risk factors because we need to split the population into different risk subgroups. If there is any continuous variable, then it should be factored into a categorical one. If none of the variables in the model are significant, this method does not produce any output.
#' For mylongevity_method3 user should specify the directory where all the temporary files will be stored. The directory should be specified as working_directory='E:/myproject'. A folder called 'temp' will be created in the specified directory. This is the folder where all necessary/temporary files will be stored. Also for method 2 and method 3 we need a function - function_for_table_of_combination() and a function calculate_life_expectancy() which are separate functions within this R package.
#' mylongevity_method3 fits the proportional hazards Cox regression using R package 'survival' and seeks  the best model using the backward elimination with the AIC criterion.
#' User has to input the data and specify the columns for K independent variables to put in Cox regression using the argument 'indexes_of_variables' and state the list of variable 'list_of_variables'.  The argument 'list_of_variables' and the first K elements in a vector
#' 'indexes_of_variables' should correspond to each other.
#'
#' In package 'survival, the proportional hazards Cox regression can be fitted with a time to event   outcome variable or by specifying  an interval for  follow up (FU) times. If using time to event, the vector 'indexes_of_variables' should additionally have columns specified for time to event and status indicator in position K+1 and K+2, respectively. For interval FU data, 'indexes_of_variables' should additionally  have columns specified for the FU starting time, FU ending time and status indicator in positions K+1,K+2 and K+3, respectively.
#' In functions mylongevity_method3(), the arguments T_start_indicator, T_stop_indicator, status_indicator correspond to the FU starting time, FU ending time and status indicator,   usually 0=alive, 1=dead.  If using  time to event, the argument T_start_indicator corresponds to follow up time and status_indicator  is usually coded  0=alive, 1=dead. In this case, the argument T_stop_indicator does not need to be specified.
#' To fit the Cox regression to time to event data, user has to include either time to event and status using the variables T_start_indicator and status _indicator to fit a model with non-interval data as "Surv(time=', T_start_indicator,',event=', status _indicator,')~' or using the variables T_start_indicator, T_stop_indicator and status _indicator for interval censored or counting process data as 'Surv(time=',T_start_indicator,',time2=', T_stop_indicator,',event=', status _indicator,')~'.
#' @keywords life_expectancy
#' @export
#' @examples
#' set.seed(1)
#' n<-10000
#' gender <- c(rep('M',times=n/2),rep('F',times=n/2))
#' townsend <- as.factor(round(runif(n, min = 1, max = 5)))
#' smokerCategory <- as.factor(round(runif(n, min = 1, max = 3)))
#' HTN_diag_treat  <- as.factor(round(runif(n, min = 1, max = 3)))
#' diabetes  <- as.factor(round(runif(n, min = 0, max = 1)))
#' hypercholesterolaemia <- as.factor(round(runif(n, min = 0, max = 1)))
#' bmiCategory <- as.factor(round(runif(n, min = 1, max = 3)))
#' cvd_risk <- as.factor(round(runif(n, min = 0, max = 2)))
#' statins <- as.factor(round(runif(n, min = 0, max = 1)))
#' aspirin<-as.factor(round(runif(n, min = 0, max = 1)))
#' Tstart<-rep(1,times=n)
#' Tstop<-abs(rnorm(n,mean=26,sd= 6))
#' death<-sample(c(0,1), replace=TRUE, size=n)
#' T_start_indicator <- "Tstart"
#' T_stop_indicator <-  "Tstop"
#' status_indicator <-  "death"
#' age <-70
#' a=-12.459132
#' b=0.11764571
#' indexes_of_variables<-c(1,2,3,4,5,6,7,8,9,10,11,12)
#' list_of_variables<-c("statins","cvd_risk","diabetes","HTN_diag_treat","hypercholesterolaemia","bmiCategory","smokerCategory","townsend","aspirin")
#' working_directory<-"E:/THIN data backup 10122019/development_of_R_package"
#' data<-data.frame(statins,cvd_risk,diabetes,HTN_diag_treat,hypercholesterolaemia,bmiCategory,smokerCategory,townsend,aspirin,Tstart,Tstop,death)
#' mylongevity_method3(data,indexes_of_variables,list_of_variables,age,a,b,T_start_indicator,T_stop_indicator,status_indicator,working_directory)
#' @return data frame with life expectancies for given data frame of clients
#'
mylongevity_method3 <- function(data,indexes_of_variables,list_of_variables,age,a,b,
T_start_indicator,T_stop_indicator=NULL,status_indicator,working_directory){
	if (missing(data))
		stop("Must specify a data via the 'data' argument.")
	if (missing(indexes_of_variables))
		stop("Must specify a indexes of columns via the 'indexes_of_variables' argument.")
	if (missing(list_of_variables))
		stop("Must specify a list of variables via the 'list_of_variables' argument.")
	if(missing(age))
		stop("Must specify age via the 'age' argument.")
	if(missing(a))
		stop("Must specify a via the 'a' argument.")
	if(missing(b))
		stop("Must specify b via the 'b' argument.")
	if (missing(working_directory))
		stop("Must specify working directory via the 'working_directory' argument.")
	if (!is.numeric(age))
			stop("age argument is not numeric")
	if (60>age)
			stop("Please specify age argument which takes values more than or equal to 60")
	if (!is.numeric(a))
			stop("a argument is not numeric")
	if (!is.numeric(b))
			stop("b argument is not numeric")

	if (missing(T_start_indicator)){
		stop("Must specify follow up time via the 'T_start_indicator' argument.")
	}
	if (missing(status_indicator)){
		stop("Must specify status_indicator via the 'status_indicator' argument.")
	}

	sub_data <- data[,indexes_of_variables]
	model_variables <- ""
	for(i in 1:(length(list_of_variables)-1)){
	model_variables <- paste(model_variables,list_of_variables[i],"+")
	}
	model_variables <- paste(model_variables,list_of_variables[length(list_of_variables)])
	###two versions of cox regression model
	if(!missing(T_start_indicator)&!missing(T_stop_indicator)&!missing(status_indicator)){
		formula_for_cox_regression <- as.formula(paste("survival::Surv(time=",T_start_indicator,",time2=",T_stop_indicator,",event=",status_indicator,")~", model_variables))
		fitted_cox_model <- function(x){MASS::stepAIC(survival::coxph(x, data = sub_data),k = sqrt(log(nrow(sub_data))), direction="backward" )}
		cox_model <- fitted_cox_model(formula_for_cox_regression)
    } else if(!missing(T_start_indicator)&missing(T_stop_indicator)&!missing(status_indicator)) {
		formula_for_cox_regression <- as.formula(paste("survival::Surv(time=",T_start_indicator,",event=",status_indicator,")~", model_variables))
		fitted_cox_model <- function(x){MASS::stepAIC(survival::coxph(x, data = sub_data),k = sqrt(log(nrow(sub_data))), direction="backward" )}
		cox_model <- fitted_cox_model(formula_for_cox_regression)
	}

	if(missing(cox_model)){
		stop("Must specify T_start_indicator,T_stop_indicator, status_indicator arguments to fit a proportional cox regression cox regression.")
	}
	cox_model_coefficients <- cox_model$coefficients
	model_coefficients <- names(cox_model_coefficients)
	LogHazardRatios <- rep(NA,times=length(cox_model_coefficients))
	if(is.null(model_coefficients)){
	     stop("None of the variables includeded in cox model are significant")
	}
	for(i in 1:length(cox_model_coefficients)){
		LogHazardRatios[i] <- cox_model_coefficients[[i]]
	}
	vector_of_coefficients <- as.character(model_coefficients)
	life_expectancy_table <- NULL
	life_expectancy_table <-
	suppressWarnings(calculate_life_expectancy(age=age,a=a,b=b,vector_of_coefficients=vector_of_coefficients,log_hazard_ratio=LogHazardRatios,data_for_weights=sub_data,working_directory=working_directory))
return(life_expectancy_table)
}

