#' my longevity method3 function
#' This function loads a file as a data frame of clients.
#' Using the argument 'indexes_of_variables', this function selects the columns of factors
#' Then using the argument 'list_of_variables' this functions selects the columns of interest to be matched
#' with coefficients (i.e log-hazard ratios) estimated in analysis
#' @param data given data frame of clients
#' @param indexes_of_variables indices for columns of interest
#' @param list_of_variables columns of interest to be matched to coefficients (i.e log-hazard ratios)
#' @param age age of clients
#' @param T_start_indicator for right censored data, this is the follow up time. For interval data, the first argument is the starting time for the interval
#' @param T_stop_indicator ending time of the interval for interval censored or counting process data only. Intervals are assumed to be open on the left and closed on the right, (start, end]. For counting process data, event indicates whether an event occurred at the end of the interval
#' @param status_indicator the status indicator, normally 0=alive, 1=dead
#' @param working_directory the working directory for any data that is produced from the functions to be saved in a folder
#' @keywords life_expectancy
#' @export
#' @examples
#' @return data frame with life expectancies for given data frame of clients
#' mylongevity_method3()
mylongevity_method3 <- function(data,indexes_of_variables,list_of_variables,age,a,b,
T_start_indicator,T_stop_indicator,status_indicator,working_directory){
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
	if (missing(T_stop_indicator)){
		stop("Must specify T_stop_indicator via the 'T_stop_indicator' argument.")
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
		stop(mstyle$stop("Must specify T_start_indicator,T_stop_indicator, status_indicator arguments to fit a proportional cox regression cox regression."))
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

