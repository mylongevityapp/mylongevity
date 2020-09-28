#' mylongevity_method1
#'
#' This function loads a file as a data frame of clients.
#' Using the argument 'indexes_of_variables', this function selects the columns of factors
#' and matches them to outcomes of life expectancy calculator https://mylongevity.org/about
#' @param data given data frame of clients
#' @param indexes_of_variables indices for columns of interest.
#' @keywords life_expectancy
#' @export
#' @examples
#' @return data frame with life expectancies for given data frame of clients
#' mylongevity_method1()

mylongevity_method1<-function(data,indexes_of_variables){
    if (missing(data))
		stop("Must specify a data via the 'data' argument.")
	if (missing(indexes_of_variables))
		stop("Must specify a list of variables via the 'indexes_of_variables' argument.")
	names_for_columns<-c("age","gender","townsend","smokerCategory","htn","diabetesCategory","hcl","bmiCategory","qRiskCategory","statins")
	if(length(indexes_of_variables)!=length(names_for_columns)){
	stop("Some required columns are missing.
	Please specify all the required indices of columns in the dataset in the following order :
	age,gender,townsend,smokerCategory,htn,diabetesCategory,hcl,bmiCategory,qRiskCategory,statins")
	}
	sub_data <- data[,indexes_of_variables]
	names(sub_data) <- names_for_columns

	all_ages <- unique(sub_data$age)
	if(sum(all_ages<60)||sum(all_ages>85)){
		stop("Age variable should only take values between 60 and 85.")
	}
	all_genders <- as.character(unique(sub_data$gender))
	all_possible_genders <- factor(c('M','F'))
	if(!all(levels(all_possible_genders) %in% all_genders)){
		stop("Gender variable should take values 'M' for males and 'F' for females.")
    }
    all_townsend <- unique(sub_data$townsend)
	all_possible_townsend <- factor(c(1,2,3,4,5))
	if(!all(levels(all_possible_townsend) %in% all_townsend)){
		stop("Townsend variable should take values between 1 to 5")
    }

	all_smokerCategory<-unique(sub_data$smokerCategory)
    all_possible_smokerCategory <- factor(c(1,2,3))
	if(!all(levels(all_possible_smokerCategory) %in% all_smokerCategory)){
		stop("Smoking category variable should take values 1 - for no smoker,2 - for ex smoker,3 - for smoker")
    }

	all_htn<-unique(sub_data$htn)
    all_possible_htn <- factor(c(1,2,3))
	if(!all(levels(all_possible_htn) %in% all_htn)){
		stop("Treated hypertension variable should take values 1 - no hypertension, 2 - treated hypertension, 3 – untreated hypetension")
    }

	all_diabetesCategory<-unique(sub_data$diabetesCategory)
    all_possible_diabetesCategory <- factor(c(0,1))
	if(!all(levels(all_possible_htn) %in% all_htn)){
		stop("Diabetes variable should take values 0 – for no diabetes diagnosis, 1 - for diabetes diagnosis")
    }

	all_hcl<-unique(sub_data$hcl)
    all_possible_hcl <- factor(c(0,1))
	if(!all(levels(all_possible_htn) %in% all_htn)){
		stop("Hypercholesterolemia variable should take values 0 – for no hypercholesterolemia diagnosis, 1 - for hypercholesterolemia diagnosis")
    }

	all_bmiCategory<-unique(sub_data$bmiCategory)
    all_possible_bmiCategory <- factor(c(1,2,3))
	if(!all(levels(all_possible_bmiCategory) %in% all_bmiCategory)){
		stop("Body Mass Index variable should take values categories 1 – for Healthy weight (BMI<25), 2 – for Overweight (BMI≥25 and BMI<30), 3 – for Obese weight (BMI≥30)")
    }

	all_qRiskCategory<-unique(sub_data$qRiskCategory)
    all_possible_qRiskCategory <- factor(c(0,1,2))
	if(!all(levels(all_possible_qRiskCategory) %in% all_qRiskCategory)){
		stop("Cardiac risk category variable should take values categories
		0 – for moderate risk (QRISK2≥20 and QRISK2<40), 1 - for high risk (QRISK2≥40 or diagnosis of CVD), 2 – for low risk (QRISK2<20)
		")
    }

    all_statins <- unique(sub_data$statins)
    all_possible_statins <- factor(c(0,1))
	if(!all(levels(all_possible_statins) %in% all_statins)){
		stop("Statin prescription variable should take values 0 – for no statin prescription, 1 - for statin prescription")
    }

    life_expectancy_table <- NULL
	life_expectancy_table <- suppressWarnings(sqldf::sqldf(
	'SELECT 	sub_data.age,sub_data.gender,sub_data.townsend,sub_data.smokerCategory,sub_data.htn,sub_data.diabetesCategory,sub_data.hcl,sub_data.bmiCategory,sub_data.qRiskCategory,sub_data.statins,my_longevity_data.b,my_longevity_data.a0,my_longevity_data.LE
	FROM sub_data left join my_longevity_data
	on
	sub_data.age=my_longevity_data.age and
	sub_data.gender=my_longevity_data.gender and
	sub_data.townsend=my_longevity_data.townsend and
	sub_data.smokerCategory=my_longevity_data.smokerCategory and
	sub_data.htn=my_longevity_data.htn and
	sub_data.diabetesCategory=my_longevity_data.diabetesCategory and
	sub_data.hcl=my_longevity_data.hcl and
	sub_data.bmiCategory=my_longevity_data.bmiCategory and
	sub_data.qRiskCategory=my_longevity_data.qRiskCategory and
	sub_data.statins=my_longevity_data.statins
	'
	))


return(life_expectancy_table)
}
