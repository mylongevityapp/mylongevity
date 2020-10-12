#' mylongevity_method1
#'
#' This function loads a file as a data frame of clients.
#' Using the argument 'indexes_of_variables', this function selects the columns of factors
#' and matches them to outcomes of life expectancy calculator https://mylongevity.org/about
#' @param data given data frame of clients
#' @param indexes_of_variables indices for columns of interest.
#' @details This method matches the attributes of inputted data with results of life expectancies from the landmark analysis described in Kulinskaya et al. (2020c). For a set of given attributes, this method produces a table with life expectancies at a given age  similar to those provided on the website https://mylongevity.org/ , see also Kulinskaya et al. (2020a).
#'
#' For mylongevity_method1, user has to specify the data and columns using the argument indexes_of_variables in following order: age,gender,townsend,smokerCategory,htn,diabetesCategory,hcl,bmiCategory,qRiskCategory,statins.
#'
#' The definition of columns are:
#'    -	age is a variable with numeric value which stands for number of years from the date of birth. age should only take values between 60 and 85
#'    -	Gender is a string variable which stands for gender of individual with possible values "M" for males and "F" for females
#'    -	Townsend is a deprivation index which takes values 1 (least deprived), 2, 3, 4, 5 (most deprived). The Townsend variable measures the prosperity of area where individual lives.
#'    -	smokerCategory is a categorical variable with numeric value which defines if individual is smoker or not: (1 - no smoker, 2 - ex smoker, 3 - smoker)
#'    -	htn is a categorical variable with numeric value for treated hypertension status. 1 - no hypertension, 2 - treated hypertension, 3 - untreated hypertension
#'    -	diabetesCategory is a categorical variable with numeric value for diabetes status. 0 - no diabetes, 1 - diabetes
#'    -	hcl is a categorical variable with numeric value for hypercholesteromia. 0 - no hypercholesteromia, 1 - hypercholesteromia
#'    -	bmiCategory is a categorical variable with numeric value for Body Mass Index  categories (1 - Healthy weight (BMI<25), 2 - Overweight (BMI>=25 and BMI<30), 3 - Obese (BMI>=30))
#'    -	qRiskCategory is a categorical variable with numeric value for cardiovascular disease risk measured by QRISK2 https://qrisk.org/2017/ (2 - for low risk (QRISK2<20), 0 - for moderate risk (QRISK2>=20 and QRISK2<40), 1 - for high risk (QRISK2>=40 or diagnosis of CVD))
#'    -	statins is a categorical variable with numeric value for statin prescription status (0 - no statin intake, 1 - on statin/prescribed statin)
#' @keywords life_expectancy
#' @export
#' @examples
#' set.seed(1234)
#' library(mylongevity)
#' n<-1000
#' age <-round(runif(n, min = 60, max = 85))
#' gender <- c(rep('M',times=n/2),rep('F',times=n/2))
#' townsend <- round(runif(n, min = 1, max = 5))
#' smokerCategory <- round(runif(n, min = 1, max = 3))
#' htn  <- round(runif(n, min = 1, max = 3))
#' diabetesCategory  <- round(runif(n, min = 0, max = 1))
#' hcl <- round(runif(n, min = 0, max = 1))
#' bmiCategory <- round(runif(n, min = 1, max = 3))
#' qRiskCategory <- round(runif(n, min = 0, max = 2))
#' statins <- round(runif(n, min = 0, max = 1))
#' data_of_clients <- data.frame(age,gender,townsend,smokerCategory,htn,diabetesCategory,hcl,bmiCategory,qRiskCategory,statins)
#' indexes_for_columns <- c(1,2,3,4,5,6,7,8,9,10)
#' mylongevity_method1(data=data_of_clients,indexes_of_variables=indexes_for_columns)
#' @return data frame with life expectancies for given data frame of clients
#'
mylongevity_method1<-function(data,indexes_of_variables){
  if (missing(data))
    stop("Must specify a dataset via the 'data' argument.")
  if (missing(indexes_of_variables))
    stop("Must specify a list of variables via the 'index_of_variables' argument")
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
  for(i in 1:length(all_genders)){
    if(!(stringr::str_detect(all_genders[i], "M")|stringr::str_detect(all_genders[i], "F"))){
      stop("Gender variable should take values 'M' for males and 'F' for females.")
    }
  }

  all_townsend <- as.character(unique(sub_data$townsend))
  for(townsend in 1:length(all_townsend)){
    if(!(stringr::str_detect(all_townsend[townsend], "1")|stringr::str_detect(all_townsend[townsend], "2")|stringr::str_detect(all_townsend[townsend], "3")|stringr::str_detect(all_townsend[townsend], "4")|stringr::str_detect(all_townsend[townsend], "5"))){
      stop("Townsend variable should take values between 1 to 5")
    }
  }
  all_smokerCategory<-as.character(unique(sub_data$smokerCategory))
  for(smokingCat in 1:length(all_smokerCategory)){
    if(!(stringr::str_detect(all_smokerCategory[smokingCat], "1")|stringr::str_detect(all_smokerCategory[smokingCat], "2")|stringr::str_detect(all_smokerCategory[smokingCat], "3"))){
      stop("Smoking category variable should take values 1 - for no smoker,2 - for ex smoker,3 - for smoker")
    }
  }
  all_htn<-as.character(unique(sub_data$htn))
  for(htnCat in 1:length(all_htn)){
    if(!(stringr::str_detect(all_htn[htnCat],"1")|stringr::str_detect(all_htn[htnCat],"2")|stringr::str_detect(all_htn[htnCat],"3"))){
      stop("Treated hypertension variable should take values 1 - for no hypertension, 2 - for treated hypertension, 3 - for untreated hypertension ")
    }
  }
  all_diabetesCategory<-as.character(unique(sub_data$diabetesCategory))
  for(diabetesCat in 1:length(all_diabetesCategory)){
    if(!(stringr::str_detect(all_diabetesCategory[diabetesCat], "0")|stringr::str_detect(all_diabetesCategory[diabetesCat], "1"))){
      stop("Diabetes variable should only take values either 0 - for no diagnosis of diabetes or 1 - for diagnosis of diabetes")
    }
  }
  all_hcl<-as.character(unique(sub_data$hcl))
  for(hclCat in 1:length(all_hcl)){
    if(!(stringr::str_detect(all_hcl[hclCat], "0")|stringr::str_detect(all_hcl[hclCat], "1"))){
      stop("Hypercholesterolemia variable should only take values either 0 - for no diagnosis of hypercholeterolemia or 1 - for diagnosis of hypercholesteromia")
    }
  }

  all_bmiCategory<-unique(sub_data$bmiCategory)
  for(bmiCat in 1:length(all_bmiCategory)){
    if(!(stringr::str_detect(all_bmiCategory[bmiCat], "1")|stringr::str_detect(all_bmiCategory[bmiCat], "2")|stringr::str_detect(all_bmiCategory[bmiCat], "3"))){
      stop("Body Mass Index variable should only take value categories 1 - for Healthy weight (BMI less than or equal to 24), 2 - for Overweight (BMI between 25 and 29) or 3 for Obese weight (BMI more than or equal 30)")
    }
  }

  all_qRiskCategory<-unique(sub_data$qRiskCategory)
  for(qriskCat in 1:length(all_qRiskCategory)){
    if(!(stringr::str_detect(all_qRiskCategory[qriskCat], "0")|stringr::str_detect(all_qRiskCategory[qriskCat], "1")|stringr::str_detect(all_qRiskCategory[qriskCat], "2"))){
      stop("Cardiac risk category variable should take value categories 0 - for moderate risk (Cardiac risk between 20 and 39), 1 - for high risk (Cardiac risk equal or more than 40 or diagnosis of cardiovascular disease) or 2 - for low risk (Cardiac risk less than 20)")
    }
  }

  all_statins <- unique(sub_data$statins)
  for(statinCat in 1:length(all_statins)){
    if(!(stringr::str_detect(all_statins[statinCat], "0")|stringr::str_detect(all_statins[statinCat], "1"))){
      stop("Statin prescription variable should take values 0 - for absence of statin prescription or 1 - for presence of statin prescription")
    }
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
