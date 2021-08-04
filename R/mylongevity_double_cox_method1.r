#' mylongevity_double_cox_method1
#'
#' This function loads a file as a data frame of clients.
#' Using the argument 'indexes_of_variables', this function selects the columns of factors
#' and matches them to outcomes of hazard ratios from type 2 diabetes study
#' @param data given data frame of clients
#' @param indexes_of_variables indices for columns of interest.
#' @param age_of_diagnosis age of diagnosis for type 2 diabetes mellitus
#' @param time_past_from_diagnosis time past from diagnosis in years from the date of type 2 diabetes mellitus
#' @details This method matches the attributes of inputted data with results of hazard ratios from type 2 diabetes study and produces a table with life expectancies
#' For mylongevity_double_cox_method1, user has to specify the data and columns using the argument indexes_of_variables in following order: atrial,birth_year,bmi_regrp,group,hf,hyperchol,hypert,miocarInfarct,pvd,sex,smokes,townsend
#' The definition of columns are:
#'    -	atrial: indicator for atrial fibrillation  (0 - absense of atrial fibrillation or 1 - presence of atrial fibrillation)
#'    -	birth_year  indicator for year of birth   (0 - born in 1930-1939, 1 - born in 1950-1960, 2 - born in  1940-1949)
#'    -	bmi_regrp : indicator for body mass index (0 - Healthy, 1 - Overweight, 2 - Obese)
#'    -	group : indicator of cases control group (0 - absense type 2 diabetes mellitus, 1 - presence type 2 diabetes mellitus)
#'    -	hf : indicator for heart failure (0 - absense of heart failure, 1 - presence of heart failure)
#'    -	hyperchol: indicator for hypercholesterolemia (0 - absense of hypercholesterolemia, 1 - presence of hypercholesterolemia)
#'    -	hypert: indicator for hypertension (0 - absense of Hypertension, 1 - Treated Hypertension 2 - Untreated Hypertension)
#'    -	miocarInfarct: indicator for myocardial infarction (0 - absense of myocardial infarction, 1 - presence of myocardial infarction)
#'    -	pvd: indicator for peripheral vascular disease (0 - absense of peripheral vascular disease, 1 - presence of peripheral vascular disease)
#'    -	sex: indicator for gender (0 - Female, 1 - Male)
#'    -	smokes: indicator of smoking status (0 non smoker, 1 - former smoker, 2 - current smoker)
#'    -	townsend - deprivation index (0 - (Townsend 3),  1 - (Townsend 1)  2 - (Townsend 2) , 3 - (Townsend 4), 4 - (Townsend 5))
#' @keywords life_expectancy
#' @import sqldf
#' @return data frame with life expectancies for given data frame of clients
#' res1<-mylongevity_double_cox_method1(data,indexes_of_variables, age_of_diagnosis,time_past_from_diagnosis)
#' @export
mylongevity_double_cox_method1<-function(data,indexes_of_variables, age_of_diagnosis,time_past_from_diagnosis){
  if (missing(data))
    stop("Must specify a dataset via the 'data' argument.")
  if (missing(indexes_of_variables))
    stop("Must specify a list of variables via the 'index_of_variables' argument")
  names_for_columns<-c("atrial","birth_year","bmi_regrp","diabetes","hf","hyperchol","hypert","miocarInfarct","pvd","sex","smokes","townsend")
  if(length(indexes_of_variables)!=length(names_for_columns)){
    stop("Some required columns are missing.
	Please specify all the required indices of columns in the dataset in the following order :
	atrial,birth_year,bmi_regrp,diabetes,hf,hyperchol,hypert,miocarInfarct,pvd,sex,smokes,townsend")
  }
  if (missing(age_of_diagnosis))
  stop("Must specify age_of_diagnosis via the 'age_of_diagnosis' argument")

  if (missing(time_past_from_diagnosis))
  stop("Must specify time_past_from_diagnosis via the 'time_past_from_diagnosis' argument")

  sub_data <- data[,indexes_of_variables]
  names(sub_data) <- names_for_columns

  all_atrial_fibrillation<-as.character(unique(sub_data$atrial))
  for(atrial_fibrillationCat in 1:length(all_atrial_fibrillation)){
    if(!(stringr::str_detect(all_atrial_fibrillation[atrial_fibrillationCat], "0")|stringr::str_detect(all_atrial_fibrillation[atrial_fibrillationCat], "1"))){
      stop("Atrial fibrillation variable should only take values either 0 - for no diagnosis of atrial fibrillation or 1 - for diagnosis of atrial fibrillation")
    }
  }


  all_birth_cohort<-as.character(unique(sub_data$birth_year))
  for(birth_cohortCat in 1:length(all_birth_cohort)){
    if(!(stringr::str_detect(all_birth_cohort[birth_cohortCat], "0")|stringr::str_detect(all_birth_cohort[birth_cohortCat], "1")|stringr::str_detect(all_birth_cohort[birth_cohortCat], "2"))){
      stop("Birth year variable should only take values either 0 - for individuals born in 1930-1939, 1 - for individuals born in 1950-1960,2 - for individuals born in 1940-1949")
    }
  }

  all_bmiCategory<-unique(sub_data$bmi_regrp)
  for(bmiCat in 1:length(all_bmiCategory)){
    if(!(stringr::str_detect(all_bmiCategory[bmiCat], "0")|stringr::str_detect(all_bmiCategory[bmiCat], "0")|stringr::str_detect(all_bmiCategory[bmiCat], "1")|stringr::str_detect(all_bmiCategory[bmiCat], "2"))){
      stop("Body Mass Index variable should only take value categories 0 - for Healthy weight (BMI less than or equal to 24), 1 - for Overweight (BMI between 25 and 29) or 2 for Obese weight (BMI more than or equal 30)")
    }
  }

  all_diabetes<-as.character(unique(sub_data$diabetes))
  for(diabetesCat in 1:length(all_diabetes)){
    if(!(stringr::str_detect(all_diabetes[diabetesCat], "0")|stringr::str_detect(all_diabetes[diabetesCat], "1"))){
      stop("Type 2 diabetes mellitus variable should be 0 for individual with no diagnosis of Type 2  diabetes mellitus, 1 - for individual with diagnosis of Type 2  diabetes mellitus")
    }
  }

  all_hf<-as.character(unique(sub_data$hf))
  for(hfCat in 1:length(all_hf)){
    if(!(stringr::str_detect(all_hf[hfCat], "0")|stringr::str_detect(all_hf[hfCat], "1"))){
      stop("Heart failure  variable  should be 0 for individual with no diagnosis of Heart failure, 1 - for individual with diagnosis of Heart failure")
    }
  }


  all_miocarInfarct<-as.character(unique(sub_data$miocarInfarct))
  for(miCat in 1:length(all_miocarInfarct)){
    if(!(stringr::str_detect(all_miocarInfarct[miCat], "0")|stringr::str_detect(all_miocarInfarct[miCat], "1"))){
      stop("Myocardial infarction  variable  should be 0 for individual with no diagnosis of Myocardial infarction, 1 - for individual with diagnosis of Myocardial infarction")
    }
  }

  all_pvd<-as.character(unique(sub_data$pvd))
  for(pvdCat in 1:length(all_pvd)){
    if(!(stringr::str_detect(all_pvd[pvdCat], "0")|stringr::str_detect(all_pvd[pvdCat], "1"))){
      stop("Peripheral vascular disease  variable  should be 0 for individual with no diagnosis of Peripheral vascular disease, 1 - for individual with diagnosis of Peripheral vascular disease")
    }
  }


  all_genders <- as.character(unique(sub_data$sex))
  for(i in 1:length(all_genders)){
    if(!(stringr::str_detect(all_genders[i], "0")|stringr::str_detect(all_genders[i], "1"))){
      stop("Gender variable should take values '1' for males and '0' for females.")
    }
  }


  all_hcl<-as.character(unique(sub_data$hyperchol))
  for(hclCat in 1:length(all_hcl)){
    if(!(stringr::str_detect(all_hcl[hclCat], "0")|stringr::str_detect(all_hcl[hclCat], "1")|stringr::str_detect(all_hcl[hclCat], "2"))){
      stop("Hypercholesterolemia variable should only take values either 0 - for no diagnosis of hypercholeterolemia or 1 - for treated hypercholesteromia or 2 - for untreated hypercholesteromia")
    }
  }

  all_htn<-as.character(unique(sub_data$hypert))
  for(htnCat in 1:length(all_htn)){
    if(!(stringr::str_detect(all_htn[htnCat],"0")|stringr::str_detect(all_htn[htnCat],"1")|stringr::str_detect(all_htn[htnCat],"2"))){
      stop("Treated hypertension variable should take values 0 - for no hypertension, 1 - for treated hypertension, 2 - for untreated hypertension ")
    }
  }


  all_smokerCategory<-as.character(unique(sub_data$smokes))
  for(smokingCat in 1:length(all_smokerCategory)){
    if(!(stringr::str_detect(all_smokerCategory[smokingCat], "0")|stringr::str_detect(all_smokerCategory[smokingCat], "1")|stringr::str_detect(all_smokerCategory[smokingCat], "2"))){
      stop("Smoking category variable should take values 0 - for no smoker,1 - for ex smoker,2 - for smoker")
    }
  }


  all_townsend <- as.character(unique(sub_data$townsend))
  for(townsend in 1:length(all_townsend)){
    if(!(stringr::str_detect(all_townsend[townsend], "0")|stringr::str_detect(all_townsend[townsend], "1")|stringr::str_detect(all_townsend[townsend], "2")|stringr::str_detect(all_townsend[townsend], "3")|stringr::str_detect(all_townsend[townsend], "4"))){
      stop("Townsend variable should take values between 0 to 4")
    }
  }



    life_expectancy_table <- NULL
	life_expectancy_table <- suppressWarnings(sqldf::sqldf(
	'SELECT
	life_expectancy_table_for_DMtype2.atrial,
	life_expectancy_table_for_DMtype2.birth_year,
	life_expectancy_table_for_DMtype2.bmi_regrp,
	life_expectancy_table_for_DMtype2.diabetes,
	life_expectancy_table_for_DMtype2.hf,
	life_expectancy_table_for_DMtype2.hyperchol,
	life_expectancy_table_for_DMtype2.hypert,
	life_expectancy_table_for_DMtype2.miocarInfarct,
	life_expectancy_table_for_DMtype2.pvd,
	life_expectancy_table_for_DMtype2.sex,
	life_expectancy_table_for_DMtype2.smokes,
	life_expectancy_table_for_DMtype2.townsend,
    life_expectancy_table_for_DMtype2.b,
	life_expectancy_table_for_DMtype2.a,
	life_expectancy_table_for_DMtype2.age_c,
	life_expectancy_table_for_DMtype2.betas_shape,
	life_expectancy_table_for_DMtype2.betaXinteraction_shape,
	life_expectancy_table_for_DMtype2.betas_scale,
	life_expectancy_table_for_DMtype2.betaXinteraction_scale
	FROM sub_data left join life_expectancy_table_for_DMtype2
	on
	sub_data.atrial=life_expectancy_table_for_DMtype2.atrial and
	sub_data.birth_year=life_expectancy_table_for_DMtype2.birth_year and
	sub_data.bmi_regrp=life_expectancy_table_for_DMtype2.bmi_regrp and
	sub_data.diabetes=life_expectancy_table_for_DMtype2.diabetes and
	sub_data.hf=life_expectancy_table_for_DMtype2.hf and
	sub_data.hyperchol=life_expectancy_table_for_DMtype2.hyperchol and
	sub_data.hypert=life_expectancy_table_for_DMtype2.hypert and
	sub_data.miocarInfarct=life_expectancy_table_for_DMtype2.miocarInfarct and
	sub_data.pvd=life_expectancy_table_for_DMtype2.pvd and
	sub_data.sex=life_expectancy_table_for_DMtype2.sex and
	sub_data.smokes=life_expectancy_table_for_DMtype2.smokes and
	sub_data.townsend=life_expectancy_table_for_DMtype2.townsend
	'
	))
	###sigma2 is taken from the double cox model result
    life_expectancy_table$sigma2<- 0.15
	life_expectancy_table$age_of_diagnosis<-age_of_diagnosis
	life_expectancy_table$time_past_from_diagnosis<-time_past_from_diagnosis


	#############################################numerator###################################################
	integral_of_Sx_Gompertz <- function(time_to_event,a,b,beta_scaleU,beta_shapeU,sigma2){
		Sx<-(1+sigma2*(exp(beta_scaleU)*(a/b)*(exp(b*exp(beta_shapeU)*time_to_event)-1)))^{-1/sigma2}
		return(Sx)
	}

	life_expectancy_Gompertz <-function(time_past_from_diagnosis,age_of_diagnosis,age_continuous,a,b,beta_scaleU,beta_shapeU,sigma2){
	integrate(integral_of_Sx_Gompertz, lower=time_past_from_diagnosis, upper=Inf,a=a,b=b,beta_scaleU=beta_scaleU+age_continuous*age_of_diagnosis,beta_shapeU=beta_shapeU,sigma2=sigma2)$value[1]
	}
	v.life_expectancy <- Vectorize(life_expectancy_Gompertz)
	#########################################################################################################
	cumH <- function(time_to_event,a,b,beta_scaleU,beta_shapeU){exp(beta_scaleU)*(a/b)*(exp(b*exp(beta_shapeU)*time_to_event)-1)}

	######################################################################################################
	cumH<-function(time_to_event,a,b,beta_scaleU,beta_shapeU){exp(beta_scaleU)*(a/b)*(exp(b*exp(beta_shapeU)*time_to_event)-1)}

	life_expectancy_table$cumH_at_t_time_past_from_diagnosis<-
	cumH(time_to_event=life_expectancy_table$time_past_from_diagnosis,a=life_expectancy_table$a,
	b=life_expectancy_table$b,
	beta_scaleU=(life_expectancy_table$betas_scale+life_expectancy_table$age_c*life_expectancy_table$age_of_diagnosis),
	beta_shapeU=life_expectancy_table$betas_shape)

	life_expectancy_table$integral_of_Sx_numerator<-v.life_expectancy(time_past_from_diagnosis=life_expectancy_table$time_past_from_diagnosis,age_of_diagnosis=life_expectancy_table$age_of_diagnosis,age_continuous=life_expectancy_table$age_c ,a=life_expectancy_table$a,b=life_expectancy_table$b,
	beta_scaleU=life_expectancy_table$betas_scale,beta_shapeU=life_expectancy_table$betas_shape,sigma2=life_expectancy_table$sigma2)

	life_expectancy_table$life_expectancy<-life_expectancy_table$integral_of_Sx_numerator/exp(-life_expectancy_table$cumH_at_t_time_past_from_diagnosis)


return(life_expectancy_table)
}
