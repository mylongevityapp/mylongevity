#' mylongevity_double_cox_method4
#'
#' This function loads a file as a data frame of clients.
#' Using the argument 'indexes_of_variables', this function selects the columns of factors
#' and matches them to outcomes of hazard ratios from transient ischemic stroke
#' @param data given data frame of clients
#' @param indexes_of_variables indices for columns of interest
#' @param age_of_diagnosis age of diagnosis for transient ischemic stroke
#' @param time_past_from_diagnosis time past from diagnosis in years from the date of transient ischemic stroke
#' @details This method matches the attributes of inputted data with results of hazard ratios from transient ischemic stroke study and produces a table with life expectancies..
#'
#' For mylongevity_double_cox_method4, user has to specify the data and columns using the argument indexes_of_variables in following order: birth_cohort,sex,IMD_Quintile,BMI_category,asthma,COPD,CKD,myocardial_infarction,PVD_PAD,SMOKING,alcohol_cat,atrial_fibrillation,Diabetes_factor,anticoagulant_agents,groupscases,antihypertensive_agents,APL,heart_failure
#'
#' The definition of columns are:
#'    - birth_cohort: indicator for year of birth (0 - born in 1908-1920, 1 - born in 1921-1930, 2 - born in  1931-1940, 3 - born in 1941 - 1960)
#'    - sex: indicator for gender (1 - Female , 2 - Male)
#'    - IMD_Quintile: deprivation index (1 (least deprived), 2, 3, 4, 5 (most deprived))
#'    - BMI_category: categorical variable with numeric value for Body Mass Index  categories (0 - Healthy weight (BMI<25), 1 - Overweight (BMI>=25 and BMI<30) and Obese (BMI>=30))
#'    - asthma: indicator for asthma (0 - absense of asthma, 1 - presence of asthma)
#'    - COPD: indicator for chronic obstructive pulmonary disease (0 - absense of chronic obstructive pulmonary disease, 1 - presence of chronic obstructive pulmonary disease)
#'    - CKD: indicator for chronic kidney disease (0 - absense of chronic kidney disease , 1 - presence of chronic kidney disease)
#'    - myocardial_infarction:  indicator for myocardial infarction (0 - absense of myocardial infarction, 1 - presence of myocardial infarction)
#'    - PVD_PAD: indicator for peripheral vascular disease (0 - absense of peripheral vascular disease, 1 - presence of peripheral vascular disease)
#'    - SMOKING: indicator of smoking status (0 - non smoker, 1 - current smoker, 2 - former smoker)
#'    - alcohol_cat: indicator for alcohol intake variable (0 - non drinker, 1 - drinker)
#'    - atrial_fibrillation: indicator for atrial fibrillation  (0 - absense of atrial fibrillation, 1 - presence of atrial fibrillation)
#'    - Diabetes_factor: indicator for diabetes (0 - absense of diabetes, 1 - presence of diabetes and treated, 2 - absense of diabetes and untreated)
#'    - anticoagulant_agents: indicator for anticoagulant agents (0 - no  anticoagulant agents, 1 - yes anticoagulant agents)
#'    - groupscases: indicator of cases control group (0 - without transient ischemic attack, 1 - with transient ischemic attack)
#'    - antihypertensive_agents: indicator for antihypertensive agents (0 - no  antihypertensive agents, 1 - yes antihypertensive agents)
#'    - APL: indicator for antiplatelet therapy (0 - no antiplatelet therapy, 1 - yes antiplatelet therapy)
#'    - heart_failure: indicator for heart failure (0 - absense of heart failure, 1 - presence of heart failure)
#' @keywords life_expectancy
#' @return data frame with life expectancies for given data frame of clients
#' @export
#' @examples
#' set.seed(1234)
#' n<-1000
#' birth_cohort<-round(runif(n, min = 0, max = 3)) #: indicator for year of birth (0 - born in 1908-1920, 1 - born in 1921-1930, 2 - born in  1931-1940, 3 - born in 1941 - 1960)
#' sex<-round(runif(n, min = 1, max = 2)) #: indicator for gender (0 - Female , 1 - Male)
#' IMD_Quintile<-round(runif(n, min = 1, max = 5)) #: deprivation index (1 (least deprived), 2, 3, 4, 5 (most deprived))
#' BMI_category<-round(runif(n, min = 0, max = 1)) #: categorical variable with numeric value for Body Mass Index  categories (0 - Healthy weight (BMI<25), 1 - Overweight (BMI>=25 and BMI<30) and Obese (BMI>=30))
#' asthma<-round(runif(n, min = 0, max = 1)) #: indicator for asthma (0 - absence of asthma, 1 - presence of asthma)
#' COPD<-round(runif(n, min = 0, max = 1)) #: indicator for chronic obstructive pulmonary disease (0 - absense of chronic obstructive pulmonary disease, 1 - presence of chronic obstructive pulmonary disease)
#' CKD<-round(runif(n, min = 0, max = 1)) #: indicator for chronic kidney disease (0 - absence of chronic kidney disease , 1 - presence of chronic kidney disease)
#' myocardial_infarction<-round(runif(n, min = 0, max = 1)) #:  indicator for myocardial infarction (0 - absense of myocardial infarction, 1 - presence of myocardial infarction)
#' PVD_PAD<-round(runif(n, min = 0, max = 1)) #: indicator for peripheral vascular disease (0 - absence of peripheral vascular disease, 1 - presence of peripheral vascular disease)
#' SMOKING<-round(runif(n, min = 0, max = 2)) #: indicator of smoking status (0 non-smoker, 1 - current smoker, 2 - former smoker)
#' alcohol_cat<-round(runif(n, min = 0, max = 1)) #: indicator for alcohol intake variable (0 – non -drinker, 1 - drinker)
#' atrial_fibrillation<-round(runif(n, min = 0, max = 1)) #: indicator for atrial fibrillation  (0 - absence of atrial fibrillation, 1 - presence of atrial fibrillation)
#' Diabetes_factor<-round(runif(n, min = 0, max = 2)) #: indicator for diabetes (0 – absence of diabetes or 1 - presence of diabetes and treated, 2 - absence of diabetes and untreated)
#' anticoagulant_agents<-round(runif(n, min = 0, max = 1)) #: indicator for anticoagulant agents (0 - no  anticoagulant agents, 1 - yes anticoagulant agents)
#' groupscases<-round(runif(n, min = 0, max = 1)) #: indicator of cases control group (0 - without transient ischemic attack, 1 - with transient ischemic attack)
#' antihypertensive_agents<-round(runif(n, min = 0, max = 1)) #: indicator for antihypertensive agents (0 - no  antihypertensive agents, 1 - yes antihypertensive agents)
#' APL<-round(runif(n, min = 0, max = 1)) #: indicator for antiplatelet therapy (0 - no antiplatelet therapy, 1 - yes antiplatelet therapy)
#' heart_failure<-round(runif(n, min = 0, max = 1)) #: indicator for heart failure (0 - absence of heart failure or 1 - presence of heart failure)
#' data<-data.frame(birth_cohort,sex,IMD_Quintile,BMI_category,asthma,COPD,CKD,myocardial_infarction,PVD_PAD,SMOKING,alcohol_cat,atrial_fibrillation,Diabetes_factor,anticoagulant_agents,groupscases,antihypertensive_agents,APL,heart_failure)
#' indexes_of_variables <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)
#' age_of_diagnosis=65
#' time_past_from_diagnosis=1
#' mylongevity_double_cox_method4(data, indexes_of_variables, age_of_diagnosis, time_past_from_diagnosis)
mylongevity_double_cox_method4<-function(data,indexes_of_variables, age_of_diagnosis,time_past_from_diagnosis){
  if (missing(data))
    stop("Must specify a dataset via the 'data' argument.")
  if (missing(indexes_of_variables))
    stop("Must specify a list of variables via the 'index_of_variables' argument")
  names_for_columns<-c("birth_cohort","sex","IMD_Quintile","BMI_category","asthma","COPD","CKD","myocardial_infarction","PVD_PAD","SMOKING","alcohol_cat","atrial_fibrillation","Diabetes_factor","anticoagulant_agents","groupscases","antihypertensive_agents","APL","heart_failure" )
  if(length(indexes_of_variables)!=length(names_for_columns)){
    stop("Some required columns are missing.
	Please specify all the required indices of columns in the dataset in the following order :
	birth_cohort,sex,IMD_Quintile,BMI_category,asthma,COPD,CKD,myocardial_infarction,PVD_PAD,SMOKING,alcohol_cat,atrial_fibrillation,Diabetes_factor,anticoagulant_agents,groupscases,antihypertensive_agents,APL,heart_failure")
  }

  if (missing(age_of_diagnosis))
  stop("Must specify age_of_diagnosis via the 'age_of_diagnosis' argument")

  if (missing(time_past_from_diagnosis))
  stop("Must specify time_past_from_diagnosis via the 'time_past_from_diagnosis' argument")


  sub_data <- data[,indexes_of_variables]
  names(sub_data) <- names_for_columns
  all_ages <- unique(sub_data$age)

  all_birth_cohort<-as.character(unique(sub_data$birth_cohort))
  for(birth_cohortCat in 1:length(all_birth_cohort)){
    if(!(stringr::str_detect(all_birth_cohort[birth_cohortCat], "0")|stringr::str_detect(all_birth_cohort[birth_cohortCat], "1")|stringr::str_detect(all_birth_cohort[birth_cohortCat], "2")|stringr::str_detect(all_birth_cohort[birth_cohortCat], "3"))){
      stop("Birth year variable should only take values either 0 - born in 1908-1920, 1 - born in 1921-1930, 2 - born in  1931-1940, 3 - born in 1941 - 1960")
    }
  }


  all_IMD_Quintile <- as.character(unique(sub_data$IMD_Quintile))
  for(IMD_Quintile in 1:length(all_IMD_Quintile)){
    if(!(stringr::str_detect(all_IMD_Quintile[IMD_Quintile], "1")|stringr::str_detect(all_IMD_Quintile[IMD_Quintile], "2")|stringr::str_detect(all_IMD_Quintile[IMD_Quintile], "3")|stringr::str_detect(all_IMD_Quintile[IMD_Quintile], "4")|stringr::str_detect(all_IMD_Quintile[IMD_Quintile], "5"))){
      stop("IMD_Quintile variable should take values between 1 to 5")
    }
  }


  all_genders <- as.character(unique(sub_data$sex))
  for(i in 1:length(all_genders)){
    if(!(stringr::str_detect(all_genders[i], "1")|stringr::str_detect(all_genders[i], "2"))){
      stop("Gender variable should take values '1' for females and '2' for males.")
    }
  }


  all_bmiCategory<-unique(sub_data$BMI_category)
  for(bmiCat in 1:length(all_bmiCategory)){
    if(!(stringr::str_detect(all_bmiCategory[bmiCat], "0")|stringr::str_detect(all_bmiCategory[bmiCat], "1")|stringr::str_detect(all_bmiCategory[bmiCat], "2"))){
      stop("Body Mass Index variable should only take value categories 0 - for Healthy weight (BMI less than or equal to 24), 1 - for Overweight (BMI between 25 and 29) or 2 for Obese weight (BMI more than or equal 30)")
    }
  }


  all_asthma<-as.character(unique(sub_data$asthma))
  for(asthmaCat in 1:length(all_asthma)){
    if(!(stringr::str_detect(all_asthma[asthmaCat], "0")|stringr::str_detect(all_asthma[asthmaCat], "1"))){
      stop("Asthma  variable  should be 0 for individual with no diagnosis of asthma, 1 - for individual with diagnosis of asthma")
    }
  }

  all_COPD<-as.character(unique(sub_data$COPD))
  for(COPDCat in 1:length(all_COPD)){
    if(!(stringr::str_detect(all_COPD[COPDCat],"0")|stringr::str_detect(all_COPD[COPDCat],"1"))){
      stop("Chronic obstructive pulmonary disease variable should take values 0 - for no Chronic obstructive pulmonary disease, 1 - for yes Chronic obstructive pulmonary disease ")
    }
  }

  all_CKD<-as.character(unique(sub_data$CKD))
  for(CKDCat in 1:length(all_CKD)){
    if(!(stringr::str_detect(all_CKD[CKDCat], "0")|stringr::str_detect(all_CKD[CKDCat], "1"))){
      stop("CKD  variable  should be 0 for individual with no diagnosis of CKD, 1 - for individual with diagnosis of CKD")
    }
  }

  all_myocardial_infarction<-as.character(unique(sub_data$myocardial_infarction))
  for(myocardial_infarctionCat in 1:length(all_myocardial_infarction)){
    if(!(stringr::str_detect(all_myocardial_infarction[myocardial_infarctionCat], "0")|stringr::str_detect(all_myocardial_infarction[myocardial_infarctionCat], "1"))){
      stop("Myocardial infarction  variable  should be 0 for individual with no diagnosis of Myocardial infarction, 1 - for individual with diagnosis of Myocardial infarction")
    }
  }


  all_pvd<-as.character(unique(sub_data$PVD_PAD))
  for(pvdCat in 1:length(all_pvd)){
    if(!(stringr::str_detect(all_pvd[pvdCat], "0")|stringr::str_detect(all_pvd[pvdCat], "1"))){
      stop("Peripheral Vascular Disease variable should only take values either 0 - for no diagnosis of Peripheral Vascular Disease or 1 - for diagnosis of Peripheral Vascular Disease")
    }
  }

  all_smokerCategory<-as.character(unique(sub_data$SMOKING))
  for(smokingCat in 1:length(all_smokerCategory)){
    if(!(stringr::str_detect(all_smokerCategory[smokingCat], "0")|stringr::str_detect(all_smokerCategory[smokingCat], "1")|stringr::str_detect(all_smokerCategory[smokingCat], "2"))){
      stop("Smoking category variable should take values 0 - for non smoker, 1 - for current smoker, 2 - for former smoker")
    }
  }


  all_alcohol_cat<-as.character(unique(sub_data$alcohol_cat))
  for(alcohol_catCat in 1:length(all_alcohol_cat)){
    if(!(stringr::str_detect(all_alcohol_cat[alcohol_catCat], "0")|stringr::str_detect(all_alcohol_cat[alcohol_catCat], "1"))){
      stop("Alcohol category variable should take values 0 - for non drinker, 1 - for drinker")
    }
  }


  all_atrial_fibrillation<-as.character(unique(sub_data$atrial_fibrillation))
  for(atrial_fibrillationCat in 1:length(all_atrial_fibrillation)){
    if(!(stringr::str_detect(all_atrial_fibrillation[atrial_fibrillationCat], "0")|stringr::str_detect(all_pvd[atrial_fibrillationCat], "1"))){
      stop("Atrial fibrillation variable should only take values either 0 - for no diagnosis of atrial fibrillation or 1 - for diagnosis of atrial fibrillation")
    }
  }

  all_Diabetes_factor<-as.character(unique(sub_data$Diabetes_factor))
  for(Diabetes_factorCat in 1:length(all_Diabetes_factor)){
    if(!(stringr::str_detect(all_Diabetes_factor[Diabetes_factorCat], "0")|stringr::str_detect(all_Diabetes_factor[Diabetes_factorCat], "1")|stringr::str_detect(all_Diabetes_factor[Diabetes_factorCat], "2"))){
      stop("Diabetes  variable  should be 0 - for absense of diabetes or 1 - for presence of diabetes and treated, 2 - for absense of diabetes and untreated")
    }
  }

  all_anticoagulant_agents<-as.character(unique(sub_data$anticoagulant_agents))
  for(anticoagulant_agentsCat in 1:length(all_anticoagulant_agents)){
    if(!(stringr::str_detect(all_anticoagulant_agents[anticoagulant_agentsCat],"0")|stringr::str_detect(all_anticoagulant_agents[anticoagulant_agentsCat],"1"))){
      stop("Anticoagulant agents variable should take values 0 for no anticoagulant agents, 1 for yes anticoagulant agents")
    }
  }

  all_group<-as.character(unique(sub_data$groupscases))
  for(groupCat in 1:length(all_group)){
    if(!(stringr::str_detect(all_group[groupCat], "0")|stringr::str_detect(all_group[groupCat], "1"))){
      stop("Transient ischemic stroke variable should be 0 for individual with no diagnosis of transient ischemic stroke, 1 - for individual with diagnosis of transient ischemic stroke")
    }
  }

  all_antihypertensive_agents<-as.character(unique(sub_data$antihypertensive_agents))
  for(antihypertensive_agentsCat in 1:length(all_antihypertensive_agents)){
    if(!(stringr::str_detect(all_antihypertensive_agents[antihypertensive_agentsCat],"0")|stringr::str_detect(all_antihypertensive_agents[antihypertensive_agentsCat],"1"))){
      stop("Antihypertensive drugs variable should take values 0 for no antihypertensive drugs, 1 for yes antihypertensive drugs  ")
    }
  }

  all_APL<-as.character(unique(sub_data$APL))
  for(APLCat in 1:length(all_APL)){
    if(!(stringr::str_detect(all_APL[APLCat], "0")|stringr::str_detect(all_APL[APLCat], "1"))){
      stop("APL variable  should be 0 for individual with no diagnosis of APL, 1 - for individual with diagnosis of APL")
    }
  }


  all_hf<-as.character(unique(sub_data$heart_failure))
  for(hfCat in 1:length(all_hf)){
    if(!(stringr::str_detect(all_hf[hfCat], "0")|stringr::str_detect(all_hf[hfCat], "1"))){
      stop("Heart failure  variable  should be 0 for individual with no diagnosis of Heart failure, 1 - for individual with diagnosis of Heart failure")
    }
  }


    life_expectancy_table <- NULL
	life_expectancy_table <- suppressWarnings(sqldf::sqldf(
	'SELECT
    sub_data.birth_cohort,
	sub_data.sex,
	sub_data.IMD_Quintile,
	sub_data.BMI_category,
	sub_data.asthma,
	sub_data.COPD,
	sub_data.CKD,
	sub_data.myocardial_infarction,
	sub_data.PVD_PAD,
	sub_data.SMOKING,
	sub_data.alcohol_cat,
	sub_data.atrial_fibrillation,
    sub_data.Diabetes_factor,
	sub_data.anticoagulant_agents,
	sub_data.groupscases,
	sub_data.antihypertensive_agents,
	sub_data.APL,
	sub_data.heart_failure,
	life_expectancy_table_for_TIA_stroke.a,
	life_expectancy_table_for_TIA_stroke.b,
	life_expectancy_table_for_TIA_stroke.betas_shape,
	life_expectancy_table_for_TIA_stroke.betas_scale,
	life_expectancy_table_for_TIA_stroke.age_c
	FROM sub_data left join life_expectancy_table_for_TIA_stroke
	on
	sub_data.birth_cohort=life_expectancy_table_for_TIA_stroke.birth_cohort and
	sub_data.sex=life_expectancy_table_for_TIA_stroke.sex and
	sub_data.IMD_Quintile=life_expectancy_table_for_TIA_stroke.IMD_Quintile and
	sub_data.BMI_category=life_expectancy_table_for_TIA_stroke.BMI_category and
	sub_data.asthma=life_expectancy_table_for_TIA_stroke.asthma and
	sub_data.COPD=life_expectancy_table_for_TIA_stroke.COPD and
	sub_data.CKD=life_expectancy_table_for_TIA_stroke.CKD and
	sub_data.myocardial_infarction=life_expectancy_table_for_TIA_stroke.myocardial_infarction and
	sub_data.PVD_PAD=life_expectancy_table_for_TIA_stroke.PVD_PAD and
	sub_data.SMOKING=life_expectancy_table_for_TIA_stroke.SMOKING and
	sub_data.alcohol_cat=life_expectancy_table_for_TIA_stroke.alcohol_cat and
	sub_data.atrial_fibrillation=life_expectancy_table_for_TIA_stroke.atrial_fibrillation and
	sub_data.Diabetes_factor=life_expectancy_table_for_TIA_stroke.Diabetes_factor and
	sub_data.anticoagulant_agents=life_expectancy_table_for_TIA_stroke.anticoagulant_agents and
	sub_data.groupscases=life_expectancy_table_for_TIA_stroke.groupscases and
	sub_data.antihypertensive_agents=life_expectancy_table_for_TIA_stroke.antihypertensive_agents and
	sub_data.APL=life_expectancy_table_for_TIA_stroke.APL and
	sub_data.heart_failure=life_expectancy_table_for_TIA_stroke.heart_failure
	'
	))



    ###sigma2 is taken from the double cox model result
	life_expectancy_table$sigma2<-0.066
	life_expectancy_table$age_of_diagnosis<-age_of_diagnosis
	life_expectancy_table$time_past_from_diagnosis<-time_past_from_diagnosis

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


	life_expectancy_table$cumH_at_t_time_past_from_diagnosis<-
	cumH(time_to_event=life_expectancy_table$time_past_from_diagnosis,a=life_expectancy_table$a,
	b=life_expectancy_table$b,
	beta_scaleU=(life_expectancy_table$betas_scale+life_expectancy_table$age_c*life_expectancy_table$age_of_diagnosis),
	beta_shapeU=life_expectancy_table$betas_shape)

	life_expectancy_table$integral_of_Sx_numerator<-v.life_expectancy(time_past_from_diagnosis=life_expectancy_table$time_past_from_diagnosis,age_of_diagnosis=life_expectancy_table$age_of_diagnosis,a=life_expectancy_table$a,b=life_expectancy_table$b,
	beta_scaleU=(life_expectancy_table$betas_scale+life_expectancy_table$age_c*life_expectancy_table$age_of_diagnosis),beta_shapeU=life_expectancy_table$betas_shape,sigma2=life_expectancy_table$sigma2)

	life_expectancy_table$life_expectancy<-life_expectancy_table$integral_of_Sx_numerator/exp(-life_expectancy_table$cumH_at_t_time_past_from_diagnosis)






	############################################numerator###################################################
	############################################numerator###################################################
	# integral_of_Sx_Weibull<-function(time_to_event,a,b,beta_scaleU,beta_shapeU,sigma2){
		# Sx<-(1+sigma2*(exp(beta_scaleU)*(time_to_event/a)^(b*exp(beta_shapeU))))^{-1/sigma2}
		# return(Sx)
	# }
	# life_expectancy_Weibull<-function(time_past_from_diagnosis,age_of_diagnosis,a,b,beta_scaleU,beta_shapeU,sigma2){
	# integrate(integral_of_Sx_Weibull, lower=time_past_from_diagnosis, upper=Inf,a=a,b=b,beta_scaleU=beta_scaleU,beta_shapeU=beta_shapeU,sigma2=sigma2)$value[1]
	# }
	# v.life_expectancy<- Vectorize(life_expectancy_Weibull)
	######################################################################################################
	# cumH<-function(time_to_event,a,b,beta_scaleU,beta_shapeU){exp(beta_scaleU)*(time_to_event/a)^{b*exp(beta_shapeU)}}


	# life_expectancy_table$cumH_at_t_time_past_from_diagnosis <-
	# cumH(time_to_event=life_expectancy_table$time_past_from_diagnosis,a=life_expectancy_table$a,
	# b=life_expectancy_table$b,beta_scaleU=(life_expectancy_table$betas_scale),
	# beta_shapeU=life_expectancy_table$betas_shape)

	# life_expectancy_table$integral_of_Sx_numerator <- v.life_expectancy(time_past_from_diagnosis=life_expectancy_table$time_past_from_diagnosis,a=life_expectancy_table$a,b=life_expectancy_table$b,
	# beta_scaleU=life_expectancy_table$betas_scale,beta_shapeU=life_expectancy_table$betas_shape,sigma2=life_expectancy_table$sigma2)
	# life_expectancy_table$life_expectancy <- life_expectancy_table$integral_of_Sx_numerator/exp(-life_expectancy_table$cumH_at_t_time_past_from_diagnosis)

return(life_expectancy_table)
}
