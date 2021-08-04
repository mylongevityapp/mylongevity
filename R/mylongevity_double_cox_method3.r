#' mylongevity_double_cox_method3
#'
#' This function loads a file as a data frame of clients.
#' Using the argument 'indexes_of_variables', this function selects the columns of factors
#' and matches them to outcomes of hazard ratios from ischemic stroke study
#' @param data given data frame of clients
#' @param indexes_of_variables indices for columns of interest.
#' @param age_of_diagnosis age of diagnosis for ischemic stroke
#' @param time_past_from_diagnosis time past from diagnosis in years from the date of ischemic stroke
#' @details This method matches the attributes of inputted data with results of hazard ratios from ischemic stroke study and produces a table with life expectancies.
#'
#' For mylongevity_double_cox_method3, user has to specify the data and columns using the argument indexes_of_variables in following order: birth_cohort,	IMD_Quintile,	BMI_category,	antiplatelet_drugs_drugs,	,COPD	,heart_failure	,myocardial_infarction,	PVD_PAD,	SMOKING,	anticoagulant_agents,	Diabetes_factor,	sex,	groupscases,	antihypertensive_agents
#'
#' The definition of columns are:
#'    - birth_cohort: indicator for year of birth (0 - born in 1908-1920, 1 - born in 1921-1930, 2 - born in  1931-1940, 3 - born in 1941 - 1960)
#'    - IMD_Quintile: deprivation index (1 (least deprived), 2, 3, 4, 5 (most deprived))
#'    - BMI_category: categorical variable with numeric value for Body Mass Index  categories (0 - Healthy weight (BMI<25), 1 - Overweight (BMI>=25 and BMI<30) and Obese (BMI>=30))
#'    - antiplatelet_drugs_drugs: indicator for antiplatelet_drugs_drugs (0 - no antiplatelet_drugs_drugs or 1 - no antiplatelet_drugs_drugs)
#'    - COPD:  indicator for chronic obstructive pulmonary disease (0 - absense of chronic obstructive pulmonary disease or 1 - presence of chronic obstructive pulmonary disease)
#'    - heart_failure:  indicator for heart failure (0 - absense of heart failure or 1 - presence of heart failure)
#'    - myocardial_infarction:	indicator for myocardial infarction (0 - absense of myocardial infarction or 1 - presence of myocardial infarction)
#'    - PVD_PAD: indicator for peripheral vascular disease (0 - absense of peripheral vascular disease or 1 - presence of peripheral vascular disease )
#'    - SMOKING: indicator of smoking status (0 non smoker, 1 - current smoker, 2 - former smoker)
#'    - anticoagulant_agents: indicator for anticoagulant agents (0 - no  anticoagulant agents, 1 - yes anticoagulant agents )
#'    - Diabetes_factor: is a categorical variable with numeric value for diabetes status (0 - absense of diabetes or 1 - presence of diabetes and treated, 2 - absense of diabetes and untreated)
#'    - sex: indicator for gender (0 - Female , 1 - Male)
#'    - groupscases: indicator of cases control group (0 - without ischemic stroke  or 1 - with ischemic stroke)
#'    - antihypertensive_agents: indicator for antihypertensive agents (0 - no  antihypertensive agents, 1 - yes antihypertensive agents)
#' @keywords life_expectancy
#' @return data frame with life expectancies for given data frame of clients
#' @export
mylongevity_double_cox_method3<-function(data,indexes_of_variables,age_of_diagnosis,time_past_from_diagnosis){
  if (missing(data))
    stop("Must specify a dataset via the 'data' argument.")
  if (missing(indexes_of_variables))
    stop("Must specify a list of variables via the 'index_of_variables' argument")
  names_for_columns<-c("birth_cohort","IMD_Quintile","BMI_category","antiplatelet_drugs_drugs","COPD","heart_failure","myocardial_infarction","PVD_PAD","SMOKING","anticoagulant_agents","Diabetes_factor","sex","groupscases","antihypertensive_agents")
  if(length(indexes_of_variables)!=length(names_for_columns)){
    stop("Some required columns are missing.
	Please specify all the required indices of columns in the dataset in the following order :
	birth_cohort,IMD_Quintile,BMI_category,antiplatelet_drugs_drugs,COPD,heart_failure,myocardial_infarction,PVD_PAD,SMOKING,anticoagulant_agents,Diabetes_factor,sex,groupscases,antihypertensive_agents")
  }
  if (missing(age_of_diagnosis))
  stop("Must specify age_of_diagnosis via the 'age_of_diagnosis' argument")

  if (missing(time_past_from_diagnosis))
  stop("Must specify time_past_from_diagnosis via the 'time_past_from_diagnosis' argument")


  sub_data <- data[,indexes_of_variables]
  names(sub_data) <- names_for_columns

  all_birth_cohort<-as.character(unique(sub_data$birth_cohort))
  for(birth_cohortCat in 1:length(all_birth_cohort)){
    if(!(stringr::str_detect(all_birth_cohort[birth_cohortCat], "0")|stringr::str_detect(all_birth_cohort[birth_cohortCat], "1")|stringr::str_detect(all_birth_cohort[birth_cohortCat], "2")|stringr::str_detect(all_birth_cohort[birth_cohortCat], "3"))){
      stop("Birth year variable should only take values either 0 - for individuals born in 1908-1920, 1 - for individuals born in 1921-1930,2 - for individuals born in 1931-1940, 3 - for individuals born in 1941 - 1960 ")
    }
  }


  all_IMD_Quintile <- as.character(unique(sub_data$IMD_Quintile))
  for(IMD_Quintile in 1:length(all_IMD_Quintile)){
    if(!(stringr::str_detect(all_IMD_Quintile[IMD_Quintile], "1")|stringr::str_detect(all_IMD_Quintile[IMD_Quintile], "2")|stringr::str_detect(all_IMD_Quintile[IMD_Quintile], "3")|stringr::str_detect(all_IMD_Quintile[IMD_Quintile], "4")|stringr::str_detect(all_IMD_Quintile[IMD_Quintile], "5"))){
      stop("IMD_Quintile variable should take values between 1 to 5")
    }
  }

  all_bmiCategory<-unique(sub_data$BMI_category)
  for(bmiCat in 1:length(all_bmiCategory)){
    if(!(stringr::str_detect(all_bmiCategory[bmiCat], "0")|stringr::str_detect(all_bmiCategory[bmiCat], "1"))){
      stop("Body Mass Index variable should only take value categories 0 - for Healthy weight (BMI less than or equal to 24), 1 - for Overweight (BMI between 25 and 29) or for Obese weight (BMI more than or equal 30)")
    }
  }


  all_antiplatelet_drugs_drugs<-as.character(unique(sub_data$antiplatelet_drugs_drugs))
  for(antiplatelet_drugs_drugsCat in 1:length(all_antiplatelet_drugs_drugs)){
    if(!(stringr::str_detect(all_antiplatelet_drugs_drugs[antiplatelet_drugs_drugsCat],"0")|stringr::str_detect(all_antiplatelet_drugs_drugs[antiplatelet_drugs_drugsCat],"1"))){
      stop("Antiplatelet drugs variable should take values 0 for no antiplatelet drugs, 1 for yes antiplatelet drugs  ")
    }
  }

  all_COPD<-as.character(unique(sub_data$COPD))
  for(COPDCat in 1:length(all_COPD)){
    if(!(stringr::str_detect(all_COPD[COPDCat],"0")|stringr::str_detect(all_COPD[COPDCat],"1"))){
      stop("Chronic obstructive pulmonary disease variable should take values 0 - for no Chronic obstructive pulmonary disease, 1 - for yes Chronic obstructive pulmonary disease ")
    }
  }


  all_hf<-as.character(unique(sub_data$heart_failure))
  for(hfCat in 1:length(all_hf)){
    if(!(stringr::str_detect(all_hf[hfCat], "0")|stringr::str_detect(all_hf[hfCat], "1"))){
      stop("Heart failure  variable  should be 0 for individual with no diagnosis of Heart failure, 1 - for individual with diagnosis of Heart failure")
    }
  }

  all_myocardial_infarction<-as.character(unique(sub_data$myocardial_infarction))
  for(myocardial_infarctionCat in 1:length(all_myocardial_infarction)){
    if(!(stringr::str_detect(all_myocardial_infarction[myocardial_infarctionCat], "0")|stringr::str_detect(all_myocardial_infarction[myocardial_infarctionCat], "1"))){
      stop("Hypercholesterolemia  variable  should be 0 for individual with no diagnosis of hypercholesterolemia, 1 - for individual with diagnosis of hypercholesterolemia")
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
      stop("Smoking category variable should take values 0 non smoker, 1 - current smoker, 2 - former smoker")
    }
  }

  all_anticoagulant_agents<-as.character(unique(sub_data$anticoagulant_agents))
  for(anticoagulant_agentsCat in 1:length(all_anticoagulant_agents)){
    if(!(stringr::str_detect(all_anticoagulant_agents[anticoagulant_agentsCat],"0")|stringr::str_detect(all_anticoagulant_agents[anticoagulant_agentsCat],"1"))){
      stop("Anticoagulant agents variable should take values 0 for no anticoagulant agents , 1 for yes anticoagulant agents")
    }
  }

  all_Diabetes_factor<-as.character(unique(sub_data$Diabetes_factor))
  for(Diabetes_factorCat in 1:length(all_Diabetes_factor)){
    if(!(stringr::str_detect(all_Diabetes_factor[Diabetes_factorCat], "0")|stringr::str_detect(all_Diabetes_factor[Diabetes_factorCat], "1")|stringr::str_detect(all_Diabetes_factor[Diabetes_factorCat], "2"))){
      stop("Diabetes  variable  should be 0 - for absense of diabetes or 1 - for presence of diabetes and treated, 2 - for absense of diabetes and untreated")
    }
  }

  all_genders <- as.character(unique(sub_data$sex))
  for(i in 1:length(all_genders)){
    if(!(stringr::str_detect(all_genders[i], "0")|stringr::str_detect(all_genders[i], "1"))){
      stop("Gender variable should take values '0' for females and '1' for males.")
    }
  }

  all_group<-as.character(unique(sub_data$groupscases))
  for(groupCat in 1:length(all_group)){
    if(!(stringr::str_detect(all_group[groupCat], "0")|stringr::str_detect(all_group[groupCat], "1"))){
      stop("Ischemic stroke variable should be 0 for individual with no diagnosis of Ischemic stroke, 1 - for individual with diagnosis of Ischemic stroke")
    }
  }

  all_antihypertensive_agents<-as.character(unique(sub_data$antihypertensive_agents))
  for(antihypertensive_agentsCat in 1:length(all_antihypertensive_agents)){
    if(!(stringr::str_detect(all_antihypertensive_agents[antihypertensive_agentsCat],"0")|stringr::str_detect(all_antihypertensive_agents[antihypertensive_agentsCat],"1"))){
      stop("Antihypertensive drugs variable should take values 0 for no antihypertensive drugs, 1 for yes antihypertensive drugs")
    }
  }


    life_expectancy_table <- NULL
	life_expectancy_table <- suppressWarnings(sqldf::sqldf(
	'SELECT
	sub_data.birth_cohort,
	sub_data.IMD_Quintile,
	sub_data.BMI_category,
	sub_data.antiplatelet_drugs_drugs,
	sub_data.COPD,sub_data.heart_failure,
	sub_data.myocardial_infarction,
	sub_data.PVD_PAD,sub_data.SMOKING,
	sub_data.anticoagulant_agents,
	sub_data.Diabetes_factor,
	sub_data.sex,
	sub_data.groupscases,
	sub_data.antihypertensive_agents,
	life_expectancy_table_for_IS_stroke.age_c,
	life_expectancy_table_for_IS_stroke.betaXinteraction_scale,
	life_expectancy_table_for_IS_stroke.betaXinteraction_shape,
	life_expectancy_table_for_IS_stroke.betas_shape,
	life_expectancy_table_for_IS_stroke.betas_scale,
	life_expectancy_table_for_IS_stroke.a,
	life_expectancy_table_for_IS_stroke.b
	FROM sub_data left join life_expectancy_table_for_IS_stroke
	on
	sub_data.birth_cohort=life_expectancy_table_for_IS_stroke.birth_cohort and
	sub_data.IMD_Quintile=life_expectancy_table_for_IS_stroke.IMD_Quintile and
	sub_data.BMI_category=life_expectancy_table_for_IS_stroke.BMI_category and
	sub_data.antiplatelet_drugs_drugs=life_expectancy_table_for_IS_stroke.antiplatelet_drugs_drugs and
	sub_data.COPD=life_expectancy_table_for_IS_stroke.COPD and
	sub_data.heart_failure=life_expectancy_table_for_IS_stroke.heart_failure and
	sub_data.myocardial_infarction=life_expectancy_table_for_IS_stroke.myocardial_infarction and
	sub_data.PVD_PAD=life_expectancy_table_for_IS_stroke.PVD_PAD and
	sub_data.SMOKING=life_expectancy_table_for_IS_stroke.SMOKING and
	sub_data.anticoagulant_agents=life_expectancy_table_for_IS_stroke.anticoagulant_agents and
	sub_data.Diabetes_factor=life_expectancy_table_for_IS_stroke.Diabetes_factor and
	sub_data.sex=life_expectancy_table_for_IS_stroke.sex and
	sub_data.groupscases=life_expectancy_table_for_IS_stroke.groupscases and
	sub_data.antihypertensive_agents=life_expectancy_table_for_IS_stroke.antihypertensive_agents
	'
	))
	###sigma2 is taken from the double cox model result
    life_expectancy_table$sigma2<-0.059
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
