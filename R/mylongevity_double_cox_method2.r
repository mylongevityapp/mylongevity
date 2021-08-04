#' mylongevity_double_cox_method2
#'
#' This function loads a file as a data frame of clients.
#' Using the argument 'indexes_of_variables', this function selects the columns of factors
#' and matches them to outcomes of life expectancy calculator from hormone replace therapy study
#' @param data given data frame of clients
#' @param indexes_of_variables indices for columns of interest
#' @param age_of_diagnosis age of start of intake for hormone replace therapy
#' @param time_past_from_diagnosis time past from start of hormone replace therapy in years
#' @details This method matches the attributes of inputted data with results of hazard ratios from hormone replace therapy study and produces a table with life expectancies
#' For mylongevity_double_cox_method2, user has to specify the data and columns using the argument indexes_of_variables in following order: B_cohort,BMIsmoke,CHD,deprivation,DMIIsmoke,hrtcat,hypertension,opho
#'
#' The definition of columns are:
#'    -	B_cohort : indicator for year of birth  (0 - born in 1921-1930 , 1 - born in 1931-1940,  2 - born in  1941-1950, 3 - born in 1951-1960)
#'    -	BMIsmoke : indicator for interaction for body mass index and smoking (0 - Healthy or Overweight and Non Smoker, 1 - healthy weight and current smoker, 2 - healthy weight and ex smoker, 3, Obese and current smoker, 4 - Obese and exsmoker, 5 - Obese and non smoker)
#'    -	CHD : indicator for heart failure (0 - absence of coronary heart disease , 1 - presence of coronary heart disease)
#'    - hrtcat : indicator for type of hormone replacement therapy (0 - non users of hormone replacement therapy, 1  - Combined Oestrogen and Progesteron , 2 - Oestrogen-only)
#'    - DMIIsmoke : interaction of diabetes and smoking variables (0 -  non diabetic and non smoker,1 - non diabetic  and current smoker ,2 - non diabetic and ex-smoker,3 - diabetic and current smoker,4 - diabetic  and ex smoker ,5 - diabetic and non-smoker)
#'    -	opho : indicator of cases control group (1 - both uterus and ovarian removed, 2 - ovaries removed but not uteras)
#'    -	hypertension: indicator for hypertension (0 - absence of Hypertension, 1 -  Hypertension)
#'    -	smokes: indicator of smoking status (0 non smoker, 1 - former smoker, 2 - current smoker)
#'    -	deprivation - deprivation index (0 - Low (Townsend 1 and Townsend 2), 1 - High (Townsend 4 and Townsend 5), 2 - Medium (Townsend 3))
#' @keywords life_expectancy
#' @return data frame with life expectancies for given data frame of clients
#' @export
mylongevity_double_cox_method2<-function(data,indexes_of_variables, age_of_diagnosis,time_past_from_diagnosis){
  if (missing(data))
    stop("Must specify a dataset via the 'data' argument.")
  if (missing(indexes_of_variables))
    stop("Must specify a list of variables via the 'index_of_variables' argument")
  names_for_columns<-c("B_cohort","BMIsmoke","CHD","deprivation","DMIIsmoke","hrtcat","hypertension","opho")
  if(length(indexes_of_variables)!=length(names_for_columns)){
    stop("Some required columns are missing.
	Please specify all the required indices of columns in the dataset in the following order :
	B_cohort,BMIsmoke,CHD,deprivation,DMIIsmoke,hrtcat,hypertension,opho")
  }
  if (missing(age_of_diagnosis))
    stop("Must specify age of hormone replace therapy via the 'age_of_diagnosis' argument")

  if (missing(time_past_from_diagnosis))
    stop("Must specify time past from_diagnosis via the 'time_past_from_diagnosis' argument")

  sub_data <- data[,indexes_of_variables]
  names(sub_data) <- names_for_columns
  all_birth_year<-as.character(unique(sub_data$B_cohort))
  for(birth_yearCat in 1:length(all_birth_year)){
    if(!(stringr::str_detect(all_birth_year[birth_yearCat], "0")|stringr::str_detect(all_birth_year[birth_yearCat], "1")|stringr::str_detect(all_birth_year[birth_yearCat], "2")|stringr::str_detect(all_birth_year[birth_yearCat], "3"))){
      stop("Birth year variable should only take values either 0 - for individuals born in 1930 and less, 1 - for individuals born in 1931-1940, 2 - for individuals born in 1941-1950, 3 - for individuals born in 1951-1960, ")
    }
  }

  all_birth_cohort<-as.character(unique(sub_data$B_cohort))
  for(birth_cohortCat in 1:length(all_birth_cohort)){
    if(!(stringr::str_detect(all_birth_cohort[birth_cohortCat], "0")|stringr::str_detect(all_birth_cohort[birth_cohortCat], "1")|stringr::str_detect(all_birth_cohort[birth_cohortCat], "2")|stringr::str_detect(all_birth_cohort[birth_cohortCat], "3"))){
      stop("Birth year variable should only take values either 0 - for individuals born in 1921-1930, 1 - for individuals born in 1931-1940,2 - for individuals born in 1941-1950,3 - for individuals born in 1951-1960")
    }
  }


  all_deprivation <- as.character(unique(sub_data$deprivation))
  for(deprivation in 1:length(all_deprivation)){
    if(!(stringr::str_detect(all_deprivation[deprivation], "0")|stringr::str_detect(all_deprivation[deprivation], "1")|stringr::str_detect(all_deprivation[deprivation], "2"))){
      stop("Deprivation variable should take values between 0 - Low (Townsend 1 and Townsend 2), 1 - High (Townsend 4 and Townsend 5), 2 - Medium (Townsend 3")
    }
  }

  all_chd<-as.character(unique(sub_data$CHD))
  for(chdCat in 1:length(all_chd)){
    if(!(stringr::str_detect(all_chd[chdCat], "0")|stringr::str_detect(all_chd[chdCat], "1"))){
      stop("Coronary Heart Disease  variable  should be 0 for individual with no diagnosis of Coronary Heart Disease, 1 - for individual with diagnosis of Coronary Heart Disease")
    }
  }

  all_hrtcat<-as.character(unique(sub_data$hrtcat))
  for(hrtCat in 1:length(all_hrtcat)){
    if(!(stringr::str_detect(all_hrtcat[hrtCat], "0")|stringr::str_detect(all_hrtcat[hrtCat], "1")|stringr::str_detect(all_hrtcat[hrtCat], "2"))){
      stop("Coronary Heart Disease  variable  should be 0 for individual with no hormone replacement therapy, 1 - for individual with Combined Oestrogen and Progesteron, 2 - for individual with  Oestrogen-only")
    }
  }

  all_DMIIsmoke<-as.character(unique(sub_data$DMIIsmoke))
  for(DMIIsmoke in 1:length(all_DMIIsmoke)){
    if(!(stringr::str_detect(all_DMIIsmoke[DMIIsmoke], "0")|stringr::str_detect(all_DMIIsmoke[DMIIsmoke], "1")|stringr::str_detect(all_DMIIsmoke[DMIIsmoke], "2")|stringr::str_detect(all_DMIIsmoke[DMIIsmoke], "3")|stringr::str_detect(all_DMIIsmoke[DMIIsmoke], "4")|stringr::str_detect(all_DMIIsmoke[DMIIsmoke], "5"))){
      stop("Interaction of diabetes and smoking variable should be 0 -  non diabetic and non smoker,1 - non diabetic  and current smoker ,2 - non diabetic and ex-smoker,3 - diabetic and current smoker,4 - diabetic  and ex smoker ,5 - diabetic and non-smoker")
    }
  }


  all_opho<-as.character(unique(sub_data$opho))
  for(ophoCat in 1:length(all_opho)){
    if(!(stringr::str_detect(all_opho[ophoCat], "0")|stringr::str_detect(all_opho[ophoCat], "1")|stringr::str_detect(all_opho[ophoCat], "2")|stringr::str_detect(all_opho[ophoCat], "3")|stringr::str_detect(all_opho[ophoCat], "4")|stringr::str_detect(all_opho[ophoCat], "5"))){
      stop("Interaction of diabetes and smoking variable should be 0 -  non diabetic and non smoker,1 - non diabetic  and current smoker ,2 - non diabetic and ex-smoker,3 - diabetic and current smoker,4 - diabetic  and ex smoker ,5 - diabetic and non-smoker")
    }
  }

  all_BMIsmoke<-as.character(unique(sub_data$BMIsmoke))
  for(BMIsmokeCat in 1:length(all_BMIsmoke)){
    if(!(stringr::str_detect(all_BMIsmoke[BMIsmokeCat], "0")|stringr::str_detect(all_BMIsmoke[BMIsmokeCat], "1")|stringr::str_detect(all_BMIsmoke[BMIsmokeCat], "2")|stringr::str_detect(all_BMIsmoke[BMIsmokeCat], "3")|stringr::str_detect(all_BMIsmoke[BMIsmokeCat], "4")|stringr::str_detect(all_BMIsmoke[BMIsmokeCat], "5"))){
      stop("Interaction of BMI and smoking variable should be 0 - Healthy or Overweight and Non Smoker, 1 - healthy weight and current smoker, 2 - healthy weight and ex smoker, 3, Obese and current smoker, 4 - Obese and exsmoker, 5 - Obese and non smoker")
    }
  }



  all_hypertension<-as.character(unique(sub_data$hypertension))
  for(hypertension in 1:length(all_hypertension)){
    if(!(stringr::str_detect(all_hypertension[hypertension], "0")|stringr::str_detect(all_hypertension[hypertension], "1")|stringr::str_detect(all_hypertension[hypertension], "2"))){
      stop("Hypertension  variable  should be 0 - absense of Hypertension, 1 - Treated Hypertension 2 - Untreated Hypertension")
    }
  }



  #life expectancy for cases
  life_expectancy_table <- NULL
  life_expectancy_table <- suppressWarnings(sqldf::sqldf(
    'SELECT
	sub_data.B_cohort,
	sub_data.BMIsmoke,
	sub_data.CHD,
	sub_data.deprivation,
	sub_data.DMIIsmoke,
	sub_data.hrtcat,
	sub_data.opho,
	sub_data.hypertension,
	life_expectancy_table_for_HRT.b,
	life_expectancy_table_for_HRT.a,
	life_expectancy_table_for_HRT.age_c,
	life_expectancy_table_for_HRT.betas_shape,
	life_expectancy_table_for_HRT.betas_scale
	FROM sub_data left join life_expectancy_table_for_HRT
	on
	sub_data.B_cohort=life_expectancy_table_for_HRT.B_cohort and
	sub_data.BMIsmoke=life_expectancy_table_for_HRT.BMIsmoke and
	sub_data.CHD=life_expectancy_table_for_HRT.CHD and
	sub_data.deprivation=life_expectancy_table_for_HRT.deprivation and
	sub_data.DMIIsmoke=life_expectancy_table_for_HRT.DMIIsmoke and
	sub_data.hrtcat=life_expectancy_table_for_HRT.hrtcat and
	sub_data.hypertension=life_expectancy_table_for_HRT.hypertension and
	sub_data.opho=life_expectancy_table_for_HRT.opho
	'
  ))
  ###sigma2 is taken from the double cox model result
  life_expectancy_table$sigma2<-0.098
  life_expectancy_table$age_of_diagnosis<-age_of_diagnosis
  life_expectancy_table$time_past_from_diagnosis<-time_past_from_diagnosis


  ###Need to double check the calculation below, in particular for age_c inclusion
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
  #######################################################################################################################################################################################################################################################
  #######################################################################################################################################################################################################################################################
  #######################################################################################################################################################################################################################################################


  ##life expectancy controls

  sub_data_controls<-sub_data
  sub_data_controls$opho<-0
  life_expectancy_table_controls <- NULL
  life_expectancy_table_controls <- suppressWarnings(sqldf::sqldf(
    'SELECT
	sub_data_controls.B_cohort,
	sub_data_controls.BMIsmoke,
	sub_data_controls.CHD,
	sub_data_controls.deprivation,
	sub_data_controls.DMIIsmoke,
	sub_data_controls.hrtcat,
	sub_data_controls.hypertension,
	sub_data_controls.opho,
	life_expectancy_table_for_HRT.b,
	life_expectancy_table_for_HRT.a,
	life_expectancy_table_for_HRT.age_c,
	life_expectancy_table_for_HRT.betas_shape,
	life_expectancy_table_for_HRT.betas_scale
	FROM sub_data_controls left join life_expectancy_table_for_HRT
	on
	sub_data_controls.B_cohort=life_expectancy_table_for_HRT.B_cohort and
	sub_data_controls.BMIsmoke=life_expectancy_table_for_HRT.BMIsmoke and
	sub_data_controls.CHD=life_expectancy_table_for_HRT.CHD and
	sub_data_controls.deprivation=life_expectancy_table_for_HRT.deprivation and
	sub_data_controls.DMIIsmoke=life_expectancy_table_for_HRT.DMIIsmoke and
	sub_data_controls.hrtcat=life_expectancy_table_for_HRT.hrtcat and
	sub_data_controls.hypertension=life_expectancy_table_for_HRT.hypertension and
	sub_data_controls.opho=life_expectancy_table_for_HRT.opho
	'
  ))
  ###sigma2 is taken from the double cox model result
  life_expectancy_table_controls$sigma2<-0.098
  life_expectancy_table_controls$age_of_diagnosis<-age_of_diagnosis
  life_expectancy_table_controls$time_past_from_diagnosis<-time_past_from_diagnosis


  ###Need to double check the calculation below, in particular for age_c inclusion
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


  life_expectancy_table_controls$cumH_at_t_time_past_from_diagnosis<-
    cumH(time_to_event=life_expectancy_table_controls$time_past_from_diagnosis,a=life_expectancy_table_controls$a,
         b=life_expectancy_table_controls$b,
         beta_scaleU=(life_expectancy_table_controls$betas_scale+life_expectancy_table_controls$age_c*life_expectancy_table_controls$age_of_diagnosis),
         beta_shapeU=life_expectancy_table_controls$betas_shape)

  life_expectancy_table_controls$integral_of_Sx_numerator<-v.life_expectancy(time_past_from_diagnosis=life_expectancy_table_controls$time_past_from_diagnosis,age_of_diagnosis=life_expectancy_table_controls$age_of_diagnosis,a=life_expectancy_table_controls$a,b=life_expectancy_table_controls$b,
                                                                             beta_scaleU=(life_expectancy_table_controls$betas_scale+life_expectancy_table_controls$age_c*life_expectancy_table_controls$age_of_diagnosis),beta_shapeU=life_expectancy_table_controls$betas_shape,sigma2=life_expectancy_table_controls$sigma2)

  life_expectancy_table_controls$life_expectancy<-life_expectancy_table_controls$integral_of_Sx_numerator/exp(-life_expectancy_table_controls$cumH_at_t_time_past_from_diagnosis)
  ##############################################################################################################################################################################################################################################################################



  ####relative life expectancies


  relative_life_expectancy_table<- NULL
  relative_life_expectancy_table <- suppressWarnings(sqldf::sqldf(
    'SELECT
	life_expectancy_table.B_cohort,
	life_expectancy_table.BMIsmoke,
	life_expectancy_table.CHD,
	life_expectancy_table.deprivation,
	life_expectancy_table.DMIIsmoke,
	life_expectancy_table.hypertension,
 	life_expectancy_table.opho,
	life_expectancy_table.b,
	life_expectancy_table.a,
	life_expectancy_table.age_c,
	life_expectancy_table.betas_shape,
	life_expectancy_table.betas_scale,
	life_expectancy_table.life_expectancy/life_expectancy_table_controls.life_expectancy as LEratio,
  life_expectancy_table.life_expectancy-life_expectancy_table_controls.life_expectancy as LEdifference
	FROM life_expectancy_table left join life_expectancy_table_controls
	on
	life_expectancy_table.B_cohort=life_expectancy_table_controls.B_cohort and
	life_expectancy_table.BMIsmoke=life_expectancy_table_controls.BMIsmoke and
	life_expectancy_table.CHD=life_expectancy_table_controls.CHD and
	life_expectancy_table.deprivation=life_expectancy_table_controls.deprivation and
	life_expectancy_table.DMIIsmoke=life_expectancy_table_controls.DMIIsmoke and
	life_expectancy_table.hrtcat=life_expectancy_table_controls.hrtcat and
	life_expectancy_table.hypertension=life_expectancy_table_controls.hypertension
	'
  ))



return(relative_life_expectancy_table)
}
