% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mylongevity_double_cox_method3.r
\name{mylongevity_double_cox_method3}
\alias{mylongevity_double_cox_method3}
\title{mylongevity_double_cox_method3}
\usage{
mylongevity_double_cox_method3(
  data,
  indexes_of_variables,
  age_of_diagnosis,
  time_past_from_diagnosis
)
}
\arguments{
\item{data}{given data frame of clients}

\item{indexes_of_variables}{indices for columns of interest.}

\item{age_of_diagnosis}{age of diagnosis for ischemic stroke}

\item{time_past_from_diagnosis}{time past from diagnosis in years from the date of ischemic stroke}
}
\value{
data frame with life expectancies for given data frame of clients
}
\description{
This function loads a file as a data frame of clients.
Using the argument 'indexes_of_variables', this function selects the columns of factors
and matches them to outcomes of hazard ratios from ischemic stroke study
}
\details{
This method matches the attributes of inputted data with results of hazard ratios from ischemic stroke study and produces a table with life expectancies.

For mylongevity_double_cox_method3, user has to specify the data and columns using the argument indexes_of_variables in following order: birth_cohort,	IMD_Quintile,	BMI_category,	antiplatelet_drugs_drugs,	,COPD	,heart_failure	,myocardial_infarction,	PVD_PAD,	SMOKING,	anticoagulant_agents,	Diabetes_factor,	sex,	groupscases,	antihypertensive_agents

The definition of columns are:
   - birth_cohort: indicator for year of birth (0 - born in 1908-1920, 1 - born in 1921-1930, 2 - born in  1931-1940, 3 - born in 1941 - 1960)
   - IMD_Quintile: deprivation index (1 (least deprived), 2, 3, 4, 5 (most deprived))
   - BMI_category: categorical variable with numeric value for Body Mass Index  categories (0 - Healthy weight (BMI<25), 1 - Overweight (BMI>=25 and BMI<30) and Obese (BMI>=30))
   - antiplatelet_drugs_drugs: indicator for antiplatelet_drugs_drugs (0 - no antiplatelet_drugs_drugs or 1 - no antiplatelet_drugs_drugs)
   - COPD:  indicator for chronic obstructive pulmonary disease (0 - absense of chronic obstructive pulmonary disease or 1 - presence of chronic obstructive pulmonary disease)
   - heart_failure:  indicator for heart failure (0 - absense of heart failure or 1 - presence of heart failure)
   - myocardial_infarction:	indicator for myocardial infarction (0 - absense of myocardial infarction or 1 - presence of myocardial infarction)
   - PVD_PAD: indicator for peripheral vascular disease (0 - absense of peripheral vascular disease or 1 - presence of peripheral vascular disease )
   - SMOKING: indicator of smoking status (0 non smoker, 1 - current smoker, 2 - former smoker)
   - anticoagulant_agents: indicator for anticoagulant agents (0 - no  anticoagulant agents, 1 - yes anticoagulant agents )
   - Diabetes_factor: is a categorical variable with numeric value for diabetes status (0 - absense of diabetes or 1 - presence of diabetes and treated, 2 - absense of diabetes and untreated)
   - sex: indicator for gender (0 - Female , 1 - Male)
   - groupscases: indicator of cases control group (0 - without ischemic stroke  or 1 - with ischemic stroke)
   - antihypertensive_agents: indicator for antihypertensive agents (0 - no  antihypertensive agents, 1 - yes antihypertensive agents)
}
\examples{
set.seed(1234)
n<-1000
birth_cohort<-round(runif(n, min = 0, max = 3)) #: indicator for year of birth (0 - born in 1908-1920, 1 - born in 1921-1930, 2 - born in  1931-1940, 3 - born in 1941 - 1960)
IMD_Quintile<-round(runif(n, min = 1, max = 5)) #: deprivation index (1 (least deprived), 2, 3, 4, 5 (most deprived))
BMI_category<-round(runif(n, min = 0, max = 1)) #: categorical variable with numeric value for Body Mass Index  categories (0 - Healthy weight (BMI<25), 1 - Overweight (BMI>=25 and BMI<30) and Obese (BMI>=30))
antiplatelet_drugs<-round(runif(n, min = 0, max = 1)) #: indicator for antiplatelet drugs (0 - no antiplatelet drugs or 1 - no antiplatelet drugs)
COPD<-round(runif(n, min = 0, max = 1)) #:  indicator for chronic obstructive pulmonary disease (0 - absense of chronic obstructive pulmonary disease or 1 - presence of chronic obstructive pulmonary disease)
heart_failure<-round(runif(n, min = 0, max = 1)) #:  indicator for heart failure (0 - absence of heart failure or 1 - presence of heart failure)
myocardial_infarction<-round(runif(n, min = 0, max = 1)) #:	indicator for myocardial infarction (0 - absense of myocardial infarction or 1 - presence of myocardial infarction)
PVD_PAD<-round(runif(n, min = 0, max = 1)) #: indicator for peripheral vascular disease (0 - absence of peripheral vascular disease or 1 - presence of peripheral vascular disease )
SMOKING<-round(runif(n, min = 0, max = 2)) #: indicator of smoking status (0 non-smoker, 1 - current smoker, 2 - former smoker)
anticoagulant_agents<-round(runif(n, min = 0, max = 1)) #: indicator for anticoagulant agents (0 - no  anticoagulant agents, 1 - yes anticoagulant agents )
Diabetes_factor<-round(runif(n, min = 0, max = 2)) #: is a categorical variable with numeric value for diabetes status (0 - absence of diabetes or 1 - presence of diabetes and treated, 2 - absence of diabetes and untreated)
sex<-round(runif(n, min = 0, max = 1)) #: indicator for gender (0 - Female , 1 - Male)
groupscases<-round(runif(n, min = 0, max = 1)) #: indicator of cases control group (0 â€“ group without ischemic stroke  or 1 â€“ group with ischemic stroke)
antihypertensive_agents<-round(runif(n, min = 0, max = 1))#: indicator for antihypertensive agents (0 - no  antihypertensive agents, 1 - yes antihypertensive agents)
data<-data.frame(birth_cohort,IMD_Quintile,BMI_category, antiplatelet_drugs, COPD, heart_failure, myocardial_infarction,PVD_PAD,SMOKING,anticoagulant_agents,Diabetes_factor,sex,groupscases,antihypertensive_agents)
indexes_of_variables <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
age_of_diagnosis=65
time_past_from_diagnosis=1
mylongevity_double_cox_method3(data, indexes_of_variables, age_of_diagnosis, time_past_from_diagnosis)
}
\keyword{life_expectancy}
