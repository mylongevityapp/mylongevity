#' mylongevity_method2
#'
#' This function loads a file as a data frame of clients.
#' Using the argument 'indexes_of_variables', this function selects the columns of factors
#' Then using the argument 'list_of_variables' this functions selects the columns of interest to be matched
#' with coefficients (i.e log-hazard ratios) estimated in analysis
#' @param data given data frame of clients,
#' @param indexes_of_variables indices for columns of interest
#' @param list_of_variables columns of interest to be matched to coefficients (i.e log-hazard ratios)
#' @param age age of clients
#' @param a intercept from Gompertz baseline hazards
#' @param b slope from Gompertz baseline hazards
#' @param working_directory the working directory for any data that is produced from the functions to be saved in a folder
#' @details This method calculates life expectancies using the log-hazard ratio estimates from the landmark analysis Kulinskaya et al. (2020c) and the weights of the risk groups estimated from the given dataset. These weights correspond to frequencies of risk profiles in the dataset.
#'
#' mylongevity_method2 uses log-hazard ratio coefficients from website mylongevity.org with inputted data. mylongevity_method2 takes the log-hazard ratio coefficients and calculates the weights from data.
#'
#' For mylongevity_method2 user should specify the directory where all the temporary files will be stored. The directory should be specified as working_directory=' E:/myproject'. A folder called 'temp' will be created in the specified directory. This is the folder where all necessary/temporary files will be stored. Also for method 2 and method 3 we need a function - function_for_table_of_combination() and a function calculate_life_expectancy() which are separate functions within this R package.
#' For mylongevity_method2, user has to input the data with columns of interest using the argument 'indexes_of_variables'. Also, user has to specify the particular age of interest for a given data to select the corresponding log-hazard ratio coefficients from landmark model for a particular age.  User also has to specify values for a and b for Gompertz distribution. User has to specify the names of variables which will be used in calculation with the same names as in inputted data using the variable list_of_variables
#' For mylongevity_method2, user can only input columns for variables
#' ('statins','cvd_risk','diabetes','HTN_diag_treat','hypercholesterolaemia','BMI','Smoking')
#' User has to specify the variables of interest using the argument 'list_of_variables'.
#' The order of specified columns using the argument 'indexes_of_variables' should match the variables of interest 'list_of_variables' by names.
#' @keywords life_expectancy
#' @export
#' @return data frame with life expectancies for given data frame of clients
#' @examples
#' set.seed(1234)
#' n<-1000
#' gender <- c(rep('M',times=n/2),rep('F',times=n/2))
#' townsend <- round(runif(n, min = 1, max = 5))
#' smokerCategory <- round(runif(n, min = 1, max = 3))
#' HTN_diag_treat  <- round(runif(n, min = 1, max = 3))
#' diabetes  <- round(runif(n, min = 0, max = 1))
#' hypercholesterolaemia <- round(runif(n, min = 0, max = 1))
#' bmiCategory <- round(runif(n, min = 1, max = 3))
#' cvd_risk <- round(runif(n, min = 0, max = 2))
#' statins <- round(runif(n, min = 0, max = 1))
#' age <-61
#' a=-12.459132
#' b=0.11764571
#' list_of_variables<-c("statins","cvd_risk","diabetes","HTN_diag_treat","hypercholesterolaemia")
#' data<-data.frame(statins,cvd_risk,diabetes,HTN_diag_treat,hypercholesterolaemia)
#' indexes_of_variables <- c(1,2,3,4,5)
#' working_directory<-"E:/Documentation for R package/" #please edit this to your working directory
#' mylongevity_method2(data=data,indexes_of_variables=indexes_of_variables,list_of_variables,age,a,b,working_directory)
mylongevity_method2<-function(data,indexes_of_variables,list_of_variables,age,a,b,working_directory){
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
	landmark_age<-seq(from=60,to=85,by=0.5)
	###There are the log-hazard ratios from landmark analysis
	statins0<-rep(0,times=length(landmark_age))
	statins1<-c(-0.090530031,-0.109226333,-0.126920542,-0.143650949,-0.159455846,-0.174373523,-0.188442272,-0.201700383,-0.214186148,-0.225937858,-0.236993803,-0.247392276,-0.257171566,-0.266369966,-0.275025766,-0.283177257,-0.290862731,-0.298120477,-0.304988789,-0.311505956,-0.31771027,-0.323640022,-0.329333502,-0.334829003,-0.340164814,-0.345379228,-0.350510535,-0.355597026,-0.360676992,-0.365788726,-0.370970516,-0.376260656,-0.381697435,-0.387319145,-0.393164077,-0.399270521,-0.40567677,-0.412421114,-0.419541845,-0.427077253,-0.435065629,-0.443545265,-0.452554451,-0.462131479,-0.47231464,-0.483142224,-0.494652524,-0.50688383,-0.519874432,-0.533662623,-0.548286694)
	cvd_risk0<-rep(0,times=length(landmark_age))
	cvd_risk1<-c(0.094661163,0.113355551,0.131498147,0.149075091,0.166072523,0.182476584,0.198273414,0.213449154,0.227989943,0.241881921,0.25511123,0.267664009,0.279526399,0.290684539,0.301124571,0.310832634,0.319794869,0.327997415,0.335426414,0.342068005,0.347908329,0.352933526,0.357129736,0.3604831,0.362979757,0.364605849,0.365347515,0.365190896,0.364122131,0.362127362,0.359192728,0.355304369,0.350448427,0.344611041,0.337778351,0.329936499,0.321071623,0.311169864,0.300217363,0.28820026,0.275104695,0.260916809,0.245622741,0.229208632,0.211660622,0.192964851,0.173107461,0.15207459,0.129852379,0.106426969,0.0817845)
	cvd_risk2<-c(-0.061875507,-0.06692498,-0.071837196,-0.076576837,-0.081108584,-0.085397118,-0.089407119,-0.09310327,-0.096450251,-0.099412744,-0.101955429,-0.104042988,-0.105640101,-0.106711451,-0.107221717,-0.107135582,-0.106417727,-0.105032832,-0.102945578,-0.100120648,-0.096522721,-0.092116479,-0.086866604,-0.080737776,-0.073694677,-0.065701987,-0.056724388,-0.04672656,-0.035673186,-0.023528946,-0.010258522,0.004173406,0.019802157,0.036663049,0.054791401,0.074222531,0.09499176,0.117134406,0.140685787,0.165681222,0.192156031,0.220145532,0.249685043,0.280809885,0.313555376,0.347956834,0.384049578,0.421868928,0.461450202,0.502828719,0.546039798)
	diabetes0<-rep(0,times=length(landmark_age))
	diabetes1<-c(0.604470848,0.587527656,0.57109435,0.55516059,0.539716036,0.524750348,0.510253184,0.496214205,0.48262307,0.46946944,0.456742973,0.444433329,0.432530168,0.421023149,0.409901933,0.399156178,0.388775545,0.378749693,0.369068282,0.359720971,0.35069742,0.341987288,0.333580236,0.325465923,0.317634009,0.310074152,0.302776014,0.295729253,0.288923529,0.282348502,0.275993832,0.269849177,0.263904198,0.258148555,0.252571906,0.247163912,0.241914233,0.236812527,0.231848455,0.227011676,0.22229185,0.217678636,0.213161695,0.208730685,0.204375267,0.2000851,0.195849843,0.191659157,0.187502701,0.183370134,0.179251117)
	BMI_cat1<-rep(0,times=length(landmark_age))
	BMI_cat2<-c(-0.044813323,-0.044167334,-0.044108101,-0.044602399,-0.045617,-0.047118677,-0.049074204,-0.051450353,-0.054213897,-0.05733161,-0.060770265,-0.064496634,-0.06847749,-0.072679608,-0.077069759,-0.081614717,-0.086281255,-0.091036146,-0.095846163,-0.100678079,-0.105498666,-0.1102747,-0.114972951,-0.119560193,-0.1240032,-0.128268744,-0.132323599,-0.136134537,-0.139668332,-0.142891756,-0.145771583,-0.148274585,-0.150367536,-0.152017209,-0.153190377,-0.153853813,-0.153974289,-0.15351858,-0.152453458,-0.150745696,-0.148362067,-0.145269344,-0.1414343,-0.136823709,-0.131404343,-0.125142976,-0.11800638,-0.109961328,-0.100974595,-0.091012952,-0.080043172)
	BMI_cat3<-c(0.098281511,0.096949605,0.095271882,0.093260399,0.090927212,0.08828438,0.085343959,0.082118007,0.078618582,0.07485774,0.070847539,0.066600036,0.062127289,0.057441355,0.052554291,0.047478155,0.042225004,0.036806896,0.031235887,0.025524035,0.019683397,0.013726032,0.007663995,0.001509345,-0.004725862,-0.011029567,-0.017389714,-0.023794245,-0.030231103,-0.03668823,-0.043153569,-0.049615064,-0.056060655,-0.062478287,-0.068855902,-0.075181442,-0.08144285,-0.087628069,-0.093725041,-0.09972171,-0.105606017,-0.111365905,-0.116989318,-0.122464197,-0.127778485,-0.132920126,-0.137877061,-0.142637233,-0.147188585,-0.15151906,-0.1556166)
	hypercholesterolaemia0<-rep(0,times=length(landmark_age))
	hypercholesterolaemia1<-c(-0.226267131,-0.22086793,-0.215542462,-0.210288728,-0.205104732,-0.199988475,-0.194937962,-0.189951194,-0.185026173,-0.180160904,-0.175353388,-0.170601627,-0.165903625,-0.161257385,-0.156660908,-0.152112197,-0.147609256,-0.143150086,-0.13873269,-0.134355072,-0.130015233,-0.125711176,-0.121440904,-0.11720242,-0.112993726,-0.108812824,-0.104657718,-0.10052641,-0.096416903,-0.092327198,-0.0882553,-0.084199211,-0.080156932,-0.076126467,-0.072105819,-0.06809299,-0.064085983,-0.0600828,-0.056081444,-0.052079918,-0.048076223,-0.044068364,-0.040054343,-0.036032161,-0.031999822,-0.027955329,-0.023896684,-0.019821889,-0.015728948,-0.011615863,-0.007480636)
	HTN_diag_treat1<-rep(0,times=length(landmark_age))
	HTN_diag_treat2<-c(0.223737253,0.213235224,0.202989487,0.192988095,0.183219104,0.173670569,0.164330543,0.155187082,0.14622824,0.137442071,0.12881663,0.120339971,0.11200015,0.10378522,0.095683237,0.087682254,0.079770327,0.07193551,0.064165857,0.056449423,0.048774262,0.04112843,0.03349998,0.025876968,0.018247447,0.010599472,0.002921099,-0.004799619,-0.012574628,-0.020415872,-0.028335296,-0.036344847,-0.04445647,-0.052682111,-0.061033714,-0.069523225,-0.07816259,-0.086963754,-0.095938662,-0.105099261,-0.114457494,-0.124025309,-0.13381465,-0.143837463,-0.154105693,-0.164631286,-0.175426187,-0.186502341,-0.197871694,-0.209546192,-0.22153778)
	HTN_diag_treat3<-c(0.056731913,0.057978892,0.058849687,0.059384395,0.059623113,0.059605941,0.059372976,0.058964317,0.058420061,0.057780307,0.057085152,0.056374696,0.055689035,0.055068269,0.054552496,0.054181812,0.053996318,0.05403611,0.054341287,0.054951948,0.055908189,0.05725011,0.059017808,0.061251381,0.063990929,0.067276548,0.071148338,0.075646395,0.080810819,0.086681707,0.093299158,0.100703269,0.108934139,0.118031866,0.128036548,0.138988284,0.15092717,0.163893306,0.17792679,0.19306772,0.209356193,0.226832309,0.245536164,0.265507858,0.286787489,0.309415153,0.333430951,0.358874979,0.385787337,0.414208121,0.444177431)
	smoking1<-rep(0,times=length(landmark_age))
	smoking2<-c(0.387438422,0.385475858,0.38376824,0.382295309,0.381036806,0.37997247,0.379082043,0.378345263,0.377741873,0.377251612,0.376854221,0.37652944,0.37625701,0.376016671,0.375788163,0.375551227,0.375285603,0.374971032,0.374587254,0.374114009,0.373531039,0.372818083,0.371954881,0.370921175,0.369696705,0.36826121,0.366594432,0.364676111,0.362485987,0.360003801,0.357209293,0.354082204,0.350602273,0.346749242,0.342502851,0.33784284,0.332748949,0.32720092,0.321178492,0.314661406,0.307629402,0.300062221,0.291939603,0.283241289,0.273947018,0.264036532,0.253489571,0.242285876,0.230405185,0.217827241,0.204531784)
	smoking3<-c(0.891144277,0.885326946,0.88000766,0.875140831,0.870680868,0.866582182,0.862799184,0.859286283,0.855997891,0.852888418,0.849912274,0.847023869,0.844177615,0.841327922,0.8384292,0.835435859,0.83230231,0.828982964,0.82543223,0.821604521,0.817454245,0.812935813,0.808003636,0.802612124,0.796715688,0.790268738,0.783225684,0.775540938,0.767168909,0.758064008,0.748180646,0.737473232,0.725896178,0.713403893,0.699950789,0.685491275,0.669979763,0.653370662,0.635618383,0.616677337,0.596501934,0.575046584,0.552265698,0.528113686,0.502544959,0.475513928,0.446975002,0.416882592,0.385191109,0.351854963,0.316828564)
	##############################################################################################
	table_of_coefficients<-rbind(statins1,cvd_risk1,cvd_risk2,diabetes1,HTN_diag_treat2,HTN_diag_treat3,hypercholesterolaemia1,BMI_cat2,BMI_cat3,smoking2,smoking3)
	table_of_coefficients<-data.frame(table_of_coefficients)
	colnames(table_of_coefficients)<-landmark_age
	all_variables<-c("statins","cvd_risk","diabetes","HTN_diag_treat","hypercholesterolaemia","BMI","Smoking")
	category_count<-c(2,2,1,2,1,2,2)
	baseline_category_of_variables<-c(0,0,0,1,0,1,1)
	max_category_of_variables<-c(2,2,1,3,1,3,3)
	full_table<-data.frame(all_variables,category_count,baseline_category_of_variables,max_category_of_variables)
	###selecting only variables that we want
	mytable=NULL
	for(i in 1:length(list_of_variables)){
		sub_table<-full_table[as.character(full_table$all_variables)==as.character(list_of_variables[i]),]
		mytable<-rbind(mytable,sub_table)
	}
	vector_of_coefficients<-rownames(table_of_coefficients)
	coefficients_for_selected_age<-table_of_coefficients[colnames(table_of_coefficients)==as.character(age)]
	coefficients_for_selected_age<-data.frame(coefficients_for_selected_age)
	colnames(coefficients_for_selected_age)<-"coefficient"
	LogHazardRatios<-coefficients_for_selected_age$coefficient
	###calculate weights
	sub_data<-data[,indexes_of_variables]
	#names(sub_data)[2:dim(sub_data)[2]]<-list_of_variables
	updated_vector_of_coefficients<-NULL
	updated_LogHazardRatios<-NULL
	for(i in 1:length(list_of_variables)){
		current_variable<-vector_of_coefficients[stringr::str_detect(vector_of_coefficients,list_of_variables[i])]
		current_log_hazard_ratio<-LogHazardRatios[stringr::str_detect(vector_of_coefficients,list_of_variables[i])]
		updated_vector_of_coefficients<-c(updated_vector_of_coefficients,current_variable)
		updated_LogHazardRatios<-c(updated_LogHazardRatios,current_log_hazard_ratio)
	}
    #this gives an output for different combinations of selected variables
	life_expectancy_table <- NULL
    life_expectancy_table <- suppressWarnings(calculate_life_expectancy(age=age,a=a,b=b,vector_of_coefficients=updated_vector_of_coefficients,log_hazard_ratio=updated_LogHazardRatios,data_for_weights=sub_data,working_directory=working_directory))
return(life_expectancy_table)
}
