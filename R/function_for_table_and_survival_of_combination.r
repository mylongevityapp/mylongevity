#' function_for_table_of_combination
#'
#' This function produces table with different combinations of scenarious and life expectancies
#' @param age age
#' @param a - intercept of Gompertz baseline hazards
#' @param b - slope of Gompertz baseline hazards
#' @param vector_of_coefficients - vector of coefficients from cox-model
#' @param LogHazardRations - log hazard rations from cox-model
#' @param data_for_weights - weights, i.e proportions for each generated combination
#' @param working_directory - the working directory for any data that is produced from the functions to be saved in a folder
#' @keywords life_expectancy
#' @export
#' @examples
#' @return data frame with life expectancies for given data frame of clients
#' function_for_table_of_combination()

function_for_table_of_combination<-
function(age,a,b,vector_of_coefficients,LogHazardRations,data_for_weights,working_directory){
    ##creating a temporary folder to store all temporary files
    temporary_folder <- paste("temp",sep = "")
	temporary_files_directory <- paste(working_directory,temporary_folder,sep="/")
	dir.create(temporary_files_directory)
	##removing digits from string verctor of coefficients
	vector_of_coefficients_with_no_integers <- gsub('[[:digit:]]+', '', vector_of_coefficients)
	#extracting unique variables from a model and number of total variables
	unique_coefficients <- unique(vector_of_coefficients_with_no_integers)
	#creating a table for unique categories
	table_of_unique_coefficients <- NULL
	#now we can go through these unique categories and idenity how many factors each variable has
	for(coefficient in 1:length(unique_coefficients)){
		current_coefficient <- unique_coefficients[coefficient]
		current_variable <- vector_of_coefficients[stringr::str_detect(vector_of_coefficients, current_coefficient)]
		matches <- regmatches(current_variable, gregexpr("[[:digit:]]+", current_variable))
		categories <- as.numeric(unlist(matches))
		baseline_category_index <- (min(categories)-1)
		max_category_index <- max(categories)
		temporary_table_for_coefficient <- data.frame(unique_coefficients[coefficient],(length(current_variable)),baseline_category_index,max_category_index)
		names(temporary_table_for_coefficient) <- c("variable","count","baseline_category_index","max_category_index")
		table_of_unique_coefficients <- rbind(table_of_unique_coefficients,temporary_table_for_coefficient)
	}

	##table_of_unique_coefficients shows the baseline category and the number of categories
	temp_variable=paste("",sep = "")
	all_start_for_loops<-paste("",sep = "")
	all_end_for_loops<-paste("",sep = "")
	table_for_all_possible_combination_of_variables=data.frame("column")
	for(current_coefficient in 1:nrow(table_of_unique_coefficients)){
		current_variable<-as.character(table_of_unique_coefficients$variable[current_coefficient])
		baseline_category_index<-table_of_unique_coefficients$baseline_category_index[current_coefficient]
		max_category_index<-table_of_unique_coefficients$max_category_index[current_coefficient]
		start_for_loop_bracket<-paste("for(",current_variable," in",baseline_category_index,":",max_category_index,"){")
		cat("\n")
		end_for_loop_bracket<-paste("}")
		all_start_for_loops<-paste(all_start_for_loops,start_for_loop_bracket)
		all_end_for_loops<-paste(all_end_for_loops,end_for_loop_bracket)
		temp_data_frame<-data.frame(0)
		names(temp_data_frame)<-as.character(current_variable)
		table_for_all_possible_combination_of_variables<-cbind(table_for_all_possible_combination_of_variables,temp_data_frame)
	}

    ##creating a table for all possible combinations of variables in a model
	table_for_all_possible_combination_of_variables<-table_for_all_possible_combination_of_variables[,-1]

	string_to_create_a_data_frame=""
	for(i in 1:nrow(table_of_unique_coefficients)){
		if(i<nrow(table_of_unique_coefficients)){
			mydata_i <- paste(names(table_for_all_possible_combination_of_variables)[i],",")
		} else if(i==nrow(table_of_unique_coefficients)){
			mydata_i <- paste(names(table_for_all_possible_combination_of_variables)[i])
		}
		string_to_create_a_data_frame <- paste(string_to_create_a_data_frame,mydata_i)
	}
	string_to_create_a_data_frame <- paste("my_table=data.frame(",string_to_create_a_data_frame,")",sep="")
   ######################################################################################################



	combination_weights=""
	creation_of_subdata="subdata=data_for_weights["
	for(i in 1:(length(unique_coefficients)-1)){
		creation_of_sub_variables=paste("data_for_weights$",unique_coefficients[i],"==",unique_coefficients[i],"&",sep="")
		combination_weights<-paste(combination_weights,creation_of_sub_variables)
	}

	sub_data_selection_by_last_variable=paste("data_for_weights$",unique_coefficients[length(unique_coefficients)],"==",unique_coefficients[length(unique_coefficients)],",]",sep="")

	final_sub_weight<-paste(creation_of_subdata,combination_weights,sub_data_selection_by_last_variable,sep="")
	code_line_for_sub_weight<-paste("my_table$proportion<-nrow(subdata)/nrow(data_for_weights)")
	#c(as.character(table_of_unique_coefficients$variable))
	#writeLines(all_start_for_loops,fileConn0)
	#writeLines(all_end_for_loops,fileConn1)
	fileConn2<-file(paste(temporary_files_directory,"/main3.txt",sep = ""))### creating a text file
	writeLines(c(paste("function_for_table<-function(data_for_weights){"),
	paste("combination=NULL"),
	all_start_for_loops,final_sub_weight,
	string_to_create_a_data_frame,
	code_line_for_sub_weight,
	paste("combination=rbind(combination,my_table)"),
	all_end_for_loops,paste("return(combination)}")),fileConn2)

	source(paste(temporary_files_directory,"/main3.txt",sep=""))
	###This is the table for all possible combination of factors that we have
	table_of_combinations<-function_for_table(data_for_weights=data_for_weights)


	for(i in 1:length(unique_coefficients)){
		temporary=unique_coefficients[i]
		current_table_of_unique_coefficients<-table_of_unique_coefficients[stringr::str_detect(table_of_unique_coefficients$variable, temporary),]
		current_dimention<-dim(table_of_combinations)[2]
		baseline_category_index<-current_table_of_unique_coefficients$baseline_category_index
		max_category_index<-current_table_of_unique_coefficients$max_category_index
		m=1
		for(j in baseline_category_index:max_category_index){
		temp_variable<-paste(current_table_of_unique_coefficients$variable,j,sep = "")
		table_of_combinations$temp_variable<-0
		table_of_combinations$temp_variable[table_of_combinations[,i]==j]<-1
		names(table_of_combinations)[current_dimention+m]<-temp_variable
		m=m+1
		}
	}
	##########################################
	current_dimension<-dim(table_of_combinations)[2]
	all_coefficient<-rep(0,times=dim(table_of_combinations)[1])
	for(i in 1:length(unique_coefficients)){
		temporary=unique_coefficients[i]
		current_LogHazardRatio<-c(0,LogHazardRations[stringr::str_detect(vector_of_coefficients, temporary)])
		current_table_of_unique_coefficients<-table_of_unique_coefficients[stringr::str_detect(table_of_unique_coefficients$variable, temporary),]
		baseline_category_index<-current_table_of_unique_coefficients$baseline_category_index
		max_category_index<-current_table_of_unique_coefficients$max_category_index
		current_variables<-NULL
			for(j in baseline_category_index:max_category_index){
				temp_variable<-paste(current_table_of_unique_coefficients$variable,j,sep = "")
				current_variables<-c(current_variables,temp_variable)
			}
		mat_i=table_of_combinations[,current_variables]
		temp_variable<-paste(temporary,"_estimates",sep = "")
		all_coefficient<-all_coefficient+t(t(mat_i))%*%t(t(current_LogHazardRatio))
		}
		##############################################################################################################
		table_of_combinations$Betas <- all_coefficient
		table_of_combinations$age <- age
		table_of_combinations$a <- a
		table_of_combinations$b <- b

    no_numbers=gsub('[[:digit:]]+', '', vector_of_coefficients)

	unique_coefficients=unique(no_numbers)

	##table_of_unique_coefficients shows the baseline category and the number of categories
	table_of_unique_coefficients=NULL
	for(coefficient in 1:length(unique_coefficients)){
		current_coefficient=unique_coefficients[coefficient]
		current_variable=vector_of_coefficients[str_detect(vector_of_coefficients, current_coefficient)]
		matches <- regmatches(current_variable, gregexpr("[[:digit:]]+", current_variable))
		categories<-as.numeric(unlist(matches))
		baseline_category_index<-min(categories)-1
		max_category_index<-max(categories)
		temporary_table<-data.frame(unique_coefficients[coefficient],(length(current_variable)),baseline_category_index,max_category_index)
		names(temporary_table)<-c("variable","count","baseline_category_index","max_category_index")
		table_of_unique_coefficients=rbind(table_of_unique_coefficients,temporary_table)
	}


	#fileConn0<-file(paste(working_directory,"/main00.txt",sep = ""))### creating a temporary text file
	#fileConn1<-file(paste(working_directory,"/main10.txt",sep = ""))### creating a temporary text file
	all_starts_for_loops0<-paste("",sep = "")
	all_ends_for_loops0<-paste("",sep = "")

	for(i in 1:nrow(table_of_unique_coefficients)){
		current_variable<-as.character(table_of_unique_coefficients$variable[i])
		baseline_category_index<-table_of_unique_coefficients$baseline_category_index[i]
		max_category_index<-table_of_unique_coefficients$max_category_index[i]
		start_for_loop<-paste("for(",current_variable," in",baseline_category_index,":",max_category_index,"){")
		cat("\n")
		end_for_loop<-paste("}")
		all_starts_for_loops0<-paste(all_starts_for_loops0,start_for_loop)
		all_ends_for_loops0<-paste(all_ends_for_loops0,end_for_loop)
	}

	combination_weights=""
	creation_of_subdata="mydata=data_for_weights["
	for(i in 1:(length(unique_coefficients)-1)){
		temportary0=paste("data_for_weights$",unique_coefficients[i],"==",unique_coefficients[i],"&",sep="")
		combination_weights<-paste(combination_weights,temportary0)
	}

	sub_data_selection_by_last_variable=paste("data_for_weights$",unique_coefficients[length(unique_coefficients)],"==",unique_coefficients[length(unique_coefficients)],",]",sep="")

	final_sub_weight<-paste(creation_of_subdata,combination_weights,sub_data_selection_by_last_variable,sep="")
	code_line_for_age<-"age<-mydata$age"
	code_line_for_b<-"b<-mydata$b"
	code_line_for_weight<-"weight<-mydata$proportion"
	code_line_for_Betas<-"all_covariates<-mydata$Betas"


	path_to_file_for_all_combinations<-file(paste(temporary_files_directory,"/file_for_all_combinations_of_factors.txt",sep = ""))### creating a text file
	writeLines(c(
		paste("function_for_mu<-function(data_for_weights){"),
		paste("combination0=paste(\"0\",sep = \"\")"),
		all_starts_for_loops0,
		final_sub_weight,
		code_line_for_age,
		code_line_for_b,
		code_line_for_weight,
		code_line_for_Betas,
		paste("combination1 <- paste(weight,\"*exp(-exp(mu+(\",all_covariates,\"))*\",b,\"^{-1}*(exp(\",b,\"*\",age,\")-1))\",sep = \"\")"),
		paste("combination0 <- paste(combination0,\"+\",combination1,sep = \"\")"),
		all_ends_for_loops0,
		"a <- unique(data_for_weights$a)",
		"b <- unique(data_for_weights$b)",
		"age_unique <- unique(data_for_weights$age)",
		"Sx <- paste(\"exp((-exp(\",a,\")/\",b,\")*(exp(\",b,\"*\",age_unique,\")-1))\",sep = \"\")",
		"whole_equation <- paste(Sx,\"-(\",combination0,\")\",sep = \"\")",
		"fileConn <- file(paste(\"E:/THIN data backup 10122019/development_of_R_package/equation.txt\",sep = \"\"))",
		"writeLines(c(",
		"\"survival_function_to_solve<-function(mu){\",whole_equation,\"}\"), fileConn)",
		"source(\"E:/THIN data backup 10122019/development_of_R_package/equation.txt\")",
		"ll<- (-1000)",
		"uu<- 0",
		"mu<- suppressWarnings(uniroot(survival_function_to_solve, c(ll, uu), tol = 0.001)[[1]])",
		"data_for_weights$mu <- mu",
		"data_for_weights$a0 <- data_for_weights$mu+data_for_weights$Betas",
		"b <- data_for_weights$b",
		"a <- data_for_weights$a0",
		"age <- data_for_weights$age",
		"z <- b^(-1)*exp(a+b*age)",
		"E1 <- (1/b)*exp(exp(a)/b)*expint::expint_E1(z)",
		"H <- (-exp(a)*(exp(b*age)-1)/b)",
		"S <- exp(H)",
		"LE <- E1/S",
		"data_for_weights$LE <- LE",
		"return(data_for_weights)}"
	),path_to_file_for_all_combinations)

	source(paste(temporary_files_directory,"/file_for_all_combinations_of_factors.txt",sep = ""))
	table_with_mu<-suppressWarnings(function_for_mu(data_for_weights=table_of_combinations))

	return(table_with_mu)
	}
