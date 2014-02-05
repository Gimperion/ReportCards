## Retiring Code

WriteComp <- function(.casdat_comp, year, level){
## Composition 
	.fay <- c("all")
	.lv <- level
	
	.ret <- c()
	.plevels <- c("Below Basic", "Basic", "Proficient", "Advanced")
	
	## d = each grade 
	.glevels <- sort(unique(.casdat_comp$tested_grade))
	
	for(g in 0:length(.glevels)){
		goutput <- ''
		.tmp <- .casdat_comp
		
		if(g == 0){
			goutput <- 'all'
		} else{
			goutput <- paste('grade', .glevels[g], sep=" ")
			.tmp <- subset(.tmp, tested_grade==.glevels[g])
		}
		
		if(nrow(.tmp)>=10){
			.add <- indent(.lv) %+% '{\n'
			
			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"key": {\n', sep="")
			up(.lv)
			
			.profs <- .tmp$comp_level
			
			.add <- .add %+% paste(indent(.lv), '"subject": "Composition",\n', sep="")			
			.add <- .add %+% paste(indent(.lv), '"grade": "',goutput,'", \n', sep="")
			.add <- .add %+% paste(indent(.lv), '"enrollment_status": "',.fay,'", \n', sep="")
			.add <- .add %+% paste(indent(.lv), '"subgroup": "All", \n', sep="")
			.add <- .add %+% paste(indent(.lv), '"year": "',year,'" \n', sep="")
			
			down(.lv)
			
			.add <- .add %+% paste(indent(.lv), '},\n', sep="")
				
			.add <- .add %+% paste(indent(.lv), '"val": {\n', sep="")
			up(.lv)
			
			.add <- .add %+% paste(indent(.lv), '"n_eligible":',length(.profs),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"n_test_takers":',length(.profs[.profs %in% .plevels]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"advanced_or_proficient":', length(.profs[.profs %in% c("Proficient", "Advanced")]),',\n', sep="")
			
			.add <- .add %+% paste(indent(.lv), '"advanced":',length(.profs[.profs %in% "Advanced"]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"proficient":',length(.profs[.profs %in% "Proficient"]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"basic":',length(.profs[.profs %in% "Basic"]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"below_basic":',length(.profs[.profs %in% "Below Basic"]),'\n', sep="")
			
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}\n', sep="")
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}', sep="")
			
			.ret[length(.ret)+1] <- .add
		}
	}
	return(paste(.ret, collapse=',\n'))
}


WriteScience <- function(.casdat_sci, year, level){
## Science
	.fay <- c("all")
	.lv <- level
	
	.ret <- c()
	.plevels <- c("Below Basic", "Basic", "Proficient", "Advanced")
	
	## d = each grade 
	.glevels <- sort(unique(.casdat_sci$tested_grade))
	
	for(g in 0:length(.glevels)){
		goutput <- ''
		.tmp <- .casdat_sci
		
		if(g == 0){
			goutput <- 'all'
		} else{
			goutput <- paste('grade', .glevels[g], sep=" ")
			.tmp <- subset(.tmp, tested_grade==.glevels[g])
		}
		
		
		if(nrow(.tmp)>=10){
			.add <- indent(.lv) %+% '{\n'
			
			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"key": {\n', sep="")
			up(.lv)
			
			.profs <- .tmp$science_level
			
			.add <- .add %+% paste(indent(.lv), '"subject": "Science",\n', sep="")					
			
			.add <- .add %+% paste(indent(.lv), '"grade": "',goutput,'", \n', sep="")
			.add <- .add %+% paste(indent(.lv), '"enrollment_status": "',.fay,'", \n', sep="")
			.add <- .add %+% paste(indent(.lv), '"subgroup": "All", \n', sep="")
			.add <- .add %+% paste(indent(.lv), '"year": "',year,'" \n', sep="")
			
			down(.lv)
			
			.add <- .add %+% paste(indent(.lv), '},\n', sep="")
				
			.add <- .add %+% paste(indent(.lv), '"val": {\n', sep="")
			up(.lv)
			
			.add <- .add %+% paste(indent(.lv), '"n_eligible":',length(.profs),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"n_test_takers":',length(.profs[.profs %in% .plevels]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"advanced_or_proficient":', length(.profs[.profs %in% c("Proficient", "Advanced")]),',\n', sep="")
			
			.add <- .add %+% paste(indent(.lv), '"advanced":',length(.profs[.profs %in% "Advanced"]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"proficient":',length(.profs[.profs %in% "Proficient"]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"basic":',length(.profs[.profs %in% "Basic"]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"below_basic":',length(.profs[.profs %in% "Below Basic"]),'\n', sep="")
			
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}\n', sep="")
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}', sep="")
			
			.ret[length(.ret)+1] <- .add
		}
	}
	return(paste(.ret, collapse=',\n'))
}


WriteCAS <- function(.casdat_mr, year, level){

	.casdat_mr$math_level[.casdat_mr$exclude=='I'] <- NA
	.casdat_mr$read_level[.casdat_mr$exclude=='I'] <- NA

	.subjects <- c("Math", "Reading")
	.fay <- c("all", "full_year")
	
	.lv <- level
	
	.ret <- c()
	.plevels <- c("Below Basic", "Basic", "Proficient", "Advanced")
	
	## A = Subject, 1 for Math, 2 for Reading
	for(a in 1:2){
		## b = full year or not
		for(b in 1:2){
			## d = each grade 
			.glevels <- sort(unique(.casdat_mr$tested_grade))
			for(g in 0:length(.glevels)){
				goutput <- ''
				.tmp <- .casdat_mr
				
				if(g == 0){
					goutput <- 'all'
				} else{
					goutput <- paste('grade', .glevels[g], sep=" ")
					.tmp <- subset(.tmp, tested_grade==.glevels[g])
				}
				.flevels <- c("N", "S", "D", "C")
				
				if(b==2){
					.flevels <- c("S")
					.tmp <- subset(.tmp, new_to_us =='NO')
					.tmp <- subset(.tmp, school_grade==tested_grade | alt_tested=="YES")
				}
				
				.subgroups <- c("African American","White","Hispanic","Asian", "Pacific Islander", "Multiracial","Special Education","English Learner","Economically Disadvantaged","Male", "Female")
				
				for(h in 0:9){
					.tmps <- SubProc(.tmp, h, b)
					
					if(h == 0){
						soutput <- 'All'
					} else{
						soutput <- .subgroups[h]
					}
					
					if((nrow(.tmps)>=10 & b==1) | (nrow(.tmps)>=25 & b==2)){
						.add <- indent(.lv) %+% '{\n'
						
						up(.lv)
						.add <- .add %+% paste(indent(.lv), '"key": {\n', sep="")
						up(.lv)
						
						if(a ==1){
							.profs <- .tmps$math_level[.tmps$full_academic_year %in% .flevels]
						} else if(a == 2){
							.profs <- .tmps$read_level[.tmps$full_academic_year %in% .flevels]
						}
						
						.add <- .add %+% paste(indent(.lv), '"subject": "',.subjects[a],'",\n', sep="")					
						
						.add <- .add %+% paste(indent(.lv), '"grade": "',goutput,'", \n', sep="")
						.add <- .add %+% paste(indent(.lv), '"enrollment_status": "',.fay[b],'", \n', sep="")
						.add <- .add %+% paste(indent(.lv), '"subgroup": "',soutput,'", \n', sep="")
						.add <- .add %+% paste(indent(.lv), '"year": "',year,'" \n', sep="")
						
						down(.lv)
						
						.add <- .add %+% paste(indent(.lv), '},\n', sep="")
							
						.add <- .add %+% paste(indent(.lv), '"val": {\n', sep="")
						up(.lv)
						
						.add <- .add %+% paste(indent(.lv), '"n_eligible":',length(.profs),',\n', sep="")
						.add <- .add %+% paste(indent(.lv), '"n_test_takers":',length(.profs[.profs %in% .plevels]),',\n', sep="")
						.add <- .add %+% paste(indent(.lv), '"advanced_or_proficient":', length(.profs[.profs %in% c("Proficient", "Advanced")]),',\n', sep="")
						
						.add <- .add %+% paste(indent(.lv), '"advanced":',length(.profs[.profs %in% "Advanced"]),',\n', sep="")
						.add <- .add %+% paste(indent(.lv), '"proficient":',length(.profs[.profs %in% "Proficient"]),',\n', sep="")
						.add <- .add %+% paste(indent(.lv), '"basic":',length(.profs[.profs %in% "Basic"]),',\n', sep="")
						.add <- .add %+% paste(indent(.lv), '"below_basic":',length(.profs[.profs %in% "Below Basic"]),'\n', sep="")
						
						down(.lv)
						.add <- .add %+% paste(indent(.lv), '}\n', sep="")
						down(.lv)
						.add <- .add %+% paste(indent(.lv), '}', sep="")
						
						.ret[length(.ret)+1] <- .add
					}
				}
			}
		}
	}
	return(paste(.ret, collapse=',\n'))
}

##cat(ExCasChunk('0210', 2))





ExCasChunk_retired <- function(scode, level){
	.lv <- level
	
	## MATH/READING
	.qry <- sprintf("SELECT A.* FROM [dbo].[assessment] A
		INNER JOIN (SELECT 
			[ea_year]
			,[school_code]
			,[grade]
		FROM [dbo].[fy14_mapping]
		WHERE [fy14_entity_code] = '%s') B
	ON A.[ea_year] = B.[ea_year] 
		AND A.[school_grade] = B.[grade] 
		AND A.[school_code] = B.[school_code]", scode)

	.dat_mr <- sqlQuery(dbrepcard, .qry13)

	.ret <- c()
	
	if(nrow(.dat13_mr)>=10 & !is.null(.dat13_mr)){
		.ret[length(.ret)+1] <- WriteCAS(.dat13_mr, 2013, .lv)
	}
	
	if(nrow(.dat12_mr)>=10 & !is.null(.dat12_mr)){
		.ret[length(.ret)+1] <- WriteCAS(.dat12_mr, 2012, .lv)
	}
	
	if(nrow(.dat11_mr)>=10 & !is.null(.dat11_mr)){
		.ret[length(.ret)+1] <- WriteCAS(.dat11_mr, 2011, .lv)
	}
	
	if(nrow(.dat10_mr)>=10 & !is.null(.dat10_mr)){
		.ret[length(.ret)+1] <- WriteCAS(.dat10_mr, 2010, .lv)
	}
	
	if(nrow(.dat09_mr)>=10 & !is.null(.dat09_mr)){
		.ret[length(.ret)+1] <- WriteCAS(.dat09_mr, 2009, .lv)
	}	
	
	## 
	.qry13c <- "SELECT * FROM [dbo].[assessment_sy1213_comp]
		WHERE [fy13_entity_code] = '" %+% scode %+% "';"
	.dat13_c <- sqlQuery(dbrepcard, .qry13c)
	if(nrow(.dat13_c)>=10){
		.ret[length(.ret)+1] <- WriteComp(.dat13_c, 2013, .lv)
	}
	
	.qry12c <- "SELECT * FROM [dbo].[assessment_sy1112_comp]
		WHERE [fy13_entity_code] = '" %+% scode %+% "';"
	.dat12_c <- sqlQuery(dbrepcard, .qry12c)
	if(nrow(.dat12_c)>=10){
		.ret[length(.ret)+1] <- WriteComp(.dat12_c, 2012, .lv)
	}
	
	.qry11c <- "SELECT * FROM [dbo].[assessment_sy1011_comp]
		WHERE [fy13_entity_code] = '" %+% scode %+% "';"
	.dat11_c <- sqlQuery(dbrepcard, .qry11c)
	if(nrow(.dat11_c)>=10){
		.ret[length(.ret)+1] <- WriteComp(.dat11_c, 2011, .lv)
	}
	
	## 
	.qry13s <- "SELECT * FROM [dbo].[assessment_sy1213_science]
		WHERE [fy13_entity_code] = '" %+% scode %+% "'
		AND science_empty = 0;"
	.dat13_s <- sqlQuery(dbrepcard, .qry13s)
	if(nrow(.dat13_s)>=10){
		.ret[length(.ret)+1] <- WriteScience(.dat13_s, 2013, .lv)
	}
	
	.qry12s <- "SELECT * FROM [dbo].[assessment_sy1112_science]
		WHERE [fy13_entity_code] = '" %+% scode %+% "'
		AND science_empty = 0;"
		
	.dat12_s <- sqlQuery(dbrepcard, .qry12s)
	if(nrow(.dat12_s)>=10){
		.ret[length(.ret)+1] <- WriteScience(.dat12_s, 2012, .lv)
	}
	
	.qry11s <- "SELECT * FROM [dbo].[assessment_sy1011_science]
	WHERE [fy13_entity_code] = '" %+% scode %+% "'
	AND science_empty = 0;"
	
	.dat11_s <- sqlQuery(dbrepcard, .qry11s)
	if(nrow(.dat11_s)>=10){
		.ret[length(.ret)+1] <- WriteScience(.dat11_s, 2011, .lv)
	}	
	return(paste(.ret, collapse=',\n'))
}



## Not Used
TestScience <- function(scode, level){
	.lv <- level
	.ret <- c()
	## 
	.qry13s <- "SELECT * FROM [dbo].[assessment_sy1213_science]
		WHERE [fy13_entity_code] = '" %+% scode %+% "'
		AND science_empty = 0;"
	.dat13_s <- sqlQuery(dbrepcard, .qry13s)
	if(nrow(.dat13_s)>=10){
		.ret[length(.ret)+1] <- WriteScience(.dat13_s, 2013, .lv)
	}
	
	.qry12s <- "SELECT * FROM [dbo].[assessment_sy1112_science]
		WHERE [fy13_entity_code] = '" %+% scode %+% "'
		AND science_empty = 0;"
		
	.dat12_s <- sqlQuery(dbrepcard, .qry12s)
	if(nrow(.dat12_s)>=10){
		.ret[length(.ret)+1] <- WriteScience(.dat12_s, 2012, .lv)
	}	
	
	return(paste(.ret, collapse=',\n'))
}

