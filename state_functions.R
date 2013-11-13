## 
ExStatePreKCAS <- function(level){
	.qry <- "SELECT A.*,
				CASE
					WHEN A.[usi] in (SELECT [usi] from [dbo].[historical_prek_static]) THEN 1
					ELSE 0
				END as [prek_participant]		
			FROM [dbo].[assessment_sy1213] A
			WHERE A.[tested_grade] = '3'"
	
	.prekcas13 <- sqlQuery(dbrepcard, .qry)
	.ret <- c()
	
	.ret <- c(.ret, WritePreKCAS(.prekcas13, level))
	
	.qry <- "SELECT A.*,
			CASE
				WHEN A.[usi] in (SELECT [usi] from [dbo].[historical_prek_static]) THEN 1
				ELSE 0
			END as [prek_participant]		
		FROM [dbo].[assessment_sy1112] A
		WHERE A.[tested_grade] = '3'"
	
	.prekcas12 <- sqlQuery(dbrepcard, .qry)
	
	.ret <- c(.ret, WritePreKCAS(.prekcas12, level))
	return(paste(.ret, collapse=',\n'))	
}

WritePreKCAS <- function(.prekcas, level){
	.lv <- level
	.ret <- c()
	
	.group <- c("PreK Participant", "Non-PreK Participant")
	.year <- .prekcas$year[1]
	.subject <- c("Math", "Reading")
	.plevels <- c("Below Basic", "Basic", "Proficient", "Advanced")
	
	for(i in 1:2){
		if(i == 1){
			.tmp <- subset(.prekcas, prek_participant==1)
		} else{
			.tmp <- subset(.prekcas, prek_participant!=1)
		}
		for(a in 1:2){
			if(a ==1){
				.profs <- .tmp$math_level
			} else if(a == 2){
				.profs <- .tmp$read_level
			}
		
			.add <- indent(.lv) %+% '{\n'
			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"key": {\n', sep="")
			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"year": "', .year, '",\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"grade": "grade 3",\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"subject": "',.subject[a], '",\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"subgroup": "', .group[i], '"\n', sep="")
			
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '},\n', sep="")
			down(.lv)
			
			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"val": {\n', sep="")

			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"test_takers": ', checkna(length(.profs[.profs %in% .plevels])), ',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"below_basic": ', checkna(length(.profs[.profs %in% c("Below Basic")])), ',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"basic": ', checkna(length(.profs[.profs %in% c("Basic")])), ',\n', sep="")			
			.add <- .add %+% paste(indent(.lv), '"proficient": ', checkna(length(.profs[.profs %in% c("Proficient")])), ',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"advanced": ', checkna(length(.profs[.profs %in% c("Advanced")])), '\n', sep="")
			
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}\n', sep="")
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}', sep="")
			
			.ret <- c(.ret, .add)
		}
	}
	##print(.naepdat)
	return(paste(.ret, collapse=',\n'))
}


ExStateCReady <- function(level){
	.lv <- level
	
	.qry <- "SELECT * FROM [dbo].[college_readiness]"	
		
	.cready <- sqlQuery(dbrepcard, .qry)
	##print(.cready)
	
	years <- unique(.cready$year)
	
	.ret <- c()

	for(i in years){
		.tmp <- subset(.cready, year == i)		
		.ret <- c(.ret, EncodeCReady(.tmp, level))
	}	
	
	return(paste(.ret, collapse=',\n'))
}

ExStateSPEDChunk <- function(level){
	.lv <- level
	
	## MATH/READING
	.qry13 <- "SELECT * FROM [dbo].[assessment_sy1213]
		WHERE [special_ed] = 'YES';"
	.dat13_mr <- sqlQuery(dbrepcard, .qry13)
	
	.qry12 <- "SELECT * FROM [dbo].[assessment_sy1112]
		WHERE [special_ed] = 'YES';"
	.dat12_mr <- sqlQuery(dbrepcard, .qry12)
	
	.qry11 <- "SELECT * FROM [dbo].[assessment_sy1011]
		WHERE [special_ed] = 'YES';"
	.dat11_mr <- sqlQuery(dbrepcard, .qry11)
	
	.qry10 <- "SELECT * FROM [dbo].[assessment_sy0910]
		WHERE [special_ed] = 'YES';"
	.dat10_mr <- sqlQuery(dbrepcard, .qry10)
	
	.qry09 <- "SELECT * FROM [dbo].[assessment_sy0809]
		WHERE [special_ed] = 'YES';"
	.dat09_mr <- sqlQuery(dbrepcard, .qry09)
	
	.ret <- c()
	
	if(nrow(.dat13_mr)>=10 & !is.null(.dat13_mr)){
		.ret <- c(.ret, WriteSPED(.dat13_mr, 2013, .lv))
	}
	
	if(nrow(.dat12_mr)>=10 & !is.null(.dat12_mr)){
		.ret <- c(.ret, WriteSPED(.dat12_mr, 2012, .lv))
	}
	
	if(nrow(.dat11_mr)>=10 & !is.null(.dat11_mr)){
		.ret <- c(.ret, WriteSPED(.dat11_mr, 2011, .lv))
	}
	
	if(nrow(.dat10_mr)>=10 & !is.null(.dat10_mr)){
		.ret <- c(.ret, WriteSPED(.dat10_mr, 2010, .lv))
	}
	
	if(nrow(.dat09_mr)>=10 & !is.null(.dat09_mr)){
		.ret <- c(.ret, WriteSPED(.dat09_mr, 2009, .lv))
	}
	
	.ret <- subset(.ret, .ret != '')
	return(paste(.ret, collapse=',\n'))
}

ExStateAcct <- function(level){
	## MATH/READING
	.lv <- level
	
	.ret <- c()

	.acct_st <- sqlFetch(dbrepcard, 'accountability_state')

	.ret <- '{\n'
	up(.lv)
	
	.ret <- .ret %+% paste(indent(.lv), '"year": "2013", \n', sep="")
	.ret <- .ret %+% paste(indent(.lv), '"score": null,\n', sep="")
	.ret <- .ret %+% paste(indent(.lv), '"classification": null,\n', sep="")
	.ret <- .ret %+% paste(indent(.lv), '"growth": null, \n', sep="")
	
	.ret <- .ret %+% paste(indent(.lv), '"subgroups": [\n', sep="")
	up(.lv)
	.sgstrings <- c()
	for(i in 1:nrow(.acct_st)){
		.add <- ''
		if(.acct_st$read_size[i] >= 25 | .acct_st$math_size[i] >= 25 | .acct_st$comp_size[i] >= 25){
			.add <- indent(.lv) %+% '{\n'
			up(.lv)
			
			.add <- .add %+% paste(indent(.lv), '"subgroup": ',checkna_str(.acct_st$subgroup[i]),', \n', sep="")
			
			.add <- .add %+% paste(indent(.lv), '"read_size": ',checkna(.acct_st$read_size[i]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"read_score": ',checkna(round(.acct_st$read_score[i],2)),',\n', sep="")
			
			.add <- .add %+% paste(indent(.lv), '"math_size": ',checkna(.acct_st$math_size[i]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"math_score": ',checkna(round(.acct_st$math_score[i],2)),',\n', sep="")
			
			.add <- .add %+% paste(indent(.lv), '"comp_size": ',checkna(.acct_st$comp_size[i]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"comp_score": ',checkna(round(.acct_st$comp_score[i],2)),'\n', sep="")
			
			down(.lv)		
			.add <- .add %+% paste(indent(.lv), '}', sep="")
			.sgstrings <- c(.sgstrings, .add)
		}
	}
	
	.ret <- .ret %+% paste(.sgstrings, collapse=',\n') %+% '\n'
	
	down(.lv)
	.ret <- .ret %+% paste(indent(.lv), ']\n', sep="")
	down(.lv)
	.ret <- .ret %+% paste(indent(.lv), '}', sep="")
	return(.ret)
}


SubNAEP <- function(subgroup){
	if(subgroup == 'American Indian/Alaska Native'){
		return('American Indian')
	} else if(subgroup=='Black'){
		return('African American')
	} else if(subgroup=='Hispanic / Latino'){
		return('Hispanic')
	} else if(subgroup %in% c('Eligible FRL', 'FRL Eligible')){
		return('Economically Disadvantaged')
	} else if(subgroup %in% c('FRL Not eligible', 'Not eligible FRL')){
		return('Not Economically Disadvantaged')
	} else if(subgroup=='ELL'){
		return('English Learner')
	} else if(subgroup=='Not ELL'){
		return('Not English Learner')
	} else if(subgroup=='SD'){
		return('Special Education')
	} else if(subgroup == 'Not SD'){
		return('Not Special Education')
	} else if(subgroup=='Asian/Pacific Islander'){
		return('Asian')
	}
	
	return(subgroup)
}

ExNaepResult <- function(level){
	.qry <- "SELECT * FROM [dbo].[naep_state_report]
		WHERE [subgroup] not in ('FRL Information not available',
			'Information not available (FRL)',
			'Parents  Education: Unknown',
			'Parents Education: Did not finish high school',
			'Parents Education: Graduated from college',
			'Parents Education: Graduated from high school',
			'Parents Education: Some education after high school',
			'Parents Education: Unknown',
			'Race: Unclassified')"
	.naepdat <- sqlQuery(dbrepcard, .qry)
	names(.naepdat) <- c("subject", "grade", "subgroup", "year", "state", "avg_scale_score", "below_basic", "at_or_above_basic", "at_or_above_proficient", "at_advanced")
	
	.lv <- level
	.ret <- c()
	
	for(i in 1:nrow(.naepdat)){
		if(!is.na(.naepdat$avg_scale_score[i])){
			.add <- indent(.lv) %+% '{\n'
			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"key": {\n', sep="")
			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"year": "', .naepdat$year[i], '",\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"subject": "', .naepdat$subject[i], '",\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"grade": "', .naepdat$grade[i], '",\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"subgroup": "', SubNAEP(.naepdat$subgroup[i]), '"\n', sep="")
			
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '},\n', sep="")
			down(.lv)
			
			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"val": {\n', sep="")

			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"average_scale_score": ', checkna(.naepdat$avg_scale_score[i]), ',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"at_below_basic": ', checkna(.naepdat$below_basic[i]), ',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"at_or_above_basic": ', checkna(.naepdat$at_or_above_basic[i]), ',\n', sep="")			
			.add <- .add %+% paste(indent(.lv), '"at_or_above_proficient": ', checkna(.naepdat$at_or_above_proficient[i]), ',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"at_advanced": ', checkna(.naepdat$at_advanced[i]), '\n', sep="")
			
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}\n', sep="")
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}', sep="")
			
			.ret <- c(.ret, .add)
		}
	}
	##print(.naepdat)
	return(paste(.ret, collapse=',\n'))
}

ExStateGrad <- function(level){
	.lv <- level
	
	.qry <- "SELECT * FROM [dbo].[graduation_sy1011]
		WHERE [cohort_status] = 'TRUE'"
	.grad11 <- sqlQuery(dbrepcard, .qry)
	.ret <- c()
		
	if(nrow(.grad11) > 10){
		.ret <- c(.ret, WriteGraduation(.grad11, .lv, 2011))
	}
	
	.qry <- "SELECT * FROM [dbo].[graduation_sy1112]
		WHERE [cohort_status] = 'TRUE'"
	.grad12 <- sqlQuery(dbrepcard, .qry)
		
	if(nrow(.grad12) > 10){
		.ret <- c(.ret, WriteGraduation(.grad12, .lv, 2012))
	}

	return(paste(.ret, collapse=',\n'))
}

WriteCASST <- function(.casdat_mr, year, level){
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
					.flevels <- c("S", "C", "D")
					.tmp <- subset(.tmp, new_to_us =='NO')
					.tmp <- subset(.tmp, school_grade==tested_grade | alt_tested=="YES")
				}
				
				.subgroups <- c("African American","White","Hispanic","Asian","Special Education","English Learner","Economically Disadvantaged","Male", "Female")
				
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

ExStateCAS <- function(level){
	.lv <- level
	
	## MATH/READING
	.qry13 <- "SELECT * FROM [dbo].[assessment_sy1213];"
	.dat13_mr <- sqlQuery(dbrepcard, .qry13)
	
	.qry12 <- "SELECT * FROM [dbo].[assessment_sy1112];"
	.dat12_mr <- sqlQuery(dbrepcard, .qry12)
	
	.qry11 <- "SELECT * FROM [dbo].[assessment_sy1011];"
	.dat11_mr <- sqlQuery(dbrepcard, .qry11)
	
	.qry10 <- "SELECT * FROM [dbo].[assessment_sy0910];"
	.dat10_mr <- sqlQuery(dbrepcard, .qry10)
	
	.qry09 <- "SELECT * FROM [dbo].[assessment_sy0809];"
	.dat09_mr <- sqlQuery(dbrepcard, .qry09)
	
	.ret <- c()
	
	if(nrow(.dat13_mr)>=10 & !is.null(.dat13_mr)){
		.ret[length(.ret)+1] <- WriteCASST(.dat13_mr, 2013, .lv)
	}
	
	if(nrow(.dat12_mr)>=10 & !is.null(.dat12_mr)){
		.ret[length(.ret)+1] <- WriteCASST(.dat12_mr, 2012, .lv)
	}
	
	if(nrow(.dat11_mr)>=10 & !is.null(.dat11_mr)){
		.ret[length(.ret)+1] <- WriteCASST(.dat11_mr, 2011, .lv)
	}
	
	if(nrow(.dat10_mr)>=10 & !is.null(.dat10_mr)){
		.ret[length(.ret)+1] <- WriteCASST(.dat10_mr, 2010, .lv)
	}
	
	if(nrow(.dat09_mr)>=10 & !is.null(.dat09_mr)){
		.ret[length(.ret)+1] <- WriteCASST(.dat09_mr, 2009, .lv)
	}	
	
	## 
	.qry13c <- "SELECT * FROM [dbo].[assessment_sy1213_comp];"
	.dat13_c <- sqlQuery(dbrepcard, .qry13c)
	if(nrow(.dat13_c)>=10){
		.ret[length(.ret)+1] <- WriteComp(.dat13_c, 2013, .lv)
	}
	
	.qry12c <- "SELECT * FROM [dbo].[assessment_sy1112_comp];"
	.dat12_c <- sqlQuery(dbrepcard, .qry12c)
	if(nrow(.dat12_c)>=10){
		.ret[length(.ret)+1] <- WriteComp(.dat12_c, 2012, .lv)
	}
	
	.qry11c <- "SELECT * FROM [dbo].[assessment_sy1011_comp];"
	.dat11_c <- sqlQuery(dbrepcard, .qry11c)
	if(nrow(.dat11_c)>=10){
		.ret[length(.ret)+1] <- WriteComp(.dat11_c, 2011, .lv)
	}
	
	## 
	.qry13s <- "SELECT * FROM [dbo].[assessment_sy1213_science]
		WHERE science_empty = 0;"
	.dat13_s <- sqlQuery(dbrepcard, .qry13s)
	if(nrow(.dat13_s)>=10){
		.ret[length(.ret)+1] <- WriteScience(.dat13_s, 2013, .lv)
	}
	
	.qry12s <- "SELECT * FROM [dbo].[assessment_sy1112_science]
		WHERE science_empty = 0;"
		
	.dat12_s <- sqlQuery(dbrepcard, .qry12s)
	if(nrow(.dat12_s)>=10){
		.ret[length(.ret)+1] <- WriteScience(.dat12_s, 2012, .lv)
	}
	
	.qry11s <- "SELECT * FROM [dbo].[assessment_sy1011_science]
	WHERE science_empty = 0;"
	
	.dat11_s <- sqlQuery(dbrepcard, .qry11s)
	if(nrow(.dat11_s)>=10){
		.ret[length(.ret)+1] <- WriteScience(.dat11_s, 2011, .lv)
	}
	
	return(paste(.ret, collapse=',\n'))
}






## ExCASChunk version 2
ExStateEnroll <- function(level){
	.lv <- level
	
	## MATH/READING
	.qry13 <- "SELECT * FROM [dbo].[enrollment_sy1213];"
	.dat13 <- sqlQuery(dbrepcard, .qry13)
	
	.qry12 <- "SELECT * FROM [dbo].[enrollment_sy1112]"
	.dat12 <- sqlQuery(dbrepcard, .qry12)
	
	.qry11 <- "SELECT * FROM [dbo].[enrollment_sy1011]"
	.dat11 <- sqlQuery(dbrepcard, .qry11)
	
	.qry10 <- "SELECT * FROM [dbo].[enrollment_sy0910];"
	.dat10 <- sqlQuery(dbrepcard, .qry10)
	
	.qry09 <- "SELECT * FROM [dbo].[enrollment_sy0809];"
	.dat09 <- sqlQuery(dbrepcard, .qry09)
	
	.qry08 <- "SELECT * FROM [dbo].[enrollment_sy0708];"
	.dat08 <- sqlQuery(dbrepcard, .qry08)
	
	.qry07 <- "SELECT * FROM [dbo].[enrollment_sy0607];"
	.dat07 <- sqlQuery(dbrepcard, .qry07)
	.ret <- c()
	
	if(nrow(.dat13)>=10 & !is.null(.dat13)){
		.ret[length(.ret)+1] <- WriteEnroll(.dat13, 2012, .lv)
	}
	
	if(nrow(.dat12)>=10 & !is.null(.dat12)){
		.ret[length(.ret)+1] <- WriteEnroll(.dat12, 2011, .lv)
	}
	
	if(nrow(.dat11)>=10 & !is.null(.dat11)){
		.ret[length(.ret)+1] <- WriteEnroll(.dat11, 2010, .lv)
	}
	
	if(nrow(.dat10)>=10 & !is.null(.dat10)){
		.ret[length(.ret)+1] <- WriteEnroll(.dat10, 2009, .lv)
	}
	
	if(nrow(.dat09)>=10 & !is.null(.dat09)){
		.ret[length(.ret)+1] <- WriteEnroll(.dat09, 2008, .lv)
	}
	
	if(nrow(.dat08)>=10 & !is.null(.dat08)){
		.ret[length(.ret)+1] <- WriteEnroll(.dat08, 2007, .lv)
	}
	
	if(nrow(.dat07)>=10 & !is.null(.dat07)){
		.ret[length(.ret)+1] <- WriteEnroll(.dat07, 2006, .lv)
	}

	return(paste(.ret, collapse=',\n'))	
}

hard_code_apr <- c('{
				"id": "apr",
				"data": [
					{
						"year": 2012,
						"indicators": {
							"1": {
								"on_target": false,
								"weight": 1,
								"val": 0.39,
								"target": 0.85,
								"target_dir": "up"
							},
							"2": {
								"on_target": false,
								"weight": 1,
								"val": 0.39,
								"target": 0.06,
								"target_dir": "down"
							},
							"3a": {
								"on_target": false,
								"weight": 0.3333,
								"val": 0.11,
								"target": 0.5,
								"target_dir": "up"
							},
							"3b_reading": {
								"on_target": true,
								"weight": 0.1667,
								"val": 0.95,
								"target": 0.95,
								"target_dir": "up"
							},
							"3b_math": {
								"on_target": true,
								"weight": 0.1667,
								"val": 0.95,
								"target": 0.95,
								"target_dir": "up"
							},
							"3c_elem-reading": {
								"on_target": false,
								"weight": 0.0833,
								"val": 0.15,
								"target": 0.7369,
								"target_dir": "up"
							},
							"3c_elem-math": {
								"on_target": false,
								"weight": 0.0833,
								"val": 0.18,
								"target": 0.7014,
								"target_dir": "up"
							},
							"3c_sec-reading": {
								"on_target": false,
								"weight": 0.0833,
								"val": 0.12,
								"target": 0.7179,
								"target_dir": "up"
							},
							"3c_sec-math": {
								"on_target": false,
								"weight": 0.0833,
								"val": 0.16,
								"target": 0.7027,
								"target_dir": "up"
							},
							"4a": {
								"on_target": false,
								"weight": 0.5,
								"val": 0.43,
								"target": 0,
								"target_dir": "down"
							},
							"4b_a": {
								"on_target": false,
								"weight": 0.25,
								"val": 0.43,
								"target": 0,
								"target_dir": "down"
							},
							"4b_b": {
								"on_target": false,
								"weight": 0.25,
								"val": 0.14,
								"target": 0,
								"target_dir": "down"
							},
							"5a": {
								"on_target": true,
								"weight": 0.3333,
								"val": 0.46,
								"target": 0.165,
								"target_dir": "up"
							},
							"5b": {
								"on_target": true,
								"weight": 0.3333,
								"val": 0.13,
								"target": 0.13,
								"target_dir": "down"
							},
							"5c": {
								"on_target": true,
								"weight": 0.3333,
								"val": 0.2,
								"target": 0.2,
								"target_dir": "down"
							},
							"6a": {
								"on_target": false,
								"weight": 0.5,
								"val": 0.53,
								"target": 0.63,
								"target_dir": "up"
							},
							"6b": {
								"on_target": false,
								"weight": 0.5,
								"val": 0.18,
								"target": 0.15,
								"target_dir": "down"
							},
							"7a_a": {
								"on_target": true,
								"weight": 0.1667,
								"val": 0.64,
								"target": 0.6,
								"target_dir": "up"
							},
							"7a_b": {
								"on_target": false,
								"weight": 0.1667,
								"val": 0.29,
								"target": 0.5,
								"target_dir": "up"
							},
							"7b_a": {
								"on_target": false,
								"weight": 0.1667,
								"val": 0.7,
								"target": 0.85,
								"target_dir": "up"
							},
							"7b_b": {
								"on_target": false,
								"weight": 0.1667,
								"val": 0.42,
								"target": 0.5,
								"target_dir": "up"
							},
							"7c_a": {
								"on_target": true,
								"weight": 0.1667,
								"val": 0.67,
								"target": 0.5,
								"target_dir": "up"
							},
							"7c_b": {
								"on_target": false,
								"weight": 0.1667,
								"val": 0.62,
								"target": 0.7,
								"target_dir": "up"
							},
							"8": {
								"on_target": false,
								"weight": 1,
								"val": 0.68,
								"target": 0.73,
								"target_dir": "up"
							},
							"9": {
								"on_target": false,
								"weight": 1,
								"val": 0.05,
								"target": 0,
								"target_dir": "down"
							},
							"10": {
								"on_target": false,
								"weight": 1,
								"val": 0.1,
								"target": 0,
								"target_dir": "down"
							},
							"11": {
								"on_target": false,
								"weight": 1,
								"val": 0.89,
								"target": 1,
								"target_dir": "up"
							},
							"12": {
								"on_target": false,
								"weight": 1,
								"val": 0.89,
								"target": 1,
								"target_dir": "up"
							},
							"13": {
								"on_target": false,
								"weight": 1,
								"val": 0.28,
								"target": 1,
								"target_dir": "up"
							},
							"14_a": {
								"on_target": true,
								"weight": 0.3333,
								"val": 0.35,
								"target": 0.26,
								"target_dir": "up"
							},
							"14_b": {
								"on_target": true,
								"weight": 0.3333,
								"val": 0.56,
								"target": 0.49,
								"target_dir": "up"
							},
							"14_c": {
								"on_target": true,
								"weight": 0.3333,
								"val": 0.68,
								"target": 0.61,
								"target_dir": "up"
							},
							"15": {
								"on_target": false,
								"weight": 1,
								"val": 0.61,
								"target": 1,
								"target_dir": "up"
							},
							"18": {
								"on_target": false,
								"weight": 1,
								"val": 0.27,
								"target": [0.55, 0.7],
								"target_dir": "btw"
							},
							"19": {
								"on_target": false,
								"weight": 1,
								"val": 0.7,
								"target": [0.45, 0.6],
								"target_dir": "btw"
							},
							"20": {
								"on_target": false,
								"weight": 1,
								"val": 0.9565,
								"target": 1,
								"target_dir": "up"
							},
						}
					}
				]
			},') 