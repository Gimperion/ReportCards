ExStateAMOs <- function(.lv){
	.qry <- "SELECT * 
		FROM [dbo].[amo_state_targets]"
		
	.amo_dat <- sqlQuery(dbrepcard, .qry)
	.ret <- c()
	
	if(nrow(.amo_dat) > 0){
		for(i in 1:nrow(.amo_dat)){
			## math chunk
			.add <- indent(.lv) %+% '{\n'
			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"key": {\n', sep="")
			up(.lv)			
			.add <- .add %+% paste(indent(.lv), '"subject": "Math",\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"grade": "all",\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"enrollment_status": "full_year",\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"subgroup": "',.amo_dat$subgroup[i],'",\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"year": "',.amo_dat$year[i],'"\n', sep="")
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '},\n', sep="")
		
			.add <- .add %+% paste(indent(.lv), '"val": {\n', sep="")
			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"baseline": ',checkna(.amo_dat$math_baseline[i]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"target": ', checkna(.amo_dat$math_target[i]),'\n', sep="")
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}\n', sep="")
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}', sep="")
			.ret <- c(.ret, .add)		
		
			## read chunk
			.add <- indent(.lv) %+% '{\n'
			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"key": {\n', sep="")
			up(.lv)			
			.add <- .add %+% paste(indent(.lv), '"subject": "Reading",\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"grade": "all",\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"enrollment_status": "full_year",\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"subgroup": "',.amo_dat$subgroup[i],'",\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"year": "',.amo_dat$year[i],'"\n', sep="")
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '},\n', sep="")
		
			.add <- .add %+% paste(indent(.lv), '"val": {\n', sep="")
			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"baseline": ', checkna(.amo_dat$read_baseline[i]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"target": ', checkna(.amo_dat$read_target[i]),'\n', sep="")
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}\n', sep="")
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}', sep="")
			.ret <- c(.ret, .add)	
		}
		return(paste(.ret, collapse=',\n'))		
	}
}


ExDiplCount <- function(.lv){
	.qry <- "SELECT * 
	FROM [dbo].[state_reg_dipl_count]"
		
	.dipl_data <- sqlQuery(dbrepcard, .qry)
	.ret <- c()

	if(nrow(.dipl_data) > 0){
		for(i in 1:nrow(.dipl_data)){
			.add <- indent(.lv+1) %+% sprintf('"%d": %d', .dipl_data$grad_year[i], .dipl_data$diplomas_issued[i])
			
			.ret <- c(.ret, .add)	
		}
		return(paste(.ret, collapse=',\n'))		
	}	
}

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

ExGradTargets <- function(level){	
	.ret <- sapply(2012:2017, function(x, lv){
		sprintf('%s{"key": %d, "value": %f}', indent(lv), x, 0.59 +(x-2011)*(0.78-0.59)/6)}, level 
	)
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
	
	.qry <- "SELECT * FROM [dbo].[assessment]
		WHERE [special_ed] = 'YES';"
		
	.dat_mr <- sqlQuery(dbrepcard, .qry)
	
	.ret <- do(group_by(.dat_mr, ea_year), WriteSPED, level)	
		
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
	.qry <- "SELECT 
		[subject]
      ,[grade]
      ,[subgroup]
      ,[year]
      ,[state]
      ,cast([average_scale_score] as int) as [average_scale_score]
      ,cast([below_basic] as int) as [below_basic]
      ,cast([at_or_above_basic] as int) as [at_or_above_basic]
      ,cast([at_or_above_proficient] as int) as [at_or_above_proficient]
      ,cast([at_advanced] as int) as [at_advanced]
      ,cast([national_average_scale_score] as int) as [national_average_scale_score]
      ,cast([national_below_basic] as int) as [national_below_basic]
      ,cast([national_at_or_above_basic] as int) as [national_at_or_above_basic]
      ,cast([national_at_or_above_proficient] as int) as [national_at_or_above_proficient]
      ,cast([national_at_advanced] as int) as [national_at_advanced]

		FROM [dbo].[naep_state_report]
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
	names(.naepdat) <- c("subject", "grade", "subgroup", "year", "state", "avg_scale_score", "below_basic", "at_or_above_basic", "at_or_above_proficient", "at_advanced","national_avg_scale_score", "national_below_basic", "national_at_or_above_basic", "national_at_or_above_proficient", "national_at_advanced")
	
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
			.add <- .add %+% paste(indent(.lv), '"at_advanced": ', checkna(.naepdat$at_advanced[i]), ',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"national_avg_scale_score": ', checkna(.naepdat$national_avg_scale_score[i]), ',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"national_below_basic": ', checkna(.naepdat$national_below_basic[i]), ',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"national_at_or_above_basic": ', checkna(.naepdat$national_at_or_above_basic[i]), ',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"national_at_or_above_proficient": ', checkna(.naepdat$national_at_or_above_proficient[i]), ',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"national_at_advanced": ', checkna(.naepdat$national_at_advanced[i]), '\n', sep="")
			
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
	
	.qry <- "SELECT A.*, B.[fy14_entity_code], B.[fy14_entity_name]
		FROM [dbo].[graduation] A
		LEFT JOIN [dbo].[fy14_mapping] B
		ON A.[school_code] = B.[school_code] 
			AND B.[grade] = '09'
			AND (A.[cohort_year]+2) = B.[ea_year]
		WHERE [cohort_status] = 1"
	.grad <- sqlQuery(dbrepcard, .qry)
		
	.ret <- do(group_by(.grad, cohort_year), WriteGraduation, level)
	
	return(paste(.ret, collapse=',\n'))
}


## Deprecated function. 
## Temporarily kept here for debugging reasons.
## Will be moved in future versions.
WriteCASST <- function(.casdat_mr, level){
	year <- .casdat_mr$year[1]
	.subjects <- c("Math", "Reading")
	.fay <- c("all", "full_year")
	
	.lv <- level
	
	.ret <- c()
	.plevels <- c("Below Basic", "Basic", "Proficient", "Advanced")
	
	.casdat_mr$math_level[.casdat_mr$exclude %in% c('I','M', 'A', 'Y')] <- NA
	.casdat_mr$read_level[.casdat_mr$exclude %in% c('I','M', 'A', 'Y')] <- NA
	
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
				
				.subgroups <- c("African American","White","Hispanic","Asian", "Pacific Islander", "Multiracial","Special Education","English Learner","Economically Disadvantaged","Male", "Female")
				
				for(h in 0:11){
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
	
	.dat_mr <- sqlFetch(dbrepcard, "assessment")
	.ret <- do(group_by(.dat_mr, ea_year), WriteCAS, level)
	
	## 
	.dat_comp <- sqlFetch(dbrepcard, 'assm_comp')
	.ret <- c(.ret, do(group_by(.dat_comp, ea_year), WriteComp, level))
	
	.dat_sci <- sqlFetch(dbrepcard, 'assm_science')
	.ret <- c(.ret, do(group_by(.dat_sci, ea_year), WriteScience, level))
	
	return(paste(.ret, collapse=',\n'))
}

ExStateEnroll <- function(level){
	.lv <- level
	
	.qry <- "SELECT A.*,
			B.[fy14_entity_code],
			B.[fy14_entity_name]
		  FROM [dbo].[enrollment] A
		INNER JOIN [dbo].[fy14_mapping] B
		ON A.[school_code] = B.[school_code] 
			AND A.[ea_year] = B.[ea_year] 
			AND A.[grade] = B.[grade]
		ORDER BY A.[ea_year]"
		
	.dat_enr <- sqlQuery(dbrepcard, .qry)
	.ret <- do(group_by(.dat_enr, ea_year), WriteEnroll, level)	
	
	return(paste(.ret, collapse=',\n'))	
}

hard_code_equity <- '			{
				"id": "unexcused_absences",
				"data": [
					{
						"key": {
							"year": "2012"
						},
						"val": {
							"1-5_days": 43,
							"6-10_days": 22,
							"11-15_days": 9,
							"16-25_days": 8,
							"more_than_25_days": 8
						}
					}
				]
			},
			{
				"id": "attendance",
				"data": [
					{
						"key": {
							"year": "2012"
						},
						"val": {
							"in_seat_attendance": 0.95,
							"average_daily_attendance": null
						}
					}
				]
			},
			{
				"id": "suspensions",
				"data": [
					{
						"key": {
							"year": "2012",
							"subgroup": "All"
						},
						"val": {
							"suspended_1": 0.12,
							"suspended_11": 0
						}
					},
					{
						"key": {
							"year": "2012",
							"subgroup": "African American"
						},
						"val": {
							"suspended_1": 0.16,
							"suspended_11": 0.01
						}
					},
					{
						"key": {
							"year": "2012",
							"subgroup": "Asian"
						},
						"val": {
							"suspended_1": 0.02,
							"suspended_11": 0
						}
					},
					{
						"key": {
							"year": "2012",
							"subgroup": "Economically Disadvantaged"
						},
						"val": {
							"suspended_1": 0.15,
							"suspended_11": 0.01
						}
					},
					{
						"key": {
							"year": "2012",
							"subgroup": "English Learner"
						},
						"val": {
							"suspended_1": 0.04,
							"suspended_11": 0
						}
					},
					{
						"key": {
							"year": "2012",
							"subgroup": "Hispanic"
						},
						"val": {
							"suspended_1": 0.04,
							"suspended_11":0
						}
					},
					{
						"key": {
							"year": "2012",
							"subgroup": "White"
						},
						"val": {
							"suspended_1": 0.01,
							"suspended_11":0
						}
					},
					{
						"key": {
							"year": "2012",
							"subgroup": "Pacific Islander"
						},
						"val": {
							"suspended_1": 0.1,
							"suspended_11":0
						}
					},
					{
						"key": {
							"year": "2012",
							"subgroup": "Multiracial"
						},
						"val": {
							"suspended_1": 0.05,
							"suspended_11":0
						}
					},
					{
						"key": {
							"year": "2012",
							"subgroup": "Special Education"
						},
						"val": {
							"suspended_1": 0.23,
							"suspended_11": 0.01
						}
					}
				]
			},
			{
				"id": "expulsions",
				"data": [
					{
						"key": {
							"year": "2012"
						},
						"val": {
							"expulsions":187,
							"expulsion_rate": 0.22
						}
					}
				]
			}'

hard_code_apr <- '			{
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
							}
						}
					}
				]
			},'