WriteJSONChunk <- function(elem_list){
    json_elems <- mapply(function(elem_name, elem_str_value){
        sprintf('"%s": %s',elem_name, elem_str_value)
    }, names(elem_list), elem_list)
    
    return('{' %+% paste(json_elems, collapse=", ") %+% '}')
}

ExAMOs <- function(orgcode, .lv){
    .qry <- sprintf("SELECT * 
        FROM [dbo].[amo_status_export]
        WHERE [current_entity_code] = %s", leadgr(orgcode, 4))
    .amo_dat <- sqlQuery(dbrepcard, .qry)
    .ret <- c()
    
    if(nrow(.amo_dat) > 0){
        for(i in 1:nrow(.amo_dat)){
            for(j in c("math", "read")){
                .add <- indent(.lv) %+% '{\n'
                up(.lv)
                .add <- .add %+% indent(.lv) %+% '"key": '
                .add <- .add %+% WriteJSONChunk(c(subject=ifelse(j=='math', '"Math"', '"Reading"'), 
                    grade='"all"',
                    enrollment_status='"full_year"',
                    subgroup=sprintf('"%s"', .amo_dat$subgroup[i]),
                    year=sprintf('"%s"', .amo_dat$year[i]))) %+% ', \n'
                .add <- .add %+% indent(.lv) %+% '"val": '
                .add <- .add %+% WriteJSONChunk(c(basline=checkna(.amo_dat[i, j %+% '_baseline']),
                    target=checkna(.amo_dat[i, j %+% '_target']))) %+% '\n'
                
                down(.lv)
                .add <- .add %+% paste(indent(.lv), '}', sep="")
                .ret <- c(.ret, .add)
            }
        }
        return(paste(.ret, collapse=',\n'))
    }
}

## cat(ExAMOs(210, 1), fill=TRUE)
ExHQTStatus <- function(org_code){
	.qry <- "SELECT * FROM [dbo].[hqt_status_sy1112]
		WHERE [school_code] = '" %+% org_code %+% "'"
	.prog <- sqlQuery(dbrepcard, .qry)

	if(nrow(.prog)>0){
		.ret <- .prog$percent_hqt_classes
		return(.ret)
		}
	else{
		return('null')
		}
}

##"subgroup": ("All","African American","White","Hispanic","Asian","Special Education","English Learner","Economically Disadvantaged","Homeless Students"),
AppendProgramInfo <- function(org_code){
	.qry <- "SELECT DISTINCT * FROM [dbo].[school_programs]
		WHERE [school_code] = '" %+% leadgr(org_code,4) %+% "'"
	.prog <- sqlQuery(dbrepcard, .qry)
	
	.ret <- c()	
	if(nrow(.prog)>0){
	
		.prog$program_string <- gsub('\n', "", .prog$program_string)
		.prog$program_string <- gsub('\r', "", .prog$program_string)
		.prog$program_string <- gsub('"', "'", .prog$program_string)
		.prog$program_string <- paste0('"',.prog$program_string,'"')
		
		return(paste(.prog$program_string, collapse=',\n'))
	} else{
		return('')
	}
}

##"subgroup": ("All","African American","White","Hispanic","Asian","Special Education","English Learner","Economically Disadvantaged","Homeless Students"),
RetMGPGroup <- function(.ingrp){
	if(.ingrp == 'All Students'){
		return('All')
	} else if(.ingrp == 'WH7'){
		return('White')
	} else if(.ingrp == 'Not-SpEd'){
		return('Not Special Education')
	} else if(.ingrp == 'SpEd'){
		return('Special Education')
	} else if (.ingrp == 'Not-LEP'){
		return('Not English Learner')
	} else if (.ingrp == 'Not-FARMS'){
		return('Not Economically Disadvantaged')
	} else if (.ingrp == 'FARMS'){
		return('Economically Disadvantaged')
	} else if (.ingrp == 'AS7'){
		return('Asian')
	} else if (.ingrp == 'BL7'){
		return('African American')
	} else if (.ingrp == 'HI7'){
		return('Hispanic')
	} else if (.ingrp == 'AM7'){
		return('American Indian/Alaskan Native')
	} else if (.ingrp == 'MU7'){
		return('Multi Racial')
	} else if (.ingrp == 'LEP'){
		return('English Learner')
	} else if (.ingrp == 'PI7'){
		return('Pacific Islander')
	} else{
		return(.ingrp)
	}
}

ExMGPResult <- function(org_code, level){
	.lv <- level
	.qry <- "SELECT * FROM [dbo].[mgp_summary]
		WHERE [fy13_entity_code] = '" %+% leadgr(org_code,4) %+% "'"
	.mgp <- sqlQuery(dbrepcard, .qry)	
	.ret <- c()	
	if(nrow(.mgp)>0){
		for(i in 1:nrow(.mgp)){
			if(.mgp$group_fay_size[i] >= 10 ){
                .add <- indent(.lv) %+% '{\n'
                up(.lv)
                .add <- .add %+% indent(.lv) %+% '"key": '
                .add <- .add %+% WriteJSONChunk(c(subject=sprintf('"%s"', .mgp$subject[i]), 
                    subgroup=sprintf('"%s"', RetMGPGroup(.mgp$group[i])),
                    year=sprintf('"%s"', .mgp$year[i]))) %+% '\n'
                .add <- .add %+% indent(.lv) %+% '"val": '
                .add <- .add %+% WriteJSONChunk(c(group_size=checkna(.mgp$group_fay_size[i]),
                    mgp_1yr=checkna(.mgp$mgp_1yr[i]),                    
                    mgp_2yr=checkna(.mgp$mgp_2yr[i]))) %+% '\n'
                down(.lv)
                .add <- .add %+% paste(indent(.lv), '}', sep="")
                .ret <- c(.ret, .add)
            }
        }
    }
    return(paste(.ret, collapse=',\n'))
}

ExCollegeReadiness <- function(org_code, level){
	.lv <- level
	
	.qry <- "SELECT * FROM [dbo].[college_readiness]
		WHERE [fy13_entity_code] = '" %+% org_code %+% "'"
		
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

EncodeCReady <- function(.dat, level){
	.lv <- level
	.subgroups <- c("African American","White","Hispanic","Asian","American Indian", "Pacific Islander", "Multi Racial","Special Education","English Learner","Economically Disadvantaged","Male", "Female")

	.ret <- c()
	for(j in 0:12){
		.tmp <- .dat
		.slice <- 'All'
		if(j > 0){
			.tmp <- SubProcGrad(.tmp, j)
			.slice <- .subgroups[j]
		}
		
		if(nrow(.tmp)>=10){
			.add <- indent(.lv) %+% '{\n'
			
			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"key": {\n', sep="")
			up(.lv)			
			.add <- .add %+% paste(indent(.lv), '"subgroup": "', .slice,'", \n', sep="")
			.add <- .add %+% paste(indent(.lv), '"year": "',.tmp$year[1],'" \n', sep="")
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '},\n', sep="")
				
			.add <- .add %+% paste(indent(.lv), '"val": {\n', sep="")
			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"graduates": ', nrow(.tmp),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"act_taker": ', nrow(subset(.tmp, act_taker=='YES')),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"sat_taker": ', nrow(subset(.tmp, sat_taker=='YES')),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"ap_taker": ', nrow(subset(.tmp, ap_taker=='YES')),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"psat_taker": ', nrow(subset(.tmp, psat_taker=='YES')),'\n', sep="")
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}\n', sep="")
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}', sep="")
			
			.ret <- c(.ret, .add)
		}	
	}
	return(paste(.ret, collapse=',\n'))
}

ExGraduation <- function(org_code, level=1){
	.qry <- sprintf("SELECT A.*, B.[fy14_entity_code], B.[fy14_entity_name]
		FROM [dbo].[graduation] A
		LEFT JOIN [dbo].[fy14_mapping] B
		ON A.[school_code] = B.[school_code] 
			AND B.[grade] = '09'
			AND (A.[cohort_year]+2) = B.[ea_year]
		WHERE [cohort_status] = 1 AND 
			[fy14_entity_code] = '%s'", leadgr(org_code,4))

	.grad <- sqlQuery(dbrepcard, .qry)
	
	.ret <- do(group_by(.grad, cohort_year), WriteGraduation, level)
	.ret <- unlist(.ret)
	.ret <- sort(subset(.ret, .ret!=''))
	return(paste(.ret, collapse=',\n'))
}


SubProcGrad <- function(.dat, lv){
	if(lv==1){
		return(subset(.dat, race=="BL7"))
	} else if(lv==2){
		return(subset(.dat, race=="WH7"))
	} else if(lv==3){
		return(subset(.dat, race=="HI7"))
	} else if(lv==4){
		return(subset(.dat, race=="AS7"))
	} else if(lv==5){
		return(subset(.dat, race=="AM7"))
	} else if(lv==6){
		return(subset(.dat, race=="PI7"))
	} else if(lv==7){
		return(subset(.dat, race=="MU7"))
	} else if(lv==8){
		return(subset(.dat, special_ed=="YES"))
	} else if(lv==9){
		return(subset(.dat, ell_prog=="YES"))
	} else if(lv==10){
		return(subset(.dat, economy == "YES"))
	} else if(lv==11){
		return(subset(.dat, gender %in% c("M", "MALE")))
	} else if(lv==12){
		return(subset(.dat, gender %in% c("F", "FEMALE")))
	}	
	return(.dat[NULL,])
}

WriteGraduation <- function(gdata, level){

	.lv <- level
	.ret <- c()
	year <- gdata$cohort_year[1] +4
	.subgroups <- c("African American","White","Hispanic","Asian","American Indian", "Pacific Islander", "Multi Racial","Special Education","English Learner","Economically Disadvantaged","Male", "Female")
	
	for(s in 0:length(.subgroups)){
		soutput <- "All"
		.tmps <- gdata
		if(s > 0){
			soutput <- .subgroups[s]
			.tmps <- SubProcGrad(gdata, s)
		}
		
		if(nrow(.tmps)>=10){
			.add <- indent(.lv) %+% '{\n'
			
			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"key": {\n', sep="")
			up(.lv)
			
			.add <- .add %+% paste(indent(.lv), '"subgroup": "', soutput,'",\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"year": "',year,'"\n', sep="")
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '},\n', sep="")
				
			.add <- .add %+% paste(indent(.lv), '"val": {\n', sep="")
			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"graduates": ', nrow(subset(.tmps, graduated==1)),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"cohort_size": ', nrow(.tmps),'\n', sep="")
			
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}\n', sep="")
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}', sep="")
			
			.ret <- c(.ret, .add)
		}		
	}
	
	return(paste(.ret, collapse=',\n'))
}

## What are we trying to do?
## 1) print profile url
## 2) print pmf by year. 


ExTest <- function(org_code){
    pull_tables <- c('pmf_sy1011', 'pmf_sy1112', 'pmf_sy1213')
    .pmf_dat <- lapply(sprintf("SELECT * FROM [dbo].[%s]
    WHERE [school_code] = '%s'", pull_tables, as.character(org_code)), sqlQuery, channel=dbrepcard)
    .pmf_dat
}

ExPMF <- function(org_code, level){
    .lv <- level
    pull_tables <- c('pmf_sy1011', 'pmf_sy1112', 'pmf_sy1213')
    
    .pmf_dat <- lapply(sprintf("SELECT * FROM [dbo].[%s]
        WHERE [school_code] = '%s'", pull_tables, as.character(org_code)), sqlQuery, channel=dbrepcard)

    names(.pmf_dat) <- c("2011", "2012", "2013")
    .pmf_dat <- .pmf_dat[sapply(.pmf_dat, nrow) > 0]
    
    if(length(.pmf_dat) >0){    
        .qry_profile <- "SELECT * FROM [dbo].[profile_urls_13]
            WHERE [school_code] = '" %+% org_code %+% "'"
        .prog_profile <- sqlQuery(dbrepcard, .qry_profile)
        
        if(nrow(.prog_profile)>0){ 
            .profile <- '"' %+% .prog_profile$url %+% '"'
        }
        else{
            .profile <- 'null'
        }
        .ret <- mapply(function(x,pmf_yr){
            .ret <- c()
            for(i in 1:nrow(x)){
                .add <- indent(.lv) %+% '{\n'
                up(.lv)
                .add <- .add %+% indent(.lv) %+% '"key": '
                .add <- .add %+% WriteJSONChunk(c(year= sprintf('"%s"', pmf_yr), 
                    category=sprintf('"%s"', x$framework[i]))) %+% ', \n'
                .add <- .add %+% indent(.lv) %+% '"val": '
                .add <- .add %+% WriteJSONChunk(c(score=checkna(x$score[i]),
                    target=checkna_str(x$tier[i]))) %+% '\n'                
                down(.lv)
                .add <- .add %+% paste(indent(.lv), '}', sep="")
                .ret <- c(.ret, .add)
            }
            return(.ret)
        }, .pmf_dat, names(.pmf_dat))
        return(paste(.ret, collapse=',\n'))
    }
}

## .subgroups <- c("African American","White","Hispanic","Asian","American Indian", "Pacific Islander", "Multi Racial","Special Education","English Learner","Economically Disadvantaged","Male", "Female")
SubCEnroll <- function(subgroup){
	if(subgroup == 'Total high school graduates'){
		return('All')
	} else if(subgroup=='Black or African American'){
		return('African American')
	} else if(subgroup=='Hispanic / Latino'){
		return('Hispanic')
	} else if(subgroup=='Two or more races'){
		return('Multiracial')
	} else if(subgroup=='Economically Disadvantaged=Y'){
		return('Economically Disadvantaged')
	} else if(subgroup=='Economically Disadvantaged=N'){
		return('Not Economically Disadvantaged')
	} else if(subgroup=='Limited English=Y'){
		return('English Learner')
	} else if(subgroup=='Limited English=N'){
		return('Not English Learner')
	} else if(subgroup=='Disability=Y'){
		return('Special Education')
	} else if(subgroup=='Disability=N'){
		return('Not Special Education')
	}
	return(subgroup)
}

ExCollegeEnroll <- function(org_code, level){
	.lv <- level
	
	.qry <- "SELECT * FROM [dbo].[college_enroll_2010]
		WHERE [fy13_entity_code] = '" %+% org_code %+% "'
		AND [Graduates] >= 10;"
	.cenr10 <- sqlQuery(dbrepcard, .qry)
	.ret <- c()

	if(nrow(.cenr10)> 0){
		.ret <- c(.ret, WriteCEnroll(.cenr10, .lv, 2010))
	}

	.qry <- "SELECT * FROM [dbo].[college_enroll_2009]
		WHERE [fy13_entity_code] = '" %+% org_code %+% "'
		AND [Graduates] >= 10;"
	.cenr09 <- sqlQuery(dbrepcard, .qry)

	if(nrow(.cenr09)> 0){
		.ret <- c(.ret, WriteCEnroll(.cenr09, .lv, 2009))
	}		
	
	.qry <- "SELECT * FROM [dbo].[college_enroll_2008]
		WHERE [fy13_entity_code] = '" %+% org_code %+% "'
		AND [Graduates] >= 10;"
	.cenr08 <- sqlQuery(dbrepcard, .qry)

	if(nrow(.cenr08)> 0){
		.ret <- c(.ret, WriteCEnroll(.cenr08, .lv, 2008))
	}		
		
	.qry <- "SELECT * FROM [dbo].[college_enroll_2007]
		WHERE [fy13_entity_code] = '" %+% org_code %+% "'
		AND [Graduates] >= 10;"
	.cenr07 <- sqlQuery(dbrepcard, .qry)

	if(nrow(.cenr07)> 0){
		.ret <- c(.ret, WriteCEnroll(.cenr07, .lv, 2007))
	}		
	
	if(length(.ret)>0){
		return(paste(.ret, collapse=',\n'))
	}else{
		return('')
	}
}

WriteCEnroll <- function(.cenr, level, year){
    .ret <- c()
    .lv <- level
    for(i in 1:nrow(.cenr)){
        .add <- indent(.lv) %+% '{\n'
        up(.lv)
        .add <- .add %+% indent(.lv) %+% '"key": '
        .add <- .add %+% WriteJSONChunk(c(cohort_year=sprintf('"%s"', year), 
            subgroup=sprintf('"%s"', SubCEnroll(.cenr$Group[i])))) %+% '\n'
        .add <- .add %+% indent(.lv) %+% '"val": '
        .add <- .add %+% WriteJSONChunk(c(hs_graduates=checkna(.cenr$Graduates[i]),
            enroll_within_16mo=checkna(.cenr$Initial_Enroll_16mo[i]),
            enroll_within_16mo_instate=checkna(.cenr$Initial_Enroll_InState_16mo[i]),
            complete_1yr_instate=checkna(.cenr$Complete_1Yr_in_State[i]))
            ) %+% '\n'
        down(.lv)
        .add <- .add %+% paste(indent(.lv), '}', sep="")
        .ret <- c(.ret, .add)
    }
    return(.ret)
}

ExAccountability <- function(org_code, level){
	## MATH/READING
	.lv <- level
	
	.qry <- "SELECT * FROM [dbo].[accountability_sy1213]
		WHERE [school_code] = '" %+% org_code %+% "';"
	.acct <- sqlQuery(dbrepcard, .qry)
	
	##print(.acct)
	.ret <- c()
	if(nrow(.acct)>0){
		.qry <- "SELECT * FROM [dbo].[accountability_sy1213_sg]
		WHERE [school_code] = '" %+% org_code %+% "';"
		.acct_sg <- sqlQuery(dbrepcard, .qry)
	
		.ret <- '{\n'
		up(.lv)
		
		.ret <- .ret %+% paste(indent(.lv), '"year": "2013", \n', sep="")
		.ret <- .ret %+% paste(indent(.lv), '"score": ',checkna(round(.acct$acct_score[1],2)),', \n', sep="")
		.ret <- .ret %+% paste(indent(.lv), '"classification": ',checkna_str(.acct$classification[1]),', \n', sep="")
		.ret <- .ret %+% paste(indent(.lv), '"growth": ',checkna(round(.acct$growth[1],2)),', \n', sep="")
		
		.ret <- .ret %+% paste(indent(.lv), '"subgroups": [\n', sep="")
		up(.lv)
		.sgstrings <- c()
		if(nrow(.acct_sg)>0){
			for(i in 1:nrow(.acct_sg)){
				.add <- ''
				if(.acct_sg$read_size[i] >= 25 | .acct_sg$math_size[i] >= 25 | .acct_sg$comp_size[i] >= 25){
					.add <- indent(.lv) %+% '{\n'
					up(.lv)
					
					.add <- .add %+% paste(indent(.lv), '"subgroup": ',checkna_str(.acct_sg$subgroup[i]),',\n', sep="")
					
					.add <- .add %+% paste(indent(.lv), '"read_size": ',checkna(.acct_sg$read_size[i]),',\n', sep="")
					.add <- .add %+% paste(indent(.lv), '"read_score": ',checkna(round(.acct_sg$read_score[i],2)),',\n', sep="")
					
					.add <- .add %+% paste(indent(.lv), '"math_size": ',checkna(.acct_sg$math_size[i]),',\n', sep="")
					.add <- .add %+% paste(indent(.lv), '"math_score": ',checkna(round(.acct_sg$math_score[i],2)),',\n', sep="")
					
					.add <- .add %+% paste(indent(.lv), '"comp_size": ',checkna(.acct_sg$comp_size[i]),',\n', sep="")
					.add <- .add %+% paste(indent(.lv), '"comp_score": ',checkna(round(.acct_sg$comp_score[i],2)),'\n', sep="")
					
					
					down(.lv)		
					.add <- .add %+% paste(indent(.lv), '}', sep="")
					.sgstrings <- c(.sgstrings, .add)
				}
			}
			.ret <- .ret %+% paste(.sgstrings, collapse=',\n') %+% '\n'
		}
		down(.lv)
		.ret <- .ret %+% paste(indent(.lv), ']\n', sep="")
		down(.lv)
		.ret <- .ret %+% paste(indent(.lv), '}', sep="")
		return(.ret)
	} else{
		return('null')
	}
	return('null')
}

ExAccreditation <- function(org_code, level=0){
	## MATH/READING
	.lv <- level
	
	.qry <- "SELECT * FROM [dbo].[ece_accr_sy1213]
		WHERE [school_code] = '" %+% org_code %+% "';"
	.accr <- sqlQuery(dbrepcard, .qry)
	
	.ret <- c()
	if(nrow(.accr)>0){
		for(i in 1:nrow(.accr)){
			.add <- indent(level) %+% '{\n'
			up(.lv)
			
			.add <- .add %+% sprintf(indent(.lv) %+% '"type": %s,\n', checkna_str(.accr$accreditation_type[i]))
			.add <- .add %+% sprintf(indent(.lv) %+% '"level": %s,\n', checkna_str(.accr$accreditation_level[i]))
			.add <- .add %+% sprintf(indent(.lv) %+% '"exp_date": %s\n', checkna_str(.accr$exp_date[i]))

			down(.lv)		
			.add <- .add %+% indent(.lv) %+% '}'
			.ret <- c(.ret, .add)
		}
	} 
	return(paste(.ret, collapse=',\n'))
}

ExSPEDChunk <- function(scode, level){
	.qry_sped_cas <- sprintf("SELECT A.* FROM [dbo].[assessment] A
		INNER JOIN (SELECT 
			[ea_year]
			,[school_code]
			,[grade]
		FROM [dbo].[fy14_mapping]
		WHERE [fy14_entity_code] = '%s') B
	ON A.[ea_year] = B.[ea_year] 
		AND A.[school_grade] = B.[grade] 
		AND A.[school_code] = B.[school_code]
	WHERE A.[special_ed] = 'YES'", leadgr(scode,4))
	
	.dat_mr <- sqlQuery(dbrepcard, .qry_sped_cas)
	.ret <- do(group_by(.dat_mr, ea_year), WriteSPED, level)
	
	.ret <- subset(.ret, .ret != '')
	return(paste(.ret, collapse=',\n'))
}

WriteSPED <- function(.casdat_mr, level){
	year <- .casdat_mr$year[1]
	.subjects <- c("Math", "Reading")
	.lv <- level
	
	.ret <- c()
	.plevels <- c("Below Basic", "Basic", "Proficient", "Advanced")
	
	## A = Subject, 1 for Math, 2 for Reading
	for(a in 1:2){
		for(b in 1:4){
			soutput <- "All SPED Students"
			.tmp <- .casdat_mr
			if(b == 2){
				.tmp <- subset(.casdat_mr, sped_acc == 'YES')
				soutput <- "With Accommodations"
			} else if(b == 3){
				.tmp <- subset(.casdat_mr, sped_acc != 'YES')
				soutput <- "No Accommodations"
			} else if(b==4){
				.tmp <- subset(.casdat_mr, alt_tested=='YES')
				soutput <- "ALT Test Takers"
			}
			
			if(a ==1){
				.profs <- .tmp$math_level
			} else if(a == 2){
				.profs <- .tmp$read_level
			}
			
			## START WRITE ##
			if(length(.profs[.profs %in% .plevels]) >= 10){
			
				.add <- indent(.lv) %+% '{\n'
				
				up(.lv)
				.add <- .add %+% paste(indent(.lv), '"key": {\n', sep="")
				up(.lv)				

				.add <- .add %+% paste(indent(.lv), '"subject": "',.subjects[a],'",\n', sep="")
				.add <- .add %+% paste(indent(.lv), '"subgroup": "',soutput,'", \n', sep="")
				.add <- .add %+% paste(indent(.lv), '"year": "',year,'"\n', sep="")
				
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
	return(paste(.ret, collapse=',\n'))	
}

ExEnrollChunk <- function(scode, level){
	.lv <- level
		
	.qry_enr <- sprintf("SELECT A.*,
			B.[fy14_entity_code],
			B.[fy14_entity_name]
		  FROM [dbo].[enrollment] A
		INNER JOIN (SELECT *
			FROM [dbo].[fy14_mapping]
			WHERE [fy14_entity_code] = '%s') B
		ON A.[school_code] = B.[school_code] 
			AND A.[ea_year] = B.[ea_year] 
			AND A.[grade] = B.[grade]
		ORDER BY A.[ea_year]", leadgr(scode,4))
	
	.dat_enr <- sqlQuery(dbrepcard, .qry_enr)
	
	.ret <- do(group_by(.dat_enr, ea_year), WriteEnroll, level)	
	
	return(paste(unlist(.ret), collapse=',\n'))	
}

##"African American","White","Hispanic","Asian","American Indian", "Pacific Islander", "Multi Racial","Special Education","English Learner","Economically Disadvantaged","Male", "Female"
SubProcEnr <- function(.dat, lv){
	if(lv==1){
		return(subset(.dat, race=="BL7" & ethnicity=='NO'))
	} else if(lv==2){
		return(subset(.dat, race=="WH7" & ethnicity=='NO'))
	} else if(lv==3){
		return(subset(.dat, ethnicity =='YES'))
	} else if(lv==4){
		return(subset(.dat, race=="AS7" & ethnicity=='NO'))
	} else if(lv==5){
		return(subset(.dat, race=="AM7" & ethnicity=='NO'))
	} else if(lv==6){
		return(subset(.dat, race=="PI7" & ethnicity=='NO'))
	} else if(lv==7){
		return(subset(.dat, race=="MU7" & ethnicity=='NO'))
	} else if(lv==8){
		return(subset(.dat, !is.na(sped_level)))
	} else if(lv==9){
		return(subset(.dat, ell_prog=='YES'))
	} else if(lv==10){
		return(subset(.dat, economy %in% c("FREE", "REDUCED", "DCERT", "CEO", "YES")))
	} else if(lv==11){
		return(subset(.dat, gender %in% c("M", "MALE")))
	} else if(lv==12){
		return(subset(.dat, gender %in% c("F", "FEMALE")))
	}	
	return(.dat[NULL,])
}

WriteEnroll <- function(.edat, level){

	.ret <- c()
	
	if(nrow(.edat) < 10){
		return(NULL)	
	}
	
	year <- .edat$ea_year[1]
	.lv <- level
	.edat$grade <- sapply(.edat$grade, leadgr, 2)
	
	.glist <- unique(.edat$grade)
	
	.subgroups <- c("African American","White","Hispanic","Asian","American Indian", "Pacific Islander", "Multi Racial","Special Education","English Learner","Economically Disadvantaged","Male", "Female")
	
	for(g in 0:length(.glist)){
		.tmp <- .edat
		goutput <- "All"
		if(g>0){
			.tmp <- subset(.tmp, grade==.glist[g])
			goutput <- .glist[g]
		}
		
		for(s in 0:length(.subgroups)){
			
			soutput <- "All"
			.tmps <- .tmp
			if(s > 0){
				soutput <- .subgroups[s]
				.tmps <- SubProcEnr(.tmp, s)
			}
			
			if(nrow(.tmps)>=10){
				.add <- indent(.lv) %+% '{\n'
				
				up(.lv)
				.add <- .add %+% paste(indent(.lv), '"key": {\n', sep="")
				up(.lv)
				
				.add <- .add %+% paste(indent(.lv), '"grade": "',goutput,'", \n', sep="")
				.add <- .add %+% paste(indent(.lv), '"subgroup": "', soutput,'", \n', sep="")
				.add <- .add %+% paste(indent(.lv), '"year": "',year,'" \n', sep="")
				down(.lv)				
				.add <- .add %+% paste(indent(.lv), '},\n', sep="")
					
				.add <- .add %+% paste(indent(.lv), '"val": ',nrow(.tmps),'\n', sep="")
				down(.lv)
				.add <- .add %+% paste(indent(.lv), '}', sep="")
				
				.ret[length(.ret)+1] <- .add		
			}
		}
	}
	
	## grade/subgroup, etc.	
	
	return(paste(.ret, collapse=',\n'))
}

##"African American","White","Hispanic","Asian","Special Education","English Learner","Economically Disadvantaged","Male", "Female")
SubProc <- function(.dat, lv, b=0){
	if(lv==0){
		return(.dat)
	} else if(lv==1){
		return(subset(.dat, race=="BL7"))
	} else if(lv==2){
		return(subset(.dat, race=="WH7"))
	} else if(lv==3){
		return(subset(.dat, race=="HI7"))
	} else if(lv==4){
		return(subset(.dat, race=="AS7"))
	} else if(lv==5){
		return(subset(.dat, race=="PI7"))
	} else if(lv==6){
		return(subset(.dat, race=="MU7"))
	} else if(lv==7){
		if(b==1){
			return(subset(.dat, special_ed=='YES' | sped_monitored=='YES'))
		} else{
			.tmp <- subset(.dat, special_ed == 'YES')
			if(nrow(.tmp) < 25){
				return(.tmp)
			} else{
				return(subset(.dat, special_ed=='YES' | sped_monitored=='YES'))
			}
		}
	} else if(lv==8){
		if(b==1){
			return(subset(.dat, ell_prog=='YES' | ell_monitored=='YES'))
		} else{
			.tmp <- subset(.dat, ell_prog == 'YES')
			if(nrow(.tmp) < 25){
				return(.tmp)
			} else{
				return(subset(.dat, ell_prog=='YES' | ell_monitored=='YES'))
			}
		}
	} else if(lv==9){
		return(subset(.dat, economy=="YES"))
	} else if(lv==10){
		return(subset(.dat, gender %in% c("M", "MALE")))
	} else if(lv==11){
		return(subset(.dat, gender %in% c("F", "FEMALE")))
	}
	return(0)
}

## rewrite
WriteCAS <- function(.casdat_mr, spaces, entity='state'){
	year <- .casdat_mr$year[1]
	
	.casdat_mr$math_level[.casdat_mr$exclude %in% c('I','M', 'A', 'Y')] <- NA
	.casdat_mr$read_level[.casdat_mr$exclude %in% c('I','M', 'A', 'Y')] <- NA

	.subjects <- c("Math", "Reading")
	.fay <- c("all", "full_year")
	
	.lv <- spaces
	
	.ret <- c()
	.plevels <- c("Below Basic", "Basic", "Proficient", "Advanced")
	
	.entity_fay <- list('state'=c("S", "C", "D"), 'lea'=c("S", "C"), 'school'=c("S"))

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
					.flevels <- .entity_fay[entity][[1]]
					.tmp <- subset(.tmp, new_to_us =='NO')
					.tmp <- subset(.tmp, school_grade==tested_grade | alt_tested=="YES")
					.tmp <- subset(.tmp, school_code %notin% c("0948", "0958"))
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
						
						.ret <- c(.ret, .add)
					}
				}
			}
		}
	}
	
	for(z in .fay){		
		.tmp <- subset(.casdat_mr, new_to_us=='YES')
		.add <- indent(.lv) %+% '{\n'
		
		up(.lv)
		.add <- .add %+% paste(indent(.lv), '"key": {\n', sep="")
		up(.lv)
		
		if(z == "full_year"){
			.tmp <- subset(.tmp, full_academic_year %in% .entity_fay[entity][[1]])
			.tmp <- subset(.tmp, school_grade==tested_grade | alt_tested=="YES")
			.tmp <- subset(.tmp, school_code %notin% c("0948", "0958"))
		}
		
		.profs <- .tmp$read_level
		
		.add <- .add %+% paste(indent(.lv), '"subject": "Reading",\n', sep="")					
		
		.add <- .add %+% paste(indent(.lv), '"grade": "all", \n', sep="")
		.add <- .add %+% paste(indent(.lv), '"enrollment_status": "',z,'", \n', sep="")
		.add <- .add %+% paste(indent(.lv), '"subgroup": "New to the US (ELL)", \n', sep="")
		.add <- .add %+% paste(indent(.lv), '"year": "',year,'" \n', sep="")
		
		down(.lv)
		
		.add <- .add %+% paste(indent(.lv), '},\n', sep="")
		.add <- .add %+% paste(indent(.lv), '"val": {\n', sep="")
		up(.lv)
		
		.add <- .add %+% paste(indent(.lv), '"n_eligible":',length(.profs),',\n', sep="")
		.add <- .add %+% paste(indent(.lv), '"n_test_takers":',length(.profs[.profs %in% .plevels]),',\n', sep="")
		.add <- .add %+% paste(indent(.lv), '"advanced_or_proficient":null,\n', sep="")
		
		.add <- .add %+% paste(indent(.lv), '"advanced":null,\n', sep="")
		.add <- .add %+% paste(indent(.lv), '"proficient":null,\n', sep="")
		.add <- .add %+% paste(indent(.lv), '"basic":null,\n', sep="")
		.add <- .add %+% paste(indent(.lv), '"below_basic":null\n', sep="")
		
		down(.lv)
		.add <- .add %+% paste(indent(.lv), '}\n', sep="")
		down(.lv)
		.add <- .add %+% paste(indent(.lv), '}', sep="")
		
		.ret <- c(.ret, .add)
	
	}
	return(paste(.ret, collapse=',\n'))
}



WriteComp <- function(.casdat_comp, level){
## Composition 
	year <- .casdat_comp$year[1]
	.fay <- c("all", "full_year")
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
				for(z in .fay){
				.add <- indent(.lv) %+% '{\n'
				
				up(.lv)
				.add <- .add %+% paste(indent(.lv), '"key": {\n', sep="")
				up(.lv)
				
				.profs <- .tmp$comp_level
				
				.add <- .add %+% paste(indent(.lv), '"subject": "Composition",\n', sep="")			
				.add <- .add %+% paste(indent(.lv), '"grade": "',goutput,'", \n', sep="")
				.add <- .add %+% paste(indent(.lv), '"enrollment_status": "',z,'", \n', sep="")
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
				
				.ret <- c(.ret, .add)
			}
		}
	}
	return(paste(sort(.ret), collapse=',\n'))
}

ExCasChunk <- function(scode, level){

	
	## MATH/READING
	.qry_mr <- sprintf("SELECT A.* FROM [dbo].[assessment] A
		INNER JOIN (SELECT 
			[ea_year]
			,[school_code]
			,[grade]
		FROM [dbo].[fy14_mapping]
		WHERE [fy14_entity_code] = '%s') B
	ON A.[ea_year] = B.[ea_year] 
		AND A.[school_grade] = B.[grade] 
		AND A.[school_code] = B.[school_code]", leadgr(scode,4))
	
	.dat_mr <- sqlQuery(dbrepcard, .qry_mr)
	.ret <- do(group_by(.dat_mr, ea_year), WriteCAS, level, "school")
	
	## Comp
	.qry_comp <- sprintf("SELECT A.* FROM [dbo].[assm_comp] A
		INNER JOIN (SELECT 
			[ea_year]
			,[school_code]
			,[grade]
		FROM [dbo].[fy14_mapping]
		WHERE [fy14_entity_code] = '%s') B
	ON A.[ea_year] = B.[ea_year] 
		AND A.[tested_grade] = B.[grade] 
		AND A.[school_code] = B.[school_code]", leadgr(scode,4))

	.dat_comp <- sqlQuery(dbrepcard, .qry_comp)
	.ret <- c(.ret, do(group_by(.dat_comp, ea_year), WriteComp, level))
	
	## Science
	.qry_sci <- sprintf("SELECT A.* FROM [dbo].[assm_science] A
		INNER JOIN (SELECT 
			[ea_year]
			,[school_code]
			,[grade]
		FROM [dbo].[fy14_mapping]
		WHERE [fy14_entity_code] = '%s') B
	ON A.[ea_year] = B.[ea_year] 
		AND A.[tested_grade] = B.[grade] 
		AND A.[school_code] = B.[school_code]", leadgr(scode,4))

	.dat_sci <- sqlQuery(dbrepcard, .qry_sci)
	.ret <- c(.ret, do(group_by(.dat_sci, ea_year), WriteScience, level))
	
	.ret <- subset(.ret, .ret != '')
	return(paste(.ret, collapse=',\n'))
}


## Science
WriteScience <- function(.casdat_sci, level){
	year <- .casdat_sci$year[1]
	.fay <- c("all", "full_year")
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
			for(z in .fay){
				.add <- indent(.lv) %+% '{\n'
				
				up(.lv)
				.add <- .add %+% paste(indent(.lv), '"key": {\n', sep="")
				up(.lv)
				
				.profs <- .tmp$science_level
				
				.add <- .add %+% paste(indent(.lv), '"subject": "Science",\n', sep="")
				
				.add <- .add %+% paste(indent(.lv), '"grade": "',goutput,'", \n', sep="")
				.add <- .add %+% paste(indent(.lv), '"enrollment_status": "',z,'", \n', sep="")
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
				
				.ret <- c(.ret, .add)
			}
		}
	}
	return(paste(sort(.ret), collapse=',\n'))
}

WriteAttendance <- function(org_code, level){
	.ret <- c()	
	.lv <- level

	att_qry <- paste0("SELECT * FROM [dbo].[equity_report_prelim]
				WHERE School_Code = '",org_code,"' AND [Metric] = 'In-Seat Attendance Rate' AND [ReportType] = 'External'")
	att <- sqlQuery(dbrepcard, att_qry)
	
	if(nrow(att) >= 1){
		for(i in unique(att$School_Year)){
			tmp <- subset(att, School_Year == i)
			.add <- indent(.lv) %+% '{\n'
			
			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"key": {\n', sep="")
			up(.lv)

			.add <- .add %+% paste(indent(.lv), '"year": "',substr(i,1,4),'"\n', sep="")		
							
			down(.lv)	
			.add <- .add %+% paste(indent(.lv), '},\n', sep="")		

			.add <- .add %+% paste(indent(.lv), '"val": {\n', sep="")
			up(.lv)

			.add <- .add %+% paste(indent(.lv), '"in_seat_attendance":',make_null(
				(tmp$SchoolScore[tmp$Metric=="In-Seat Attendance Rate"])/100
				),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"state_in_seat_attendance":',make_null(
				(tmp$AverageScore[tmp$Metric=="In-Seat Attendance Rate"])/100
				),',\n', sep="")

			.add <- .add %+% paste(indent(.lv), '"average_daily_attendance":null,\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"state_average_daily_attendance":null\n', sep="")

			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}\n', sep="")
			down(.lv)
			
			.add <- .add %+% paste(indent(.lv), '}', sep="")

			.ret[length(.ret)+1] <- .add
		}
	}
	return(paste(.ret, collapse=',\n'))
}

WriteAbsences <- function(org_code, level){
	.ret <- c()	
	.lv <- level

	abs_qry <- paste0("SELECT * FROM [dbo].[equity_report_prelim]
				WHERE School_Code = '",org_code,"' AND [Metric] in ('Unexcused Absences 1-5', 'Unexcused Absences 6-10','Unexcused Absences 11-15','Unexcused Absences 16-25','Unexcused Absences > 25') AND [ReportType] = 'External'")
	abs <- sqlQuery(dbrepcard, abs_qry)
	if(nrow(abs) >= 1){
		for(i in unique(abs$School_Year)){
			tmp <- subset(abs, School_Year == i)
			.add <- indent(.lv) %+% '{\n'
			
			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"key": {\n', sep="")
			up(.lv)

			.add <- .add %+% paste(indent(.lv), '"year": "',substr(i,1,4),'"\n', sep="")		
							
			down(.lv)
							
			.add <- .add %+% paste(indent(.lv), '},\n', sep="")
								
			.add <- .add %+% paste(indent(.lv), '"val": {\n', sep="")
			up(.lv)
							
			.add <- .add %+% paste(indent(.lv), '"1-5_days":',make_null(tmp$SchoolScore[tmp$Metric=="Unexcused Absences 1-5"]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"6-10_days":',make_null(tmp$SchoolScore[tmp$Metric=="Unexcused Absences 6-10"]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"11-15_days":',make_null(tmp$SchoolScore[tmp$Metric=="Unexcused Absences 11-15"]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"16-25_days":',make_null(tmp$SchoolScore[tmp$Metric=="Unexcused Absences 16-25"]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"more_than_25_days":',make_null(tmp$SchoolScore[tmp$Metric=="Unexcused Absences > 25"]),',\n', sep="")

			.add <- .add %+% paste(indent(.lv), '"state_1-5_days":',make_null(tmp$AverageScore[tmp$Metric=="Unexcused Absences 1-5"]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"state_6-10_days":',make_null(tmp$AverageScore[tmp$Metric=="Unexcused Absences 6-10"]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"state_11-15_days":',make_null(tmp$AverageScore[tmp$Metric=="Unexcused Absences 11-15"]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"state_16-25_days":',make_null(tmp$AverageScore[tmp$Metric=="Unexcused Absences 16-25"]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"state_more_than_25_days":',make_null(tmp$AverageScore[tmp$Metric=="Unexcused Absences > 25"]),'\n', sep="")

			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}\n', sep="")
			down(.lv)
			
			.add <- .add %+% paste(indent(.lv), '}', sep="")

			.ret[length(.ret)+1] <- .add
		}
	}
	return(paste(.ret, collapse=',\n'))
}

RetSuspensionGroup <- function(x){
	if(x == 'All Students'){
		return('All')
	} else if(x == 'Black non-Hispanic'){
		return('African American')
	} else if (x == 'Hispanic / Latino'){
		return('Hispanic')
	} else if (x == 'Asian'){
		return('Asian')
	} else if (x == 'White non-Hispanic'){
		return('White')
	} else if (x == 'Multiracial'){
		return('Multi Racial')
	} else if (x == 'Limited English Proficiency'){
		return('English Learner')
	} else if (x == 'Special Education'){
		return('Special Education')
	} else if(x == 'Free or Reduced Lunch'){
		return('Economically Disadvantaged')
	} else{
		return(x)
	}
} 

WriteSuspensions <- function(org_code, level){
	.ret <- c()	
	.lv <- level

	sus_qry <- paste0("SELECT * FROM [dbo].[equity_report_prelim]
				WHERE School_Code = '",org_code,"' AND [Metric] in ('Suspended 1+','Suspended 11+') AND [ReportType] = 'External'")
	sus <- sqlQuery(dbrepcard, sus_qry)

	if(nrow(sus) >= 1){
		for(i in unique(sus$School_Year)){
			tmp <- subset(sus, School_Year == i)
			for(f in unique(tmp$Student_Group)){	
				.add <- indent(.lv) %+% '{\n'
				
				up(.lv)
				.add <- .add %+% paste(indent(.lv), '"key": {\n', sep="")
				up(.lv)

				.add <- .add %+% paste(indent(.lv), '"year": "',substr(i,1,4),'",\n', sep="")		
				.add <- .add %+% paste(indent(.lv), '"subgroup": "',RetSuspensionGroup(f),'"\n', sep="")				
				down(.lv)
								
				.add <- .add %+% paste(indent(.lv), '},\n', sep="")
									
				.add <- .add %+% paste(indent(.lv), '"val": {\n', sep="")
				up(.lv)

				.add <- .add %+% paste(indent(.lv), '"suspended_1":',make_null((tmp$SchoolScore[tmp$Metric=="Suspended 1+" & tmp$Student_Group ==f])/100),',\n', sep="")
				.add <- .add %+% paste(indent(.lv), '"suspended_11":',make_null((tmp$SchoolScore[tmp$Metric=="Suspended 11+" & tmp$Student_Group ==f])/100),',\n', sep="")
				.add <- .add %+% paste(indent(.lv), '"state_suspended_1":',make_null((tmp$AverageScore[tmp$Metric=="Suspended 1+" & tmp$Student_Group ==f])/100),',\n', sep="")
				.add <- .add %+% paste(indent(.lv), '"state_suspended_11":',make_null((tmp$AverageScore[tmp$Metric=="Suspended 11+" & tmp$Student_Group ==f])/100),'\n', sep="")

				down(.lv)
				.add <- .add %+% paste(indent(.lv), '}\n', sep="")
				down(.lv)
				
				.add <- .add %+% paste(indent(.lv), '}', sep="")

				.ret[length(.ret)+1] <- .add
			}
		}	
	}		
	return(paste(.ret, collapse=',\n'))
}

WriteExpulsions <- function(org_code, level){
	.ret <- c()	
	.lv <- level

	exp_qry <- paste0("SELECT * FROM [dbo].[equity_report_prelim]
				WHERE School_Code = '",org_code,"' AND [Metric] = 'Expulsions' AND [ReportType] = 'External'")
	exp <- sqlQuery(dbrepcard, exp_qry)

	if(nrow(exp) >= 1){
		for(i in unique(exp$School_Year)){
			tmp <- subset(exp, School_Year == i)

			.add <- indent(.lv) %+% '{\n'
			
			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"key": {\n', sep="")
			up(.lv)

			.add <- .add %+% paste(indent(.lv), '"year": "',substr(i,1,4),'"\n', sep="")		
							
			down(.lv)
							
			.add <- .add %+% paste(indent(.lv), '},\n', sep="")
								
			.add <- .add %+% paste(indent(.lv), '"val": {\n', sep="")
			up(.lv)
							
			.add <- .add %+% paste(indent(.lv), '"expulsions":',make_null(tmp$SchoolScore),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"state_expulsions":',make_null(tmp$AverageScore),'\n', sep="")

			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}\n', sep="")
			down(.lv)
			
			.add <- .add %+% paste(indent(.lv), '}', sep="")

			.ret[length(.ret)+1] <- .add
		}
	}
	return(paste(.ret, collapse=',\n'))
}

RetMonthInt <- function(x){
	if(x == 'January'){
		return(1)
	} else if(x == 'February'){
		return(2)
	} else if (x == 'March'){
		return(3)
	} else if (x == 'April'){
		return(4)
	} else if (x == 'May'){
		return(5)
	} else if (x == 'June'){
		return(6)
	} else if (x == 'July'){
		return(7)
	} else if (x == 'August'){
		return(8)
	} else if(x == 'September'){
		return(9)
	} else if(x == 'October'){
		return(10)
	} else if(x == 'November'){
		return(11)
	} else if(x == 'December'){
		return(12)
	} else{
		return(x)
	}
}

WriteEnterWithdraw <- function(org_code, level){
	.ret <- c()
	.lv <- level

	ent_qry <- paste0("SELECT * FROM [dbo].[equity_report_prelim]
				WHERE School_Code = '",org_code,"' AND [Metric] in ('Entry','Net Cumulative','Withdrawal') AND [ReportType] = 'External'")
	ent <- sqlQuery(dbrepcard, ent_qry)

	if(nrow(ent) >= 1){
		for(i in unique(ent$School_Year)){
			for(f in unique(ent$Month)){	
				tmp <- subset(ent, School_Year == i & Month == f)

				.add <- indent(.lv) %+% '{\n'
				
				up(.lv)
				.add <- .add %+% paste(indent(.lv), '"key": {\n', sep="")
				up(.lv)

				.add <- .add %+% paste(indent(.lv), '"year": "',substr(i,1,4),'",\n', sep="")		
				.add <- .add %+% paste(indent(.lv), '"month": ',RetMonthInt(f),'\n', sep="")				
				down(.lv)

				.add <- .add %+% paste(indent(.lv), '},\n', sep="")
									
				.add <- .add %+% paste(indent(.lv), '"val": {\n', sep="")
				up(.lv)

				.add <- .add %+% paste(indent(.lv), '"entry":',make_null(tmp$SchoolScore[tmp$Metric=="Entry"]),',\n', sep="")
				.add <- .add %+% paste(indent(.lv), '"withdrawal":',make_null(tmp$SchoolScore[tmp$Metric=="Withdrawal"]),',\n', sep="")
				.add <- .add %+% paste(indent(.lv), '"net_cumulative":',make_null(tmp$SchoolScore[tmp$Metric=="Net Cumulative"]),'\n', sep="")

				down(.lv)
				.add <- .add %+% paste(indent(.lv), '}\n', sep="")
				down(.lv)
				
				.add <- .add %+% paste(indent(.lv), '}', sep="")

				.ret[length(.ret)+1] <- .add
			}
		}
	}
	return(paste(.ret, collapse=',\n'))
}