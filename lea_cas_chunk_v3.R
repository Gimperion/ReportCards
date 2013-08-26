



LeaCasChunk <- function(lea_code, level){
	.lv <- level
	
	## MATH/READING

	.qry13 <- "SELECT * FROM [dbo].[assessment_sy1213] WHERE [fy13_entity_code] in (
			SELECT DISTINCT [fy13_entity_code] FROM [dbo].[assessment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "');"
		.dat13_mr <- sqlQuery(dbrepcard, .qry13)

	.qry12 <- "SELECT * FROM [dbo].[assessment_sy1112] WHERE [fy13_entity_code] in (
			SELECT DISTINCT [fy13_entity_code] FROM [dbo].[assessment_sy1112] WHERE [lea_code] = '" %+% lea_code %+% "');"
		.dat12_mr <- sqlQuery(dbrepcard, .qry12)

	.qry11 <- "SELECT * FROM [dbo].[assessment_sy1011] WHERE [fy13_entity_code] in (
			SELECT DISTINCT [fy13_entity_code] FROM [dbo].[assessment_sy1011] WHERE [lea_code] = '" %+% lea_code %+% "');"
		.dat11_mr <- sqlQuery(dbrepcard, .qry11)

	.qry10 <- "SELECT * FROM [dbo].[assessment_sy0910] WHERE [fy13_entity_code] in (
			SELECT DISTINCT [fy13_entity_code] FROM [dbo].[assessment_sy0910] WHERE [lea_code] = '" %+% lea_code %+% "');"
		.dat10_mr <- sqlQuery(dbrepcard, .qry10)

	.qry09 <- "SELECT * FROM [dbo].[assessment_sy0809] WHERE [fy13_entity_code] in (
			SELECT DISTINCT [fy13_entity_code] FROM [dbo].[assessment_sy0809] WHERE [lea_code] = '" %+% lea_code %+% "');"
		.dat09_mr <- sqlQuery(dbrepcard, .qry09)


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
		WHERE [fy13_entity_code] in (
			SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "');"
	.dat13_c <- sqlQuery(dbrepcard, .qry13c)
	if(nrow(.dat13_c)>=10){
		.ret[length(.ret)+1] <- WriteComp(.dat13_c, 2013, .lv)
	}
	
	.qry12c <- "SELECT * FROM [dbo].[assessment_sy1112_comp]
		WHERE [fy13_entity_code] in (
			SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "');"
	.dat12_c <- sqlQuery(dbrepcard, .qry12c)
	if(nrow(.dat12_c)>=10){
		.ret[length(.ret)+1] <- WriteComp(.dat12_c, 2012, .lv)
	}
	
	.qry11c <- "SELECT * FROM [dbo].[assessment_sy1011_comp]
		WHERE [fy13_entity_code] in (
			SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "');"
	.dat11_c <- sqlQuery(dbrepcard, .qry11c)
	if(nrow(.dat11_c)>=10){
		.ret[length(.ret)+1] <- WriteComp(.dat11_c, 2011, .lv)
	}
	
	## 
	.qry13s <- "SELECT * FROM [dbo].[assessment_sy1213_science]
		WHERE [fy13_entity_code] in (
			SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')
			AND science_empty = 0;"
	.dat13_s <- sqlQuery(dbrepcard, .qry13s)
	if(nrow(.dat13_s)>=10){
		.ret[length(.ret)+1] <- WriteScience(.dat13_s, 2013, .lv)
	}
	
	.qry12s <- "SELECT * FROM [dbo].[assessment_sy1112_science]
		WHERE [fy13_entity_code] in (
			SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')
			AND science_empty = 0;"
	.dat12_s <- sqlQuery(dbrepcard, .qry12s)
	if(nrow(.dat12_s)>=10){
		.ret[length(.ret)+1] <- WriteScience(.dat12_s, 2012, .lv)
	}
	
	.qry11s <- "SELECT * FROM [dbo].[assessment_sy1011_science]
	WHERE [fy13_entity_code] in (
			SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')
			AND science_empty = 0;"
	.dat11_s <- sqlQuery(dbrepcard, .qry11s)
	if(nrow(.dat11_s)>=10){
		.ret[length(.ret)+1] <- WriteScience(.dat11_s, 2011, .lv)
	}
	
	return(paste(.ret, collapse=',\n'))
}


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
	} else if(lv==6){
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
	} else if(lv==7){
		return(subset(.dat, economy=="YES"))
	} else if(lv==8){
		return(subset(.dat, gender %in% c("M", "MALE")))
	} else if(lv==9){
		return(subset(.dat, gender %in% c("F", "FEMALE")))
	}
	return(0)
}


WriteCAS <- function(.casdat_mr, year, level){
	.subjects <- c("Math", "Reading")
	.fay <- c("all", "full_year")
	
	.lv <- level

	.ret <- c()
	.plevels <- c("Below Basic", "Basic", "Proficient", "Advanced", "1","2","3","4")
	
	## A = Subject, 1 for Math, 2 for Reading
	for(a in 1:2){
		## b = full year or not
		for(b in 1:2){
			## d = each grade 
			.glevels <- sort(unique(.casdat_mr$tested_grade))
			for(g in 0:length(.glevels)){
				goutput <- ''
				.tmp <- .casdat_mr
				##print(.tmp)
				if(g == 0){
					goutput <- 'all'
				} else{
					goutput <- paste('grade', .glevels[g], sep=" ")
					.tmp <- subset(.tmp, tested_grade==.glevels[g])
				}
				.flevels <- c("N", "S", "D", "C")
				
				if(b==2){
					.flevels <- c("S", "C")
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

						##print(.profs)
						
						.add <- .add %+% paste(indent(.lv), '"n_eligible":',length(.profs),',\n', sep="")
						.add <- .add %+% paste(indent(.lv), '"n_test_takers":',length(.profs[.profs %in% .plevels]),',\n', sep="")
						.add <- .add %+% paste(indent(.lv), '"advanced_or_proficient":', length(.profs[.profs %in% c("Proficient", "Advanced", "3","4")]),',\n', sep="")
						
						.add <- .add %+% paste(indent(.lv), '"advanced":',length(.profs[.profs %in% c("Advanced", "4")]),',\n', sep="")
						.add <- .add %+% paste(indent(.lv), '"proficient":',length(.profs[.profs %in% c("Proficient", "3")]),',\n', sep="")
						.add <- .add %+% paste(indent(.lv), '"basic":',length(.profs[.profs %in% c("Basic", "2")]),',\n', sep="")
						.add <- .add %+% paste(indent(.lv), '"below_basic":',length(.profs[.profs %in% c("Below Basic", "1")]),'\n', sep="")
						
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


WriteScience <- function(.casdat_sci, year, level){
## Science
	.fay <- c("all")
	.lv <- level
	
	.ret <- c()
	.plevels <- c("Below Basic", "Basic", "Proficient", "Advanced", "1","2","3","4")
	
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
			.add <- .add %+% paste(indent(.lv), '"advanced_or_proficient":', length(.profs[.profs %in% c("Proficient", "Advanced", "3", "4")]),',\n', sep="")
			
			.add <- .add %+% paste(indent(.lv), '"advanced":',length(.profs[.profs %in% c("Advanced", "4")]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"proficient":',length(.profs[.profs %in% c("Proficient", "3")]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"basic":',length(.profs[.profs %in% c("Basic","2")]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"below_basic":',length(.profs[.profs %in% c("Below Basic", "1")]),'\n', sep="")
			
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}\n', sep="")
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}', sep="")
			
			.ret[length(.ret)+1] <- .add
		}
	}
	return(paste(.ret, collapse=',\n'))
}




WriteComp <- function(.casdat_comp, year, level){
## Composition 
	.fay <- c("all")
	.lv <- level
	
	.ret <- c()
	.plevels <- c("Below Basic", "Basic", "Proficient", "Advanced", "1","2","3","4")
	
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
			.add <- .add %+% paste(indent(.lv), '"advanced_or_proficient":', length(.profs[.profs %in% c("Proficient", "Advanced", "3", "4")]),',\n', sep="")
			
			.add <- .add %+% paste(indent(.lv), '"advanced":',length(.profs[.profs %in% c("Advanced", "4")]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"proficient":',length(.profs[.profs %in% c("Proficient", "3")]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"basic":',length(.profs[.profs %in% c("Basic", "2")]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"below_basic":',length(.profs[.profs %in% c("Below Basic", "1")]),'\n', sep="")
			
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}\n', sep="")
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}', sep="")
			
			.ret[length(.ret)+1] <- .add
		}
	}
	return(paste(.ret, collapse=',\n'))
}




LeaGraduation <- function(lea_code, level){
	.lv <- level
	
	.qry11 <- "SELECT * FROM [dbo].[graduation_sy1011]
		WHERE [fy13_entity_code] in (
			SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')
		AND [cohort_status] = 'TRUE'"
	.grad11 <- sqlQuery(dbrepcard, .qry11)
	
	.qry12 <- "SELECT * FROM [dbo].[graduation_sy1112]
		WHERE [fy13_entity_code] in (
			SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')
		AND [cohort_status] = 'TRUE'"
	.grad12 <- sqlQuery(dbrepcard, .qry12)
	
	.ret <- c()

	if(nrow(.grad11) > 10){
		.ret <- c(.ret, WriteGraduation(.grad11, .lv, 2011))
	}
	
	if(nrow(.grad12) > 10){
		.ret <- c(.ret, WriteGraduation(.grad12, .lv, 2012))
	}
	
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



WriteGraduation <- function(gdata, level, year){
	.lv <- level
	.ret <- c()
	
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
			
			.profs <- .tmps$comp_level
			.add <- .add %+% paste(indent(.lv), '"subgroup": "', soutput,'", \n', sep="")
			.add <- .add %+% paste(indent(.lv), '"year": "',year,'" \n', sep="")
			down(.lv)				
			.add <- .add %+% paste(indent(.lv), '},\n', sep="")
				
			.add <- .add %+% paste(indent(.lv), '"val": {\n', sep="")
			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"graduates": ', nrow(subset(.tmps, grad_y4==TRUE)),',\n', sep="")
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


LeaCollegeReadiness <- function(lea_code, level){
    .lv <- level
    
    .qry <- "SELECT * FROM [dbo].[college_readiness]
                    WHERE [fy13_entity_code] in (
SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')"
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
            .profs <- .tmp$comp_level
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

LeaEnrollChunk <- function(lea_code, level){
	.lv <- level
	
	## MATH/READING
	.qry13 <- "SELECT * FROM [dbo].[enrollment_sy1213]
		WHERE [fy13_entity_code] in (
SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')"
	.dat13 <- sqlQuery(dbrepcard, .qry13)
	
	.qry12 <- "SELECT * FROM [dbo].[enrollment_sy1112]
		WHERE [fy13_entity_code] in (
SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')"
	.dat12 <- sqlQuery(dbrepcard, .qry12)
	
	.qry11 <- "SELECT * FROM [dbo].[enrollment_sy1011]
		WHERE [fy13_entity_code] in (
SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')"
	.dat11 <- sqlQuery(dbrepcard, .qry11)
	
	.qry10 <- "SELECT * FROM [dbo].[enrollment_sy0910]
		WHERE [fy13_entity_code] in (
SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')"
	.dat10 <- sqlQuery(dbrepcard, .qry10)
	
	.qry09 <- "SELECT * FROM [dbo].[enrollment_sy0809]
		WHERE [fy13_entity_code] in (
SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')"
	.dat09 <- sqlQuery(dbrepcard, .qry09)
	
	.qry08 <- "SELECT * FROM [dbo].[enrollment_sy0708]
		WHERE [fy13_entity_code] in (
SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')"
	.dat08 <- sqlQuery(dbrepcard, .qry08)
	
	.qry07 <- "SELECT * FROM [dbo].[enrollment_sy0607]
		WHERE [fy13_entity_code] in (
SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')"
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
		return(subset(.dat, economy %in% c("FREE", "REDUCED", "DCERT", "CEO")))
	} else if(lv==11){
		return(subset(.dat, gender %in% c("M", "MALE")))
	} else if(lv==12){
		return(subset(.dat, gender %in% c("F", "FEMALE")))
	}	
	return(.dat[NULL,])
}

WriteEnroll <- function(.edat, year, level){
	.ret <- c()
	.lv <- level
	
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
				
				.profs <- .tmp$comp_level
						
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

LeaSPEDChunk <- function(scode, level){
	.lv <- level
	
	## MATH/READING
	.qry13 <- "SELECT * FROM [dbo].[assessment_sy1213]
		WHERE [fy13_entity_code] in (
SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')
		AND [special_ed] = 'YES';"
	.dat13_mr <- sqlQuery(dbrepcard, .qry13)
	
	.qry12 <- "SELECT * FROM [dbo].[assessment_sy1112]
		WHERE [fy13_entity_code] in (
SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')
		AND [special_ed] = 'YES';"
	.dat12_mr <- sqlQuery(dbrepcard, .qry12)
	
	.qry11 <- "SELECT * FROM [dbo].[assessment_sy1011]
		WHERE [fy13_entity_code] in (
SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')
		AND [special_ed] = 'YES';"
	.dat11_mr <- sqlQuery(dbrepcard, .qry11)
	
	.qry10 <- "SELECT * FROM [dbo].[assessment_sy0910]
		WHERE [fy13_entity_code] in (
SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')
		AND [special_ed] = 'YES';"
	.dat10_mr <- sqlQuery(dbrepcard, .qry10)
	
	.qry09 <- "SELECT * FROM [dbo].[assessment_sy0809]
		WHERE [fy13_entity_code] in (
SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')
		AND [special_ed] = 'YES';"
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

WriteSPED <- function(.casdat_mr, year, level){
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
				soutput <- "With Accomodations"
			} else if(b == 3){
				.tmp <- subset(.casdat_mr, sped_acc != 'YES')
				soutput <- "No Accomodations"
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
	return(paste(.ret, collapse=',\n'))	
}


LeaCollegeEnroll <- function(lea_code, level){
	.lv <- level
	
	.qry <- "SELECT * FROM [dbo].[college_enroll_2010]
		WHERE [fy13_entity_code] in (
SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')
		AND [Graduates] >= 10;"
	.cenr10 <- sqlQuery(dbrepcard, .qry)
	.ret <- c()

	if(nrow(.cenr10)> 0){
		.ret <- c(.ret, WriteCEnroll(.cenr10, .lv, 2010))
	}

	.qry <- "SELECT * FROM [dbo].[college_enroll_2009]
		WHERE [fy13_entity_code] in (
SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')
		AND [Graduates] >= 10;"
	.cenr09 <- sqlQuery(dbrepcard, .qry)

	if(nrow(.cenr09)> 0){
		.ret <- c(.ret, WriteCEnroll(.cenr09, .lv, 2009))
	}		
	
	.qry <- "SELECT * FROM [dbo].[college_enroll_2008]
		WHERE [fy13_entity_code] in (
SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')
		AND [Graduates] >= 10;"
	.cenr08 <- sqlQuery(dbrepcard, .qry)

	if(nrow(.cenr08)> 0){
		.ret <- c(.ret, WriteCEnroll(.cenr08, .lv, 2008))
	}		
		
	.qry <- "SELECT * FROM [dbo].[college_enroll_2007]
		WHERE [fy13_entity_code] in (
SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')
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
		.add <- .add %+% paste(indent(.lv), '"key": {\n', sep="")
		up(.lv)

		.add <- .add %+% paste(indent(.lv), '"cohort_year": "',year,'",\n', sep="")
		.add <- .add %+% paste(indent(.lv), '"subgroup": "', SubCEnroll(.cenr$Group[i]),'"\n', sep="")
		
		down(.lv)
		.add <- .add %+% paste(indent(.lv), '},\n', sep="")
			
		.add <- .add %+% paste(indent(.lv), '"val": {\n', sep="")
		up(.lv)
		.add <- .add %+% paste(indent(.lv), '"hs_graduates": ',.cenr$Graduates[i],',\n', sep="")
		.add <- .add %+% paste(indent(.lv), '"enroll_within_16mo": ',.cenr$Initial_Enroll_16mo[i],',\n', sep="")
		.add <- .add %+% paste(indent(.lv), '"enroll_within_16mo_instate": ',.cenr$Initial_Enroll_InState_16mo[i],',\n', sep="")
		.add <- .add %+% paste(indent(.lv), '"complete_1yr_instate": ',.cenr$Complete_1Yr_in_State[i],' \n', sep="")

		down(.lv)
		.add <- .add %+% paste(indent(.lv), '}\n', sep="")

		down(.lv)
		.add <- .add %+% paste(indent(.lv), '}', sep="")
		.ret <- c(.ret, .add)
	}
	return(.ret)
}

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

RetMGPGroup <- function(.ingrp){
	if(.ingrp == 'All Students'){
		return('All')
	} else if(.ingrp == 'WH7'){
		return('White')
	} else if(.ingrp == 'Not-SpEd'){
		return('Not Special Education')
	} else if(.ingrp == 'SpEd'){
		return('Special Education')
	} else if (.ingrp == 'Not-ELP'){
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
	}
}


LeaMGPResult <- function(lea_code, level){
	.lv <- level
	.qry <- "SELECT * FROM [dbo].[mgp_summary]
		WHERE [fy13_entity_code] in (
SELECT DISTINCT [fy13_entity_code] FROM [dbo].[enrollment_sy1213] WHERE [lea_code] = '" %+% lea_code %+% "')
		AND [group] not like 'Grade%'"
	.mgp <- sqlQuery(dbrepcard, .qry)	
	.ret <- c()	
	if(nrow(.mgp)>0){
		for(i in 1:nrow(.mgp)){
			if(.mgp$group_fay_size[i] >= 10 ){
				.add <- indent(.lv) %+% '{\n'
				up(.lv)
				.add <- .add %+% paste(indent(.lv), '"key": {\n', sep="")
				up(.lv)			
				.profs <- .mgp$comp_level
				.add <- .add %+% paste(indent(.lv), '"subject": "',.mgp$subject[i] ,'", \n', sep="")
				.add <- .add %+% paste(indent(.lv), '"subgroup": "',RetMGPGroup(.mgp$group[i]),'", \n', sep="")
				.add <- .add %+% paste(indent(.lv), '"year": "',.mgp$year[i],'" \n', sep="")
				down(.lv)
				.add <- .add %+% paste(indent(.lv), '},\n', sep="")
			
				.add <- .add %+% paste(indent(.lv), '"val": {\n', sep="")
				up(.lv)
				.add <- .add %+% paste(indent(.lv), '"group_size": ', checkna(.mgp$group_fay_size[i]),',\n', sep="")
				.add <- .add %+% paste(indent(.lv), '"mgp_1yr": ', checkna(.mgp$mgp_1yr[i]),',\n', sep="")
				.add <- .add %+% paste(indent(.lv), '"mgp_2yr": ', checkna(.mgp$mgp_2yr[i]),'\n', sep="")

				down(.lv)
				.add <- .add %+% paste(indent(.lv), '}\n', sep="")
				down(.lv)
				.add <- .add %+% paste(indent(.lv), '}', sep="")
				.ret <- c(.ret, .add)
			}	
		}
	}
	return(paste(.ret, collapse=',\n'))
}


checkna <- function(x){
	if(is.na(x)){
		return('null')
	}
	return(x)
}

checkna_str <- function(x){
	if(is.na(x)){
		return('null')
	}
	return('"' %+% x %+%'"')
}