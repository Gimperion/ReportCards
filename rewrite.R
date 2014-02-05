## rewrite
WriteCAS <- function(.casdat_mr, level){
	year <- .casdat_mr$year[1]
	
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


WriteComp <- function(.casdat_comp, level){
## Composition 
	year <- .casdat_comp$year[1]
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

