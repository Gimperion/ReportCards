setwd("C:\\Users\\tommy.shen\\Documents\\GitHub\\ReportCards")

source("./imports/tomkit.R")
source("./imports/ODBC.R")
source("./school_functions.R")
##source("./state_functions.R")

setwd("C:\\test_repcard\\")
## Insert Version Controlled Folder Location Here
## setwd("")


school_dir <- sqlFetch(dbrepcard, 'schooldir_linked')

school_dir$twitter <- NA ## Placeholder
school_dir$external <- NA ## Placeholder

school_dir$school_code <- sapply(school_dir$school_code, leadgr, 4)


## Unreadable Special Characters and Paragraph Spacings
school_dir$description <- gsub('"', "'", school_dir$description)
school_dir$description <- gsub('â€œ', "'", school_dir$description)
school_dir$description <- gsub('â€', "'", school_dir$description)
school_dir$description <- gsub('\n', "", school_dir$description)
school_dir$description <- gsub('\r', "", school_dir$description)
school_dir$website <- gsub('\n', "", school_dir$website)
school_dir$website <- gsub('\r', "", school_dir$website)

InsertPeople <- function(org_code, level){
	.lv <- level
	
	## MATH/READING
	.qry <- "SELECT * FROM [dbo].[people_sy1213]
		WHERE [school_code] = '" %+% org_code %+% "';"
	.dat_peep <- sqlQuery(dbrepcard, .qry)
	##print(.dat_peep)
	
	.ret <- c()
	if(!is.null(.dat_peep) & nrow(.dat_peep)>0){
		for(i in 1:nrow(.dat_peep)){
			.add <- indent(.lv) %+% '{\n'
			
			up(.lv)
			.add <- .add %+% paste(indent(.lv), '"name": ', checkna_str(.dat_peep$contact_name[i]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"title": ', checkna_str(.dat_peep$role[i]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"phone": ', checkna_str(.dat_peep$contact_phone[i]),',\n', sep="")
			.add <- .add %+% paste(indent(.lv), '"email": ', checkna_str(.dat_peep$contact_email[i]), '\n', sep="")
			
			down(.lv)
			.add <- .add %+% paste(indent(.lv), '}', sep="")
				
			.ret <- c(.ret, .add)		
		}
	}
	return(paste(.ret, collapse=',\n'))
}

GetGrades <- function(org_code){
	## MATH/READING
	.qry <- "SELECT DISTINCT [grade] FROM [dbo].[enrollment_sy1213]
		WHERE [school_code] = '" %+% org_code %+% "';"
	.sgrades <- sqlQuery(dbrepcard, .qry)
	##print(.dat_peep)
	##print(.sgrades)
	
	.ret <- c()
	if(nrow(.sgrades)>0){
		for(i in 1:nrow(.sgrades)){
			.ret <- c(.ret, '"' %+%.sgrades$grade[i] %+% '"')
		}
	}
	return(paste(.ret, collapse=','))
}
	
## Yay for-loops!!!!!
for(i in 1:nrow(school_dir)){
	org_type <- "school"
	org_code <- school_dir$school_code[i]

	newfile <- file(paste(org_type, '_', org_code, '_overview.JSON', sep=""), encoding="UTF-8")
	sink(file=newfile)
	cat('{', fill=TRUE)

	level <- 1
	cat(indent(level),'"org_type": "school",', sep="", fill=TRUE)
	cat(indent(level),'"org_name": "',school_dir$profile_name[i],'",', sep="", fill=TRUE)
	cat(indent(level),'"org_code": "',org_code,'",', sep="", fill=TRUE)
	
	cstat <- ifelse(school_dir$charter_status[i]=='Yes', 'true', 'false')
	
	cat(indent(level),'"charter": ',cstat,',', sep="", fill=TRUE)
	cat(indent(level), '"school_type": [', sep="", fill=TRUE)
	level <- level + 1

	## FILL SCHOOL_TYPE VAR ##
	cat(indent(level), '"',school_dir$school_type[i],'"', sep="", fill=TRUE)
	level <- level - 1
	cat(indent(level), '],', fill=TRUE)
	
	if(school_dir$lea_code[i] != '1'){
		#PCSB PMF
		cat(indent(level),'"pmf": {', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "pcsb_pmf",', sep="", fill=TRUE)
		cat(indent(level), '"data": [', sep="", fill=TRUE)
		cat(ExPMF(org_code, level+1), fill=TRUE)		
		cat(indent(level), ']', sep="", fill=TRUE)
		down(level)
		cat(indent(level),'},', sep="", fill=TRUE)
	}
	
	cat(indent(level),'"ward": "',school_dir$ward[i],'",', sep="", fill=TRUE)  ## fill 
	cat(indent(level),'"grades_serviced": [',GetGrades(org_code),'],', sep="", fill=TRUE) 
	cat(indent(level) %+% '"description": "' %+% school_dir$description[i] %+% '",', fill=TRUE)
	
	cat(indent(level), '"address": {', fill=TRUE)
	level <- level + 1
	
	##checkna(school_dir$address_1[i])
	cat(indent(level),'"line_1": ',checkna_str(school_dir$address_1[i]),',', sep="", fill=TRUE)  ## fill ward
	cat(indent(level),'"line_2": ',checkna_str(school_dir$address_2[i]),',', sep="", fill=TRUE)  ## fill ward
	cat(indent(level),'"city": "Washington",', sep="", fill=TRUE)  ## fill ward
	cat(indent(level),'"state": "DC",', sep="", fill=TRUE)  ## fill ward
	cat(indent(level),'"zip": ',checkna_str(school_dir$zipcode[i]),',', sep="", fill=TRUE)  ## fill ward
	cat(indent(level),'"lat": ',checkna(school_dir$latitude[i]),',', sep="", fill=TRUE)  ## fill ward
	cat(indent(level),'"long": ',checkna(school_dir$longitude[i]), sep="", fill=TRUE)  ## fill ward
	
	level <- level - 1
	cat(indent(level), '},', fill=TRUE)
	
	cat(indent(level) %+% '"transit": "' %+% school_dir$routes[i] %+% '",', fill=TRUE)
	cat(indent(level) %+% '"website": ' %+% checkna_str(school_dir$website[i]) %+% ',', fill=TRUE)  ## fill ward
	cat(indent(level),'"facebook": ',checkna_str(school_dir$facebook[i]),',', sep="", fill=TRUE)  ## fill ward
	cat(indent(level),'"twitter": ',checkna_str(school_dir$twitter[i]),',', sep="", fill=TRUE)  ## fill ward
	cat(indent(level),'"external_report_card": ',checkna_str(school_dir$twitter[i]),',', sep="", fill=TRUE)  ## fill ward
	
	cat(indent(level), '"contact": [', sep="", fill=TRUE)
	cat(InsertPeople(org_code, level+1))
	cat('\n', indent(level), ']', sep='', fill=TRUE)
		
	cat('}', fill=TRUE)
	sink()
	close(newfile)
}
