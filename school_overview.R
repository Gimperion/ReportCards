active <- shell('echo %HOMEPATH%', intern=TRUE)
active <- gsub('\\\\', '/', active)
mainDir <- paste0('C:', active, '/Documents/Github/ReportCards')

setwd(mainDir)

source("./imports/tomkit.R")
source("http://www.straydots.com/code/ODBC.R")
source("./school_functions.R")
##source("./state_functions.R")


overview_version <- sqlQuery(dbrepcard, "SELECT TOP 1 
			[version_number],
			[timestamp]
		FROM [dbo].[ver_control_soverview]
		ORDER BY [version_number] DESC")
		
next_version <- overview_version$version_number + 0.1


subDir <- "school_overview"
mainDir <- "./data/"
dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
setwd(file.path(mainDir, subDir))

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
school_dir$profile_name <- gsub('\n', "", school_dir$profile_name)
school_dir$profile_name <- gsub('\r', "", school_dir$profile_name)

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
	.qry <- sprintf("SELECT DISTINCT [grade] FROM [dbo].[current_roster_grades]
		WHERE [school_code] = '%s';",org_code)
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

WriteProfile <- function(org_code){
	.qry_profile <- sprintf("SELECT * FROM [dbo].[profile_urls]
		WHERE [school_code] = '%s'",org_code )
	.prog_profile <- sqlQuery(dbrepcard, .qry_profile)
	
	if(nrow(.prog_profile) == 0){
		return(NA)
	} 
	
	x <- .prog_profile$url[1]
	
	return(x)
}

GSUrl <- function(org_code){
	.qry_gsurl <- "SELECT * FROM [dbo].[gs_url]
		WHERE [school_code] = '" %+% org_code %+% "'"
	.gs_dat <- sqlQuery(dbrepcard, .qry_gsurl)
	
	if(nrow(.gs_dat)>0){
		return(trimall(.gs_dat$gs_url[1]))
	} else{
		return(NA)
	}
}


EquityUrl <- function(org_code){
	.qry_equity <- "SELECT * FROM [dbo].[equity_report_url_mapping]
		WHERE [school_code] = '" %+% org_code %+% "' AND [url_check] = 1"
	.equity_dat <- sqlQuery(dbrepcard, .qry_equity)
	if(nrow(.equity_dat )>0){
		return(trimall(.equity_dat$equity_url[1]))
	} else{
		return(NA)
	}
}

JSONTrueFalse <- function(x){
	
	x <- toupper(x)

	if(is.na(x) | is.null(x)){
		return('null')
	} else if(x %in% c('YES', 'TRUE', '1')){
		return('true')
	} else if(x %in% c('NO', 'FALSE', '0')){
		return('false')
	} else{
		return('null')
	}
}

## Yay for-loops!!!!!
for(i in 1:nrow(school_dir)){
	org_type <- "school"
	org_code <- school_dir$school_code[i]

	newfile <- file(paste(org_type, '_', org_code, '_overview.JSON', sep=""), encoding="UTF-8")

	if(!is.na(school_dir$profile_name[i])){
		prof_name <- school_dir$profile_name[i]
	} else{
		prof_name <- school_dir$school_name[i]
	}
	print(prof_name)
	
	sink(file=newfile)
	cat('{', fill=TRUE)
	
	level <- 1
	cat(indent(level),'"timestamp": "',date(),'",', sep="", fill=TRUE)
	cat(indent(level),'"org_type": "school",', sep="", fill=TRUE)
	cat(indent(level),'"org_name": "',prof_name,'",', sep="", fill=TRUE)
	cat(indent(level),'"org_code": "',org_code,'",', sep="", fill=TRUE)
	
	cat(indent(level),'"closed": ',JSONTrueFalse(school_dir$closing[i]), ',', sep="", fill=TRUE)
	
	cstat <- JSONTrueFalse(school_dir$charter_status[i])
	
	cat(sprintf('%s"charter": %s,',indent(level),cstat), fill=TRUE)
	cat(sprintf('%s"school_type": [',indent(level)), fill=TRUE)
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

	{
		#Great Schools PMF
		cat(indent(level),'"great_schools": {', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"gs_url": ', checkna_str(GSUrl(org_code)), sep="", fill=TRUE)
		down(level)
		cat(indent(level),'},', sep="", fill=TRUE)
	}
	
	cat(indent(level),sprintf('"ward": %s,',checkna_str(substr(school_dir$ward[i], 6,6))), sep="", fill=TRUE) 	
	cat(indent(level),sprintf('"grades_serviced": [%s],', GetGrades(org_code)), sep="", fill=TRUE) 
	cat(indent(level) %+% '"description": "' %+% school_dir$description[i] %+% '",', fill=TRUE)
	
	cat(indent(level), '"address": {', fill=TRUE)
	level <- level + 1
	
	##checkna(school_dir$address_1[i])
	cat(indent(level),'"line_1": ',checkna_str(school_dir$address_1[i]),',', sep="", fill=TRUE)  
	cat(indent(level),'"line_2": ',checkna_str(school_dir$address_2[i]),',', sep="", fill=TRUE) 
	cat(indent(level),'"city": "Washington",', sep="", fill=TRUE) 
	cat(indent(level),'"state": "DC",', sep="", fill=TRUE) 
	cat(indent(level),'"zip": ',checkna_str(school_dir$zipcode[i]),',', sep="", fill=TRUE)  
	cat(indent(level),'"lat": ',checkna(school_dir$latitude[i]),',', sep="", fill=TRUE) 
	cat(indent(level),'"long": ',checkna(school_dir$longitude[i]), sep="", fill=TRUE)  
	
	level <- level - 1
	cat(indent(level), '},', fill=TRUE)

	cat(indent(level) %+% '"transit": "' %+% school_dir$routes[i] %+% '",', fill=TRUE)
	cat(indent(level) %+% '"website": ' %+% checkna_str(school_dir$website[i]) %+% ',', fill=TRUE) 
	cat(indent(level),'"facebook": ',checkna_str(school_dir$facebook[i]),',', sep="", fill=TRUE)
	cat(indent(level),'"twitter": ',checkna_str(school_dir$twitter[i]),',', sep="", fill=TRUE)  
	cat(indent(level),'"external_report_card": ',checkna_str(WriteProfile(org_code)),',', sep="", fill=TRUE)  
	cat(indent(level),'"equity_report_url": \n',checkna_str(EquityUrl(org_code)),',\n', sep="", fill=FALSE) 

	cat(indent(level), '"contact": [', sep="", fill=TRUE)
	cat(InsertPeople(org_code, level+1))
	cat('\n', indent(level), ']', sep='', fill=TRUE)

	cat('}', fill=TRUE)
	sink()
	close(newfile)
}

## SUCCESSFUL PUSH!  
update_vcontrol <- overview_version
update_vcontrol$version_number <- next_version
update_vcontrol$timestamp <- Sys.time()

sqlSave(dbrepcard, update_vcontrol, tablename="ver_control_soverview", append=TRUE, safer=TRUE, rownames=FALSE, varType=c(timestamp="datetime"))

