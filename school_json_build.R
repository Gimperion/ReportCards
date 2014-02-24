## Generate School File ## 
## Detail Data File Build ##
##setwd("U:/REPORT CARD/GIT Report Cards/ReportCards")

active <- shell('echo %CODE_PATH%', intern=TRUE)
active <- gsub('\\\\', '/', active)
setwd(paste0(active, 'ReportCards'))

source("./imports/tomkit.R")
source("./imports/ODBC.R")
source("./school_functions.R")
library(dplyr)

## setwd("U:/REPORT CARD/GIT Report Cards/ReportCards/JSON Output")

sfile_version <- sqlQuery(dbrepcard, "SELECT TOP 1 
			[version_number],
			[timestamp]
		FROM [dbo].[ver_control_schoolfile]
		ORDER BY [version_number] DESC")

next_version <- sfile_version$version_number + 0.1

subDir <- "school_report"
mainDir <- "./data/"

dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
setwd(file.path(mainDir, subDir))

school_dir <- sqlFetch(dbrepcard, 'schooldir_linked')
school_dir$school_code <- sapply(school_dir$school_code, leadgr, 4)

## Start File Generation
for(i in 1:nrow(school_dir)){
	org_type <- "school"
	org_code <- school_dir$school_code[i]
	
	if(!is.na(school_dir$profile_name[i])){
		prof_name <- school_dir$profile_name[i]
	} else{
		prof_name <- school_dir$school_name[i]
	}
	
	print(prof_name)
	
	newfile <- file(paste(org_type, '_', org_code, '.JSON', sep=""), , encoding="UTF-8")
	
	sink(newfile)
	cat('{', fill=TRUE)
	
	level <- 1
	cat(indent(level),'"timestamp": "',date(),'",', sep="", fill=TRUE)
	cat(indent(level),'"org_type": "school",', sep="", fill=TRUE)
	cat(indent(level),'"org_name": "',prof_name,'",', sep="", fill=TRUE)
	
	cat(indent(level),'"report_card": {', sep="", fill=TRUE)
	up(level)
	cat(indent(level),'"sections": [', sep="", fill=TRUE)
	up(level)
	
	######### INSERT DATA HERE ##################
	{	
		### Accountability
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "accountability",', sep="", fill=TRUE)
		cat(indent(level), '"data": [', sep="")
		up(level)
		cat(ExAccountability(org_code, level), fill=TRUE) 
		down(level)
		cat(indent(level),']', sep="", fill=TRUE)
		down(level)
		cat(indent(level),'},', sep="", fill=TRUE)
	}
	{
		## Assessment
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "dccas",', sep="", fill=TRUE)
		cat(indent(level), '"data": [', sep="", fill=TRUE)
		cat(ExCasChunk(org_code, level+1))
		cat('\n',indent(level), ']', sep="", fill=TRUE)
		down(level)
		cat(indent(level),'},', sep="", fill=TRUE)
	}	
	{
		## Highly Qualified Teacher Status
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "hqt_status",', sep="", fill=TRUE)
		
		cat(indent(level), '"data":', ExHQTStatus(org_code), sep="", fill=TRUE)	
		down(level)
		cat(indent(level),'},', sep="", fill=TRUE)
	}
	{
		## Early Childhood
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "early_childhood",', sep="", fill=TRUE)
		
		cat(indent(level), '"data": [', sep="", fill=TRUE)
		cat(ExAccreditation(org_code, level+1), fill=TRUE)
		
		cat(indent(level), ']', sep="", fill=TRUE)
		down(level)
		cat(indent(level),'},', sep="", fill=TRUE)
	}
	{
		#College Enrollment
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "college_enroll",', sep="", fill=TRUE)
		cat(indent(level), '"data": [', sep="", fill=TRUE)
		##cat(ExCollegeEnroll(org_code, level+1), fill=TRUE)		
		cat(indent(level), ']', sep="", fill=TRUE)
		down(level)
		cat(indent(level),'},', sep="", fill=TRUE)
	}
	if(school_dir$lea_code[i] != '1'){
		#PCSB PMF
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "pcsb_pmf",', sep="", fill=TRUE)
		cat(indent(level), '"data": [', sep="", fill=TRUE)
		cat(ExPMF(org_code, level+1), fill=TRUE)		
		cat(indent(level), ']', sep="", fill=TRUE)
		down(level)
		cat(indent(level),'},', sep="", fill=TRUE)
	}			
	{
		## Graduation  -- end of report card section
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "graduation",', sep="", fill=TRUE)
		cat(indent(level), '"data": [', sep="", fill=TRUE)
		cat(ExGraduation(org_code, level+1), fill=TRUE)
		cat('\n',indent(level), ']', sep="", fill=TRUE)
		down(level)
		cat(indent(level),'}', sep="", fill=TRUE)
	}
	
	down(level)
	cat(indent(level),']', sep="", fill=TRUE)
	down(level)
	
	cat(indent(level), '},', fill=TRUE)
	cat('\n', fill=TRUE)
	cat(indent(level),'"profile": {', sep="", fill=TRUE)
	up(level)
	cat(indent(level), '"sections": [', sep="", fill=TRUE)
	up(level)
	{
		## Program Info
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "program_info",', sep="", fill=TRUE)
		
		cat(indent(level), '"data": [', sep="")
		cat(AppendProgramInfo(org_code), fill=TRUE)
		
		cat(indent(level), ']', sep="", fill=TRUE)
		down(level)
		cat(indent(level),'},', sep="", fill=TRUE)
	}
	{
		#Enrollment
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "enrollment",', sep="", fill=TRUE)
		cat(indent(level), '"data": [', sep="", fill=TRUE)
		cat(ExEnrollChunk(org_code, level+1), fill=TRUE)
		cat(indent(level), ']', sep="", fill=TRUE)
		down(level)
		cat(indent(level),'},', sep="", fill=TRUE)
	}
	{
		## Median Growth Percentile ##
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "mgp_scores",', sep="", fill=TRUE)
		cat(indent(level), '"data": [', sep="", fill=TRUE)
		cat(ExMGPResult(org_code, level+1), fill=TRUE)
		cat(indent(level), ']', sep="", fill=TRUE)
		down(level)
		cat(indent(level),'},', sep="", fill=TRUE)
	}
	{
		## College Readiness##
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "college_readiness",', sep="", fill=TRUE)
		cat(indent(level), '"data": [', sep="", fill=TRUE)
		cat(ExCollegeReadiness(org_code, level+1), fill=TRUE)
		cat(indent(level), ']', sep="", fill=TRUE)
		down(level)
		cat(indent(level),'},', sep="", fill=TRUE)
	}
	{
		## SPED Testing##
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "special_ed",', sep="", fill=TRUE)
		cat(indent(level), '"data": [', sep="", fill=TRUE)
		cat(ExSPEDChunk(org_code, level+1), fill=TRUE)
		cat(indent(level), ']', sep="", fill=TRUE)
		down(level)
		cat(indent(level),'},', sep="", fill=TRUE)
	}
	{
		## ELL/AMAO Stuff ##	
	}
	## Equity Report Starts Here
	{
		#### Absences
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "unexcused_absences",', sep="", fill=TRUE)
		cat(indent(level), '"data": [', sep="", fill=TRUE)
		cat(WriteAbsences(org_code, level+1), fill=TRUE)
		cat(indent(level), ']', sep="", fill=TRUE)
		down(level)
		cat(indent(level),'},', sep="", fill=TRUE)
	}
	{
		#### Attendance
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "attendance",', sep="", fill=TRUE)
		cat(indent(level), '"data": [', sep="", fill=TRUE)
		cat(WriteAttendance(org_code, level+1), fill=TRUE)
		cat(indent(level), ']', sep="", fill=TRUE)
		down(level)
		cat(indent(level),'},', sep="", fill=TRUE)
	}
	{
		#### Suspensions
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "suspensions",', sep="", fill=TRUE)
		cat(indent(level), '"data": [', sep="", fill=TRUE)
		cat(WriteSuspensions(org_code, level+1), fill=TRUE)
		cat(indent(level), ']', sep="", fill=TRUE)
		down(level)
		cat(indent(level),'},', sep="", fill=TRUE)
	}
	{
		#### Expulsions
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "expulsions",', sep="", fill=TRUE)
		cat(indent(level), '"data": [', sep="", fill=TRUE)
		cat(WriteExpulsions(org_code, level+1), fill=TRUE)
		cat(indent(level), ']', sep="", fill=TRUE)
		down(level)
		cat(indent(level),'},', sep="", fill=TRUE)
	}
	{
		#### Mid-Year Entry and Withdrawal
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "mid_year_entry_and_withdrawal",', sep="", fill=TRUE)
		cat(indent(level), '"data": [', sep="", fill=TRUE)
		cat(WriteEnterWithdraw(org_code, level+1), fill=TRUE)
		cat(indent(level), ']', sep="", fill=TRUE)
		down(level)
		cat(indent(level),'}', sep="", fill=TRUE)
	}

	down(level)
	cat(indent(level), ']', sep="", fill=TRUE)
	
	down(level)
	cat(indent(level), '}', fill=TRUE)
	
	cat('}', fill=TRUE)
	sink()
	close(newfile)
}

## SUCCESSFUL PUSH!  
update_vcontrol <- sfile_version
update_vcontrol$version_number <- next_version
update_vcontrol$timestamp <- Sys.time()

sqlSave(dbrepcard, update_vcontrol, tablename="ver_control_schoolfile", append=TRUE, safer=TRUE, rownames=FALSE, varType=c(timestamp="datetime"))
