library(RODBC)
# setwd("C:\\Users\\tommy.shen\\Documents\\GitHub\\ReportCards")
setwd("U:\\REPORT CARD\\GIT Report Cards\\ReportCards")
source("./imports/tomkit.R")
##source("./imports/ODBC.R")
source("./lea_functions.R")
setwd("./JSON Output")

dbrepcard <- odbcDriverConnect('driver={SQL Server};server=OSSEEDM1;database=reportcard_dev;trusted_connection=true')
school_dir <- sqlFetch(dbrepcard, 'schooldir_sy1213')

charter_dir <- school_dir[school_dir$lea_code != 1,]
lea_dir <- unique(charter_dir[c("lea_code","lea_name")])

# lea codes need to be four digits and in quotes
lea_dir$lea_code <- sapply(lea_dir$lea_code, leadgr, y=4)

for(i in 1:nrow(charter_dir)){

	org_type <- "lea"
	lea_name <- lea_dir$lea_name[i]
	lea_code <- lea_dir$lea_code[i]

	newfile <- file(paste(org_type, '_', lea_code, '.JSON', sep=""), , encoding="UTF-8")
	
	sink(newfile)
	cat('{', fill=TRUE)
	level <- 1
	cat(indent(level),'"timestamp": "',date(),'",', sep="", fill=TRUE)
	cat(indent(level),'"org_type": "',org_type, '",', sep="", fill=TRUE)
	cat(indent(level),'"org_name": "',lea_name,'",', sep="", fill=TRUE)
	
	cat(indent(level),'"report_card": {', sep="", fill=TRUE)
	up(level)
	cat(indent(level),'"sections": [', sep="", fill=TRUE)
	up(level)

	######### INSERT DATA HERE ##################
	{
		## Assessment
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "dccas",', sep="", fill=TRUE)
		cat(indent(level), '"data": [', sep="", fill=TRUE)
		cat(LeaCasChunk(lea_code, level+1))
		cat('\n',indent(level), ']', sep="", fill=TRUE)
		down(level)
		cat(indent(level),'},', sep="", fill=TRUE)
	}	
	{
		## Highly Qualified Teacher Status
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "hqt_status",', sep="", fill=TRUE)
		
		cat(indent(level), '"data":', LeaHQTStatus(lea_code), sep="", fill=TRUE)	
		down(level)
		cat(indent(level),'},', sep="", fill=TRUE)
	}
	{
		## Graduation
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "graduation",', sep="", fill=TRUE)
		cat(indent(level), '"data": [', sep="", fill=TRUE)
		cat(LeaGraduation(lea_code, level+1), fill=TRUE)
		cat('\n',indent(level), ']', sep="", fill=TRUE)
		down(level)
		cat(indent(level),'},', sep="", fill=TRUE)
	}
	{
		#College Enrollment
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "college_enroll",', sep="", fill=TRUE)
		cat(indent(level), '"data": [', sep="", fill=TRUE)
		cat(LeaCollegeEnroll(lea_code, level+1), fill=TRUE)		
		cat(indent(level), ']', sep="", fill=TRUE)
		down(level)
		cat(indent(level),'}', sep="", fill=TRUE)
	}
	down(level)
	cat(indent(level),']', sep="", fill=TRUE)
	down(level)

	cat(indent(level), '},', fill=TRUE)
	cat('\n', fill=TRUE)

	## END OF REPORT CARD PAGE

	cat(indent(level),'"profile": {', sep="", fill=TRUE)
	up(level)
	cat(indent(level), '"sections": [', sep="", fill=TRUE)
	up(level)
	{
		#Enrollment
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "enrollment",', sep="", fill=TRUE)
		cat(indent(level), '"data": [', sep="", fill=TRUE)
		cat(LeaEnrollChunk(lea_code, level+1), fill=TRUE)
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
		cat(LeaMGPResult(lea_code, level+1), fill=TRUE)
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
        cat(LeaCollegeReadiness(lea_code, level+1), fill=TRUE)
        cat(indent(level), ']', sep="", fill=TRUE)
        down(level)
        cat(indent(level),'},', sep="", fill=TRUE)
    }
	{
		## SPED Testing
		cat(indent(level),'{', sep="", fill=TRUE)
		up(level)
		cat(indent(level), '"id": "special_ed",', sep="", fill=TRUE)
		cat(indent(level), '"data": [', sep="", fill=TRUE)
		cat(LeaSPEDChunk(lea_code, level+1), fill=TRUE)
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