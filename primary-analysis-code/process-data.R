library(tidyverse)


#All causes no race no hisp all causes
system("Rscript standard-pop-2000-parse.R") #DONE
system("Rscript gather-data.R all-deaths") #DONE
system("Rscript create-interim-files.R all-deaths 2000") #DONE
system("Rscript run-figures.R all-deaths 2000")

#All causes no race no hisp ICD
system("Rscript split-icd.R icd") #DONE


dirs <- list.dirs(path = "../raw-data", recursive = FALSE)
icddirs <- dirs[grepl("icd-.*-", dirs)]
icddirs <- icddirs[!grepl("hisp|race", icddirs)]
base_icddirs <- basename(icddirs)


for (x in base_icddirs){

	system(paste0("Rscript create-interim-files.R ", x, " 2000"))
	system(paste0("Rscript run-figures.R ", x, " 2000"))

}

system("Rscript ensemble-icd-figures.R icd 2000")
system("Rscript male-female-ratio.R icd")

system("Rscript figure-compilation.R 2000")
