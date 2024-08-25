
args = commandArgs(trailingOnly=TRUE)

project <- args[1]

first_year <- args[2]

system(paste0("Rscript excess-mortality-by-age.R ", project))
system(paste0("Rscript excess-mortality-by-year.R ", project, " ", first_year))

system(paste0("Rscript life-years-lost-by-age.R ", project))
system(paste0("Rscript life-years-lost-by-year.R ", project, " ", first_year))

system(paste0("Rscript figure-stitch-generic.R ", project, " ", first_year))
