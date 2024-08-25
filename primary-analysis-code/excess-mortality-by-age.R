library(tidyverse)
library(haven)

args = commandArgs(trailingOnly=TRUE)

source("preprocess-function.R")

project <- args[1]

#create age categorical buckets
age_intervals <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf)

#files with age related data
#starts with deaths_crude_race_gender_age but does not contain year in the file name

datadir <- "../processed-data-files"

agefiles <- list.files(file.path(datadir, project), "^deaths_crude_gender_age_", full.names=TRUE)
agefiles <- agefiles[!grepl("year", agefiles)]

#CDC Wonder input filea
# "export_deaths_crude_race_gender_age.txt"
inputfile <- file.path(file.path(datadir, project), "export_deaths_crude_gender_age_year.tsv")

#output file name and directory
outputdir <- file.path("../results", project, "excess-mortality-by-age")

#table directory

#palette of colors
cbb <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

#plot path
plotdir <- file.path("../outputs", project, "excess-mortality-by-age")


tabledir <- file.path(dirname(plotdir), "tables")
create_output_dir(tabledir)
######### CODE ####################

process_df_tsv <- function(data, age_intervals){

	agecodes <- data$`Single-Year Ages Code`

	data <- data %>% select(-ends_with("code"))

	data$age <- agecodes

	data <- data %>% mutate(age=ifelse(age == 999, NA, age))

	data <- data %>% type.convert()

	data <- data %>% mutate(age_cat = cut(age, breaks=age_intervals, right=FALSE)) %>% mutate(age_cat=as.character(age_cat))

	data <- data %>% type.convert(as.is=TRUE)

	#age buckets formatting
	data <- data %>% mutate(age_lb = as.numeric(sub("\\[(\\d+),.*", "\\1", age_cat)), age_ub = as.numeric(sub(".*,(\\d+)\\)", "\\1", age_cat)), age_cat = paste0(age_lb, "-", age_ub-1), age_cat=ifelse(age_cat=="0-0", "< 1", age_cat)) %>% mutate(age=age_lb) %>% select(-c(age_lb, age_ub))

	data <- data %>% mutate(age_cat = ifelse(age == 85, "85+", age_cat))


	return(data)

}


process_df_five_year <- function(data){

	agecodes <- data$`Five-Year Age Groups`

	data <- data %>% select(-ends_with("code"))

	data$age_cat <- agecodes

	data <- data %>% type.convert(as.is = TRUE)


	return(data)

}


#race = 1 means men and race = 3 means females

compute_statistics <- function(data){

	data <- data %>% filter(is.finite(as.numeric(Deaths))) %>% filter(is.finite(as.numeric(Population))) %>% type.convert(as.is=TRUE)

	data_agg <- data %>% group_by(age_cat, Gender) %>% summarize(deaths=sum(Deaths), population=sum(Population),cruderate=mean(`Crude Rate`) ) %>% ungroup()

	excess_deaths_age <- data_agg %>% group_by(age_cat) %>% summarize(unadj_excess_deaths_number = deaths[Gender=="M"] - (population[Gender=="M"]*(deaths[Gender=="F"]/population[Gender=="F"])), unadj_excess_deaths_rate = ((deaths[Gender=="M"]/population[Gender=="M"])*100000) - ((deaths[Gender=="F"]/population[Gender=="F"])*100000), ratio_unadj_excess_deaths_rate = ((deaths[Gender=="M"]/population[Gender=="M"])*100000) / ((deaths[Gender=="F"]/population[Gender=="F"])*100000), male_death_rate = (deaths[Gender=="M"]/population[Gender=="M"])*100000, female_death_rate = (deaths[Gender=="F"] / population[Gender=="F"])*100000)

	return(list(data_agg=data_agg, excess_deaths_age = excess_deaths_age))
}

wrapper_age_excess_deaths <- function(relativefilepath){

	data <- preprocess_cdc_wonder(relativefilepath)

	relativefilepath <- basename(relativefilepath)

	data <- process_df_five_year(data)
	excess_deaths_age <- compute_statistics(data)$excess_deaths_age

	excess_deaths_age <- excess_deaths_age %>% mutate(Year = gsub("deaths_crude_gender_age_|\\.txt", "", relativefilepath))

	return(excess_deaths_age)
}


data <- preprocess_cdc_wonder(inputfile)

excess_deaths_age <- data %>% process_df_five_year(.) %>% compute_statistics() %>% purrr::pluck("excess_deaths_age")

age.data <- do.call(rbind, lapply(agefiles, function(x) wrapper_age_excess_deaths(x)))


#Table component

aadr_indiv_table <- excess_deaths_age %>% select(age_cat, female_death_rate, male_death_rate) %>% ungroup() %>% select(age_cat, female_death_rate, male_death_rate) %>% magrittr::set_colnames(c("Age Group", "Female Death Rate", "Male Death Rate")) 


excess_aamr_table <- excess_deaths_age %>% select(age_cat, unadj_excess_deaths_number, unadj_excess_deaths_rate) %>% ungroup() %>% magrittr::set_colnames(c("Age Group", "Excess Deaths Number", "Excess Death Rate"))


write_csv(aadr_indiv_table, file = file.path(tabledir, "aadr_sex_age.csv"))
write_csv(excess_aamr_table, file = file.path(tabledir, "excess_aadr_age.csv"))


#Graphical Component
excess_deaths_age <- excess_deaths_age %>% mutate(age = sub("-(.*)", "", age_cat) %>% as.numeric()) %>% mutate(age = ifelse(is.na(age), 0, age)) 

agevector <- excess_deaths_age %>% select(age_cat, age) %>% distinct() %>% arrange(age)

agevector <- agevector %>% mutate(age_cat = gsub(" years*", "", age_cat)) %>% mutate(age.data.merge = age) %>% mutate(age = rank(age)) %>% arrange(age)

age.data <- age.data %>% mutate(age = sub("-(.*)", "", age_cat) %>% as.numeric()) %>% mutate(age = ifelse(is.na(age), 0, age))  

age.data <- age.data %>% left_join(agevector %>% select(age, age.data.merge), by=c("age"="age.data.merge")) %>% select(-age) 

colnames(age.data)[colnames(age.data)=="age.y"] <- "age"

excess_deaths_age <- excess_deaths_age %>% mutate(age = rank(age))


#remove 85+ 
agevector <- agevector %>% filter(age < 85)
excess_deaths_age <- excess_deaths_age %>% filter(age < 85)
age.data <- age.data %>% filter(age < 85)

sizing_theme <- theme(axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text=element_text(size=14), legend.title=element_text(size=16), plot.title=element_text(size=18, hjust=0.5)) 

panel_theme <- theme_bw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor=element_blank())


indiv_death_rate_fig <- excess_deaths_age %>% select(age_cat, age, female_death_rate, male_death_rate) %>% ungroup() %>% pivot_longer(-c(age_cat, age)) %>% ggplot(aes(x=age, y=value, color=name)) + geom_line(size = 0.5) + ylab("Deaths per 100K individuals") + scale_color_manual(values = c("male_death_rate"="maroon", "female_death_rate"="navy"), labels = c("male_death_rate" = "Male", "female_death_rate" = "Female")) + panel_theme + xlab("Age group (years)") + geom_point(size = 2.5) + theme(axis.text.x = element_text(angle=45, hjust=1)) + scale_x_continuous(limits=c(min(agevector$age),max(agevector$age)), breaks=agevector$age, labels=agevector$age_cat) + theme(legend.title = element_blank())


excess_death_rate_age_fig <- excess_deaths_age %>% ggplot(aes(x=age, y=unadj_excess_deaths_rate)) + geom_line(size=0.5)  + sizing_theme + panel_theme + scale_x_continuous(limits=c(min(agevector$age),max(agevector$age)), breaks=agevector$age, labels=agevector$age_cat) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(size=2.5) + scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +  theme(plot.title = element_text(hjust = 0.5)) + ylab("Excess deaths per 100K Individuals") + ggtitle("Excess Mortality Rate By Age")


mortality_rate_ratio_fig <- excess_deaths_age %>% ggplot(aes(x=age, y=ratio_unadj_excess_deaths_rate)) + geom_line(size=.5) + ylab("Mortality Rate Ratio (Male / Female)") + geom_hline(yintercept=1, linetype="dashed") + sizing_theme  + ggtitle("Mortality Rate Ratio by Age") + panel_theme + scale_x_continuous(limits=c(min(agevector$age),max(agevector$age)), breaks=agevector$age, labels=agevector$age_cat) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(size=2.5) + scale_y_continuous(breaks=scales::pretty_breaks(n = 8)) 

age.data %>% mutate(Year = str_remove(Year, ".tsv")) %>% write_csv(., file = file.path(tabledir, "aadr_sex_age_year.csv"))

excess_death_rate_age_year_fig <- age.data %>% mutate(Year = str_remove(Year, ".tsv")) %>% ggplot(aes(x=age, y=unadj_excess_deaths_rate, group=Year)) + geom_line(aes(color=Year), size=0.5) + ylab("Excess deaths per 100K individuals")  + sizing_theme + scale_color_manual(name="Year Range", values=cbb[1:6]) + ggtitle("Excess Mortality Rate Among Black Population by Age and Gender") + panel_theme + scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(aes(color=Year), size=3) + theme(strip.text = element_text(size=11), strip.background=element_rect(fill="transparent", color="transparent")) + scale_y_continuous(breaks = scales::pretty_breaks(n=8)) + scale_x_continuous(limits=c(min(agevector$age),max(agevector$age)), breaks=agevector$age, labels=agevector$age_cat)





 save_plot(excess_death_rate_age_fig, plotdir)
 save_plot(mortality_rate_ratio_fig, plotdir)
  save_plot(excess_death_rate_age_year_fig, plotdir)
   save_plot(indiv_death_rate_fig, plotdir)


 save_rds(excess_death_rate_age_fig, outputdir)
 save_rds(mortality_rate_ratio_fig, outputdir)
  save_rds(excess_death_rate_age_year_fig, outputdir)
   save_rds(indiv_death_rate_fig, outputdir)


