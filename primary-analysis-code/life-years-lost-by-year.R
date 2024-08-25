library(tidyverse)
library(haven)
library(splines)
library(readxl)

args = commandArgs(trailingOnly=TRUE)

source("preprocess-function.R")

project <- args[1]

first_year <- args[2]

years <- seq(first_year, 2022, 1)

datadir <- "../processed-data-files"

inputfile <- file.path(file.path(datadir, project), "export_deaths_gender_age_year.tsv")

lifeexp_file <- "../data/file_life_expectancy_1999_to_2020.dta"
lifeexp_file2 <- "../data/file_life_expectancy_2021.xlsx" 
lifeexp_file3 <- "../data/file_life_expectancy_2022.xlsx" 

outputdir <- file.path("../results", project, "life-years-lost-by-year")

#create age categorical buckets
age_intervals <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf)

#plot path
plotdir <- file.path("../outputs", project, "life-years-lost-by-year")


tabledir <- file.path(dirname(plotdir), "tables")
create_output_dir(tabledir)

#palette of colors
cbb <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")


##### CODE ######

data <- preprocess_cdc_wonder(inputfile)

agecodes <- data$`Age_groups`

data <- data %>% select(-ends_with("code"))

data$age_cat <- agecodes

data <- data %>% type.convert(as.is = TRUE)

data <- data %>% filter(!is.na(Population))

data_by_age_gender <- data %>% group_by(age_cat, Year, Gender) %>% summarize(deaths=sum(Deaths), population=sum(Population),cruderate=mean(`Crude Rate`) ) %>% ungroup() 

#combine with life expectancy table
life_exp <- read_dta(lifeexp_file) %>% mutate(Gender=factor(gender)) %>% mutate(Gender=ifelse(Gender=="1", "Female", "Male")) %>% select(-gender) %>% mutate(Gender = as.character(Gender))

life_exp <- rbind(lifeexp_file2 %>% read_excel(), lifeexp_file3 %>% read_excel(), life_exp) %>% as_tibble()

life_exp <- life_exp %>% filter(age_cat < 85)

data_by_age_gender <- data_by_age_gender %>% mutate(Gender = ifelse(Gender == "M", "Male", "Female"))

data_combined <- data_by_age_gender %>% mutate(age = sub("-(.*)", "", age_cat) %>% as.numeric()) %>% left_join(life_exp, by=c("age"="age_cat", "Year"="year", "Gender"="Gender")) %>% filter(!is.na(life_expectancy))

#variable life expectancy is the average remaining years people would have lived if didn't die:

data_combined <- data_combined %>% mutate(yrs_lost=life_expectancy*((deaths/population)*100000) )

#Estimating yrs_lost SE.

zcrit <- qnorm(0.975)


#Wald interval, which is based on the assumption that the sampling distribution of the proportion is approximately normal. The CI is calculated as proportion +/- z*standard_error
#standard error of binomial is sqrt(p*(1-p) / n)

lly <- data_combined %>% mutate(prop = deaths / population, se = sqrt(prop * (1-prop) * (1/population) ), yrs_lost_se = (life_expectancy * se * 100000 )^2 ) %>% group_by(Gender, Year) %>% summarize(yrs_lost=mean(yrs_lost), n=n(), yrs_lost_se=sum(yrs_lost_se), population=sum(population)) %>% ungroup() %>% mutate(yrs_lost_se = sqrt(yrs_lost_se/n))


excess_pll <- lly %>% group_by(Year) %>% summarize(excess_yrs_lost = yrs_lost[Gender=="Male"] - yrs_lost[Gender=="Female"], excess_yrs_lost_se = sqrt(sum(yrs_lost_se^2) ), excess_yrs_lost_lb = excess_yrs_lost - (excess_yrs_lost_se * zcrit), excess_yrs_lost_ub = excess_yrs_lost + (excess_yrs_lost_se * zcrit), ratio_excess_yrs_lost = yrs_lost[Gender=="Male"]/yrs_lost[Gender=="Female"], exc_ypll_number = excess_yrs_lost * population[Gender=="Male"] * (1/100000), yrs_lost_male = yrs_lost[Gender=="Male"], yrs_lost_female=yrs_lost[Gender=="Female"]) %>% ungroup()



#GRAPHICAL COMPONENT

sizing_theme <- theme(axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text=element_text(size=14), legend.title=element_text(size=16), plot.title=element_text(size=18, hjust=0.5)) 

year_label <- scale_x_continuous(breaks=years, labels= function(x) ifelse(x %% 2 == 1, x, ""))

panel_theme <- theme_bw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor=element_blank())

excess_pll_rate_fig <- ggplot(data=excess_pll, aes(x=Year, y=excess_yrs_lost)) + geom_line(size=1) + geom_point(size = 3) + geom_hline(yintercept=0, linetype="dotted")  + scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + ylab("Excess YPLL per 100K individuals") + xlab("Year") + year_label + panel_theme + scale_color_manual(values=c("maroon", "navy")) + sizing_theme  + ggtitle("Estimated Excess Years of Potential Life Lost Rate") 

mortality_rate_ratio_fig <- excess_pll %>% ggplot(aes(x=Year, y=ratio_excess_yrs_lost)) + geom_line(size=1.25) + ylab("YPLL Rate Ratio (Male / Female)") + scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + geom_hline(yintercept=1, linetype="dashed")   + ggtitle("Male-Female YPLL rate ratio") + year_label + panel_theme + sizing_theme

excess_pll_fig <- excess_pll %>% ggplot(aes(x=Year, y=exc_ypll_number)) + geom_line(size=1.25)+ sizing_theme + year_label + panel_theme  + theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + scale_color_manual(values=c("maroon", "navy")) + ylab("Years of Potential Life Lost") + ggtitle("Excess Years of Potential Life Lost") + theme(axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text=element_text(size=14), legend.title=element_text(size=16), plot.title=element_text(size=18, hjust=0.5)) 

excess_pll_fig_label_correct <- excess_pll %>% mutate(exc_ypll_number = round(exc_ypll_number/100000)) %>% ggplot(aes(x=Year, y=exc_ypll_number)) + geom_line(size=1.25)+ sizing_theme + year_label + panel_theme  + theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + scale_color_manual(values=c("maroon", "navy")) + ylab("Years of Potential Life Lost\n(Hundred Thousands)") + ggtitle("Excess Years of Potential Life Lost") + theme(axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text=element_text(size=14), legend.title=element_text(size=16), plot.title=element_text(size=18, hjust=0.5)) 


cum_sex <- excess_pll %>% arrange(Year) %>% mutate(pll = cumsum(exc_ypll_number), Year=Year) %>% mutate(pll=pll/1000000)

cumulative_sex_pll_fig <- cum_sex %>% ggplot(aes(x=Year, y=pll)) + geom_point() + geom_line() + year_label + panel_theme + sizing_theme + ggtitle("Cumulative Excess Years of Potential Life") + ylab("Excess Years of Potential Life Lost (Millions)") + scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + scale_color_manual(name="", values=c("maroon", "navy"))


yrslost <- excess_pll %>% select(yrs_lost_female, yrs_lost_male, Year) %>% pivot_longer(-c(Year))

yrs_lost_sex_fig <- yrslost %>% ggplot(aes(x=Year, y=value, color=name)) + geom_line(size=0.5) + geom_point(size=2) + panel_theme + sizing_theme + year_label + scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + ylab("Years Lost per 100K Individuals") + ggtitle("Years of Potential Life Lost") + scale_color_manual("", values=c("yrs_lost_female" = cbb[2], "yrs_lost_male" = cbb[1]), labels = c("yrs_lost_female" = "Female", "yrs_lost_male" = "Male"))


#tables
pll_indiv_table <- excess_pll %>% select(Year, excess_yrs_lost, exc_ypll_number) %>% magrittr::set_colnames(c("Year", "Excess YPLL Rate", "Excess YPLL Number"))

write_csv(pll_indiv_table, file = file.path(tabledir, "ypll_sex_year.csv"))

lly_table <- lly %>% select(Gender, Year, yrs_lost)

write_csv(lly_table,file = file.path(tabledir, "ypll_year.csv"))






  save_plot(excess_pll_rate_fig , plotdir)

  save_plot(mortality_rate_ratio_fig , plotdir)

  save_plot( excess_pll_fig, plotdir)

  save_plot(cumulative_sex_pll_fig , plotdir)

  save_plot(yrs_lost_sex_fig , plotdir)


 save_rds(excess_pll_rate_fig ,  outputdir)

  save_rds( mortality_rate_ratio_fig,  outputdir)

  save_rds( excess_pll_fig,  outputdir)

  save_rds(cumulative_sex_pll_fig ,  outputdir)

  save_rds(yrs_lost_sex_fig ,  outputdir)


  save_plot( excess_pll_fig_label_correct, plotdir)


  save_rds( excess_pll_fig_label_correct,  outputdir)

















