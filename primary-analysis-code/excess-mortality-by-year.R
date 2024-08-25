library(tidyverse)
library(haven)
library(splines)

args = commandArgs(trailingOnly=TRUE)

source("preprocess-function.R")

project <- args[1]
first_year <- args[2]

years <- seq(first_year, 2022, 1)

datadir <- "../processed-data-files"

inputfile <- file.path(file.path(datadir, project), "export_age_adjusted_deaths_gender_year_se.tsv")
inputfile2 <- file.path(file.path(datadir, project), "export_deaths_gender_age_year.tsv")

#output directory and file
outputdir <- file.path("../results", project, "excess-mortality-by-year")

#table directory

#plot output dir
plotdir <- file.path("../outputs", project, "excess-mortality-by-year")

#palette of colors
cbb <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

tabledir <- file.path(dirname(plotdir), "tables")
create_output_dir(tabledir)

######## CODE ######
data <- preprocess_cdc_wonder(inputfile)

data <- data %>% select(-ends_with("code"))

data <- data %>% type.convert(as.is=TRUE)

zcrit <- qnorm(0.975)

data <- data  %>% filter(is.finite(as.numeric(AA_rate))) 

colnames(data)[colnames(data) == "AA_rate"] <- "Age Adjusted Rate"

excess_deaths_year <- data %>% group_by(Year) %>% summarize(diff_male=`Age Adjusted Rate`[Gender=="M"] - `Age Adjusted Rate`[Gender=="F"], adj_excess_deaths_rate = diff_male, ratio_adj_excess_deaths_rate = `Age Adjusted Rate`[Gender=="M"]/`Age Adjusted Rate`[Gender=="F"], ageadjrate_male=`Age Adjusted Rate`[Gender=="M"], ageadjrate_female = `Age Adjusted Rate`[Gender=="F"]) %>% ungroup() 


data2 <- preprocess_cdc_wonder(inputfile2)
colnames(data2)[which(colnames(data2)=="Age_groups")] <- "agegroup"
data2 <- data2 %>% filter(agegroup!="Not Stated") %>% filter(Population!="Not Applicable") %>% select(-ends_with("code")) %>% type.convert(as.is=TRUE)
data2 <- data2 %>% mutate(age=as.integer(factor(agegroup))-1)

data2 <- data2 %>% filter(is.finite(as.numeric(Population))) %>% filter(is.finite(as.numeric(Deaths))) %>% filter(is.finite(as.numeric(`Crude Rate`))) %>% type.convert(as.is=TRUE)

#To estimate the excess number of deaths, we estimated the annual age-specific mortality rate using actual age by race and then multiplied the White population age-specific mortality rate by the Black population size for that calendar year. Then, the hypothetical number of deaths among the Black population was divided by the Black population size to arrive at a hypothetical annual Black population mortality rate (expected_rate_black). We subtracted this hypothetical rate from the observed Black population mortality rate to arrive at the estimated excess age-specific Black population mortality rate (observed_rate_black), which we multiplied by the observed Black population size to obtain the total annual and 22-year cumulative number of excess deaths among the Black population. 

excess_deaths <- data2 %>% group_by(agegroup, Year) %>% summarize(expected_deaths_male = Population[Gender=="M"]*(Deaths[Gender=="F"]/Population[Gender=="F"]), age_specific_rate_ratio=`Crude Rate`[Gender=="M"]/`Crude Rate`[Gender=="F"], observed_rate_male=(Deaths[Gender=="M"]/Population[Gender=="M"])*100000, observed_rate_female=(Deaths[Gender=="F"]/Population[Gender=="F"])*100000, expected_rate_male=(expected_deaths_male/Population[Gender=="M"])*100000, excess_rate_male = observed_rate_male - expected_rate_male, excess_deaths_male = (excess_rate_male * Population[Gender=="M"])/100000, observed_vs_expected_rate_ratio = observed_rate_male / expected_rate_male, hypothetical_excess = ((observed_rate_male - expected_rate_male)*Population[Gender=="M"])/100000 ) %>% ungroup()

ageadj_mortality_rate <- excess_deaths_year


#Create Tables

excess_deaths %>% group_by(Year) %>% summarize(excess_deaths_male = sum(excess_deaths_male)) %>% ungroup() %>% write_csv(., file = file.path(tabledir, "excess_deaths.csv"))

hyp_excess <- excess_deaths %>% select(agegroup, Year, expected_deaths_male, hypothetical_excess, expected_rate_male)

write_csv(hyp_excess, file = file.path(tabledir, "hypothetical_excess_gender_year_age.csv"))

hyp_excess_year <- excess_deaths %>% select(agegroup, Year, hypothetical_excess) %>% group_by(Year) %>% summarize(hyp = sum(hypothetical_excess)) %>% magrittr::set_colnames(c("Year",  "Hypothetical_Excess_Deaths"))

write_csv(hyp_excess_year, file = file.path(tabledir, "hypothetical_excess_gender_year.csv"))

aadr_indiv_table <- ageadj_mortality_rate %>% select(Year, ageadjrate_male, ageadjrate_female)


excess_aamr_table <- ageadj_mortality_rate %>% mutate(mrr = ageadjrate_male / ageadjrate_female) %>% select(mrr, diff_male, Year) %>% magrittr::set_colnames(c("mrr", "excess_aamr", "Year")) 


write_csv(aadr_indiv_table, file = file.path(tabledir, "aadr_sex_year.csv"))
write_csv(excess_aamr_table, file = file.path(tabledir, "excess_aadr_year.csv"))


#GRAPHICAL COMPONENT

sizing_theme <- theme(axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text=element_text(size=12), legend.title=element_text(size=16), plot.title=element_text(size=18, hjust=0.5)) 

year_label <- scale_x_continuous(breaks=years, labels= function(x) ifelse(x %% 2 == 1, x, ""))

panel_theme <- theme_bw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor=element_blank())


excess_death_rate_fig <- ggplot(data=ageadj_mortality_rate, aes(x=Year, y=diff_male)) + geom_line(size=1) + year_label + panel_theme + sizing_theme + ggtitle("Excess Age Adjusted Mortality Rate") + scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + ylab("Mortality Rate\nper 100,000 Individuals") + xlab("Year") 


mortality_rate_ratio_fig <- excess_deaths_year %>% ggplot(aes(x=Year, y=ratio_adj_excess_deaths_rate)) + geom_line(size=1.25) + panel_theme + sizing_theme + year_label + ylab("Mortality Rate Ratio (Male / Female)") + scale_y_continuous(breaks = scales::pretty_breaks(n=8)) + ggtitle("Age Adjusted Mortality Rate Ratio") 


edy <- excess_deaths_year %>% select(ageadjrate_female, ageadjrate_male, Year) %>% pivot_longer(-c(Year)) %>% mutate(Gender=gsub(".*_", "", name) %>% str_to_title(.)) %>% select(-name)

age_adj_deaths_sex_fig <-  edy %>% ggplot(aes(x=Year, y=value, color=Gender)) + geom_line(size=0.5) + geom_point(size=2) + panel_theme + sizing_theme + year_label + scale_y_continuous(breaks=scales::pretty_breaks(n=8)) + ylab("Mortality Rate per 100K Individuals") + ggtitle("Age Adjusted Mortality Rate") + scale_color_manual(values=cbb[1:2])


edm <- excess_deaths %>% group_by(Year) %>% summarize(excess_deaths_male=sum(excess_deaths_male))

mortality_excess_numbers_fig <-  edm %>% ggplot(aes(x=Year, y=excess_deaths_male)) + geom_point() + geom_line() + panel_theme + sizing_theme + year_label  + scale_y_continuous(breaks=scales::pretty_breaks(n = 8)) + ggtitle("Estimated Excess Deaths\nFor Males Relative to Females") + ylab("Number of Deaths")

cumedm <- excess_deaths %>% group_by(Year) %>% summarize(excess_deaths_male = sum(excess_deaths_male)) %>% ungroup() %>% arrange(Year) %>% mutate(excess_deaths_male=cumsum(excess_deaths_male)) %>% mutate(edm=excess_deaths_male/1000)

cumulative_excess_deaths_fig <- cumedm %>% ggplot(aes(x=Year, y=edm)) + geom_point() + geom_line() + year_label + panel_theme + sizing_theme + ggtitle("Cumulative Number of Excess Deaths") + ylab("Excess Number of Deaths (Thousands)") + scale_y_continuous(breaks = scales::pretty_breaks(n = 9)) + scale_color_manual(name="", values=c("maroon", "navy"))





  save_plot(cumulative_excess_deaths_fig , plotdir)
    save_plot(mortality_excess_numbers_fig , plotdir)
      save_plot(age_adj_deaths_sex_fig , plotdir)
        save_plot(mortality_rate_ratio_fig , plotdir)
          save_plot( excess_death_rate_fig, plotdir)

  save_rds(cumulative_excess_deaths_fig , outputdir)
    save_rds(mortality_excess_numbers_fig , outputdir)
      save_rds(age_adj_deaths_sex_fig , outputdir)
        save_rds(mortality_rate_ratio_fig , outputdir)
          save_rds( excess_death_rate_fig, outputdir)







































