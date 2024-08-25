library(tidyverse)
library(cowplot)

source("preprocess-function.R")

args = commandArgs(trailingOnly=TRUE)

project <- args[1]

first_year <- args[2]

years <- seq(first_year, 2022, 1)

year_label <- scale_x_continuous(breaks=years, labels= function(x) ifelse(x %% 2 == 0, x, ""))

figdir <- file.path("../figs", project)

resultsdir <- file.path("../results", project)

create_output_dir(resultsdir)
create_output_dir(figdir)

sizing_theme <- theme(axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text=element_text(size=12), legend.title=element_text(size=16), plot.title=element_text(size=18, hjust=0.5)) 

panel_theme <- theme_bw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor=element_blank())


#Set of ICD Chapter (ms/st-icd.csv.docx)
#Terrorism, SARS, vaping, and COVID19 combined in special purposes and was therefore omitted from cause-specific analyses
#Got rid of pregnancy, and signs/sx/not elsewhere classified too

icdmap <- "../raw-data/icd/icd-chapter-map.csv" %>% read_csv() %>% magrittr:::set_colnames(c("code", "name"))

icdmap$shortname <- c("Infections", "Neoplasms", "Blood-related", "Endocrine and Metabolic", "Mental Health", "Neurological", "Eye and Adnexa", "Ear and Mastoid", "Circulatory", "Respiratory", "Digestive", "Skin and Subcutaneous\nTissue","MSK and Connective\nTissue", "Genitourinary", "Pregnancy", "Perinatal", "Congentinal", "Not elsewhere classified", "Special Purposes", "External causes")

#icds to drop
#U00-U99, R00-R99 are special codes (COVID19, clinical/lab findings not explained elsewhere)
filter_out_codes <- c("U00-U99", "R00-R99") %>% paste0(., collapse = "|")

#collect all the folders
dirs <- list.dirs(path = "../outputs", recursive = FALSE)
icddirs <- dirs[grepl("icd-.*-", dirs)]
icddirs <- icddirs[!grepl("hisp|race", icddirs)]

icddirs <- icddirs[!icddirs %>% grepl(filter_out_codes, .)]

tabledir <- file.path(icddirs, "tables")

aadrfiles <- file.path(tabledir, "aadr_sex_year.csv")
eaadrfiles <- file.path(tabledir, "excess_aadr_year.csv")
ypllfiles <- file.path(tabledir, "ypll_sex_year.csv")

cpal <- c("#374e55", "#df8f44")
colorpal <- ggsci::pal_jama("default")(7)

colorcorrect <- function(bpal = colorpal[c(4,6)]){
	scale_color_manual(values = bpal[1:2])
}


extract_information <- function(file){

	icdcode <- (file %>% dirname() %>% dirname() %>% basename() ) %>% str_sub(., 5, -1)

	df <- file %>% read_csv()

	df$icdcode <- icdcode

	df

}

sex_aamrs <- lapply(aadrfiles, function(x) x %>% extract_information(.)) %>% do.call(rbind, .)

sex_aamrs <- sex_aamrs %>% left_join(icdmap, by=c("icdcode"="code"))

sex_aamrs <- sex_aamrs %>% pivot_longer(-c(Year, icdcode, name, shortname), names_to = "Gender", values_to="AAMR")

sex_aamrs <- sex_aamrs %>% mutate(Gender = ifelse(grepl("female", Gender), "Female", "Male"))


icdmap <- icdmap %>% mutate(keep = ifelse(shortname %in% (sex_aamrs %>% filter(AAMR >= 20) %>% pull(shortname) %>% unique()), TRUE, FALSE))

icdmap %>% select(code, shortname) %>% write_csv(., file = file.path(resultsdir, "icdmap.csv"))

sex_aamrs_fig <- sex_aamrs %>% mutate(name = str_wrap(name, width = 25)) %>% ggplot(aes(x=Year, y=AAMR, color = Gender, fill = Gender)) + geom_point(size = 2.5, alpha = 0.75) + geom_line(size = 0.85) + theme_minimal() +  theme_minimal() + theme(legend.position = "right") + scale_x_continuous(breaks = c(2000, 2010, 2020), labels = c(2000, 2010, 2020)) + theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) + theme(axis.ticks = element_line(color = "black")) + ylab("AAMR per 100,000") + xlab("") + theme(panel.grid = element_blank()) + xlab("") + theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10)) + facet_wrap(~shortname, nrow = 4, scales = "free") + theme(strip.text = element_text(hjust = 0.5, size = 9)) + scale_color_manual(values = c("Male" = colorpal[1], "Female" = colorpal[2])) + scale_y_continuous(breaks = scales::pretty_breaks(n = 5))




lapply(file.path(tabledir, "excess_aadr_year.csv"), function(x) x %>% extract_information())  %>% do.call(rbind, .) %>% left_join(icdmap, by=c("icdcode" = "code")) %>% select(-c(name, icdcode, keep)) %>% write_csv(., file = file.path(resultsdir, "icd-eaamr-year.csv"))


lapply(file.path(tabledir, "ypll_sex_year.csv"), function(x) x %>% extract_information())  %>% do.call(rbind, .) %>% left_join(icdmap, by=c("icdcode" = "code")) %>% select(-c(name, icdcode, keep)) %>% write_csv(., file = file.path(resultsdir, "icd-ypll-year.csv"))


lapply(file.path(tabledir, "aadr_sex_year.csv"), function(x) x %>% extract_information())  %>% do.call(rbind, .) %>% left_join(icdmap, by=c("icdcode" = "code")) %>% select(-c(name, icdcode, keep)) %>% write_csv(., file = file.path(resultsdir, "icd-aamr-sex-year.csv"))




eaamrs <- lapply(eaadrfiles, function(x) x %>% extract_information(.)) %>% do.call(rbind, .)

eaamrs_fig <- (eaamrs %>% select(-mrr) %>% left_join(icdmap, by=c("icdcode"="code")) %>% mutate(name = str_wrap(name, width = 25))) %>% ggplot(., aes(x=Year, y=excess_aamr, color=keep)) + geom_point(size = 2.25, alpha = 0.75) + geom_line(size = 0.5) + theme_minimal() +  theme_minimal() + theme(legend.position = "right") + scale_x_continuous(breaks = c(2000, 2010, 2020), labels = c(2000, 2010, 2020)) + theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) + theme(axis.ticks = element_line(color = "black")) + ylab("Excess AAMR per 100,000") + xlab("") + theme(panel.grid = element_blank()) + xlab("") + theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10)) + facet_wrap(~shortname, nrow = 4, scales = "free") + theme(strip.text = element_text(hjust = 0.5, size = 9)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) + scale_color_manual(values = c("FALSE" = "maroon", "TRUE" = "forestgreen"), name = "AAMR \u2265 20")


eaamrs_fig_keep_true <- (eaamrs %>% select(-mrr) %>% left_join(icdmap, by=c("icdcode"="code")) %>% mutate(name = str_wrap(name, width = 25)) %>% filter(keep)) %>% ggplot(., aes(x=Year, y=excess_aamr)) + geom_point(size = 2.25, alpha = 0.75, color = "grey70") + geom_line(size = 0.5, color = "grey70") + theme_minimal() +  theme_minimal() + theme(legend.position = "right") + scale_x_continuous(breaks = c(2000, 2010, 2020), labels = c(2000, 2010, 2020)) + theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) + theme(axis.ticks = element_line(color = "black")) + ylab("Excess AAMR per 100,000") + xlab("") + theme(panel.grid = element_blank()) + xlab("") + theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10)) + facet_wrap(~shortname, nrow = 4, scales = "free") + theme(strip.text = element_text(hjust = 0.5, size = 9)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) 


ypll <- lapply(ypllfiles, function(x) x %>% extract_information(.)) %>% do.call(rbind, .)

ypll_fig <- (ypll %>% left_join(icdmap, by=c("icdcode"="code")) %>% mutate(name = str_wrap(name, width = 25))) %>% mutate(`Excess YPLL Number` = round(`Excess YPLL Number`/1000)) %>% ggplot(., aes(x=Year, y=`Excess YPLL Number`, color = keep)) + geom_point(size = 2.25, alpha = 0.75) + geom_line(size = 0.5) + theme_minimal() +  theme_minimal() + theme(legend.position = "right") + scale_x_continuous(breaks = c(2000, 2010, 2020), labels = c(2000, 2010, 2020)) + theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) + theme(axis.ticks = element_line(color = "black")) + ylab("Excess Years of Potential Life Lost\n(Thousands)") + xlab("") + theme(panel.grid = element_blank()) + xlab("") + theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10)) + facet_wrap(~shortname, nrow = 4, scales = "free") + theme(strip.text = element_text(hjust = 0.5, size = 9)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) + scale_color_manual(values = c("FALSE" = "maroon", "TRUE" = "forestgreen"), name = "AAMR \u2265 20")

ypll_fig_keep_true <- (ypll %>% left_join(icdmap, by=c("icdcode"="code")) %>% mutate(name = str_wrap(name, width = 25))) %>% mutate(`Excess YPLL Number` = round(`Excess YPLL Number`/1000)) %>% filter(keep) %>% ggplot(., aes(x=Year, y=`Excess YPLL Number`)) + geom_point(size = 2.25, alpha = 0.75, color = "grey70") + geom_line(size = 0.5, color = "grey70") + theme_minimal() +  theme_minimal() + theme(legend.position = "right") + scale_x_continuous(breaks = c(2000, 2010, 2020), labels = c(2000, 2010, 2020)) + theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) + theme(axis.ticks = element_line(color = "black")) + ylab("Excess Years of Potential Life Lost\n(Thousands)") + xlab("") + theme(panel.grid = element_blank()) + xlab("") + theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10)) + facet_wrap(~shortname, nrow = 4, scales = "free") + theme(strip.text = element_text(hjust = 0.5, size = 9)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 4))


aamr_age <- lapply(file.path(tabledir, "aadr_sex_age.csv"), function(x) x %>% extract_information(.)) %>% do.call(rbind, .) %>% mutate(age = sub("-(.*)", "", `Age Group`) %>% as.numeric())

agevector <- aamr_age %>% select(`Age Group`, age) %>% distinct() %>% arrange(age) %>% mutate(`Age Group` = gsub(" years", "", `Age Group`)) 

agevector <- agevector %>% mutate(age = ifelse(`Age Group`== "< 1 year", 0, age))  %>% mutate(`Age Group` = gsub(" years*", "", `Age Group`)) %>% arrange(age)

agevector <- agevector %>% mutate(r = rank(age))

aamr_age <- aamr_age %>% pivot_longer(-c(`Age Group`, age, icdcode), names_to="Gender") %>% left_join(icdmap, by=c("icdcode"="code")) %>% mutate(name = str_wrap(name, width = 25)) %>% mutate(`Age Group` = gsub(" years", "", `Age Group`)) %>% mutate(age = ifelse(is.na(age), 0, age)) %>% left_join(agevector %>% select(r, age), by=c("age"="age")) %>% select(-age) 

colnames(aamr_age)[colnames(aamr_age)=="r"] <- "age"


aamr_age_fig <- aamr_age  %>% ggplot(aes(x=age, y=value, color=Gender)) + geom_point(size = 2, alpha = 0.75) + geom_line(size = 0.85) + theme_minimal() + theme(legend.position = "right")  + theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) + theme(axis.ticks = element_line(color = "black")) + ylab("AAMR per 100,000") + theme(panel.grid = element_blank()) + xlab("Age Group") + theme(axis.text.y = element_text(size = 10), axis.title = element_text(size = 10)) + theme(strip.text = element_text(hjust = 0.5, size = 9)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) + scale_x_continuous(limits=c(min(agevector$r),max(agevector$r)), breaks=agevector$r, labels=agevector$`Age Group`) + scale_color_manual(values = c("Male Death Rate"=colorpal[1], "Female Death Rate"=colorpal[2]), labels = c("Male Death Rate" = "Male", "Female Death Rate" = "Female")) + facet_wrap(~shortname, scales="free", nrow = 4) + theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))


eaamr_age <- lapply(file.path(tabledir, "excess_aadr_age.csv"), function(x) x %>% extract_information(.)) %>% do.call(rbind, .) %>% mutate(age = sub("-(.*)", "", `Age Group`) %>% as.numeric()) %>% magrittr::set_colnames(c("Age Group", "edn", "edr", "icdcode", "age"))

eaamr_age <- eaamr_age %>% pivot_longer(-c(`Age Group`, age, icdcode), names_to="metric") %>% left_join(icdmap, by=c("icdcode"="code")) %>% mutate(name = str_wrap(name, width = 25)) %>% mutate(`Age Group` = gsub(" years", "", `Age Group`)) %>% mutate(age = ifelse(is.na(age), 0, age)) %>% left_join(agevector %>% select(r, age), by=c("age"="age")) %>% select(-age) 


colnames(eaamr_age)[colnames(eaamr_age)=="r"] <- "age"


eaamr_age_fig <- eaamr_age %>% filter(metric == "edr") %>% ggplot(aes(x=age, y=value, color = keep)) + theme_minimal() + geom_point(size = 2.5, alpha = 0.75) + geom_line(size = 0.5)  + theme_minimal() + theme(legend.position = "right")  + theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) + theme(axis.ticks = element_line(color = "black")) + ylab("Excess AAMR per 100,000") + theme(panel.grid = element_blank()) + xlab("Age Group") + theme(axis.text.y = element_text(size = 10), axis.title = element_text(size = 10)) + theme(strip.text = element_text(hjust = 0.5, size = 9)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) + scale_x_continuous(limits=c(min(agevector$r),max(agevector$r)), breaks=agevector$r, labels=agevector$`Age Group`) + facet_wrap(~shortname, scales="free", nrow = 4) + theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) + scale_color_manual(values = c("FALSE" = "maroon", "TRUE" = "forestgreen"), name = "AAMR \u2265 20")


eaamr_age_subset_fig <- eaamr_age %>% filter(keep) %>% filter(metric == "edr") %>% ggplot(aes(x=age, y=value)) + theme_minimal() + geom_point(size = 2.5, alpha = 0.75, color = "grey70") + geom_line(size = 0.5, color = "grey70")  + theme_minimal() + theme(legend.position = "right")  + theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) + theme(axis.ticks = element_line(color = "black")) + ylab("Excess AAMR per 100,000") + theme(panel.grid = element_blank()) + xlab("Age Group") + theme(axis.text.y = element_text(size = 10), axis.title = element_text(size = 10)) + theme(strip.text = element_text(hjust = 0.5, size = 9)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) + scale_x_continuous(limits=c(min(agevector$r),max(agevector$r)), breaks=agevector$r, labels=agevector$`Age Group`) + facet_wrap(~shortname, scales="free", nrow = 4) + theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) 


eaamr_age %>% filter(metric == "edr") %>% select(1,4,6,7) %>% write_csv(., file.path(resultsdir, "icd-eaamr-age.csv"))

aamr_age %>% select(1,3,4,6,7) %>% write_csv(., file.path(resultsdir, "icd-aamr-age.csv"))



ypll_age <- lapply(file.path(tabledir, "ypll_age.csv"), function(x) x %>% extract_information(.)) %>% do.call(rbind, .) %>% mutate(age = sub("-(.*)", "", age_cat) %>% as.numeric())

agevector <- ypll_age %>% select(age_cat, age) %>% distinct() %>% arrange(age) %>% mutate(age_cat = gsub(" years", "", age_cat)) 

agevector <- agevector %>% mutate(age = ifelse(age_cat== "< 1 year", 0, age))  %>% mutate( age_cat = gsub(" years*", "", age_cat)) %>% arrange(age)

agevector <- agevector %>% mutate(r = rank(age))


ypll_age <- ypll_age %>% pivot_longer(-c(age_cat, age, icdcode), names_to="Gender") %>% left_join(icdmap, by=c("icdcode"="code")) %>% mutate(name = str_wrap(name, width = 25)) %>% mutate(age_cat = gsub(" years", "", age_cat)) %>% mutate(age = ifelse(is.na(age), 0, age)) %>% left_join(agevector %>% select(r, age), by=c("age"="age")) %>% select(-age) 

colnames(ypll_age)[colnames(ypll_age)=="r"] <- "age"


ypll_age_fig <- ypll_age  %>% ggplot(aes(x=age, y=value, color=Gender)) + geom_point(size = 2, alpha = 0.75) + geom_line(size = 0.85) + theme_minimal() + theme(legend.position = "right")  + theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) + theme(axis.ticks = element_line(color = "black")) + ylab("YPLL per 100,000") + theme(panel.grid = element_blank()) + xlab("Age Group") + theme(axis.text.y = element_text(size = 10), axis.title = element_text(size = 10)) + theme(strip.text = element_text(hjust = 0.5, size = 9)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) + scale_x_continuous(limits=c(min(agevector$r),max(agevector$r)), breaks=agevector$r, labels=agevector$age_cat) + scale_color_manual(values = c("male_years_lost"=colorpal[1], "female_years_lost"=colorpal[2]), labels = c("male_years_lost" = "Male", "female_years_lost" = "Female")) + facet_wrap(~shortname, scales="free", nrow = 4) + theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))


eypll_age <- lapply(file.path(tabledir, "ypll_sex_age.csv"), function(x) x %>% extract_information(.)) %>% do.call(rbind, .) %>% magrittr::set_colnames(c("age_cat", "excess_rate", "excess_ypll_number", "icdcode")) %>% mutate(age = sub("-(.*)", "", age_cat) %>% as.numeric())

eypll_age <- eypll_age %>% pivot_longer(-c(age_cat, age, icdcode), names_to="metric") %>% left_join(icdmap, by=c("icdcode"="code")) %>% mutate(name = str_wrap(name, width = 25)) %>% mutate(age_cat = gsub(" years", "", age_cat)) %>% mutate(age = ifelse(is.na(age), 0, age)) %>% left_join(agevector %>% select(r, age), by=c("age"="age")) %>% select(-age) 

colnames(eypll_age)[colnames(eypll_age)=="r"] <- "age"



eypll_age_fig <- eypll_age %>% filter(metric == "excess_rate") %>% ggplot(aes(x=age, y=value, color = keep)) + theme_minimal() + geom_point(size = 2.5, alpha = 0.75) + geom_line(size = 0.5)  + theme_minimal() + theme(legend.position = "right")  + theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) + theme(axis.ticks = element_line(color = "black")) + ylab("Excess YPLL Rate per 100,000") + theme(panel.grid = element_blank()) + xlab("Age Group") + theme(axis.text.y = element_text(size = 10), axis.title = element_text(size = 10)) + theme(strip.text = element_text(hjust = 0.5, size = 9)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) + scale_x_continuous(limits=c(min(agevector$r),max(agevector$r)), breaks=agevector$r, labels=agevector$age_cat) + facet_wrap(~shortname, scales="free", nrow = 4) + theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) + scale_color_manual(values = c("FALSE" = "maroon", "TRUE" = "forestgreen"), name = "AAMR \u2265 20")


eypll_age %>% select(1,3,4,6,7) %>% write_csv(., file.path(resultsdir, "icd-ypll-age.csv"))

eypll_age_subset_fig <- eypll_age %>% filter(keep) %>% filter(metric == "excess_rate") %>% ggplot(aes(x=age, y=value)) + theme_minimal() + geom_point(size = 2.5, alpha = 0.75, color = "grey70") + geom_line(size = 0.5, color = "grey70")  + theme_minimal() + theme(legend.position = "right")  + theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) + theme(axis.ticks = element_line(color = "black")) + ylab("Excess YPLL Rate per 100,000") + theme(panel.grid = element_blank()) + xlab("Age Group") + theme(axis.text.y = element_text(size = 10), axis.title = element_text(size = 10)) + theme(strip.text = element_text(hjust = 0.5, size = 9)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) + scale_x_continuous(limits=c(min(agevector$r),max(agevector$r)), breaks=agevector$r, labels=agevector$age_cat) + facet_wrap(~shortname, scales="free", nrow = 4) + theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) 




###### START HERE
ypll_age_year <- lapply(file.path(tabledir, "ypll_sex_age_year.csv"), function(x) x %>% extract_information(.)) %>% do.call(rbind, .) 

ypll_age_year <- ypll_age_year %>% left_join(icdmap, by=c("icdcode"="code")) %>% mutate(name = str_wrap(name, width = 25))


ypll_age_year %>% select(1,2,7,11,12) %>% write_csv(., file.path("../results/icd/icd-ypll-age-year.csv"))


ypll_age_year_fig <- ypll_age_year %>% ggplot(aes(x=age, y=excess_yrs_lost, group = Year, color = Year, shape = keep)) + theme_minimal() + sizing_theme + panel_theme + geom_line(size=0.5) + ylab("Excess YPLL per 100,000") + scale_y_continuous(breaks=scales::pretty_breaks(n = 5)) + scale_color_manual(name="Year Range", values=colorpal[1:4]) + scale_x_continuous(limits=c(min(agevector$r),max(agevector$r)), breaks=agevector$r, labels=agevector$age_cat) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(size=2.5) + theme(strip.text = element_text(size=11), strip.background=element_rect(fill="transparent", color="transparent")) + facet_wrap(~shortname, scales="free", nrow = 6) + theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) + scale_shape_manual(values = c("FALSE" = "triangle", "TRUE" = "circle"), name = "AAMR \u2265 20")


ypll_age_year_subset_fig <- ypll_age_year %>% filter(keep) %>% ggplot(aes(x=age, y=excess_yrs_lost, group = Year, color = Year)) + theme_minimal() + sizing_theme + panel_theme + geom_line(size=0.5) + ylab("Excess YPLL per 100,000") + scale_y_continuous(breaks=scales::pretty_breaks(n = 5)) + scale_color_manual(name="Year Range", values=colorpal[1:4]) + scale_x_continuous(limits=c(min(agevector$r),max(agevector$r)), breaks=agevector$r, labels=agevector$age_cat) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(size=2.5) + theme(strip.text = element_text(size=11), strip.background=element_rect(fill="transparent", color="transparent")) + facet_wrap(~shortname, scales="free", nrow = 6) + theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) 


aadr_age_year <- lapply(file.path(tabledir, "aadr_sex_age_year.csv"), function(x) x %>% extract_information(.)) %>% do.call(rbind, .) 

aadr_age_year <- aadr_age_year %>% left_join(icdmap, by=c("icdcode"="code")) %>% mutate(name = str_wrap(name, width = 25)) 

aadr_age_year %>% select(1,3,7,11, 12) %>% write_csv(., file.path("../results/icd/icd-eaamr-age-year.csv"))

aamr_age_year_fig <- aadr_age_year %>% ggplot(aes(x=age, y=unadj_excess_deaths_rate, group = Year, color = Year, shape = keep)) + theme_minimal() + sizing_theme + panel_theme + geom_line(size=0.5) + ylab("Excess Death Rate per 100,000") + scale_y_continuous(breaks=scales::pretty_breaks(n = 5)) + scale_color_manual(name="Year Range", values=colorpal[1:4]) + scale_x_continuous(limits=c(min(agevector$r),max(agevector$r)), breaks=agevector$r, labels=agevector$age_cat) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(size=2.5) + theme(strip.text = element_text(size=11), strip.background=element_rect(fill="transparent", color="transparent")) + facet_wrap(~shortname, scales="free", nrow = 6) + theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) + scale_shape_manual(values = c("FALSE" = "triangle", "TRUE" = "circle"), name = "AAMR \u2265 20")


aamr_age_year_subset_fig <- aadr_age_year %>% filter(keep) %>% ggplot(aes(x=age, y=unadj_excess_deaths_rate, group = Year, color = Year)) + theme_minimal() + sizing_theme + panel_theme + geom_line(size=0.5) + ylab("Excess Death Rate per 100,000") + scale_y_continuous(breaks=scales::pretty_breaks(n = 5)) + scale_color_manual(name="Year Range", values=colorpal[1:4]) + scale_x_continuous(limits=c(min(agevector$r),max(agevector$r)), breaks=agevector$r, labels=agevector$age_cat) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(size=2.5) + theme(strip.text = element_text(size=11), strip.background=element_rect(fill="transparent", color="transparent")) + facet_wrap(~shortname, scales="free", nrow = 6) + theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) 



aamr_age_year_fig %>% ggsave(filename = file.path(figdir, "aadr_age_year_fig.pdf"), plot = ., width = 15, height = 15, units = "in", device = cairo_pdf)
ypll_age_year_fig %>% ggsave(filename = file.path(figdir, "ypll_age_year_fig.pdf"), plot = ., width = 15, height = 15, units = "in", device = cairo_pdf)


ypll_age_fig %>% ggsave(filename = file.path(figdir, "ypll_age_fig.pdf"), plot = ., width = 12, height = 12, units = "in", device = cairo_pdf)
aamr_age_fig %>% ggsave(filename = file.path(figdir, "aamr_age_fig.pdf"), plot = ., width = 12, height = 12, units = "in", device = cairo_pdf)
 

eypll_age_fig %>% ggsave(filename = file.path(figdir, "eypll_age_fig.pdf"), plot = ., width = 12, height = 12, units = "in", device = cairo_pdf)
eaamr_age_fig %>% ggsave(filename = file.path(figdir, "eaamr_age_fig.pdf"), plot = ., width = 12, height = 12, units = "in", device = cairo_pdf)


eaamrs_fig %>% ggsave(filename = file.path(figdir, "eaamrs_fig.pdf"), plot = ., width = 10, height = 10, units = "in", device = cairo_pdf)
sex_aamrs_fig %>% ggsave(filename = file.path(figdir, "sex_aamrs_fig.pdf"), plot = ., width = 10, height = 10, units = "in", device = cairo_pdf)
ypll_fig %>% ggsave(filename = file.path(figdir, "ypll_fig.pdf"), plot = ., width = 10, height = 10, units = "in", device = cairo_pdf)
eaamrs_fig_keep_true %>% ggsave(filename = file.path(figdir, "eaamrs_fig_subset.pdf"), plot = ., width = 8, height = 10, units = "in", device = cairo_pdf)
ypll_fig_keep_true %>% ggsave(filename = file.path(figdir, "ypll_fig_subset.pdf"), plot = ., width = 8, height = 10, units = "in", device = cairo_pdf)

aamr_age_year_fig %>% saveRDS(file = file.path(resultsdir, "eaamr_age_year_fig.rds"), object = .)
ypll_age_year_fig %>% saveRDS(file = file.path(resultsdir, "ypll_age_year_fig.rds"), object = .)

aamr_age_year_subset_fig %>% saveRDS(file = file.path(resultsdir, "eaamr_age_year_subset_fig.rds"), object = .)
ypll_age_year_subset_fig %>% saveRDS(file = file.path(resultsdir, "ypll_age_year_subset_fig.rds"), object = .)

ypll_age_fig %>% saveRDS(file = file.path(resultsdir, "ypll_age_fig.rds"), object = .)
aamr_age_fig %>% saveRDS(file = file.path(resultsdir, "aamr_age_fig.rds"), object = .)
 
eypll_age_fig %>% saveRDS(file = file.path(resultsdir, "eypll_age_fig.rds"), object = .)
eaamr_age_fig %>% saveRDS(file = file.path(resultsdir, "eaamr_age_fig.rds"), object = .)

eypll_age_subset_fig %>% saveRDS(file = file.path(resultsdir, "eypll_age_subset_fig.rds"), object = .)
eaamr_age_subset_fig %>% saveRDS(file = file.path(resultsdir, "eaamr_age_subset_fig.rds"), object = .)

eaamrs_fig %>% saveRDS(file = file.path(resultsdir, "eaamrs_fig.rds"), object = .)
sex_aamrs_fig %>% saveRDS(file = file.path(resultsdir, "sex_aamrs_fig.rds"), object = .)
ypll_fig %>% saveRDS(file = file.path(resultsdir, "ypll_fig.rds"), object = .)

eaamrs_fig_keep_true %>% saveRDS(file = file.path(resultsdir, "eaamrs_fig_subset.rds"), object = .)
ypll_fig_keep_true %>% saveRDS(file = file.path(resultsdir, "ypll_fig_subset.rds"), object = .)













