library(tidyverse)

resultsdir <- file.path("../results")

msdir <- file.path("../ms")
msdirtable <- file.path(msdir, "tables")

if (!dir.exists(msdir)){
	dir.create(msdir)
}

if (!dir.exists(msdirtable)){
	dir.create(msdirtable)
}

args = commandArgs(trailingOnly=TRUE)

first_year <- args[1] %>% as.numeric()

years <- seq(first_year, 2022, 1)

year_label <- scale_x_continuous(breaks=years, labels= function(x) ifelse(x %% 2 == 0, x, "")) 

colorpal <- ggsci::pal_jama("default")(7)

colorcorrect <- function(bpal = colorpal[c(4,6)]){
	scale_color_manual(values = bpal[1:2])
}

#F1 - a) population pct, b) excess aamr by year, c) excess ypll by year
fig1 <- cowplot::plot_grid(resultsdir %>% file.path("icd", "pop_pct_fig.rds") %>% readRDS(), cowplot::plot_grid(resultsdir %>% file.path("all-deaths", "excess-mortality-by-year", "excess_death_rate_fig.rds") %>% readRDS() + year_label + geom_line(color = "grey70", size = 1.25) + theme(panel.grid = element_blank()) + ylab("Excess AAMR per 100,000") + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ theme(plot.title = element_blank()) + scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(200, 300)), resultsdir %>% file.path("all-deaths", "life-years-lost-by-year", "excess_pll_fig_label_correct.rds") %>% readRDS() + ylab("Excess YPLL (Hundred Thousands)") + theme(axis.text.x = element_text(angle = 45)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(60, 110)) + year_label + geom_line(color = "grey70", size = 1.25) + theme(plot.title = element_blank()) + theme(panel.grid = element_blank()), nrow = 2, ncol = 1, labels = c("B", "C"), label_size = 18), nrow = 1, rel_widths = c(1.2, 1), labels = c("A", ""), label_size = 18 )

ggsave(plot = fig1, filename = file.path(msdir, "fig1.pdf"), width = 17, height = 10, units = "in", device = cairo_pdf)

file.path(resultsdir, "icd", "population-ratio.csv") %>% read_csv() %>% select(-gender) %>% magrittr::set_colnames(c("Year", "Age_Group", "Population Male", "Population", "Ratio M/F", "Male (%)")) %>% write_csv(., file.path(msdirtable, "population-percentages.csv"))



#SF1 population heatmap
sfig1 <- resultsdir %>% file.path("icd", "pop_pct_heatmap_fig.rds") %>% readRDS() + year_label + theme(axis.ticks = element_line(color = "black"), axis.text.y = element_text(hjust = 1)) + scale_x_continuous(expand = c(0, 0), breaks=years, labels= function(x) ifelse(x %% 2 == 0, x, ""))  + theme(axis.text.y = element_text(size = 10))

ggsave(plot = sfig1, filename = file.path(msdir, "sfig1.pdf"), width = 13, height = 9, units = "in", device = cairo_pdf)

#SF2 aamr by sex by year
("../outputs/all-deaths/tables/aadr_sex_year.csv" %>% read_csv()   %>% pivot_longer(-Year) %>% ggplot(aes(x=Year, y=value, color=name)) + geom_line(size=0.5) + geom_point(size=3, alpha = 0.75) + theme_bw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor=element_blank()) + theme(axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text=element_text(size=12), legend.title=element_text(size=16), plot.title=element_text(size=18, hjust=0.5)) + year_label + scale_y_continuous(breaks=scales::pretty_breaks(n=8)) + ylab("AAMR per 100,000") + scale_color_manual(values = c("ageadjrate_male" = colorpal[1], "ageadjrate_female" = colorpal[2]), labels = c("ageadjrate_male" = "Male", "ageadjrate_female" = "Female"), name = "Sex") + theme(axis.text.x = element_text(angle = 45, hjust = 1))) %>% ggsave(plot = ., filename = file.path(msdir, "sfig2.pdf"), width = 8, height = 5, units = "in", device = cairo_pdf)





#F2 - excess aamr and ypll by cause by year for >= 20 AAMR
fig2a <- resultsdir %>% file.path("icd", "eaamrs_fig_subset.rds") %>% readRDS() + year_label + theme(axis.title = element_text(size = 14), strip.text = element_text(size = 12)) + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
fig2b <- resultsdir %>% file.path("icd", "ypll_fig_subset.rds") %>% readRDS() + year_label + theme(axis.title = element_text(size = 14), strip.text = element_text(size = 12)) + ylab("Excess YPLL (Thousands)") + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))

ggsave(plot = cowplot::plot_grid(fig2a, fig2b, nrow = 1, labels = c("A", "B"), label_size = 18), filename = file.path(msdir, "fig2.pdf"), width = 13, height = 12, units = "in", device = cairo_pdf)


#SF3 AAMR by cause by year
sfig3 <- resultsdir %>% file.path("icd", "sex_aamrs_fig.rds") %>% readRDS() + theme(axis.title = element_text(size = 14)) 

ggsave(plot = sfig3, filename = file.path(msdir, "sfig3.pdf"), width = 13, height = 10, units = "in", device = cairo_pdf)

#ST1

file.path(resultsdir, "icd", "icdmap.csv") %>% read_csv(.)  %>% magrittr::set_colnames(c("ICD Code", "Name")) %>% write_csv(., file = file.path(msdir, "st1.csv"))

#SF4 EXCESS AAMR by cause by year

(resultsdir %>% file.path("icd", "eaamrs_fig.rds") %>% readRDS() + year_label + theme(axis.title = element_text(size = 14), strip.text = element_text(size = 12)) + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))) %>% ggsave(plot = ., filename = file.path(msdir, "sfig4.pdf"), width = 14, height = 11, units = "in", device = cairo_pdf)


#SF5 YPLL by cause by year

(resultsdir %>% file.path("icd", "ypll_fig.rds") %>% readRDS() + year_label + theme(axis.title = element_text(size = 14), strip.text = element_text(size = 12)) + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))) %>% ggsave(plot = ., filename = file.path(msdir, "sfig5.pdf"), width = 14, height = 11, units = "in", device = cairo_pdf)



#TABLES TO BE INCLUDED 

rbind(file.path(resultsdir,"icd", "icd-eaamr-year.csv") %>% read_csv(), "../outputs/all-deaths/tables/excess_aadr_year.csv" %>% read_csv() %>% mutate(shortname = "Overall")) %>% select(-mrr) %>% pivot_wider(names_from=shortname, values_from=excess_aamr) %>% write_csv(., file=file.path(msdirtable, "eaamr-year.csv"))


rbind(file.path(resultsdir, "icd", "icd-ypll-year.csv") %>% read_csv(), "../outputs/all-deaths/tables/ypll_sex_year.csv" %>% read_csv() %>% mutate(shortname = "Overall"))  %>% select(-`Excess YPLL Rate`) %>% pivot_wider(names_from=shortname, values_from="Excess YPLL Number") %>% write_csv(., file=file.path(msdirtable, "ypll-year.csv"))


rbind(file.path(resultsdir, "icd", "icd-aamr-sex-year.csv") %>% read_csv(), "../outputs/all-deaths/tables/aadr_sex_year.csv" %>% read_csv() %>% mutate(shortname = "Overall")) %>% write_csv(., file = file.path(msdirtable, "aamr-year-sex.csv"))




#F3 - by age all-deaths excess AAMR and YPLL and by year and age
fig3 <- cowplot::plot_grid(resultsdir %>% file.path("all-deaths", "excess-mortality-by-age", "excess_death_rate_age_fig.rds") %>% readRDS() + theme(panel.grid = element_blank()) + ylab("Excess Death Rate per 100,000") + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 10)) + xlab("Age Group") + geom_point(size = 3, color = "grey70") + geom_line(color = "grey70") + theme(plot.title = element_blank()),

resultsdir %>% file.path("all-deaths", "excess-mortality-by-age", "excess_death_rate_age_year_fig.rds") %>% readRDS()  + theme(panel.grid = element_blank()) + ylab("Excess Death Rate per 100,000") + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 10)) + xlab("Age Group")  + theme(plot.title = element_blank()) + scale_color_manual(name="Year Range", values=colorpal[1:4]),

resultsdir %>% file.path("all-deaths", "life-years-lost-by-age", "excess_pll_fig.rds") %>% readRDS() + theme(panel.grid = element_blank()) + ylab("Excess YPLL Rate per 100,000") + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 10)) + xlab("Age Group") + geom_point(size = 3, color = "grey70") + geom_line(color = "grey70") + theme(plot.title = element_blank()),

resultsdir %>% file.path("all-deaths", "life-years-lost-by-age", "excess_pll_year_fig.rds") %>% readRDS() + theme(panel.grid = element_blank()) + ylab("Excess YPLL Rate per 100,000") + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 10)) + xlab("Age Group")  + theme(plot.title = element_blank()) + scale_color_manual(name="Year Range", values=colorpal[1:4]), nrow = 2, ncol = 2, rel_widths = c(1, 1.25), labels = c("A", "B", "C", "D"), label_size = 16)

ggsave(plot = fig3, filename = file.path(msdir, "fig3.pdf"), width = 13, height = 9, units = "in", device = cairo_pdf)


rbind("../outputs/all-deaths/tables/aadr_sex_age.csv" %>% read_csv() %>% magrittr::set_colnames(c("age", "female_aamr", "male_aamr")) %>% mutate(year = "overall"), "../outputs/all-deaths/tables/aadr_sex_age_year.csv" %>% read_csv() %>% select(1,5,6,7) %>% magrittr::set_colnames(c("age", "male_aamr", "female_aamr", "year"))) %>% as_tibble() %>% write_csv(., file = file.path(msdirtable, "overall-aamr-age-sex.csv"))

rbind("../outputs/all-deaths/tables/excess_aadr_age.csv" %>% read_csv() %>% select(1,3) %>% mutate(year = "overall") %>% magrittr::set_colnames(c("age", "excess_aamr", "year")), "../outputs/all-deaths/tables/aadr_sex_age_year.csv" %>% read_csv() %>% select(1,3,7) %>% magrittr::set_colnames(c("age", "excess_aamr", "year"))) %>% as_tibble() %>% pivot_wider(names_from = "year", values_from="excess_aamr") %>% write_csv(., file = file.path(msdirtable, "overall-eaamr-age.csv"))



agevector <- read_csv(file.path(msdirtable, "overall-aamr-age-sex.csv")) %>% filter(year == "overall") %>% mutate(age_group = age, agenum = sub("-(.*)", "", age) %>% as.numeric()) %>% select(agenum, age, age_group) %>% distinct() %>% arrange(agenum) %>% mutate(age = gsub(" years", "", age)) %>% mutate(agenum = ifelse(age == "< 1 year", 0, agenum))  %>% mutate(age = gsub(" years*", "", age)) %>% arrange(agenum) %>% mutate(r = rank(agenum))

sizing_theme <- theme(axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text=element_text(size=12), legend.title=element_text(size=16), plot.title=element_text(size=18, hjust=0.5)) 

panel_theme <- theme_bw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor=element_blank())

(read_csv(file.path(msdirtable, "overall-aamr-age-sex.csv")) %>% filter(year == "overall") %>% pivot_longer(-c(year, age)) %>% left_join(agevector %>% select(r, age_group), by=c("age"="age_group")) %>% ggplot(aes(x=r, y=value, color=name)) + sizing_theme + panel_theme + geom_point(size = 3.5, alpha = 0.75) + geom_line(size = 0.85) + theme_minimal() + theme(legend.position = "right")  + theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) + theme(axis.ticks = element_line(color = "black")) + ylab("AAMR per 100,000") + theme(panel.grid = element_blank()) + xlab("Age Group") + scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) + scale_x_continuous(limits=c(min(agevector$r),max(agevector$r)), breaks=agevector$r, labels=agevector$age) + scale_color_manual(values = c("male_aamr"=colorpal[1], "female_aamr"=colorpal[2]), labels = c("male_aamr" = "Male", "female_aamr" = "Female"), name = "AAMR") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) ) %>% ggsave(plot = ., filename = file.path(msdir, "sfig6.pdf"), width = 7, height = 7, units = "in", device = cairo_pdf)


rbind("../outputs/all-deaths/tables/ypll_sex_age.csv" %>% read_csv() %>% mutate(year = "overall") %>% select(1,2,4) %>% magrittr::set_colnames(c("age", "ypll_rate", "year")), "../outputs/all-deaths/tables/ypll_sex_age_year.csv" %>% read_csv() %>% select(age_cat, excess_yrs_lost, Year) %>% magrittr::set_colnames(c("age", "ypll_rate", "year"))) %>% pivot_wider(names_from = "year", values_from = "ypll_rate") %>% write_csv(., file = file.path(msdirtable, "overall-ypll-age.csv"))


read_csv("../results/icd/icd-eaamr-age.csv") %>% write_csv(., file.path(msdirtable, "icd-eaamr-age.csv"))

read_csv("../results/icd/icd-ypll-age.csv") %>% write_csv(., file.path(msdirtable, "icd-ypll-age.csv"))


sfig7 <- resultsdir %>% file.path(., "icd", "eaamr_age_fig.rds") %>% readRDS() + theme(panel.grid = element_blank()) + ylab("Excess Death Rate per 100,000") + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 10)) + theme(strip.text = element_text(size = 14))

ggsave(plot = sfig7, filename = file.path(msdir, "sfig7.pdf"), width = 17, height = 14, units = "in", device = cairo_pdf)

sfig8 <- resultsdir %>% file.path(., "icd", "aamr_age_fig.rds") %>% readRDS() + theme(panel.grid = element_blank()) + ylab("AAMR per 100,000") + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 9))

ggsave(plot = sfig8, filename = file.path(msdir, "sfig8.pdf"), width = 16, height = 10, units = "in", device = cairo_pdf)


sfig9 <- resultsdir %>% file.path(., "icd", "eypll_age_fig.rds") %>% readRDS() + theme(panel.grid = element_blank()) + ylab("Excess YPLL Rate per 100,000") + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 10)) + theme(strip.text = element_text(size = 14))

ggsave(plot = sfig9, filename = file.path(msdir, "sfig9.pdf"), width = 18, height = 14, units = "in", device = cairo_pdf)


#fig4 <- cowplot::plot_grid(resultsdir %>% file.path("icd", "eaamr_age_subset_fig.rds") %>% readRDS() + ylab("Excess Death Rate per 100,000"), resultsdir %>% file.path("icd", "eypll_age_subset_fig.rds") %>% readRDS() + ylab("Excess YPLL Rate per 100,000"), labels = c("A", "B"), label_size = 16)

#ggsave(plot = fig4, filename = file.path(msdir, "fig4.pdf"), width = 18, height = 12, units = "in", device = cairo_pdf)


read_csv("../results/icd/icd-eaamr-age-year.csv") %>% write_csv(., file.path(msdirtable, "icd-eaamr-age-year.csv"))

read_csv("../results/icd/icd-ypll-age-year.csv") %>% write_csv(., file.path(msdirtable, "icd-ypll-age-year.csv"))

fig4 <- cowplot::plot_grid(resultsdir %>% file.path("icd", "eaamr_age_year_subset_fig.rds") %>% readRDS() + ylab("Excess Death Rate per 100,000") + theme(panel.grid = element_blank()), resultsdir %>% file.path("icd", "ypll_age_year_subset_fig.rds") %>% readRDS() + ylab("Excess YPLL Rate per 100,000") + theme(panel.grid = element_blank()), labels = c("A", "B"), label_size = 16)

ggsave(plot = fig4, filename = file.path(msdir, "fig5.pdf"), width = 18, height = 14, units = "in", device = cairo_pdf)


sfig10 <- resultsdir %>% file.path(., "icd", "eaamr_age_year_fig.rds") %>% readRDS() + theme(panel.grid = element_blank()) + ylab("Excess Death Rate per 100,000")  + theme(plot.title = element_blank())

ggsave(plot = sfig10, filename = file.path(msdir, "sfig10.pdf"), width = 14, height = 16, units = "in", device = cairo_pdf)

sfig11 <- resultsdir %>% file.path(., "icd", "ypll_age_year_fig.rds") %>% readRDS() + theme(panel.grid = element_blank()) + ylab("Excess YPLL Rate per 100,000") + theme(plot.title = element_blank())

ggsave(plot = sfig11, filename = file.path(msdir, "sfig11.pdf"), width = 14, height = 16, units = "in", device = cairo_pdf)


















#