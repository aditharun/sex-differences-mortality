library(tidyverse)

args = commandArgs(trailingOnly=TRUE)

project <- args[1]

resultsdir <- file.path("../results", project)
figdir <- file.path("../figs", project)

if (!dir.exists(figdir)){
	create.dir(figdir)
}


if (!dir.exists(resultsdir)){
	create.dir(resultsdir)
}

set.seed(123)

aatheme <- theme_bw() + theme(panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + theme(axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text=element_text(size=10), legend.title=element_text(size=14), plot.title=element_text(size=18, hjust=0.5))


cpal  <- c("#374E5599", "#Df8F4499", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#6A6599F")


df <-  "../raw-data/all-deaths/master-population.csv" %>% read_csv()

df <- df %>% filter(Population != "Not Applicable") %>% type.convert(as.is = TRUE) %>% group_by(Gender, Year, `Five-Year Age Groups`) %>% summarize(p = sum(Population)) %>% ungroup() %>% magrittr::set_colnames(c("gender", "year", "age_group", "pop"))

df <- df %>% mutate(age_group = age_group %>% str_sub(., 1, -7))
df <- df %>% mutate(age_group = ifelse(age_group == "< ", "< 1", age_group))


df <- df %>% group_by(age_group, year) %>%  mutate(total = sum(pop), ratio = pop[gender == "Female"] / pop[gender == "Male"]) %>% ungroup() %>% mutate(pct = round((pop/total)*100, 2)) 


vec <- df$age_group %>% unique() %>% {c(last(.), head(., -1))}
tmp <- vec %>% str_sub(., 1, 2) 
numtmp <- str_extract(tmp, "\\d+")
age_levels <- data.frame(vec = vec, tmp = tmp, numtmp = numtmp) %>% type.convert(as.is = TRUE) %>% arrange(numtmp) %>% pull(vec) %>% {c(last(.), head(., -1))}


df <- df %>% mutate(age_group = factor(age_group, levels = age_levels))


#df %>% ggplot(aes(x = age_group, y = pct, fill = gender)) + geom_bar(stat = "identity", alpha = 0.65, width = 0.9) + aatheme + xlab("Age Group") + ylab("Population (%)") + scale_fill_manual(name = "Gender", values = cpal[1:2]) + facet_wrap(~year, scales = "fixed") + theme(strip.text = element_text(size = 10), strip.background = element_rect(fill = "transparent", color = "transparent")) + theme()

#you can see here that as time goes on, starting around 40 years old moving forward, women make up less and less of the population
#interestingly, the population is not 50% at <1 year. I attribute this to the fact that less male infants (0-364d) make it alive

a <- df %>% filter(gender == "Male") %>% ggplot(aes(x = age_group, y = pct, color = year, group = year)) + geom_point() + geom_line() + aatheme + scale_y_continuous(breaks = seq(30, 50, 2)) + ylab("% Male in Population") + xlab("Age Group")

#Ratio used to be 1.7:1 of female to male at age 80-84, now its ~1.31:1 female to male. This ratio has declined 22% from 1999 to 2022. 
#In 1999, with 1000 people at 80-84 there would be 1000 / 2.7 = 370 men and 630 women
#In 2022, with 1000 people at 80-84, there would be 1000 / 2.33 = 429 men and 571 women
#In absolute terms, the number of women in 80-84 decreased by 59 women per 1000
b <- df %>% select(year, age_group, ratio) %>% distinct() %>% ggplot(aes(x = age_group, y = ratio, color = year, group = year)) + geom_point() + geom_line() + aatheme + scale_y_continuous(breaks = seq(1, 1.9, 0.1)) + ylab("Female:Male Population Ratio") + xlab("Age Group")

df %>% filter(gender == "Male") %>% filter(year > 1999) %>% write_csv(., file.path(resultsdir, "population-ratio.csv"))


#Age group line chart with the dots having the last two digits of the year showing % female

poppct_fig <- df %>% filter(gender == "Male") %>% filter(year > 1999) %>% ggplot(aes(x = age_group, y = pct, group = year, color = year)) + theme_bw() + theme(panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size = 10, angle = 30, hjust = 1)) + theme(axis.title=element_text(size=16), legend.text=element_text(size=10), legend.title=element_text(size=14), plot.title=element_text(size=18, hjust=0.5)) + geom_point(size = 3) + geom_line(size = 0.25) + scale_y_continuous(breaks = seq(34, 52, 2), limits = c(36, 52)) + ylab("% Male in Population") + xlab("Age Group") + scale_color_gradient(name = "Year", breaks = c(seq(2000, 2015, 5), 2022))

poppct_fig %>% ggsave(filename = file.path(figdir, "pop_pct_fig.pdf"), plot = ., width = 14, height = 9, units = "in", device = cairo_pdf)

poppct_fig %>% saveRDS(file = file.path(resultsdir, "pop_pct_fig.rds"), object = .)


#Heatmap of year (x axis) by age group (y axis) and the color showing the % female (Supplement)

cpal  <- c("#374E5599", "#Df8F4499", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#6A6599F")

poppct_heatmap_fig <- ggplot(df %>% filter(gender == "Male") %>% filter(year > 1999), aes(x=year, y= age_group, fill = pct)) + geom_tile() +  scale_fill_gradient(low = cpal[2], high = cpal[3], na.value = "lightgrey", breaks = scales::pretty_breaks(n = 12), limits = c(35, 52))  + labs(x = "", y="", fill = "Male\nPopulation (%)") + theme_minimal() + theme(plot.title = element_text(size = 9, hjust = 0.5), axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 10)) + theme(panel.background = element_blank(), panel.grid = element_blank()) + ylab("Age Group") + xlab("Year") + theme(axis.title = element_text(size = 14))  + theme(legend.position = "left", legend.text = element_text(size = 8, vjust = 0.5), legend.key.size = unit(0.8, "cm"), legend.title = element_text(size = 10, hjust = 0.5)) + geom_text(aes(label = round(pct)), size = 3.75)


poppct_heatmap_fig %>% ggsave(filename = file.path(figdir, "pop_pct_heatmap_fig.pdf"), plot = ., width = 12, height = 12, units = "in", device = cairo_pdf)

poppct_heatmap_fig %>% saveRDS(file = file.path(resultsdir, "pop_pct_heatmap_fig.rds"), object = .)

# F > M at each age group from birth but F/M ratio equilizing as time goes on (either Males getting more healthier or Females getting less healthy or both





#