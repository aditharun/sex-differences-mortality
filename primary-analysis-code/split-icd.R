library(tidyverse)

args = commandArgs(trailingOnly=TRUE)

set.seed(123)

readTextFile_alldeaths_norace_nohisp <- function(filepath){
	lines <- readLines(filepath)
	index <- which(lines == '"---"')

	if (length(index) > 0) {
	  lines <- lines[1:(index[1] - 1)]
	}

	list_of_vectors <- lapply(lines, function(x) unlist(strsplit(x, "\t")))

	special_encoding <- which(is.na(list_of_vectors))

	if (length(special_encoding) > 0){

		for (idx in special_encoding){
				Encoding(lines[idx]) <- "latin1"
		}

		list_of_vectors <- lapply(lines, function(x) unlist(strsplit(x, "\t")))

	}

	df <- do.call(rbind, lapply(list_of_vectors, function(x) as.data.frame(t(x))))

	df <- df %>% as_tibble()

	totalcols <- ncol(df)

	df[,1:totalcols] <- lapply(df[,1:totalcols], function(x) gsub('\\"', "", x))

	colnames(df) <- df[1,]

	df <- df[-1,]

	df <- df %>% select(-Notes)

	#remove not stated age groups AND #remove total rows which sum for males, females and males + females (i.e., invalid year code)

	df <- df %>% filter(Year != "") %>% filter(`Five-Year Age Groups` != "Not Stated") %>% filter(`Five-Year Age Groups` != "")

	df
}

input_name <- args[1]

file_1999_2020_data <- paste0("../raw-data/", input_name, "/1999-2020-", input_name, ".txt")

data_1999_2020 <- file_1999_2020_data %>% readTextFile_alldeaths_norace_nohisp(.)

icdchapter_map <- data_1999_2020 %>% select(`ICD Chapter Code`, `ICD Chapter`) %>% distinct()
icdoutputfile <- paste0("../raw-data/", input_name, "/icd-chapter-map.csv")

write_csv(icdchapter_map, file = icdoutputfile)

data_1999_2020 <- data_1999_2020 %>% select(-c(`Year Code`))

data_1999_2020 <- data_1999_2020 %>% filter(Deaths > 0) %>% select(`Five-Year Age Groups`, Gender, Deaths, Population, Year, `ICD Chapter Code`) %>% as_tibble()

file_2021_2022_data <- file.path("../raw-data", input_name, "2021+2022", paste0("2021-2022-", input_name, ".txt"))

data_2021_2022 <- file_2021_2022_data %>% readTextFile_alldeaths_norace_nohisp(.) %>% filter(Deaths > 0) %>% select(`Five-Year Age Groups`, Gender, Deaths, Population, Year, `ICD Chapter Code`) %>% as_tibble()


df <- rbind(data_1999_2020, data_2021_2022) %>% as_tibble()

df <- df %>% filter(Population != "Not Applicable")

#handle supression if it exists
if ((sum(df$Deaths=="Suppressed") + sum(df$Population == "") + sum(df$Population == "Unreliable")) > 0){

	print("handling suppression")

	#Which chapters and age groups contain the suppression
	#df %>% filter(Deaths == "Suppressed") %>% pull(6) %>% table
	#df %>% filter(Deaths == "Suppressed") %>% pull(1) %>% table

	df <- df %>% mutate(Deaths = ifelse(Deaths == "Suppressed", sample(c(1:9), size = sum(Deaths == "Suppressed"), replace = TRUE), Deaths)) %>% mutate(Population = ifelse(Population == "Suppressed", sample(c(1:9), size = sum(Population == "Suppressed"), replace = TRUE), Population)) %>% mutate(Population = ifelse(Population == "Unreliable", sample(c(11:19), size = sum(Population == "Unreliable"), replace = TRUE), Population)) %>% filter(Population != "Not Applicable")



}

df <- df %>% type.convert(as.is = TRUE)

df <- df %>% group_by(`ICD Chapter Code`) %>% group_split()
names(df) <- lapply(df, function(x) x$`ICD Chapter Code` %>% unique()) %>% unlist()

df <- lapply(df, function(x) x %>% dplyr::select(-`ICD Chapter Code`))

#get rid of pregnancy related because these are only female and the excess female - male does not make sense here
which(names(df) == "O00-O99")

df <- df[-15]

#write out each icd chapter 

write_out_icd_chapter <- function(df, idx, input_name){

	icdchapter <- names(df)[idx]
	foldername <- paste0(input_name, "-", icdchapter)

	outdir <- file.path("../raw-data", foldername)

	if (!dir.exists(outdir)){
		dir.create(outdir)
	}

	mat <- df[[idx]]

	output_file <- file.path(outdir, paste0(foldername, "-deaths.csv"))
	master_pop_file <- file.path(outdir, "master-population.csv")

	#deaths file
	mat_deaths <- mat %>% select(-Population)
	mat_deaths %>% write_csv(., file = output_file)

	#population file
	master_pop <- mat %>% select(Population, `Five-Year Age Groups`, Gender,  Year) %>% distinct()
	master_pop %>% write_csv(., file = master_pop_file)

}

lapply(seq_along(df), function(x) write_out_icd_chapter(df, x, input_name))
















##