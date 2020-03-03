#' -----------------------------------------------------------------------------
#' Date created: December 10, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Project: MAP-ERC Teacher's Study
#' 
#' Description: Clean ORC score data from Jenny Apresnig
#' -----------------------------------------------------------------------------

library(tidyverse)
library(readxl)

#' -----------------------------------------------------------------------------
#' Read in the ORC data from Jenny
#' -----------------------------------------------------------------------------

d_path <- "R:/RSTOR-Magzamen/Research/Projects/EPA_Healthy_Schools/Raw_Data/"

e_name <- "Original ORC Scores_Formatted.xlsx"
e_data <- read_xlsx(paste0(d_path, e_name))
colnames(e_data)
colnames(e_data) <- tolower(gsub(" ", "_", colnames(e_data)))
colnames(e_data)

#' What schools are represented?
unique(e_data$school)

#' -----------------------------------------------------------------------------
#' Match the schools in this dataset with the ones in the survey data
#' -----------------------------------------------------------------------------

q_data <- read_csv(here::here("Data", "Cleaned_Indoor_Env_Survey_Data.csv")) %>% 
  select(school)

unique(q_data$school)

#' Are these school names the same?
unique(q_data$school) %in% unique(e_data$school)

#' Which ones are different?
unique(q_data$school)[!(unique(q_data$school) %in% unique(e_data$school))]

#' STEM Magnet Lab = STEM Lab
e_data[which(e_data$school == "STEM Magnet Lab"), "school"] <- "STEM Lab"

#' Check to make sure all the school names are correct
unique(q_data$school)[!(unique(q_data$school) %in% unique(e_data$school))]

#' -----------------------------------------------------------------------------
#' Save the cleaned ORC data
#' -----------------------------------------------------------------------------

write_csv(e_data, here::here("Data", "Cleaned_ORC_Data.csv"))

#' -----------------------------------------------------------------------------
#' Read in the teacher climate survey data
#' These data were sent as a PDF. I manually entered them into Excel and then
#' had Grace Kuiper check my data entry
#' -----------------------------------------------------------------------------

s_data <- read_xlsx(here::here("Data", "Teacher_Climate_Survey_Data.xlsx"))

#' What schools are represented?
unique(s_data$school)

#' Are these school names the same?
unique(q_data$school) %in% unique(s_data$school)

#' Which ones are different?
unique(q_data$school)[!(unique(q_data$school) %in% unique(s_data$school))]

s_data$school <- paste(s_data$school, "Elementary")
unique(s_data$school)
unique(q_data$school)

#' Fix school names
s_data <- s_data %>% 
  mutate(school = ifelse(school == "STEM Launch Elementary", "STEM Launch", school),
         school = ifelse(school == "Northglenn HS Elementary", "Northglenn High School", school),
         school = ifelse(school == "STEM Lab Elementary", "STEM Lab", school),
         school = ifelse(school == "Colton Creek Elementary", "Cotton Creek Elementary", school))

#' Which ones are different?
unique(q_data$school)[!(unique(q_data$school) %in% unique(s_data$school))]

#' -----------------------------------------------------------------------------
#' Save the cleaned teacher climate survey data
#' -----------------------------------------------------------------------------

write_csv(s_data, here::here("Data", "Cleaned_Climate_Survey_Data.csv"))

#' -----------------------------------------------------------------------------
#' Read in the school SES data
#' -----------------------------------------------------------------------------

d_path <- "R:/RSTOR-Magzamen/Research/Projects/EPA_Healthy_Schools/Raw_Data/"

l_name <- "School Free and Reduced Lunch_15-16.xlsx"
l_data <- read_xlsx(paste0(d_path, l_name))

#' What schools are represented?
unique(l_data$school)

#' Are these school names the same?
unique(q_data$school) %in% unique(l_data$school)

#' Which ones are different?
unique(q_data$school)[!(unique(q_data$school) %in% unique(l_data$school))]

#' STEM Lab and STEM launch are single buildings which house elementary and middle
#' school students. These schools are separated in the free and reduced lunch data
#' set. Here I'm going to average them and replace the values in the lunch data

stem_data <- filter(l_data, str_detect(school, "STEM")) %>% 
  mutate(school = str_split(school, pattern = " ")) %>% 
  rowwise() %>% 
  mutate(school = paste(school[[1]], school[[2]])) %>% 
  group_by(school) %>% 
  summarize(pct_reduced = mean(pct_reduced),
            pct_free = mean(pct_free),
            pct_free_reduced = mean(pct_free_reduced))

l_data <- filter(l_data, !str_detect(school, "STEM")) %>% 
  bind_rows(stem_data)

#' Which ones are different?
unique(q_data$school)[!(unique(q_data$school) %in% unique(l_data$school))]

#' -----------------------------------------------------------------------------
#' Save the cleaned school SES data
#' -----------------------------------------------------------------------------

write_csv(l_data, here::here("Data", "Cleaned_School_SES_Data.csv"))
