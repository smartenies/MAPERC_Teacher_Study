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
#' Read in the Qultrix data (with the text responses, not the codes)
#' -----------------------------------------------------------------------------

d_path <- "R:/RSTOR-Magzamen/Research/Projects/EPA_Healthy_Schools/Raw_Data/"
d_name <- "Original ORC Scores_Formatted.xlsx"

e_data <- read_xlsx(paste0(d_path, d_name))
colnames(e_data)
colnames(e_data) <- tolower(gsub(" ", "_", colnames(e_data)))
colnames(e_data)

#' What schools are represented?
unique(e_data$school)

