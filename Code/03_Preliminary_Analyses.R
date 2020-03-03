#' -----------------------------------------------------------------------------
#' Date created: December 12, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Project: MAP-ERC Teacher's Study
#' 
#' Description: Preliminary analysis comparing ORC scores to outcomes reported
#' on the participant survey
#' -----------------------------------------------------------------------------

library(tidyverse)
library(readxl)

#' -----------------------------------------------------------------------------
#' Read in the clean survey and ORC data
#' Create a single dataset
#' Drop original survey variables
#' -----------------------------------------------------------------------------

q_data <- read_csv(here::here("Data", "Cleaned_Indoor_Env_Survey_Data.csv"))
e_data <- read_csv(here::here("Data", "Cleaned_ORC_Data.csv"))

df <- left_join(q_data, e_data, by = "school") %>% 
  select(RecordedDate, ResponseId, school, 
         job_cat:loc_missing,
         light_dissatisfied, light_impact_work, 
         sound_dissatisfied, sound_impact_work, 
         temp_dissatisfied, temp_impact_work, 
         air_qual_dissatisfied,
         any_headaches:freq_symptoms_count,
         environment_dissatisfied,
         energy_efficiency:iaq)

#' -----------------------------------------------------------------------------
#' Summarize the study population
#' -----------------------------------------------------------------------------

# How many surveys did we have?
df2 <- filter(df, !is.na(school))
nrow(df2)
summary(df2)

#' How many schools?
length(unique(df2$school))
table(df2$school, useNA = 'ifany')

#' Jobs represeted?
table(df2$job_cat, useNA = 'ifany')
prop.table(table(df2$job_cat, useNA = 'ifany'))

#' Primary work location
table(df2$primary_loc, useNA = 'ifany')
prop.table(table(df2$primary_loc, useNA = 'ifany'))

#' -----------------------------------------------------------------------------
#' Summarize the outcomes-- 
#' For the sake of the abstract, focusing on:
#'     dissatisfaction (somewhat or extremely)
#'     number of frequent (> 1 per week) symptoms reported
#' -----------------------------------------------------------------------------

#' Dissatisfied with the lighting?
#' 27%
table(df2$light_dissatisfied, useNA = 'ifany')
prop.table(table(df2$light_dissatisfied, useNA = 'ifany'))

#' Dissatisfied with the acoustics?
#' 21%
table(df2$sound_dissatisfied, useNA = 'ifany')
prop.table(table(df2$sound_dissatisfied, useNA = 'ifany'))

#' Dissatisfied with the thermal comfort?
#' 48%
table(df2$temp_dissatisfied, useNA = 'ifany')
prop.table(table(df2$temp_dissatisfied, useNA = 'ifany'))

#' Dissatisfied with the IAQ?
#' 15%
table(df2$air_qual_dissatisfied, useNA = 'ifany')
prop.table(table(df2$air_qual_dissatisfied, useNA = 'ifany'))

#' Overall environment?
#' 17%
table(df2$environment_dissatisfied, useNA = 'ifany')
prop.table(table(df2$environment_dissatisfied, useNA = 'ifany'))

#' Summary of frequent health symptoms reported
summary(df2$freq_symptoms_count)

#' -----------------------------------------------------------------------------
#' Summarize the ORC scores
#' Calculate an "average orc" for the 5 variables assessed
#' -----------------------------------------------------------------------------

df2 <- df2 %>% 
  mutate(mean_orc = (energy_efficiency + thermal_comfort +
                           acoustics + visual_quality + iaq)/5)
View(head(select(df2, energy_efficiency:mean_orc)))

summary(df2$energy_efficiency)
summary(df2$thermal_comfort)
summary(df2$acoustics)
summary(df2$visual_quality)
summary(df2$iaq)

summary(df2$mean_orc)

hist(df2$energy_efficiency)
hist(df2$thermal_comfort)
hist(df2$acoustics)
hist(df2$visual_quality)
hist(df2$iaq)

#' scale the ORC scores
df3 <- df2 %>% 
  mutate(energy_efficiency_s = scale(energy_efficiency),
         thermal_comfort_s = scale(thermal_comfort),
         acoustics_s = scale(acoustics),
         visual_quality_s = scale(visual_quality),
         iaq_s = scale(iaq))

hist(df3$energy_efficiency_s)
hist(df3$thermal_comfort_s)
hist(df3$acoustics_s)
hist(df3$visual_quality_s)
hist(df3$iaq_s)

#' log-transform
df3 <- df3 %>% 
  mutate(energy_efficiency_log = log(energy_efficiency),
         thermal_comfort_log = log(thermal_comfort),
         acoustics_log = log(acoustics),
         visual_quality_log = log(visual_quality),
         iaq_log = log(iaq))

hist(df3$energy_efficiency_log)
hist(df3$thermal_comfort_log)
hist(df3$acoustics_log)
hist(df3$visual_quality_log)
hist(df3$iaq_log)

#' -----------------------------------------------------------------------------
#' Correlations between outcomes and exposures
#' -----------------------------------------------------------------------------

library(ggcorrplot)

df_corr <- df3 %>% 
  select(energy_efficiency:mean_orc,
         light_dissatisfied, sound_dissatisfied,
         air_qual_dissatisfied, environment_dissatisfied,
         any_symptoms_count, freq_symptoms_count,
         job_teacher, loc_classroom)
cor(df_corr)

ggcorrplot(round(cor(df_corr), 1), method = "circle", type = "upper")

#' -----------------------------------------------------------------------------
#' Associations between environment dissatisfaction and ORC scores
#' To help with model convergence, use scaled predictors
#' -----------------------------------------------------------------------------

library(lme4)

#' Overall environment dissatisfaction
plot(df3$environment_dissatisfied, df3$energy_efficiency_s)
env_effic <- glmer(environment_dissatisfied ~ energy_efficiency_s + job_teacher +
                     loc_classroom + (1|school), 
                   family = binomial, data = df3)
summary(env_effic)

plot(df3$environment_dissatisfied, df3$thermal_comfort_s)
env_therm <- glmer(environment_dissatisfied ~ thermal_comfort_s + job_teacher +
                     loc_classroom + (1|school), 
                   family = binomial, data = df3)
summary(env_therm)

plot(df3$environment_dissatisfied, df3$acoustics_s)
env_sound <- glmer(environment_dissatisfied ~ acoustics_s + job_teacher +
                     loc_classroom + (1|school), 
                   family = binomial, data = df3)
summary(env_sound)

plot(df3$environment_dissatisfied, df3$visual_quality)
env_visual <- glmer(environment_dissatisfied ~ visual_quality_s + job_teacher +
                      loc_classroom + (1|school), 
                   family = binomial, data = df3)
summary(env_visual)

plot(df3$environment_dissatisfied, df3$iaq)
env_iaq <- glmer(environment_dissatisfied ~ iaq_s + job_teacher +
                   loc_classroom + (1|school), 
                   family = binomial, data = df3)
summary(env_iaq)

#' Number of frequent symptoms reported

plot(df3$freq_symptoms_count, df3$energy_efficiency_s)
sym_effic <- glmer(freq_symptoms_count ~ energy_efficiency_s + job_teacher +
                     loc_classroom + (1|school), 
                   family = poisson, data = df3)
summary(sym_effic)

plot(df3$freq_symptoms_count, df3$thermal_comfort_s)
sym_therm <- glmer(freq_symptoms_count ~ thermal_comfort_s + job_teacher +
                     loc_classroom + (1|school), 
                   family = poisson, data = df3)
summary(sym_therm)

plot(df3$freq_symptoms_count, df3$acoustics_s)
sym_sound <- glmer(freq_symptoms_count ~ acoustics_s + job_teacher +
                   loc_classroom + (1|school), 
                   family = poisson, data = df3)
summary(sym_sound)

plot(df3$freq_symptoms_count, df3$visual_quality_s)
sym_visual <- glmer(freq_symptoms_count ~ visual_quality_s + job_teacher +
                      loc_classroom + (1|school), 
                    family = poisson, data = df3)
summary(sym_visual)

plot(df3$freq_symptoms_count, df3$iaq_s)
sym_iaq <- glmer(freq_symptoms_count ~ iaq_s + job_teacher +
                   loc_classroom + (1|school), 
                 family = poisson, data = df3)
summary(sym_iaq)
