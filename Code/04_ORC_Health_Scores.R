#' -----------------------------------------------------------------------------
#' Date created: March 2, 2020
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Project: MAP-ERC Teacher's Study
#' 
#' Description: Analysis of the associations between the OCR scores (overall and
#' individual components) and self-reported teacher health outcomes
#' 
#' This is a more detailed version of the analysis submited for the ERC abstract
#' (See 03_Preliminary_Analyses.R)
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
         light_too_dark:light_issues_count,
         sound_dissatisfied, sound_impact_work, 
         sound_other_voices:sound_issues_count,
         temp_dissatisfied, temp_impact_work, 
         temp_heat_from_sun:light_issues_count,
         air_qual_dissatisfied, air_qual_stuffy, air_qual_musty,
         air_smell_dissatisfied,
         air_qual_trash:air_qual_issues_count,
         any_pets, pest_problem,
         any_headaches:freq_symptoms_count,
         leave_confident:leave_sad,
         environment_dissatisfied,
         energy_efficiency:n_participants)

#' -----------------------------------------------------------------------------
#' Summarize the study population
#' -----------------------------------------------------------------------------

# How many surveys did we have?
df2 <- filter(df, !is.na(school))
nrow(df2)

#' How many schools?
length(unique(df2$school))

#' How many responses at each school?
table(df2$school, useNA = 'ifany')

#' Jobs represeted?
table(df2$job_cat, useNA = 'ifany')
prop.table(table(df2$job_cat, useNA = 'ifany'))

#' Primary work location
table(df2$primary_loc, useNA = 'ifany')
prop.table(table(df2$primary_loc, useNA = 'ifany'))

#' -----------------------------------------------------------------------------
#' Summarize the outcomes-- 
#' Focusing on health outcomes here
#' -----------------------------------------------------------------------------

#' How many participants were dissatisfied with their environments overall?
#' n = 23 (17%)
table(df2$environment_dissatisfied, useNA = 'ifany')
prop.table(table(df2$environment_dissatisfied, useNA = 'ifany'))

#' Summary of any and frequent health symptoms reported
hist(df2$any_symptoms_count)
hist(df2$freq_symptoms_count)

#' What's the prevalence of any and frequent headaches?
#' 67% and 28%
prop.table(table(df2$any_headaches, useNA = 'ifany'))
prop.table(table(df2$freq_headaches, useNA = 'ifany'))

#' What's the prevalence of any and frequent wheeze?
#' 13% and 4%
prop.table(table(df2$any_wheeze, useNA = 'ifany'))
prop.table(table(df2$freq_wheeze, useNA = 'ifany'))

#' What's the prevalence of any and frequent eye_irritation?
#' 56% and 28%
prop.table(table(df2$any_eye_irritation, useNA = 'ifany'))
prop.table(table(df2$freq_eye_irritation, useNA = 'ifany'))

#' What's the prevalence of any and frequent sore_throat?
#' 55% and 22%
prop.table(table(df2$any_sore_throat, useNA = 'ifany'))
prop.table(table(df2$freq_sore_throat, useNA = 'ifany'))

#' What's the prevalence of any and frequent sinus_symp?
#' 71% and 28%
prop.table(table(df2$any_sinus_symp, useNA = 'ifany'))
prop.table(table(df2$freq_sinus_symp, useNA = 'ifany'))

#' What's the prevalence of any and frequent sneeze?
#' 62% and 28%
prop.table(table(df2$any_sneeze, useNA = 'ifany'))
prop.table(table(df2$freq_sneeze, useNA = 'ifany'))

#' What's the prevalence of any and frequent eye_strain?
#' 64% and 40%
prop.table(table(df2$any_eye_strain, useNA = 'ifany'))
prop.table(table(df2$freq_eye_strain, useNA = 'ifany'))

#' What's the prevalence of any and frequent tension?
#' 55% and 28%
prop.table(table(df2$any_tension, useNA = 'ifany'))
prop.table(table(df2$freq_tension, useNA = 'ifany'))

#' What's the prevalence of any and frequent lack_atten?
#' 42% and 15%
prop.table(table(df2$any_lack_atten, useNA = 'ifany'))
prop.table(table(df2$freq_lack_atten, useNA = 'ifany'))

#' What's the prevalence of any and frequent dizziness?
#' 25% and 10%
prop.table(table(df2$any_dizziness, useNA = 'ifany'))
prop.table(table(df2$freq_dizziness, useNA = 'ifany'))

#' What's the prevalence of any and frequent nausea?
#' 22% and 6%
prop.table(table(df2$any_nausea, useNA = 'ifany'))
prop.table(table(df2$freq_nausea, useNA = 'ifany'))

#' What's the prevalence of any and frequent depression?
#' 28% and 13%
prop.table(table(df2$any_depression, useNA = 'ifany'))
prop.table(table(df2$freq_depression, useNA = 'ifany'))

#' What's the prevalence of any and frequent lethargy?
#' 60% and 34%
prop.table(table(df2$any_lethargy, useNA = 'ifany'))
prop.table(table(df2$freq_lethargy, useNA = 'ifany'))

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

hist(df2$mean_orc)

#' Going to try scaled exposures too
df3 <- df2 %>% 
  mutate(scale_energy_efficiency = scale(energy_efficiency),
         scale_thermal_comfort = scale(thermal_comfort),
         scale_acoustics = scale(acoustics),
         scale_visual_quality = scale(visual_quality),
         scale_iaq = scale(iaq),
         scale_mean_orc = scale(mean_orc))

hist(df3$scale_energy_efficiency)
hist(df3$scale_thermal_comfort)
hist(df3$scale_acoustics)
hist(df3$scale_visual_quality)
hist(df3$scale_iaq)

hist(df3$scale_mean_orc)

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
#' Covariates include: job_teacher (binary), loc_classroom (binary)
#' 
#' The models include a random intercept for school
#' Using the scaled version of the exposures to improve model fit
#' -----------------------------------------------------------------------------

library(lme4)

#' Overall environment dissatisfaction?
plot(df3$environment_dissatisfied, df3$scale_energy_efficiency)
env_effic <- glmer(environment_dissatisfied ~ scale_energy_efficiency + job_teacher +
                     loc_classroom + (1|school), 
                   family = binomial, data = df3)
summary(env_effic)

plot(df3$environment_dissatisfied, df3$scale_thermal_comfort)
env_therm <- glmer(environment_dissatisfied ~ scale_thermal_comfort + job_teacher +
                     loc_classroom + (1|school), 
                   family = binomial, data = df3)
summary(env_therm)

plot(df3$environment_dissatisfied, df3$scale_acoustics)
env_sound <- glmer(environment_dissatisfied ~ scale_acoustics + job_teacher +
                     loc_classroom + (1|school), 
                   family = binomial, data = df3)
summary(env_sound)

plot(df3$environment_dissatisfied, df3$scale_visual_quality)
env_visual <- glmer(environment_dissatisfied ~ scale_visual_quality + job_teacher +
                      loc_classroom + (1|school), 
                   family = binomial, data = df3)
summary(env_visual)

plot(df3$environment_dissatisfied, df3$scale_iaq)
env_iaq <- glmer(environment_dissatisfied ~ scale_iaq + job_teacher +
                   loc_classroom + (1|school), 
                   family = binomial, data = df3)
summary(env_iaq)

plot(df3$environment_dissatisfied, df3$mean_orc)
env_orc <- glmer(environment_dissatisfied ~ scale_mean_orc + job_teacher +
                   loc_classroom + (1|school), 
                 family = binomial, data = df3)
summary(env_orc)

#' Number of and symptoms reported (use family = Poisson)
plot(df3$any_symptoms_count, df3$scale_energy_efficiency)
sym_effic <- glmer(any_symptoms_count ~ scale_energy_efficiency + job_teacher +
                     loc_classroom + (1|school), 
                   family = poisson, data = df3)
summary(sym_effic)

plot(df3$any_symptoms_count, df3$scale_thermal_comfort)
sym_therm <- glmer(any_symptoms_count ~ scale_thermal_comfort + job_teacher +
                     loc_classroom + (1|school), 
                   family = poisson, data = df3)
summary(sym_therm)

plot(df3$any_symptoms_count, df3$scale_acoustics)
sym_sound <- glmer(any_symptoms_count ~ scale_acoustics + job_teacher +
                   loc_classroom + (1|school), 
                 family = poisson, data = df3)
summary(sym_sound)

plot(df3$any_symptoms_count, df3$scale_visual_quality)
sym_visual <- glmer(any_symptoms_count ~ scale_visual_quality + job_teacher +
                      loc_classroom + (1|school), 
                    family = poisson, data = df3)
summary(sym_visual)

plot(df3$any_symptoms_count, df3$scale_iaq)
sym_iaq <- glmer(any_symptoms_count ~ scale_iaq + job_teacher +
                   loc_classroom + (1|school), 
                 family = poisson, data = df3)
summary(sym_iaq)

plot(df3$any_symptoms_count, df3$scale_mean_orc)
sym_orc <- glmer(any_symptoms_count ~ scale_mean_orc + job_teacher +
                   loc_classroom + (1|school), 
                 family = poisson, data = df3)
summary(sym_orc)

#' Number of frequent symptoms reported (use family = Poisson)
plot(df3$freq_symptoms_count, df3$scale_energy_efficiency)
freq_sym_effic <- glmer(freq_symptoms_count ~ scale_energy_efficiency + job_teacher +
                     loc_classroom + (1|school), 
                   family = poisson, data = df3)
summary(freq_sym_effic)

plot(df3$freq_symptoms_count, df3$scale_thermal_comfort)
freq_sym_therm <- glmer(freq_symptoms_count ~ scale_thermal_comfort + job_teacher +
                     loc_classroom + (1|school), 
                   family = poisson, data = df3)
summary(freq_sym_therm)

plot(df3$freq_symptoms_count, df3$scale_acoustics)
freq_sym_sound <- glmer(freq_symptoms_count ~ scale_acoustics + job_teacher +
                     loc_classroom + (1|school), 
                   family = poisson, data = df3)
summary(freq_sym_sound)

plot(df3$freq_symptoms_count, df3$scale_visual_quality)
freq_sym_visual <- glmer(freq_symptoms_count ~ scale_visual_quality + job_teacher +
                      loc_classroom + (1|school), 
                    family = poisson, data = df3)
summary(freq_sym_visual)

plot(df3$freq_symptoms_count, df3$scale_iaq)
freq_sym_iaq <- glmer(freq_symptoms_count ~ scale_iaq + job_teacher +
                   loc_classroom + (1|school), 
                 family = poisson, data = df3)
summary(freq_sym_iaq)

plot(df3$freq_symptoms_count, df3$scale_mean_orc)
freq_sym_orc <- glmer(freq_symptoms_count ~ scale_mean_orc + job_teacher +
                   loc_classroom + (1|school), 
                 family = poisson, data = df3)
summary(freq_sym_orc)

#' Frequent headaches?
plot(df3$freq_headaches, df3$scale_energy_efficiency)
f_head_effic <- glmer(freq_headaches ~ scale_energy_efficiency + job_teacher +
                     loc_classroom + (1|school), 
                   family = binomial, data = df3)
summary(f_head_effic)

plot(df3$freq_headaches, df3$scale_thermal_comfort)
f_head_therm <- glmer(freq_headaches ~ scale_thermal_comfort + job_teacher +
                     loc_classroom + (1|school), 
                   family = binomial, data = df3)
summary(f_head_therm)

plot(df3$freq_headaches, df3$scale_acoustics)
f_head_sound <- glmer(freq_headaches ~ scale_acoustics + job_teacher +
                     loc_classroom + (1|school), 
                   family = binomial, data = df3)
summary(f_head_sound)

plot(df3$freq_headaches, df3$scale_visual_quality)
f_head_visual <- glmer(freq_headaches ~ scale_visual_quality + job_teacher +
                      loc_classroom + (1|school), 
                    family = binomial, data = df3)
summary(f_head_visual)

plot(df3$freq_headaches, df3$scale_iaq)
f_head_iaq <- glmer(freq_headaches ~ scale_iaq + job_teacher +
                   loc_classroom + (1|school), 
                 family = binomial, data = df3)
summary(f_head_iaq)

plot(df3$freq_headaches, df3$mean_orc)
f_head_orc <- glmer(freq_headaches ~ scale_mean_orc + job_teacher +
                   loc_classroom + (1|school), 
                 family = binomial, data = df3)
summary(f_head_orc)
