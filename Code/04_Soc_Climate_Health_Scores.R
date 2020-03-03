#' -----------------------------------------------------------------------------
#' Date created: March 2, 2020
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Project: MAP-ERC Teacher's Study
#' 
#' Description: Analysis of the associations between the social climate survey
#' data and self-reported teacher health outcomes
#' -----------------------------------------------------------------------------

library(tidyverse)
library(readxl)

#' -----------------------------------------------------------------------------
#' Read in the clean indoor environment survey and social climate
#' Create a single dataset
#' Drop original survey variables
#' -----------------------------------------------------------------------------

q_data <- read_csv(here::here("Data", "Cleaned_Indoor_Env_Survey_Data.csv")) %>% 
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
         environment_dissatisfied) 

q_schools <- unique(q_data$school)

e_data <- read_csv(here::here("Data", "Cleaned_ORC_Data.csv")) %>% 
  filter(school %in% q_schools)

s_data <- read_csv(here::here("Data", "Cleaned_Climate_Survey_Data.csv")) %>% 
  filter(school %in% q_schools)

l_data <- read_csv(here::here("Data", "Cleaned_School_SES_Data.csv")) %>% 
  filter(school %in% q_schools)

df <- left_join(q_data, l_data, by = "school") %>% 
  left_join(s_data, by = "school") %>% 
  left_join(e_data, by = "school")
  
glimpse(df)

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

#' Jobs represented?
table(df2$job_cat, useNA = 'ifany')
prop.table(table(df2$job_cat, useNA = 'ifany'))

#' Primary work location
table(df2$primary_loc, useNA = 'ifany')
prop.table(table(df2$primary_loc, useNA = 'ifany'))

#' Distribution of free and reduced lunch
summary(df2$pct_free_reduced)
hist(df2$pct_free_reduced)

#' -----------------------------------------------------------------------------
#' Summarize the outcomes-- 
#' Focusing on health outcomes here
#' -----------------------------------------------------------------------------

#' How many participants were dissatisfied with their environments overall?
#' n = 23 (17%)
table(df2$environment_dissatisfied, useNA = 'ifany')
prop.table(table(df2$environment_dissatisfied, useNA = 'ifany'))

#' Summary of any and frequent (at least once a week) health symptoms reported
summary(df2$any_symptoms_count)
hist(df2$any_symptoms_count)

summary(df2$freq_symptoms_count)
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
#' 
#' The non-annotated scores are the average of the "site" and "occupant" scores
#' mean_orc is the average of the averaged scores for each component of the ORC
#' -----------------------------------------------------------------------------

#' Look at the raw data
summary(e_data$energy_efficiency) # No site vs. occupant for this one

summary(e_data$thermal_comfort)
summary(e_data$thermal_comfort_site)
summary(e_data$thermal_comfort_occupant)

summary(e_data$acoustics)
summary(e_data$acoustics_site)
summary(e_data$acoustics_occupant)

summary(e_data$visual_quality)
summary(e_data$visual_quality_site)
summary(e_data$visual_quality_occupant)

summary(e_data$iaq)
summary(e_data$iaq_site)
summary(e_data$iaq_occupant)

#' Calulcate the mean score
df2 <- df2 %>% 
  mutate(mean_orc = (energy_efficiency + thermal_comfort +
                           acoustics + visual_quality + iaq)/5)

summary(df2$mean_orc)
hist(df2$mean_orc)

#' Going to calculate scaled exposures and log-transformed exposures, too
df3 <- df2 %>% 
  mutate(scale_energy_efficiency = scale(energy_efficiency),
         scale_thermal_comfort = scale(thermal_comfort),
         scale_acoustics = scale(acoustics),
         scale_visual_quality = scale(visual_quality),
         scale_iaq = scale(iaq),
         scale_mean_orc = scale(mean_orc),
         log_energy_efficiency = log(energy_efficiency),
         log_thermal_comfort = log(thermal_comfort),
         log_acoustics = log(acoustics),
         log_visual_quality = log(visual_quality),
         log_iaq = log(iaq),
         log_mean_orc = log(mean_orc))

hist(df3$scale_energy_efficiency)
hist(df3$scale_thermal_comfort)
hist(df3$scale_acoustics)
hist(df3$scale_visual_quality)
hist(df3$scale_iaq)
hist(df3$scale_mean_orc)

summary(df3$scale_energy_efficiency)
summary(df3$scale_thermal_comfort)
summary(df3$scale_acoustics)
summary(df3$scale_visual_quality)
summary(df3$scale_iaq)
summary(df3$scale_mean_orc)

hist(df3$log_energy_efficiency)
hist(df3$log_thermal_comfort)
hist(df3$log_acoustics)
hist(df3$log_visual_quality)
hist(df3$log_iaq)
hist(df3$log_mean_orc)

summary(df3$log_energy_efficiency)
summary(df3$log_thermal_comfort)
summary(df3$log_acoustics)
summary(df3$log_visual_quality)
summary(df3$log_iaq)
summary(df3$log_mean_orc)

#' -----------------------------------------------------------------------------
#' Summarize the climate survey data (school level)
#' -----------------------------------------------------------------------------

#' Percentage of respondents with positive view of social and learning climate?
summary(df3$overall_score_current)
hist(df3$overall_score_current)

#' Percentage of respondents who would strongly recommend their school to a 
#' colleague?
summary(df3$recommend_strongly)
hist(df3$recommend_strongly)

#' Percentage of respondents who see the school environment as positive
summary(df3$work_env_positive)
hist(df3$work_env_positive)

#' Percentage of respondents who say students are enthusiastic about being at 
#' school
summary(df3$students_enthusiatic)
hist(df3$students_enthusiatic)

#' Percentage of respondents who say they are optimistic that the school will 
#' improve in the future
summary(df3$optimistic_improvement)
hist(df3$optimistic_improvement)

#' Percentage of respondents who say students are supportive of each other
summary(df3$students_supportive)
hist(df3$students_supportive)

#' Percentage of respondents who say their colleagues have positive attitudes
summary(df3$colleagues_pos_attitude)
hist(df3$colleagues_pos_attitude)

#' Percentage of respondents who say relationships between students and teachers
#' are respectful
summary(df3$teach_student_respectful)
hist(df3$teach_student_respectful)

#' Percentage of respondents who say they observe students helping each other
#' without being prompted
summary(df3$students_helping)
hist(df3$students_helping)

#' Percentage of respondents who say colleagues are supportive of initiatives to
#' improve teaching on campus
summary(df3$colleagues_support_initiatives)
hist(df3$colleagues_support_initiatives)

#' Percentage of respondents who say teachers are trusted to teach the way they
#' think is best
summary(df3$teachers_trusted)
hist(df3$teachers_trusted)

#' Calculate scaled exposures
df3$scale_overall_score_current <- scale(df3$overall_score_current)
df3$scale_recommend_strongly <- scale(df3$recommend_strongly)
df3$scale_work_env_positive <- scale(df3$work_env_positive)
df3$scale_students_enthusiatic <- scale(df3$students_enthusiatic)
df3$scale_optimistic_improvement <- scale(df3$optimistic_improvement)
df3$scale_students_supportive <- scale(df3$students_supportive)
df3$scale_colleagues_pos_attitude <- scale(df3$colleagues_pos_attitude)
df3$scale_teach_student_respectful <- scale(df3$teach_student_respectful)
df3$scale_students_helping <- scale(df3$students_helping)
df3$scale_colleagues_support_initiatives <- scale(df3$colleagues_support_initiatives)
df3$scale_teachers_trusted <- scale(df3$teachers_trusted)

#' -----------------------------------------------------------------------------
#' Correlations between outcomes and exposures (ORC scores or social climate)
#' -----------------------------------------------------------------------------

library(ggcorrplot)

df_corr_env <- df3 %>% 
  select(energy_efficiency:iaq,mean_orc,
         light_dissatisfied, sound_dissatisfied,
         air_qual_dissatisfied, environment_dissatisfied,
         any_symptoms_count, freq_symptoms_count)
cor(df_corr_env)

ggcorrplot(round(cor(df_corr_env), 1), method = "circle", type = "upper")

df_corr_soc <- df3 %>% 
  select(overall_score_current, work_env_positive:teachers_trusted,
         any_symptoms_count, freq_symptoms_count) %>% 
  select(-contains("delta"))
cor(df_corr_soc)

ggcorrplot(round(cor(df_corr_soc), 1), method = "circle", type = "upper")

df_corr_soc_env <- df3 %>% 
  select(overall_score_current, work_env_positive:teachers_trusted,
         energy_efficiency:iaq,mean_orc,
         light_dissatisfied, sound_dissatisfied,
         air_qual_dissatisfied, environment_dissatisfied) %>% 
  select(-contains("delta"))
cor(df_corr_soc_env)

ggcorrplot(round(cor(df_corr_soc_env), 1), method = "circle", type = "upper")

df_corr_soc_ses <- df3 %>% 
  select(overall_score_current, work_env_positive:teachers_trusted,
         pct_reduced:pct_free_reduced) %>% 
  select(-contains("delta")) 
cor(df_corr_soc_ses)

ggcorrplot(round(cor(df_corr_soc_ses), 1), method = "circle", type = "upper")

df_corr_env_ses <- df3 %>% 
  select(energy_efficiency:iaq,mean_orc,
         light_dissatisfied, sound_dissatisfied,
         air_qual_dissatisfied, environment_dissatisfied,
         pct_reduced:pct_free_reduced)
cor(df_corr_env_ses)

ggcorrplot(round(cor(df_corr_env_ses), 1), method = "circle", type = "upper")

#' -----------------------------------------------------------------------------
#' Associations between ORC scores and number of health outcomes reported (count)
#' Associations between social climate and number of health outcomes reported (count)
#' "Two pollutant" models with ORC and social climate
#' 
#' The models include a random intercept for school
#' Using the scaled versions of the ORC scores for better model convergence
#' Covariates: job_teacher (binary), loc_classroom (binary), pct_free_reduced (continuous)
#' 
#' Primary findings:
#'     - After controlling for being a teacher and working in a classroom, 
#'     overall social climate score is associated with the number of frequent
#'     health outcomes reported
#'     - 
#' -----------------------------------------------------------------------------

library(lme4)
library(ggeffects)

#' Overall ORC and the number of symptoms reported (any)
plot(df3$scale_mean_orc, df3$any_symptoms_count)
orc_any_ct <- glmer(any_symptoms_count ~ scale_mean_orc + 
                      job_teacher + loc_classroom + pct_free_reduced + 
                      (1|school), 
                    family = poisson, data = df3)
summary(orc_any_ct)

#' Overall ORC and the number of symptoms reported (frequent)
plot(df3$scale_mean_orc, df3$freq_symptoms_count)
orc_freq_ct <- glmer(freq_symptoms_count ~ scale_mean_orc + 
                       job_teacher + loc_classroom + pct_free_reduced +
                       (1|school), 
                     family = poisson, data = df3)
summary(orc_freq_ct)

#' Overall social climate score and the number of symptoms reported (any)
plot(df3$scale_overall_score_current, df3$any_symptoms_count)
soc_any_ct <- glmer(any_symptoms_count ~ scale_overall_score_current + 
                      job_teacher + loc_classroom + pct_free_reduced + 
                      (1|school), 
                    family = poisson, data = df3)
summary(soc_any_ct)

#' Overall social climate score and the number of symptoms reported (frequent)
plot(df3$scale_overall_score_current, df3$any_symptoms_count)
soc_freq_ct <- glmer(freq_symptoms_count ~ scale_overall_score_current + 
                       job_teacher + loc_classroom + pct_free_reduced +
                       (1|school), 
                     family = poisson, data = df3)
summary(soc_freq_ct)

soc_freq_ct_pred <- ggeffect(soc_freq_ct, terms = c("scale_overall_score_current"), 
                             ci.lvl = 0.95)
head(soc_freq_ct_pred)
plot(soc_freq_ct_pred)

#' Overall ORC + SOC and the number of symptoms reported (frequent)
orc_soc_freq_ct <- glmer(freq_symptoms_count ~ scale_overall_score_current + 
                           scale_mean_orc + job_teacher + loc_classroom + pct_free_reduced +
                           (1|school), 
                         family = poisson, data = df3)
summary(orc_soc_freq_ct)

orc_soc_freq_ct_pred <- ggeffect(orc_soc_freq_ct, terms = c("scale_overall_score_current"), 
                                 ci.lvl = 0.95)
head(orc_soc_freq_ct_pred)
plot(orc_soc_freq_ct_pred)

#' Testing for an interaction between overall ORC + SOC
orc_soc_freq_ct_int <- glmer(freq_symptoms_count ~ scale_overall_score_current + 
                               scale_mean_orc + scale_overall_score_current*scale_mean_orc +
                               job_teacher + loc_classroom + pct_free_reduced +
                           (1|school), 
                         family = poisson, data = df3)
summary(orc_soc_freq_ct_int)

#' Are there differences for teachers and non-teachers?
orc_soc_freq_ct_pred_teach <- ggeffect(orc_soc_freq_ct, 
                                       terms = c("scale_overall_score_current", 
                                                 "job_teacher"), 
                                       ci.lvl = 0.95)
head(orc_soc_freq_ct_pred_teach)
plot(orc_soc_freq_ct_pred_teach)

#' Are there differences for classroom vs. non-classroom?
orc_soc_freq_ct_pred_croom <- ggeffect(orc_soc_freq_ct, 
                                       terms = c("scale_overall_score_current", 
                                                 "loc_classroom"), 
                                       ci.lvl = 0.95)
head(orc_soc_freq_ct_pred_croom)
plot(orc_soc_freq_ct_pred_croom)

#' Are there differences for high vs. low SES schools?
orc_soc_freq_ct_pred_ses <- ggeffect(orc_soc_freq_ct, 
                                     terms = c("scale_overall_score_current", 
                                               "pct_free_reduced[quart2]"), 
                                       ci.lvl = 0.95)
head(orc_soc_freq_ct_pred_ses)
plot(orc_soc_freq_ct_pred_ses)

#' Are there differences for high vs. low ORC schools?
orc_soc_freq_ct_pred_orc <- ggeffect(orc_soc_freq_ct, 
                                     terms = c("scale_overall_score_current", 
                                               "scale_mean_orc[quart2]"), 
                                     ci.lvl = 0.95)
head(orc_soc_freq_ct_pred_orc)
plot(orc_soc_freq_ct_pred_orc)
