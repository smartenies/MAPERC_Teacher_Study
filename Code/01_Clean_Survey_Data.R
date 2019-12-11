#' -----------------------------------------------------------------------------
#' Date created: December 10, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Project: MAP-ERC Teacher's Study
#' 
#' Description: Clean teacher survey data from Qualtrix
#' -----------------------------------------------------------------------------

library(tidyverse)
library(readxl)

#' -----------------------------------------------------------------------------
#' Read in the Qultrix data (with the text responses, not the codes)
#' -----------------------------------------------------------------------------

d_path <- "R:/RSTOR-Magzamen/Research/Projects/EPA_Healthy_Schools/Raw_Data/"
d_name <- "CSU Adams 12 Indoor Environmental Quality Occupancy Survey_Choice_Text.csv"

q_data <- read_csv(paste0(d_path, d_name)) %>% 
  #' drop idenifiers
  select(-c(RecipientLastName:RecipientEmail, Q1)) %>% 
  #' drop first two lines (not participant data)
  slice(-c(1:2)) %>% 
  mutate(Finished = ifelse(Finished == "True", TRUE, FALSE))

#' How many participants started/completed the survey?
#' What percentage of particpants completed the survey?
nrow(q_data)
sum(q_data$Finished)

(sum(q_data$Finished) / nrow(q_data)) * 100 

#' What languages did the participants take the survey in?
table(q_data$UserLanguage, useNA = "always")

#' What schools are represented?
table(q_data$Q2, useNA = "always")

#' -----------------------------------------------------------------------------
#' Who are our participants?
#' -----------------------------------------------------------------------------

#' Q3 What is your position?

table(q_data$Q3, useNA = "always")
q_data$Q3_5_TEXT[!is.na(q_data$Q3_5_TEXT)]

#' Reclassify "other" jobs and create dummy variables
#' Preserve the original variables to go back later
q_data <- q_data %>% 
  mutate(job_cat_orig = Q3,
         job_cat_other = tolower(Q3_5_TEXT)) %>% 
  mutate(job_cat_other2 = ifelse(str_detect(job_cat_other, "office"), "Administrative Staff", job_cat_other)) %>%
  mutate(job_cat_other2 = ifelse(str_detect(job_cat_other, "psych|emotional|slp|interventionist"), "Counselor", job_cat_other2)) %>% 
  mutate(job_cat_other2 = ifelse(str_detect(job_cat_other, "base|monitor|site aide"), "Aide_Monitor", job_cat_other2)) %>%  
  mutate(job_cat_other2 = ifelse(str_detect(job_cat_other, "para|dlp"), "Paraeducator", job_cat_other2)) %>%
  mutate(job_cat_other2 = ifelse(str_detect(job_cat_other, "coach"), "Coach", job_cat_other2)) %>%
  mutate(job_cat_other2 = ifelse(str_detect(job_cat_other, "health|ha"), "Health_Aide", job_cat_other2)) %>%
  mutate(job_cat_other2 = ifelse(str_detect(job_cat_other, "media"), "Media_Tech", job_cat_other2)) %>%
  mutate(job_cat_other2 = ifelse(str_detect(job_cat_other, "custodian"), "Maintenance, operations, kitchen or janitorial staff", job_cat_other2))
# View(select(q_data, job_cat_orig, job_cat_other, job_cat_other2))
table(q_data$job_cat_other2, useNA = "always")

q_data <- q_data %>% 
  mutate(job_cat = ifelse(job_cat_orig == "Other", job_cat_other2, job_cat_orig)) %>% 
  mutate(job_cat = ifelse(job_cat == "Maintenance, operations, kitchen or janitorial staff", "Operations", job_cat)) %>% 
  mutate(job_cat = ifelse(job_cat == "Administrative Staff", "Admin", job_cat)) %>%
  mutate(job_cat = ifelse(job_cat_orig == "Other" & is.na(job_cat_other2), "Other_Not_Spec", job_cat)) %>% 
  mutate(job_cat = ifelse(is.na(job_cat_orig), "Missing", job_cat)) %>% 
  
  #' Create dummy variables for job types
  mutate(job_teacher = ifelse(job_cat == "Teacher", 1, 0),
         job_not_teacher = ifelse(job_cat == "Teacher", 0, 1),
         job_site_aide = ifelse(job_cat == "Aide_Monitor", 1, 0),
         job_coach = ifelse(job_cat == "Coach", 1, 0),
         job_counselor = ifelse(job_cat == "Counselor", 1, 0),
         job_health = ifelse(job_cat == "Health_Aide", 1, 0),
         job_media = ifelse(job_cat == "Media_Tech", 1, 0),
         job_operations = ifelse(job_cat == "Operations", 1, 0),
         job_paraed = ifelse(job_cat == "Paraeducator", 1, 0),
         job_other_not_spec = ifelse(job_cat_orig == "Other" & is.na(job_cat_other2), 1, 0),
         job_missing = ifelse(is.na(job_cat_orig), 1, 0))

# View(select(q_data, job_cat_orig, job_cat_other, job_cat:job_missing))

#' Where do our participants primarily work?
#' Dummy variables for locations
table(q_data$Q5, useNA = "always")

q_data <- q_data %>% 
  mutate(primary_loc = Q5) %>% 
  mutate(loc_clasroom = ifelse(primary_loc == "Classroom", 1, 0)) %>% 
  mutate(loc_cafeteria = ifelse(primary_loc == "Kitchen/Cafeteria", 1, 0)) %>% 
  mutate(loc_library = ifelse(primary_loc == "Library", 1, 0)) %>% 
  mutate(loc_no_primary = ifelse(primary_loc == "No primary space", 1, 0)) %>% 
  mutate(loc_office = ifelse(primary_loc == "Office", 1, 0)) %>% 
  mutate(loc_other = ifelse(primary_loc == "Other", 1, 0)) %>% 
  mutate(loc_missing = ifelse(is.na(primary_loc), 1, 0))

table(q_data$job_cat, q_data$primary_loc, useNA = "always")

#' -----------------------------------------------------------------------------
#' Lighting questions
#' -----------------------------------------------------------------------------

#' Q6 How satisfied are you with the lighting in your classroom or work space?
#' 5 is extremely satisfied, 1 = extremely dissatisfied

table(q_data$Q6_1, useNA = "always") #' Natural light
table(q_data$Q6_2, useNA = "always") #' Electric light
table(q_data$Q6_3, useNA = "always") #' Overall light

q_data <- q_data %>% 
  mutate(light_natural_sat_cat = ifelse(Q6_1 == "Extremely dissatisfied", 1, 
                                 ifelse(Q6_1 == "Somewhat dissatisfied", 2, 
                                 ifelse(Q6_1 == "Neither satisfied nor dissatisfied", 3,
                                 ifelse(Q6_1 == "Somewhat satisfied", 4,
                                 ifelse(Q6_1 == "Extremely satisfied", 5, NA)))))) %>% 
  mutate(light_electric_sat_cat = ifelse(Q6_2 == "Extremely dissatisfied", 1, 
                                  ifelse(Q6_2 == "Somewhat dissatisfied", 2, 
                                  ifelse(Q6_2 == "Neither satisfied nor dissatisfied", 3,
                                  ifelse(Q6_2 == "Somewhat satisfied", 4,
                                  ifelse(Q6_2 == "Extremely satisfied", 5, NA)))))) %>% 
  mutate(light_overall_sat_cat = ifelse(Q6_3 == "Extremely dissatisfied", 1, 
                                 ifelse(Q6_3 == "Somewhat dissatisfied", 2, 
                                 ifelse(Q6_3 == "Neither satisfied nor dissatisfied", 3,
                                 ifelse(Q6_3 == "Somewhat satisfied", 4,
                                 ifelse(Q6_3 == "Extremely satisfied", 5, NA))))))

table(q_data$Q6_1, q_data$light_natural_sat_cat, useNA = "always") #' Natural light
table(q_data$Q6_2, q_data$light_electric_sat_cat, useNA = "always") #' Electric light
table(q_data$Q6_3, q_data$light_overall_sat_cat, useNA = "always") #' Overall light

q_data$light_dissatisfied <- ifelse(q_data$light_overall_sat_cat %in% c(1, 2), 1, 0)
table(q_data$light_dissatisfied)

#' Q9 Overall, does the lighting in your classroom or work space impact your 
#' ability to teach or work?
table(q_data$Q9, useNA = "always")

q_data <- q_data %>% 
  mutate(light_impact_work_cat = ifelse(Q9 == "It makes it much harder to teach/work", 1, 
                                 ifelse(Q9 == "It makes it somewhat harder to teach/work", 2, 
                                 ifelse(Q9 == "It makes it neither easier nor harder to teach/work", 3,
                                 ifelse(Q9 == "It makes it somewhat easier to teach/work", 4,
                                 ifelse(Q9 == "It makes it much easier to teach/work", 5, 
                                 ifelse(Q9 == "Don't know", 99, NA)))))))

table(q_data$Q9, q_data$light_impact_work_cat, useNA = "always") #' How much does light impact your work?

q_data <- q_data %>% 
  mutate(light_impact_work = ifelse(light_impact_work_cat %in% c(1, 2), 1, 0))

table(q_data$Q9, q_data$light_impact_work, useNA = "always") #' Does light 
impact your work?

#' Q10 Which of the following lighting issues distract you, at least once a 
#' week, in your classroom or work space? 
#' Create indicator variables if these issues are present
head(q_data$Q10)
# View(q_data$Q10)

q_data <- q_data %>% 
  mutate(light_issues_orig = Q10,
         light_issues_other = Q10_12_TEXT)
# View(select(q_data, light_issues_orig, light_issues_other))
q_data$Q10_12_TEXT[!is.na(q_data$Q10_12_TEXT)]


#' Recode some of these participant supplied responses from the "other" category
#' and from question 11
q_data <- q_data %>% 
  mutate(light_issues_other2 = NA) %>% 
  mutate(light_issues_other2 = ifelse(str_detect(light_issues_other, "I don't have any windows, so there is no natural light"),
                                      "Not enough daylight,Other", light_issues_other2)) %>% 
  mutate(light_issues_other2 = ifelse(str_detect(light_issues_other, "perect lighting"),
                                      "Not applicable", light_issues_other2)) %>% 
  mutate(light_issues_other2 = ifelse(str_detect(light_issues_other, "No distractions"),
                                      "Not applicable", light_issues_other2)) %>% 
  mutate(light_issues_other2 = ifelse(str_detect(light_issues_other, "lighting is fine"),
                                      "Not applicable", light_issues_other2)) %>% 
  mutate(light_issues_other2 = ifelse(str_detect(light_issues_other, "have to keep blinds down"),
                                      "Too much daylight", light_issues_other2)) %>%
  mutate(light_issues_other2 = ifelse(str_detect(light_issues_other, "Buzzing/humming loud"),
                                      "Noise from lights", light_issues_other2)) %>%
  mutate(light_issues_other2 = ifelse(str_detect(light_issues_other, "noise from"),
                                      "Noise from lights", light_issues_other2)) %>%
  mutate(light_issues_other2 = ifelse(str_detect(light_issues_other, "lights turn off"),
                                      "Automatic timer", light_issues_other2)) %>% 
  mutate(light_issues_other2 = ifelse(str_detect(light_issues_other, "lights turning off"),
                                      "Automatic timer", light_issues_other2)) %>% 
  mutate(light_issues_other2 = ifelse(str_detect(light_issues_other, "Glare on Smart Board"),
                                      "Glare", light_issues_other2)) %>% 
  mutate(light_issues_other2 = ifelse(str_detect(light_issues_other, "Curtains can’t be opened all the way or it’s too hot/bright"),
                                      "Not enough daylight,Too bright", light_issues_other2)) %>% 
  mutate(light_issues_other2 = ifelse(str_detect(light_issues_other, "color is too harsh"),
                                      "Dislike color", light_issues_other2)) %>% 
  mutate(light_issues_other2 = ifelse(str_detect(light_issues_other, "Lights hang low"),
                                      "Light placement", light_issues_other2)) %>%
  mutate(light_issues_other2 = ifelse(str_detect(light_issues_other, "Placement of the lights"),
                                      "Light placement", light_issues_other2)) %>%
  mutate(light_issues_other2 = ifelse(str_detect(light_issues_other, "Total dark when lights out"),
                                      "Not enough daylight", light_issues_other2))
# View(select(q_data, light_issues_orig, light_issues_other, light_issues_other2))

q_data <- q_data %>% 
  mutate(light_issues = paste(str_remove(light_issues_orig,"Other"), light_issues_other2, sep = ",")) %>% 
  mutate(light_issues = str_remove(light_issues, "NA"))
# View(select(q_data, light_issues_orig, light_issues_other, light_issues_other2, light_issues))

q_data <- q_data %>% 
  mutate(light_too_dark = ifelse(str_detect(light_issues, "Too dark"), 1, 0),
         light_too_bright = ifelse(str_detect(light_issues, "Too bright"), 1, 0),
         light_not_enough_daylight = ifelse(str_detect(light_issues, "Not enough daylight"), 1, 0),
         light_too_much_daylight = ifelse(str_detect(light_issues, "Too much daylight"), 1, 0),
         light_not_enough_electric = ifelse(str_detect(light_issues, "Not enough electric"), 1, 0),
         light_too_much_electric = ifelse(str_detect(light_issues, "Too much electric"), 1, 0),
         light_flickers = ifelse(str_detect(light_issues, "flickers"), 1, 0),
         light_dislike_color = ifelse(str_detect(light_issues, "Dislike color"), 1, 0),
         light_shadows = ifelse(str_detect(light_issues, "Shadows"), 1, 0),
         light_glare = ifelse(str_detect(light_issues, "Glare"), 1, 0),
         light_placement = ifelse(str_detect(light_issues, "Light placement"), 1, 0),
         light_auto_shutoff = ifelse(str_detect(light_issues, "Automatic"), 1, 0),
         light_noise = ifelse(str_detect(light_issues, "Noise"), 1, 0),
         light_symptoms = ifelse(str_detect(light_issues, "Symptoms"), 1, 0),
         light_dont_know = ifelse(str_detect(light_issues, "Don't Know"), 1, 0),
         light_other_issue = ifelse(str_detect(light_issues, "Other"), 1, 0),
         light_na_issue = ifelse(str_detect(light_issues, "Not applicable"), 1, 0),
         light_issue_missing = ifelse(is.na(light_issues), 1, 0))

#' How many light issues are reported?
q_data <- q_data %>% 
  rowwise() %>% 
  mutate(light_issues_count = sum(light_too_bright, light_too_dark, 
                                  light_not_enough_daylight, light_too_much_daylight, 
                                  light_not_enough_electric, light_too_much_electric, 
                                  light_flickers, light_dislike_color,
                                  light_shadows, light_glare, light_placement, 
                                  light_auto_shutoff, light_noise, light_symptoms,
                                  light_dont_know, light_other_issue, na.rm = T))
# View(select(q_data, light_too_bright:light_issues_count))
summary(q_data$light_issues_count)

#' -----------------------------------------------------------------------------
#' Sound/acoustics
#' -----------------------------------------------------------------------------

#' Q13 Overall, how satisfied are you with the acoustics in your classroom or 
#' work space? 
#' 5 is extremely satisfied, 1 = extremely dissatisfied
table(q_data$Q13, useNA = "always") #' Acoustics

q_data <- q_data %>% 
  mutate(sound_sat_cat = ifelse(Q13 == "Extremely dissatisfied", 1, 
                         ifelse(Q13 == "Somewhat dissatisfied", 2, 
                         ifelse(Q13 == "Neither satisfied nor dissatisfied", 3,
                         ifelse(Q13 == "Somewhat satisfied", 4,
                         ifelse(Q13 == "Extremely satisfied", 5, NA))))))
table(q_data$Q13, q_data$sound_sat_cat, useNA = "always")

q_data$sound_dissatisfied <- ifelse(q_data$sound_sat_cat %in% c(1, 2), 1, 0)
table(q_data$sound_dissatisfied)

#' Q14 How often do your students have trouble hearing you from the back of the classroom?
#' Of sometimes, often or all the time, yes
table(q_data$Q14, useNA = "always")

q_data <- q_data %>% 
  mutate(stud_trouble_hearing_freq = Q14) %>% 
  mutate(stud_trouble_hearing = ifelse(stud_trouble_hearing_freq %in% c("Sometimes", "Often", "All the time"), 1, 
                                ifelse(is.na(stud_trouble_hearing_freq), NA, 0)))
  
table(q_data$stud_trouble_hearing_freq, q_data$stud_trouble_hearing, useNA = "always")

#' Q15 Overall, does your students’ ability to hear in this classroom impact 
#' your ability to get through your lesson plans?
#' 1 = much harder, 5 = much easier
table(q_data$Q15, useNA = "always")

q_data <- q_data %>% 
  mutate(sound_impact_work_cat = ifelse(Q15 == "It makes it much harder to teach", 1, 
                                 ifelse(Q15 == "It makes it somewhat harder to teach", 2, 
                                 ifelse(Q15 == "It makes it neither easier nor harder to teach", 3,
                                 ifelse(Q15 == "It makes it somewhat easier to teach", 4,
                                 ifelse(Q15 == "It makes it much easier to teach", 5, 
                                 ifelse(Q15 == "Don't know", 99, NA)))))))

table(q_data$Q15, q_data$sound_impact_work_cat, useNA = "always") 

q_data <- q_data %>% 
  mutate(sound_impact_work = ifelse(sound_impact_work_cat %in% c(1, 2), 1, 0))

table(q_data$Q15, q_data$sound_impact_work, useNA = "always") 

#' Q17  Which of the following noises are loud or distract you, at least once a 
#' week, in your classroom or work space?
#' Create indicator variables if these issues are present
head(q_data$Q17)
# View(q_data$Q17)

q_data <- q_data %>% 
  mutate(sound_issues_orig = Q17,
         sound_issues_other = Q17_9_TEXT)
# View(select(q_data, sound_issues_orig, sound_issues_other))
q_data$Q17_9_TEXT[!is.na(q_data$Q17_9_TEXT)]

#' Recode some of these participant supplied responses from the "other" category
#' and from question 18
q_data <- q_data %>% 
  mutate(sound_issues_other2 = NA) %>% 
  mutate(sound_issues_other2 = ifelse(str_detect(sound_issues_other, "noise from upstairs"),
                                      "Noise from neighboring rooms", sound_issues_other2)) %>% 
  mutate(sound_issues_other2 = ifelse(str_detect(sound_issues_other, "Noise from the room above me"),
                                      "Noise from neighboring rooms", sound_issues_other2)) %>% 
  mutate(sound_issues_other2 = ifelse(str_detect(sound_issues_other, "partition divider in the middle of my room doesn't close"),
                                      "Noise from neighboring rooms", sound_issues_other2)) %>% 
  mutate(sound_issues_other2 = ifelse(str_detect(sound_issues_other, "quiet room and another professional"),
                                      "Noise from neighboring rooms", sound_issues_other2)) %>% 
  mutate(sound_issues_other2 = ifelse(str_detect(sound_issues_other, "Outside noise maintenance of area"),
                                      "Maintenance", sound_issues_other2)) %>% 
  mutate(sound_issues_other2 = ifelse(str_detect(sound_issues_other, "cafeteria downstairs"),
                                      "Noise from neighboring rooms", sound_issues_other2)) %>% 
  mutate(sound_issues_other2 = ifelse(str_detect(sound_issues_other, "located in the Hallway"),
                                      "Hallway noise", sound_issues_other2)) %>% 
  mutate(sound_issues_other2 = ifelse(str_detect(sound_issues_other, "noise like leaf blowing"),
                                      "Maintenance", sound_issues_other2)) %>% 
  mutate(sound_issues_other2 = ifelse(str_detect(sound_issues_other, "the walls are made in a way that you can ear"),
                                      "Noise from neighboring rooms", sound_issues_other2)) %>% 
  mutate(sound_issues_other2 = ifelse(str_detect(sound_issues_other, "Bathroom noises"),
                                      "Noise from neighboring rooms", sound_issues_other2)) %>% 
  mutate(sound_issues_other2 = ifelse(str_detect(sound_issues_other, "Noise from other classrooms"),
                                      "Noise from neighboring rooms", sound_issues_other2)) %>% 
  mutate(sound_issues_other2 = ifelse(str_detect(sound_issues_other, "other teachers' sound systems"),
                                      "Noise from neighboring rooms", sound_issues_other2))

q_data <- q_data %>% 
  mutate(sound_issues = paste(str_remove(sound_issues_orig,"Other"), sound_issues_other2, sep = ",")) %>% 
  mutate(sound_issues = str_remove(sound_issues, "NA"))
# View(select(q_data, sound_issues_orig, sound_issues_other, sound_issues_other2, sound_issues))

q_data <- q_data %>% 
  mutate(sound_other_voices = ifelse(str_detect(sound_issues, "Other people's voices"), 1, 0),
         sound_traffic = ifelse(str_detect(sound_issues, "traffic"), 1, 0),
         sound_playground = ifelse(str_detect(sound_issues, "playground"), 1, 0),
         sound_maintenance = ifelse(str_detect(sound_issues, "Maintenance"), 1, 0),
         sound_hallway = ifelse(str_detect(sound_issues, "Hallway"), 1, 0),
         sound_other_rooms = ifelse(str_detect(sound_issues, "neighboring rooms"), 1, 0),
         sound_mechanical = ifelse(str_detect(sound_issues, "Mechanical"), 1, 0),
         sound_dont_know = ifelse(str_detect(sound_issues, "Don't Know"), 1, 0),
         sound_other_issue = ifelse(str_detect(sound_issues, "Other"), 1, 0),
         sound_na_issue = ifelse(str_detect(sound_issues, "Not applicable"), 1, 0),
         sound_issue_missing = ifelse(is.na(sound_issues), 1, 0))

#' How many sound issues are reported?
q_data <- q_data %>% 
  rowwise() %>% 
  mutate(sound_issues_count = sum(sound_other_voices, sound_traffic, sound_playground,
                                  sound_maintenance, sound_hallway, sound_other_rooms,
                                  sound_mechanical, sound_other_issue, na.rm = T))
# View(select(q_data, sound_other_voices:sound_issues_count))
summary(q_data$sound_issues_count)

#' -----------------------------------------------------------------------------
#' Thermal comfort
#' -----------------------------------------------------------------------------

#' Q20 Overall, how satisfied are you with the temperature in your classroom or 
#' work space?
#' 5 is extremely satisfied, 1 = extremely dissatisfied
table(q_data$Q20, useNA = "always") #' Acoustics

q_data <- q_data %>% 
  mutate(temp_sat_cat = ifelse(Q20 == "Extremely dissatisfied", 1, 
                        ifelse(Q20 == "Somewhat dissatisfied", 2, 
                        ifelse(Q20 == "Neither satisfied nor dissatisfied", 3,
                        ifelse(Q20 == "Somewhat satisfied", 4,
                        ifelse(Q20 == "Extremely satisfied", 5, NA))))))
table(q_data$Q20, q_data$temp_sat_cat, useNA = "always")

q_data$temp_dissatisfied <- ifelse(q_data$temp_sat_cat %in% c(1, 2), 1, 0)
table(q_data$temp_dissatisfied)

#' Q21 How hot or cold is your classroom or work space …
table(q_data$Q21_1, useNA = "always") #' When it is warm/hot outside?
table(q_data$Q21_2, useNA = "always") #' When it is cool/cold outside?

q_data <- q_data %>% 
  mutate(room_temp_hot_outside = Q21_1,
         room_temp_cold_outside = Q21_2)

#' Q22 Overall, does the temperature in your classroom or work space impact your 
#' ability to teach or work?
#' 1 = much harder, 5 = much easier
table(q_data$Q22, useNA = "always")

q_data <- q_data %>% 
  mutate(temp_impact_work_cat = ifelse(Q22 == "It makes it much harder to teach/work", 1, 
                                ifelse(Q22 == "It makes it somewhat harder to teach/work", 2, 
                                ifelse(Q22 == "It makes it neither easier nor harder to teach/work", 3,
                                ifelse(Q22 == "It makes it somewhat easier to teach/work", 4,
                                ifelse(Q22 == "It makes it much easier to teach/work", 5, 
                                ifelse(Q22 == "Don't know", 99, NA)))))))

table(q_data$Q22, q_data$temp_impact_work_cat, useNA = "always") 

q_data <- q_data %>% 
  mutate(temp_impact_work = ifelse(temp_impact_work_cat %in% c(1, 2), 1, 0))

table(q_data$Q22, q_data$temp_impact_work, useNA = "always") 

#' Q23 Which of the following temperature-related issues distract you, at least 
#' once a week, in your classroom or work space? (Check all that apply.)
#' Create indicator variables if these issues are present
head(q_data$Q23)
# View(q_data$Q23)

q_data <- q_data %>% 
  mutate(temp_issues_orig = Q23,
         temp_issues_other = Q23_11_TEXT)
# View(select(q_data, temp_issues_orig, temp_issues_other))
q_data$Q23_11_TEXT[!is.na(q_data$Q23_11_TEXT)]

#' Recode some of these participant supplied responses from the "other" category
#' and from question 24
q_data <- q_data %>% 
  mutate(temp_issues_other2 = NA) %>% 
  mutate(temp_issues_other2 = ifelse(str_detect(temp_issues_other, "now it is hotter and not as comfortable"),
                                      "Too warm", temp_issues_other2)) %>% 
  mutate(temp_issues_other2 = ifelse(str_detect(temp_issues_other, "Warm"),
                                     "Too warm", temp_issues_other2)) %>% 
  mutate(temp_issues_other2 = ifelse(str_detect(temp_issues_other, "not enough air conditioner"),
                                     "Too warm", temp_issues_other2)) %>% 
  mutate(temp_issues_other2 = ifelse(str_detect(temp_issues_other, "Sometimes, my classroom gets hot"),
                                     "Too warm", temp_issues_other2)) %>% 
  mutate(temp_issues_other2 = ifelse(str_detect(temp_issues_other, "unpredictable"),
                                     "Temperature changes between hot and cold", temp_issues_other2)) %>% 
  mutate(temp_issues_other2 = ifelse(str_detect(temp_issues_other, "Gets very cold"),
                                     "Too cold", temp_issues_other2)) %>% 
  mutate(temp_issues_other2 = ifelse(str_detect(temp_issues_other, "Building is not regulated at all"),
                                     "Temperature changes between hot and cold", temp_issues_other2)) %>% 
  mutate(temp_issues_other2 = ifelse(str_detect(temp_issues_other, "student bodies add to it"),
                                     "Too warm", temp_issues_other2)) %>% 
  mutate(temp_issues_other2 = ifelse(str_detect(temp_issues_other, "Certain places in my classroon are warmer"),
                                     "Too warm", temp_issues_other2)) %>% 
  mutate(temp_issues_other2 = ifelse(str_detect(temp_issues_other, "68 with air conditioner too cold"),
                                     "Too cold", temp_issues_other2))

# View(select(q_data, Q23, Q23_11_TEXT, Q24))
# q_data$Q24[!is.na(q_data$Q24)]

q_data <- q_data %>% 
  # mutate(temp_issues = paste(str_remove(temp_issues_orig,"Other"), 
  #                             temp_issues_other2, temp_issues_other3, sep = ",")) %>% 
  mutate(temp_issues = paste(str_remove(temp_issues_orig,"Other"), temp_issues_other2, sep = ",")) %>% 
  mutate(temp_issues = str_remove(temp_issues, "NA"))
# View(select(q_data, temp_issues_orig, temp_issues_other, temp_issues_other2,
#             temp_issues_other3, temp_issues))

q_data <- q_data %>% 
  mutate(temp_heat_from_sun = ifelse(str_detect(temp_issues, "Heat from sun"), 1, 0),
         temp_heat_from_tech = ifelse(str_detect(temp_issues, "Heat from equipment"), 1, 0),
         temp_drafts = ifelse(str_detect(temp_issues, "Drafts"), 1, 0),
         temp_swings = ifelse(str_detect(temp_issues, "changes between hot and cold"), 1, 0),
         temp_too_humid = ifelse(str_detect(temp_issues, "Too humid or damp"), 1, 0),
         temp_too_dry = ifelse(str_detect(temp_issues, "Too dry"), 1, 0),
         temp_air_movement = ifelse(str_detect(temp_issues, "Too much air movement"), 1, 0),
         temp_air_stillness = ifelse(str_detect(temp_issues, "Not enough air movement"), 1, 0),
         temp_dont_know = ifelse(str_detect(temp_issues, "Don't Know"), 1, 0),
         temp_other_issue = ifelse(str_detect(temp_issues, "Other"), 1, 0),
         temp_na_issue = ifelse(str_detect(temp_issues, "Not applicable"), 1, 0),
         temp_issue_missing = ifelse(is.na(temp_issues), 1, 0))

#' How many temp issues are reported?
q_data <- q_data %>% 
  rowwise() %>% 
  mutate(temp_issues_count = sum(temp_heat_from_sun, temp_heat_from_tech,
                                 temp_drafts, temp_swings, temp_too_humid,
                                 temp_too_dry, temp_air_movement, 
                                 temp_air_stillness, temp_other_issue, na.rm = T))
# View(select(q_data, temp_other_voices:temp_issues_count))
summary(q_data$temp_issues_count)

#' -----------------------------------------------------------------------------
#' Air quality
#' -----------------------------------------------------------------------------

#' Q26 Overall, how satisfied are you with the indoor air quality in your 
#' classroom or work space? Indoor air quality takes into account factors that 
#' affect how clean and comfortable to breathe the air is.
#' 1 = extremely dissatisfied, 5 = extremely satisfied
table(q_data$Q26, useNA = "always") #' Acoustics

q_data <- q_data %>% 
  mutate(air_qual_sat_cat = ifelse(Q26 == "Extremely dissatisfied", 1, 
                            ifelse(Q26 == "Somewhat dissatisfied", 2, 
                            ifelse(Q26 == "Neither satisfied nor dissatisfied", 3,
                            ifelse(Q26 == "Somewhat satisfied", 4,
                            ifelse(Q26 == "Extremely satisfied", 5, NA))))))
table(q_data$Q26, q_data$air_qual_sat_cat, useNA = "always")

q_data$air_qual_dissatisfied <- ifelse(q_data$air_qual_sat_cat %in% c(1, 2), 1, 0)
table(q_data$air_qual_dissatisfied)

#' Q27 How stuffy is the air in your classroom or work space?
#' 1 = extremely stuffy, 5 = not at all stuffy
table(q_data$Q27, useNA = "always") 

q_data <- q_data %>% 
  mutate(air_qual_stuffy_cat = ifelse(Q27 == "Extremely stuffy", 1,
                               ifelse(Q27 == "Very stuffy", 2,
                               ifelse(Q27 == "Stuffy", 3,
                               ifelse(Q27 == "A little stuffy", 4,
                               ifelse(Q27 == "Not at all stuffy", 5, NA))))))
table(q_data$Q27, q_data$air_qual_stuffy_cat, useNA = "always")

q_data$air_qual_stuffy <- ifelse(q_data$air_qual_stuffy_cat %in% c(1, 2, 3), 1, 0)
table(q_data$air_qual_stuffy, useNA = "always") 

#' How musty is your classroom or work space?
#' 1 = extremely musty, 5 = not at all musty
table(q_data$Q28, useNA = "always") 

q_data <- q_data %>% 
  mutate(air_qual_musty_cat = ifelse(Q28 == "Extremely musty", 1,
                                      ifelse(Q28 == "Very musty", 2,
                                             ifelse(Q28 == "Musty", 3,
                                                    ifelse(Q28 == "A little musty", 4,
                                                           ifelse(Q28 == "Not at all musty", 5, NA))))))
table(q_data$Q28, q_data$air_qual_musty_cat, useNA = "always")

q_data$air_qual_musty <- ifelse(q_data$air_qual_musty_cat %in% c(1, 2, 3), 1, 0)
table(q_data$air_qual_musty, useNA = "always") 

#' Q29   How satisfied are you with how your classroom or work space generally 
#' smells?  
#' 1 = extremely dissatisfied, 5 = extremely satisfied
table(q_data$Q29, useNA = "always")

q_data <- q_data %>% 
  mutate(air_smell_sat_cat = ifelse(Q29 == "Extremely dissatisfied", 1, 
                             ifelse(Q29 == "Somewhat dissatisfied", 2, 
                             ifelse(Q29 == "Neither satisfied nor dissatisfied", 3,
                             ifelse(Q29 == "Somewhat satisfied", 4,
                             ifelse(Q29 == "Extremely satisfied", 5, NA))))))
table(q_data$Q29, q_data$air_smell_sat_cat, useNA = "always")

q_data$air_smell_dissatisfied <- ifelse(q_data$air_smell_sat_cat %in% c(1, 2), 1, 0)
table(q_data$air_smell_dissatisfied)

#' Q30 Which of the following odors distract you, at least once a week, in your 
#' classroom or work space? (Check all that apply.)
#' Create indicator variables if these issues are present

head(q_data$Q30)
# View(q_data$Q30)

q_data <- q_data %>% 
  mutate(air_qual_issues_orig = Q30,
         air_qual_issues_other = Q30_14_TEXT)

View(select(q_data, air_qual_issues_orig, air_qual_issues_other))
q_data$Q30_14_TEXT[!is.na(q_data$Q30_14_TEXT)]

#' Recode some of these participant supplied responses from the "other" category
#' and from question 24
q_data <- q_data %>% 
  mutate(air_qual_issues_other2 = NA) %>% 
  mutate(air_qual_issues_other2 = ifelse(str_detect(air_qual_issues_other, "It just smells older"),
                                         "Other issue", air_qual_issues_other2)) %>% 
  mutate(air_qual_issues_other2 = ifelse(str_detect(air_qual_issues_other, "room where we change diapers"),
                                         "Students/Hygiene", air_qual_issues_other2)) %>% 
  mutate(air_qual_issues_other2 = ifelse(str_detect(air_qual_issues_other, "Unidentified smess"),
                                         "Other issue", air_qual_issues_other2)) %>% 
  mutate(air_qual_issues_other2 = ifelse(str_detect(air_qual_issues_other, "Mop bucket water"),
                                         "Cleaning products or residue", air_qual_issues_other2))

q_data <- q_data %>% 
  # mutate(air_qual_issues = paste(str_remove(air_qual_issues_orig,"Other"), 
  #                             air_qual_issues_other2, air_qual_issues_other3, sep = ",")) %>% 
  mutate(air_qual_issues = paste(str_remove(air_qual_issues_orig,"Other"), air_qual_issues_other2, sep = ",")) %>% 
  mutate(air_qual_issues = str_remove(air_qual_issues, "NA"))
# View(select(q_data, air_qual_issues_orig, air_qual_issues_other, air_qual_issues_other2,
#             air_qual_issues_other3, air_qual_issues))

q_data <- q_data %>% 
  mutate(air_qual_trash = ifelse(str_detect(air_qual_issues, "Trash containers"), 1, 0),
         air_qual_cleaning = ifelse(str_detect(air_qual_issues, "Cleaning products or residue"), 1, 0),
         air_qual_exhaust = ifelse(str_detect(air_qual_issues, "Car/bus exhaust"), 1, 0),
         air_qual_tobacco = ifelse(str_detect(air_qual_issues, "Tobacco smoke"), 1, 0),
         air_qual_printers = ifelse(str_detect(air_qual_issues, "Printers/Copiers"), 1, 0),
         air_qual_carpet = ifelse(str_detect(air_qual_issues, "Carpet"), 1, 0),
         air_qual_furniture = ifelse(str_detect(air_qual_issues, "Furniture"), 1, 0),
         air_qual_hygiene = ifelse(str_detect(air_qual_issues, "Students/hygiene"), 1, 0),
         air_qual_food = ifelse(str_detect(air_qual_issues, "Food"), 1, 0),
         air_qual_supplies = ifelse(str_detect(air_qual_issues, "School supplies"), 1, 0),
         air_qual_outside = ifelse(str_detect(air_qual_issues, "Other outside smells"), 1, 0),
         air_qual_dont_know = ifelse(str_detect(air_qual_issues, "Don't know"), 1, 0),
         air_qual_other_issue = ifelse(str_detect(air_qual_issues, "Other issue"), 1, 0),
         air_qual_na_issue = ifelse(str_detect(air_qual_issues, "Not applicable"), 1, 0),
         air_qual_issue_missing = ifelse(is.na(air_qual_issues), 1, 0))

#' How many air_qual issues are reported?
q_data <- q_data %>% 
  rowwise() %>% 
  mutate(air_qual_issues_count = sum(air_qual_trash, air_qual_cleaning, 
                                     air_qual_exhaust, air_qual_tobacco,
                                     air_qual_printers, air_qual_carpet, 
                                     air_qual_furniture, air_qual_hygiene,
                                     air_qual_food, air_qual_supplies,
                                     air_qual_outside,
                                     air_qual_other_issue, na.rm = T))
# View(select(q_data, air_qual_trash:air_qual_issues_count))
summary(q_data$air_qual_issues_count)

#' Q31 Do you clean your classroom or work space with your own cleaning products?
table(q_data$Q31, useNA = "always")

q_data$use_cleaning_prod <- ifelse(q_data$Q31 == "Yes", 1, 0)
table(q_data$use_cleaning_prod, useNA = "always")

#' Q32  What types of products do you use? (Check all that apply.)
#' Create dummy variables for these
View(select(q_data, Q31, Q32_1:Q32_7))
head(q_data$Q32_1, n = 20)

q_data <- q_data %>% 
  mutate(use_cleaner_surface_std = ifelse(str_detect(Q32_1, "Standard"), 1, 0),
         use_cleaner_surface_grn = ifelse(str_detect(Q32_1, "Green"), 1, 0),
         use_cleaner_allpurp_std = ifelse(str_detect(Q32_2, "Standard"), 1, 0),
         use_cleaner_allpurp_grn = ifelse(str_detect(Q32_2, "Green"), 1, 0),
         use_cleaner_window_std = ifelse(str_detect(Q32_3, "Standard"), 1, 0),
         use_cleaner_window_grn = ifelse(str_detect(Q32_3, "Green"), 1, 0),
         use_cleaner_abrasive_std = ifelse(str_detect(Q32_4, "Standard"), 1, 0),
         use_cleaner_abrasive_grn = ifelse(str_detect(Q32_4, "Green"), 1, 0),
         use_cleaner_degrease_std = ifelse(str_detect(Q32_5, "Standard"), 1, 0),
         use_cleaner_degrease_grn = ifelse(str_detect(Q32_5, "Green"), 1, 0),
         use_cleaner_bleach_std = ifelse(str_detect(Q32_6, "Standard"), 1, 0),
         use_cleaner_bleach_grn = ifelse(str_detect(Q32_6, "Green"), 1, 0))
View(select(q_data, Q31, Q32_1:Q32_7_TEXT, use_cleaner_surface_std:use_cleaner_bleach_grn))

#' Check out the "other" cleaners
q_data$Q32_7_TEXT <- tolower(q_data$Q32_7_TEXT)
q_data$Q32_7_TEXT[!is.na(q_data$Q32_7_TEXT)]

q_data$use_cleaner_other_std <- ifelse(q_data$Q31 == "No", NA, 0)
q_data$use_cleaner_other_grn <- ifelse(q_data$Q31 == "No", NA, 0)

q_data[grepl("clorox", q_data$Q32_7_TEXT) == T, "use_cleaner_bleach_std"] <- 1
q_data[grepl("lysol", q_data$Q32_7_TEXT) == T, "use_cleaner_surface_std"] <- 1
q_data[grepl("disinfective", q_data$Q32_7_TEXT) == T, "use_cleaner_surface_std"] <- 1
q_data[grepl("oxivir", q_data$Q32_7_TEXT) == T, "use_cleaner_allpurp_std"] <- 1
q_data[grepl("febreeze", q_data$Q32_7_TEXT) == T, "use_cleaner_other_std"] <- 1
q_data[grepl("rug cleaner", q_data$Q32_7_TEXT) == T, "use_cleaner_other_std"] <- 1
q_data[grepl("magic eraser", q_data$Q32_7_TEXT) == T, "use_cleaner_other_std"] <- 1
q_data[grepl("white board", q_data$Q32_7_TEXT) == T, "use_cleaner_other_std"] <- 1

View(select(q_data, Q31, Q32_1:Q32_7_TEXT, use_cleaner_surface_std:use_cleaner_other_std))

#' Q33  Do you have any classroom pets during any part of the year?
table(q_data$Q33, useNA = "always")

q_data$any_pets <- ifelse(q_data$Q33 == "Yes", 1, 0)
table(q_data$any_pets, useNA = "always")

q_data$Q34[!is.na(q_data$Q34)]
q_data$Q34_8_TEXT[!is.na(q_data$Q34_8_TEXT)]

#' Q35  Does your classroom or work space have any pest problems?
table(q_data$Q35, useNA = "always")

q_data$pest_problem <- ifelse(q_data$Q35 == "Yes", 1, 0)
table(q_data$pest_problem, useNA = "always")

#' 36 What kind of pests?
View(select(q_data, Q36))

#' Check out the "other" pests
q_data$Q36_7_TEXT[!is.na(q_data$Q36_7_TEXT)]
q_data$Q36_7_TEXT <- tolower(q_data$Q36_7_TEXT)

q_data <- q_data %>% 
  mutate(pests_ants = ifelse(str_detect(Q36, "Ants"), 1, 0),
         pests_bees = ifelse(str_detect(Q36, "Bees"), 1, 0),
         pests_mosquitos = ifelse(str_detect(Q36, "Mosquitos"), 1, 0),
         pests_termites = ifelse(str_detect(Q36, "Termites"), 1, 0),
         pests_roaches = ifelse(str_detect(Q36, "Roaches"), 1, 0),
         pests_rodents = ifelse(str_detect(Q36, "Mice"), 1, 0),
         pests_other = ifelse(str_detect(Q36, "Other"), 1, 0))
View(select(q_data, pest_problem:pests_other))

#' -----------------------------------------------------------------------------
#' Health and wellbeing
#' -----------------------------------------------------------------------------

#' Q42 During the last month you were at school, how often have you experienced 
#' each of these symptoms while in the school building?

table(q_data$Q42_1, useNA = "always") #' headaches
q_data$any_headaches <- ifelse(q_data$Q42_1 == "Not at all in last month", 0, 1)
q_data$freq_headaches <- ifelse(str_detect(q_data$Q42_1, "week"), 1, 0)
table(q_data$any_headaches, useNA = "always")
table(q_data$freq_headaches, useNA = "always")

table(q_data$Q42_2, useNA = "always") #' wheeze
q_data$any_wheeze <- ifelse(q_data$Q42_2 == "Not at all in last month", 0, 1)
q_data$freq_wheeze <- ifelse(str_detect(q_data$Q42_2, "week"), 1, 0)
table(q_data$any_wheeze, useNA = "always")
table(q_data$freq_wheeze, useNA = "always")

table(q_data$Q42_3, useNA = "always") #' eye_irritation
q_data$any_eye_irritation <- ifelse(q_data$Q42_3 == "Not at all in last month", 0, 1)
q_data$freq_eye_irritation <- ifelse(str_detect(q_data$Q42_3, "week"), 1, 0)
table(q_data$any_eye_irritation, useNA = "always")
table(q_data$freq_eye_irritation, useNA = "always")

table(q_data$Q42_4, useNA = "always") #' sore throat
q_data$any_sore_throat <- ifelse(q_data$Q42_4 == "Not at all in last month", 0, 1)
q_data$freq_sore_throat <- ifelse(str_detect(q_data$Q42_4, "week"), 1, 0)
table(q_data$any_sore_throat, useNA = "always")
table(q_data$freq_sore_throat, useNA = "always")

table(q_data$Q42_5, useNA = "always") #' sinus symptoms
q_data$any_sinus_symp <- ifelse(q_data$Q42_5 == "Not at all in last month", 0, 1)
q_data$freq_sinus_symp <- ifelse(str_detect(q_data$Q42_5, "week"), 1, 0)
table(q_data$any_sinus_symp, useNA = "always")
table(q_data$freq_sinus_symp, useNA = "always")

table(q_data$Q42_6, useNA = "always") #' sneezing
q_data$any_sneeze <- ifelse(q_data$Q42_6 == "Not at all in last month", 0, 1)
q_data$freq_sneeze <- ifelse(str_detect(q_data$Q42_6, "week"), 1, 0)
table(q_data$any_sneeze, useNA = "always")
table(q_data$freq_sneeze, useNA = "always")

table(q_data$Q42_7, useNA = "always") #' eye_strain
q_data$any_eye_strain <- ifelse(q_data$Q42_7 == "Not at all in last month", 0, 1)
q_data$freq_eye_strain <- ifelse(str_detect(q_data$Q42_7, "week"), 1, 0)
table(q_data$any_eye_strain, useNA = "always")
table(q_data$freq_eye_strain, useNA = "always")

table(q_data$Q42_8, useNA = "always") #' tension, irritability, nervousness
q_data$any_tension <- ifelse(q_data$Q42_8 == "Not at all in last month", 0, 1)
q_data$freq_tension <- ifelse(str_detect(q_data$Q42_8, "week"), 1, 0)
table(q_data$any_tension, useNA = "always")
table(q_data$freq_tension, useNA = "always")

table(q_data$Q42_9, useNA = "always") #' lack of attention
q_data$any_lack_atten <- ifelse(q_data$Q42_9 == "Not at all in last month", 0, 1)
q_data$freq_lack_atten <- ifelse(str_detect(q_data$Q42_9, "week"), 1, 0)
table(q_data$any_lack_atten, useNA = "always")
table(q_data$freq_lack_atten, useNA = "always")

table(q_data$Q42_10, useNA = "always") #' dizziness/lightheadedness
q_data$any_dizziness <- ifelse(q_data$Q42_10 == "Not at all in last month", 0, 1)
q_data$freq_dizziness <- ifelse(str_detect(q_data$Q42_10, "week"), 1, 0)
table(q_data$any_dizziness, useNA = "always")
table(q_data$freq_dizziness, useNA = "always")

table(q_data$Q42_11, useNA = "always") #' nausea
q_data$any_nausea <- ifelse(q_data$Q42_11 == "Not at all in last month", 0, 1)
q_data$freq_nausea <- ifelse(str_detect(q_data$Q42_11, "week"), 1, 0)
table(q_data$any_nausea, useNA = "always")
table(q_data$freq_nausea, useNA = "always")

table(q_data$Q42_12, useNA = "always") #' feeling depressed
q_data$any_depression <- ifelse(q_data$Q42_12 == "Not at all in last month", 0, 1)
q_data$freq_depression <- ifelse(str_detect(q_data$Q42_12, "week"), 1, 0)
table(q_data$any_depression, useNA = "always")
table(q_data$freq_depression, useNA = "always")

table(q_data$Q42_13, useNA = "always") #' feeling lethargic
q_data$any_lethargy <- ifelse(q_data$Q42_13 == "Not at all in last month", 0, 1)
q_data$freq_lethargy <- ifelse(str_detect(q_data$Q42_13, "week"), 1, 0)
table(q_data$any_lethargy, useNA = "always")
table(q_data$freq_lethargy, useNA = "always")

#' How many symptoms were reported?
#' How many frequent symptoms were reported?
q_data <- q_data %>% 
  rowwise() %>% 
  mutate(any_symptoms_count = sum(any_headaches, any_wheeze, any_eye_irritation,
                                  any_sore_throat, any_sinus_symp, any_sneeze,
                                  any_eye_strain, any_tension, any_lack_atten,
                                  any_dizziness, any_nausea, any_depression,
                                  any_lethargy, na.rm = T)) %>% 
  mutate(freq_symptoms_count = sum(freq_headaches, freq_wheeze, freq_eye_irritation,
                                   freq_sore_throat, freq_sinus_symp, freq_sneeze,
                                   freq_eye_strain, freq_tension, freq_lack_atten,
                                   freq_dizziness, freq_nausea, freq_depression,
                                   freq_lethargy, na.rm = T))
View(select(q_data, any_headaches:any_lethargy, freq_symptoms_count))
summary(q_data$any_symptoms_count)
summary(q_data$freq_symptoms_count)

#' Q43 How much do you agree with each of the following statements?: On a 
#' typical school day, when I leave school, I feel...
#' Options are strongly agree to strongly disagree
#' Binary variables where somewhat agree and strongly agree == Yes

table(q_data$Q43_1, useNA = "always")
q_data$leave_confident <- ifelse(str_detect(q_data$Q43_1, "Strongly agree") | 
                                   str_detect(q_data$Q43_1, "Somewhat agree"), 1, 0)
table(q_data$leave_confident, useNA = "always")

table(q_data$Q43_2, useNA = "always")
q_data$leave_exhausted <- ifelse(str_detect(q_data$Q43_2, "Strongly agree") | 
                                   str_detect(q_data$Q43_2, "Somewhat agree"), 1, 0)
table(q_data$leave_exhausted, useNA = "always")

table(q_data$Q43_3, useNA = "always")
q_data$leave_agitated <- ifelse(str_detect(q_data$Q43_3, "Strongly agree") | 
                                   str_detect(q_data$Q43_3, "Somewhat agree"), 1, 0)
table(q_data$leave_agitated, useNA = "always")

table(q_data$Q43_4, useNA = "always")
q_data$leave_rejuvenated <- ifelse(str_detect(q_data$Q43_4, "Strongly agree") | 
                                   str_detect(q_data$Q43_4, "Somewhat agree"), 1, 0)
table(q_data$leave_rejuvenated, useNA = "always")

table(q_data$Q43_5, useNA = "always")
q_data$leave_sad <- ifelse(str_detect(q_data$Q43_5, "Strongly agree") | 
                                     str_detect(q_data$Q43_5, "Somewhat agree"), 1, 0)
table(q_data$leave_sad, useNA = "always")

#' Q39 Overall, how satisfied are you with the environment of your classroom or 
#' work space?
#' 1 = extremely disatisfied, 5 = extremely satisfied
table(q_data$Q39, useNA = "always")

q_data <- q_data %>% 
  mutate(environment_sat_cat = ifelse(Q39 == "Extremely dissatisfied", 1, 
                               ifelse(Q39 == "Somewhat dissatisfied", 2, 
                               ifelse(Q39 == "Neither satisfied nor dissatisfied", 3,
                               ifelse(Q39 == "Somewhat satisfied", 4,
                               ifelse(Q39 == "Extremely satisfied", 5, NA))))))
table(q_data$Q39, q_data$environment_sat_cat, useNA = "always")

q_data$environment_dissatisfied <- ifelse(q_data$environment_sat_cat %in% c(1, 2), 1, 0)
table(q_data$environment_dissatisfied)



