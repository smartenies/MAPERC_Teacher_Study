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
#' For these, code a discrete variable where 1 is bad and 5 is good, 99 = Don't know
#' -----------------------------------------------------------------------------

#' Q6 How satisfied are you with the lighting in your classroom or work space?

table(q_data$Q6_1, useNA = "always") #' Natural light
table(q_data$Q6_2, useNA = "always") #' Electric light
table(q_data$Q6_3, useNA = "always") #' Overall light

q_data <- q_data %>% 
  mutate(light_natural_sat = ifelse(Q6_1 == "Extremely dissatisfied", 1, 
                             ifelse(Q6_1 == "Somewhat dissatisfied", 2, 
                             ifelse(Q6_1 == "Neither satisfied nor dissatisfied", 3,
                             ifelse(Q6_1 == "Somewhat satisfied", 4,
                             ifelse(Q6_1 == "Extremely satisfied", 5, NA)))))) %>% 
  mutate(light_electric_sat = ifelse(Q6_2 == "Extremely dissatisfied", 1, 
                              ifelse(Q6_2 == "Somewhat dissatisfied", 2, 
                              ifelse(Q6_2 == "Neither satisfied nor dissatisfied", 3,
                              ifelse(Q6_2 == "Somewhat satisfied", 4,
                              ifelse(Q6_2 == "Extremely satisfied", 5, NA)))))) %>% 
  mutate(light_overall_sat = ifelse(Q6_3 == "Extremely dissatisfied", 1, 
                             ifelse(Q6_3 == "Somewhat dissatisfied", 2, 
                             ifelse(Q6_3 == "Neither satisfied nor dissatisfied", 3,
                             ifelse(Q6_3 == "Somewhat satisfied", 4,
                             ifelse(Q6_3 == "Extremely satisfied", 5, NA))))))

table(q_data$Q6_1, q_data$light_natural_sat, useNA = "always") #' Natural light
table(q_data$Q6_2, q_data$light_electric_sat, useNA = "always") #' Electric light
table(q_data$Q6_3, q_data$light_overall_sat, useNA = "always") #' Overall light

#' Q9 Overall, does the lighting in your classroom or work space impact your ability to teach or work?
table(q_data$Q9, useNA = "always")

q_data <- q_data %>% 
  mutate(light_impact_work = ifelse(Q9 == "It makes it much harder to teach/work", 1, 
                             ifelse(Q9 == "It makes it somewhat harder to teach/work", 2, 
                             ifelse(Q9 == "It makes it neither easier nor harder to teach/work", 3,
                             ifelse(Q9 == "It makes it somewhat easier to teach/work", 4,
                             ifelse(Q9 == "It makes it much easier to teach/work", 5, 
                             ifelse(Q9 == "Don't know", 99, NA)))))))

table(q_data$Q9, q_data$light_impact_work, useNA = "always") #' Does light impact your work?

#' Q10 Which of the following lighting issues distract you, at least once a week, in your classroom or work space? 
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
  mutate(light_issues_other2 = light_issues_other) %>% 
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

#' Q11 Please share any other issues related to the lighting in your classroom or work space that are important to you
#' Going to include a health variable here
q_data$Q11[!is.na(q_data$Q11)]

q_data <- q_data %>% 
  mutate(light_issues_other3 = NA) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "Lack of natural light"),
                                      "Not enough daylight", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "headache"),
                                      "Symptoms", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "no way to dim the clasroom lights"),
                                      "Too bright", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "I miss the natural light"),
                                      "Not enough daylight", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "wear my glasses"),
                                      "Symptoms", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "no windows or natural light in my office"),
                                      "Not enough daylight", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "The noise is off-putting"),
                                      "Noise from lights", light_issues_other3)) %>%
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "sometimes hurt my eyes"),
                                      "Symptoms", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "hard to concentrate"),
                                      "Too much electric light", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "lights are not centered"),
                                      "Light placement", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "lack of natural lighting affects it"),
                                      "Not enough daylight", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "Too harsh on my eyes"),
                                      "Symptoms", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "wish there was more natural lighting or the ability"),
                                      "Not enough daylight,Too much electric light", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "both lights on is too much"),
                                      "Too bright", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "lighting sometimes makes noise that is sensitive"),
                                      "Symptoms", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "too bright with the natural light"),
                                      "Too bright", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "miss not having any windows"),
                                      "Too much natural light", light_issues_other3)) %>%
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "impacted by science lesson plans"),
                                      "Not enough naural light,Other", light_issues_other3)) %>%
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "Corners are darker"),
                                      "Light placement", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "no windows or an outside view at all"),
                                      "Not enough natural light", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "overhead lights sometimes feel too bright"),
                                      "Too bright", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "Too bright on one side of the room and too dark"),
                                      "Light placement", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "enough outlets in my room to use different lighting"),
                                      "Too bright,Symptoms,Not enough natural light", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "Lighting seems dark"),
                                      "Too dark", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "Migraines triggered"),
                                      "Symptoms", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "I cannot open the blinds on my 1 window"),
                                      "Not enough natural light", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "More natural light would be nice"),
                                      "Not enough natural light", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "No natural light. Floresent lighting"),
                                      "Not enough natural light", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "Lights hang loq and make it hard to use all of the wall"),
                                      "Light placement", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "There are no windows.  The lights are on or off"),
                                      "Not enough natural light", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "fluorescent and no natural lighting"),
                                      "Not enough natural light", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "only one window that looks out to an enclosed courtyard"),
                                      "Not enough natural light", light_issues_other3)) %>% 
  mutate(light_issues_other3 = ifelse(str_detect(Q11, "lights make a lot of noise when they are on"),
                                      "Noise from lights,Not enough natural light", light_issues_other3))

q_data <- q_data %>% 
  mutate(light_issues = paste(str_remove(light_issues_orig,"Other"), light_issues_other2, 
                              light_issues_other3, sep = ",")) %>% 
  mutate(light_issues = str_remove(light_issues, "NA"))
# View(select(q_data, light_issues_orig, light_issues_other, light_issues_other2, 
#             light_issues_other3, light_issues))

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
#' For these, code a discrete variable where 1 is bad and 5 is good, 99 = Don't know
#' -----------------------------------------------------------------------------

#' Q13 Overall, how satisfied are you with the acoustics in your classroom or work space? 

table(q_data$Q13, useNA = "always") #' Acoustics

q_data <- q_data %>% 
  mutate(sound_sat = ifelse(Q13 == "Extremely dissatisfied", 1, 
                     ifelse(Q13 == "Somewhat dissatisfied", 2, 
                     ifelse(Q13 == "Neither satisfied nor dissatisfied", 3,
                     ifelse(Q13 == "Somewhat satisfied", 4,
                     ifelse(Q13 == "Extremely satisfied", 5, NA))))))
table(q_data$Q13, q_data$sound_sat, useNA = "always")

#' Q14 How often do your students have trouble hearing you from the back of the classroom?
#' Of sometimes, often or all the time, yes
table(q_data$Q14, useNA = "always")

q_data <- q_data %>% 
  mutate(stud_trouble_hearing_freq = Q14) %>% 
  mutate(stud_trouble_hearing = ifelse(stud_trouble_hearing_freq %in% c("Sometimes", "Often", "All the time"), 1, 
                                ifelse(is.na(stud_trouble_hearing_freq), NA, 0)))
  
table(q_data$stud_trouble_hearing_freq, q_data$stud_trouble_hearing, useNA = "always")

#' Q15 Overall, does your students’ ability to hear in this classroom impact your ability to get through your lesson plans?
table(q_data$Q15, useNA = "always")

q_data <- q_data %>% 
  mutate(sound_impact_teaching = ifelse(Q15 == "It makes it much harder to teach", 1, 
                                 ifelse(Q15 == "It makes it somewhat harder to teach", 2, 
                                 ifelse(Q15 == "It makes it neither easier nor harder to teach", 3,
                                 ifelse(Q15 == "It makes it somewhat easier to teach", 4,
                                 ifelse(Q15 == "It makes it much easier to teach", 5, 
                                 ifelse(Q15 == "Don't know", 99, NA)))))))
table(q_data$Q15, q_data$sound_impact_teaching, useNA = "always")

#' Q17  Which of the following noises are loud or distract you, at least once a week, in your classroom or work space?
#' Create indicator variables if these issues are present
head(q_data$Q17)
# View(q_data$Q17)

q_data <- q_data %>% 
  mutate(sound_issues_orig = Q17,
         sound_issues_other = Q17_9_TEXT)
View(select(q_data, sound_issues_orig, sound_issues_other))
q_data$Q17_9_TEXT[!is.na(q_data$Q17_9_TEXT)]

#' Recode some of these participant supplied responses from the "other" category
#' and from question 18
q_data <- q_data %>% 
  mutate(sound_issues_other2 = sound_issues_other) %>% 
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

q_data$Q18[!is.na(q_data$Q18)]

q_data <- q_data %>% 
  mutate(sound_issues_other3 = NA) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "on the main hallway"),
                                      "Hallway noise", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "On occasion we hear 'stomping'"),
                                      "Noise from neighboring rooms", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "thin walls make it easy to hear other classrooms"),
                                      "Noise from neighboring rooms,Hallway noise", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "sound system in the next room"),
                                      "Noise from neighboring rooms", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "when it is loud in the halls"),
                                      "Hallway noise", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "classes next foor are listening to music"),
                                      "Noise from neighboring rooms", sound_issues_other3)) %>%
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "divider in my room that doesn't block out sound"),
                                      "Noise from neighboring rooms", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "borders the attendance office"),
                                      "Noise from neighboring rooms,Hallway noise,Mechanical noise", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "crying children in preschool"),
                                      "Noise from neighboring rooms", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "noise from the pod infront"),
                                      "Noise from neighboring rooms", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "voices coming from next door on both sides"),
                                      "Noise from neighboring rooms", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "can hear other classroons and it can be very distracting"),
                                      "Noise from neighboring rooms", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "oven and ventilation loud"),
                                      "Mechanical noise", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "noise from upstairs"),
                                      "Noise from neighboring rooms", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "do not have an accoustic amplifier"),
                                      "Accomodations", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "Large class sizes"),
                                      "Other", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "Speaker for smart board is centered"),
                                      "Other", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "upgrade allowing us to access the schools speaker system"),
                                      "Accomodations", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "noise carries from the hall and other classrooms"),
                                      "Noise from neighboring rooms,Hallway noise", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "classroom is across from the boys bathroom"),
                                      "Noise from neighboring rooms", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "Noise from the preschool"),
                                      "Noise from neighboring rooms", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "air conditioning vents warbled all last year"),
                                      "Mechanical noise", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "share a vent with the neighboring classroom"),
                                      "Noise from neighboring rooms", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "noise from the classroom next door to us is so distracting"),
                                      "Noise from neighboring rooms", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "gap between the nonfiction section of the library and a computer room"),
                                      "Mechanical noise", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "mechanical noise in this room is awful"),
                                      "Mechanical noise", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "mobile wall with another room"),
                                      "Noise from neighboring rooms", sound_issues_other3)) %>% 
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "is by a very busy hallway and it makes it difficult to instruct"),
                                      "Hallway noise", sound_issues_other3)) %>%
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "Banging and clanging from custodial offices"),
                                      "Maintenance", sound_issues_other3)) %>%
  mutate(sound_issues_other3 = ifelse(str_detect(Q18, "odd shape and the two main, biggest walls "),
                                      "Noise from neighboring rooms", sound_issues_other3))

q_data <- q_data %>% 
  mutate(sound_issues = paste(str_remove(sound_issues_orig,"Other"), sound_issues_other2, 
                              sound_issues_other3, sep = ",")) %>% 
  mutate(sound_issues = str_remove(sound_issues, "NA"))
# View(select(q_data, sound_issues_orig, sound_issues_other, sound_issues_other2,
#             sound_issues_other3, sound_issues))

q_data <- q_data %>% 
  mutate(sound_other_voices = ifelse(str_detect(sound_issues, "Other people's voices"), 1, 0),
         sound_traffic = ifelse(str_detect(sound_issues, "traffic"), 1, 0),
         sound_playground = ifelse(str_detect(sound_issues, "playground"), 1, 0),
         sound_maintenance = ifelse(str_detect(sound_issues, "Maintenance"), 1, 0),
         sound_hallway = ifelse(str_detect(sound_issues, "Hallway"), 1, 0),
         sound_other_rooms = ifelse(str_detect(sound_issues, "neighboring rooms"), 1, 0),
         sound_mechanical = ifelse(str_detect(sound_issues, "Mechanical"), 1, 0),
         sound_accomodations = ifelse(str_detect(sound_issues, "Accomodations"), 1, 0),
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
#' For these, code a discrete variable where 1 is bad and 5 is good, 99 = Don't know
#' -----------------------------------------------------------------------------



#' -----------------------------------------------------------------------------
#' Air quality
#' For these, code a discrete variable where 1 is bad and 5 is good, 99 = Don't know
#' -----------------------------------------------------------------------------

#' -----------------------------------------------------------------------------
#' Health and wellbeing
#' For these, code a discrete variable where 1 is bad and 5 is good, 99 = Don't know
#' -----------------------------------------------------------------------------
