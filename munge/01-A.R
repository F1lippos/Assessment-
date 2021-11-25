# Example preprocessing script.
#cyber <-security.1_step-activity


library(dplyr)

##  2nd set of data 
data_list2 <- cyber.security.2_enrolments
enrolment2 <- data.frame(data_list2)
enrolment2 =na.omit(enrolment2)
enrolment2 <- enrolment2  [(!(enrolment2$gender  =="Unknown") & !(enrolment2$age_range  =="Unknown")),]
#head(df2)
activity2 =na.omit(cyber.security.2.step.activity)


## 3rd set of data 
enrolment3 <-cyber.security.3_enrolments
enrolment3 <- data.frame(enrolment3)
enrolment3 =na.omit(enrolment3)
enrolment3 <- enrolment3  [(!(enrolment3$gender  =="Unknown") & !(enrolment3$age_range  =="Unknown")),]
 
activity3 =na.omit(cyber.security.1.step.activity)

## nrow(activity3)
 
## activity2 <- cyber.security.2.step.activity
## activity1 <- cyber.security.1.step.activity

## Questionresp4 <- cyber.security.4.question.response

####  4th set of data 

cyber.security.4.archetype.survey.responses
leaving4 <- na.omit(cyber.security.4.leaving.survey.responses)
## str(cyber.security.4.leaving.survey.responses)
enrolment4 <- na.omit(cyber.security.4.enrolments)

enrolment4 <- cyber.security.4.enrolments  [(!(cyber.security.4.enrolments$gender  =="Unknown") & !(cyber.security.4.enrolments$age_range  =="Unknown")),]
enrolment4 <- data.frame(enrolment4)
Questionresp4 <- cyber.security.4.question.response
activity4 <- cyber.security.4.step.activity
members4 <- na.omit(cyber.security.4.team.members)
video4 <- cyber.security.4.video.stats
responses4 <- cyber.security.4.weekly.sentiment.survey.responses

######################333


#enrolment1 <- cyber.security.1.enrolments  [(!(cyber.security.1.enrolments$gender  =="Unknown") & !(cyber.security.1.enrolments$age_range  =="Unknown")),]
#enrolment1 <- data.frame(enrolment1)

#enrolment5 <- cyber.security.5.enrolments  [(!(cyber.security.5.enrolments$gender  =="Unknown") & !(cyber.security.5.enrolments$age_range  =="Unknown")),]
#enrolment5 <- data.frame(enrolment5)

#enrolment6 <- cyber.security.6.enrolments  [(!(cyber.security.6.enrolments$gender  =="Unknown") & !(cyber.security.6.enrolments$age_range  =="Unknown")),]
#enrolment6 <- data.frame(enrolment6)



####  7th set of data 

####  cyber.security.7.archetype.survey.responses
####  leaving7 <- na.omit(cyber.security.7.leaving.survey.responses)
## str(cyber.security.4.leaving.survey.responses)
####  enrolment7 <- na.omit(cyber.security.7.enrolments)

####  enrolment7 <- cyber.security.7.enrolments  [(!(cyber.security.7.enrolments$gender  =="Unknown") & !(cyber.security.7.enrolments$age_range  =="Unknown")),]
####  enrolment7 <- data.frame(enrolment7)
####  Questionresp7 <- cyber.security.7.question.response
####  activity7 <- cyber.security.7.step.activity
####  members7 <- na.omit(cyber.security.7.team.members)
####  video7 <- cyber.security.7.video.stats
####  responses7 <- cyber.security.7.weekly.sentiment.survey.responses


