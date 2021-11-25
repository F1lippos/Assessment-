# Example preprocessing script.
#cyber <-security.1_step-activity

library(dplyr)

####  2nd set of data 
data_list2 <- cyber.security.2_enrolments
enrolment2 <- data.frame(data_list2)
enrolment2 =na.omit(enrolment2)
df2 <- enrolment2  [(!(enrolment2$gender  =="Unknown") & !(enrolment2$age_range  =="Unknown")),]
activity2 =na.omit(cyber.security.2.step.activity)

#### 3rd set of data 
enrolment3 <-cyber.security.3_enrolments
enrolment3 <- data.frame(enrolment3)
enrolment3 =na.omit(enrolment3)
df3 <- enrolment3  [(!(enrolment3$gender  =="Unknown") & !(enrolment3$age_range  =="Unknown")),]

activity3 =na.omit(cyber.security.1.step.activity)

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
