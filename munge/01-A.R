##  01-A.R
## 
## Author Pikrides Filippos - 210229134

library(dplyr)

##  2nd set of data 
##   clean and assign to variables
data_list2 <- cyber.security.2_enrolments
enrolment2 <- data.frame(data_list2)
enrolment2 =na.omit(enrolment2)
enrolment2 <- enrolment2  [(!(enrolment2$gender  =="Unknown") & !(enrolment2$age_range  =="Unknown")),]
#head(df2)
activity2 =na.omit(cyber.security.2.step.activity)


## 3rd set of data 
##   clean and assign to variables
enrolment3 <-cyber.security.3_enrolments
enrolment3 <- data.frame(enrolment3)
enrolment3 =na.omit(enrolment3)
enrolment3 <- enrolment3  [(!(enrolment3$gender  =="Unknown") & !(enrolment3$age_range  =="Unknown")),]
 
activity3 =na.omit(cyber.security.1.step.activity)

 

####  4th set of data 
##   clean and assign to variables
cyber.security.4.archetype.survey.responses
leaving4 <- na.omit(cyber.security.4.leaving.survey.responses)
## str(cyber.security.4.leaving.survey.responses)
enrolment4All <- na.omit(cyber.security.4.enrolments)
enrolment4  <- enrolment4All
enrolment4 <- cyber.security.4.enrolments  [(!(cyber.security.4.enrolments$gender  =="Unknown") & !(cyber.security.4.enrolments$age_range  =="Unknown")),]
enrolment4 <- data.frame(enrolment4)
Questionresp4 <- cyber.security.4.question.response
activity4 <- cyber.security.4.step.activity
members4 <- na.omit(cyber.security.4.team.members)
video4 <- cyber.security.4.video.stats
responses4 <- cyber.security.4.weekly.sentiment.survey.responses



####  5th set of data 
##   clean and assign to variables
 
leaving5 <- na.omit(cyber.security.5.leaving.survey.responses)
## str(cyber.security.4.leaving.survey.responses)
enrolment5All <- na.omit(cyber.security.5.enrolments)
enrolment5 <- enrolment5All
enrolment5 <- cyber.security.5.enrolments  [(!(cyber.security.5.enrolments$gender  =="Unknown") & !(cyber.security.5.enrolments$age_range  =="Unknown")),]
enrolment5 <- data.frame(enrolment5)
Questionresp54 <- cyber.security.5.question.response
activit5 <- cyber.security.5.step.activity
 




###################### 
##   merge data 
## join the enrollments from  set of files  2,3,4
data23 = bind_rows(enrolment2,enrolment3)
data = bind_rows(data23,enrolment4)


## join the activities for set of data 2,3,4
activity23 =bind_rows(activity2,activity3)
activity   =bind_rows(activity23,activity4)
