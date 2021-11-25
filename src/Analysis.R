#install.packages("ProjectTemplate")
library("ProjectTemplate")
setwd("~/R/CSCFilProject")
load.project()

library(dplyr)
library(ggplot2)
library( dplyr )
 
###############################################################################
#############################  Analysis 4th set of files  ########################################
###############################################################################
#install.packages("tidyverse")
## cyber.security.4.archetype.survey.responses

## leaving4 <- na.omit(cyber.security.4.leaving.survey.responses)
## str(cyber.security.4.leaving.survey.responses)

## enrolments4 <- na.omit(cyber.security.4.enrolments)
## enrolments4 <- cyber.security.4.enrolments  [(!(cyber.security.4.enrolments$gender  =="Unknown") & !(cyber.security.4.enrolments$age_range  =="Unknown")),]

##  1.   Graph  from enrolment file Age_Range Vs Gender 
##########################################################
head(enrolment4)
#enrolments4 <- cyber.security.4.enrolments  [(!(cyber.security.4.enrolments$gender  =="Unknown") & !(cyber.security.4.enrolments$age_range  =="Unknown")),]
 

dfenrolment4 <- enrolment4 %>%
  dplyr::count(age = enrolment4$age_range, gender =enrolment4$gender, sort = TRUE)

#ggplot(data=dfenrolment4) + geom_point(aes(x=age  , y=n  ,colour=gender, size = 3  )) +
#  geom_text(aes(x=age,label = n,y=n), size = 3)  +
#  labs(title = "Enrolments regarding age/gender",
#       x = "Age-Range",
#       y = "Number of Genders")

#ggplot(dfenrolment4 ,aes(x = age, y = n   , fill = gender))+
#  geom_bar(stat="identity")+
#  geom_text(aes(label = n,y=n), size = 3) +
#  labs(title = "Enrolments regarding age/gender",
#       x = "Age-Range",
#       y = "Number of Ocurances")

ga = ggplot(dfenrolment4, aes(x=age, y=n, group=gender)) +
  geom_line(aes(color=gender))+
  geom_point(aes(color=gender)) +
labs(title = "Enrolments regarding age/gender",
     x = "Age-Range",
     y = "Number of Ocurances")
print(ga)



##Age_range vs Gender in Enrollments
dfageeducation4 <- enrolment4 %>%
  dplyr::count(age= enrolment4$age_range,education=enrolment4$highest_education_level, sort = TRUE) 

ggplot(data=dfageeducation4) + geom_point(aes(x=age , y=education  ,colour=age, size = 4 ))+
  geom_text(aes(x=age,label = n,y=education), size = 3)+
  labs(title = "Education per Age",
       x = "Age-Range",
       y = "highest_education_level")


##Graph of gender vs employment_area
dfemployment4 <- enrolment4 %>%
  dplyr::count(gender= enrolment4$gender,employment=enrolment4$employment_area, sort = TRUE) 
ggplot(data=dfemployment4) + geom_point(aes(x=gender , y=employment  ,colour=gender, size = 4))+
  geom_text(aes(x=gender,label = n,y=employment), size = 3)+
  labs(title = "Employment per Gender",
       x = "Gender",
       y = "Employment's")


##  graph  gender vs   country that enrol in a course 
table (data$country)

dfcountry4 <- enrolment4 %>%
  dplyr::count(gender= enrolment4$gender, country=enrolment4$country, sort = TRUE) 

dfcountry4 = head(dfcountry4,7)

#ggplot(data=dfcountry4) + geom_point(aes(x=gender , y=country  ,colour=gender, size = 4 ))+
#  geom_text(aes(x=gender,label = n,y=country), size = 3)+
#  labs(title = " 7 top countries Enrolments by Gender ",
#       x = "gender",
#       y = "countries")

ggplot(dfcountry4 ,aes(x = gender, y = n   , fill = country))+
  geom_bar(stat="identity")+
 # geom_text(aes(label = n), size = 3) +
  labs(title = "Enrolments regarding age/gender",
       x = "Age-Range",
       y = "Number of Ocurances")

## 2.   Activity  File    
################################  

 table(activity4$step)
#table(activity4$step_number )
#activity4 <- cyber.security.4.step.activity

dfsteps4 <- activity4 %>%
  dplyr::count(stepcount= str_sub(activity4$step,1,1) , sort = TRUE) 

#ggplot(dfsteps4 ,aes(x = stepcount     , y = n   , fill = stepcount        ))+
#  geom_bar(stat="identity")+
#  geom_text(aes(label = n,y=n), size = 3) +
#  labs(title = "Step Activity",
#       x = "Steps set ",
#       y = "Number of trials")
#

 
####
dfsteps44 <- activity4 %>%
  dplyr::count(step =  activity4$step  , sort = TRUE)

g = ggplot(data=dfsteps44, aes(x=step, y=n))
g1 = g + geom_point() + stat_smooth(linetype=2) +
  xlab("set of Steps") + ylab("Number of Trials in activiries")
print(g1)


#####  Another graph to see the duration of teh activities 

table(activity4$last_completed_at)
activity4Date <- subset(activity4, last_completed_at != "")
table(activity4Date$last_completed_at)

difftime(as.POSIXlt(activity4Date$last_completed_at),as.POSIXlt(activity4Date$first_visited_at), units = "mins")

dfstepduration <- data.frame(activity4Date ,difftime =difftime(as.POSIXlt(activity4Date$last_completed_at),as.POSIXlt(activity4Date$first_visited_at), units = "mins"))
head(dfstepduration)
glimpse(dfstepduration)
table(dfstepduration$step)



dfsteps4dueation <- dfstepduration %>%
  dplyr::count(step = str_sub(dfstepduration$step,1,1)  , difftime,sort = TRUE)
summary(dfsteps4dueation)


table(dfsteps4dueation$step)




# load dplyr library
library("dplyr")                            


par(mfrow=c(1,2))

### the average time oin minutes for each set to complete
dfstepdurationinmin   <- dfstepduration %>% group_by(step = str_sub(dfstepduration$step,1,1)) %>%
  summarise_at(vars(difftime),              # Specify column
               list(name =  mean ))  

dfstepdurationinmin$name <- round(dfstepdurationinmin$name)

ggplot(dfstepdurationinmin ,aes(x = step     , y = name  ,fill=step      ))+
  geom_bar(stat="identity")+
  geom_text(aes(x = step , label =name, y=name), size = 3) +
  labs(title = "Steps Duration",
       x = "Steps set ",
       y = "minutes")


###  total time in hours is spend to complete the sets s
dfstepdurationinmintotal   <- dfstepduration %>% group_by(step = str_sub(dfstepduration$step,1,1)) %>%
  summarise_at(vars(difftime),              # Specify column
               list(name =  sum ))  

dfstepdurationinmintotal$name <- round(dfstepdurationinmintotal$name/60)

ggplot(dfstepdurationinmintotal ,aes(x = step     , y = name  ,fill=step      ))+
  geom_bar(stat="identity")+
  geom_text(aes(x = step , label =name, y=name), size = 3) +
  labs(title = "Steps Duration total",
       x = "Steps set ",
       y = "hours")



par(mfrow=c(1,1))
 



## 3.   Graph  Quiz vs response  
################################
table(Questionresp4$quiz_question)

## show the correct/false answers 

dfQuestionresp4 <- Questionresp4 %>%
  dplyr::count(quiz= str_sub(Questionresp4$quiz_question, 1, 1),response=Questionresp4$correct, sort = TRUE) 

ggplot(data=dfQuestionresp4) + geom_point(aes(x=quiz  , y=n  ,colour=response, size = 4 ))+
  geom_text(aes(x=quiz ,label = n,y=n    ), size = 3)+
  labs(title = "Quastion/Response of quiz",
       x = "Quiz-Range",
       y = "Number of Trialsl")


## Merge 2 files to extract data regarding the age range and the wrong answers in Questions 

DF_QuestionAnswerJoin = merge(x=enrolment4,y=Questionresp4,by="learner_id")
#keep only those data with wrong answers
DF_QuestionAnswerJoin <- subset(DF_QuestionAnswerJoin, correct =="false")

dfQuestionrespFalse <- DF_QuestionAnswerJoin %>%
  dplyr::count(Age=  DF_QuestionAnswerJoin$age_range , sort = TRUE)


ggplot(dfQuestionrespFalse ,aes(x = Age     , y = n  ,fill=Age      ))+
  geom_bar(stat="identity")+
  geom_text(aes(x = Age , label =n, y=n), size = 3) +
  labs(title = "Incorrect answerd Per Age  ",
       x = "Age Range ",
       y = "No. Incorrect answers")


###  4  graph    Reson Leaving the course vs year
#table (leaving4$leaving_reason   )
#table (leaving4$leaving_reason ,  )

dfleavingReason4  <- leaving4 %>%
  dplyr:: count(LeftYear=  format(as.POSIXct(as.Date(leaving4$left_at ),format = "%m/%d/%Y %H:%M:%S") , format = "%Y")  , reason =leaving4$leaving_reason , sort = TRUE)

ggplot(dfleavingReason4 ,aes(x = LeftYear, y = reason  , fill = LeftYear , colour= LeftYear,size = 3   ))+
  #  geom_bar(stat="identity")+
  geom_text(aes(x=LeftYear,label = n,y=reason), size = 3) +
  labs(title = "Leaving Reason from Course",
       x = "Reason",
       y = "Year Left")

###############################
##more investigation
#cyber.security.4.enrolments  [ cyber.security.4.enrolments$learner_id  == "091df104-705f-4ee2-a67b-9d90043c4f56",]
#cyber.security.4.step.activity  [ cyber.security.4.step.activity$learner_id  == "091df104-705f-4ee2-a67b-9d90043c4f56",]
#cyber.security.4.question.response  [ cyber.security.4.question.response$learner_id  == "091df104-705f-4ee2-a67b-9d90043c4f56",]
#cyber.security.4.weekly.sentiment.survey.responses  [ cyber.security.4.weekly.sentiment.survey.responses$learner_id  == "091df104-705f-4ee2-a67b-9d90043c4f56",]

###########################################################################
##  end of basic analysis of 4th set of data 
###########################################################################


#############################################################################
##      Analysis with Multiple sets  2,3 ,4 
#############################################################################

glimpse(enrolment2)
glimpse(enrolment3)

## join the enrolments per week 
data23 = bind_rows(enrolment2,enrolment3)
data = bind_rows(data23,enrolment4)


## join the activities per week
activity23 =bind_rows(activity2,activity3)
activity   =bind_rows(activity23,activity4)
#class(activity)
#table(activity$learner_id)

## inner join the enrolmets and activities
DF_innerjoin = merge(x=data,y=activity,by="learner_id")
## to check the activituy for a specific learner
DF_innerjoin[DF_innerjoin$learner_id %in%  "feb41f66-257c-4839-a209-dd4e0f75ecf5" ,] 
 


##  cleansing of data - remove the unknown 
head(data)
nrow(data)
data <- data  [(!(data$gender  =="Unknown") & !(data$age_range  =="Unknown")),]
nrow(data)
head(data)
## Another findings of not valid data for statistical purposes 

###################################################################
##      1st plot  of many sets  -   Enrolments By Yea 
###################################################################
dfenrolments   <- data %>%
  dplyr::count(Enrolemts=  format(as.POSIXct(as.Date(data$enrolled_at ),format = "%m/%d/%Y %H:%M:%S") , format = "%Y") , sort = TRUE) 


ggplot(dfenrolments ,aes(x = Enrolemts, y = n   , fill = Enrolemts   ))+
  geom_bar(stat="identity")+
  geom_text(aes(label = n,y=n), size = 3) +
  labs(title = "Enrolments By Year",
       x = "Year",
       y = "Number of Enrolments")


 
##  durations of enrolemnet / unelrolment










## model of prediction 







 
