#install.packages("ProjectTemplate")
library("ProjectTemplate")
setwd("~/R/CSCFilProject")
load.project()

## load Libraries

library(dplyr)
library(ggplot2)
library( dplyr )
 
###############################################################################
#############################  Analysis of the  4th set of files  #############
###############################################################################
 
## nomber of rows of enrlomnets file 
nrow(enrolment4)
glimpse(enrolment4)
head(enrolment4)
summary(enrolment4)
 

###################################################################################
## graph to show realtion between Age Range and gender in Enrolment procedure
###################################################################################
## using count function to group the age and gender 
dfenrolment4 <- enrolment4 %>%
  dplyr::count(age = enrolment4$age_range, gender =enrolment4$gender, sort = TRUE)

## plot using the bar 
ggplot(dfenrolment4 ,aes(x = age, y = n   , fill = gender))+
  geom_bar(stat="identity")+
  geom_text(aes(label = n,y=n), size = 3) +
  labs(title = "Enrolments regarding age/gender",
     x = "Age-Range",
       y = "Number of Ocurances")

##   plot using line 
ga = ggplot(dfenrolment4, aes(x=age, y=n, group=gender)) +
  geom_line(aes(color=gender))+
  geom_point(aes(color=gender)) +
labs(title = "Enrolments regarding age/gender",
     x = "Age-Range",
     y = "Number of Ocurances")
print(ga)


###################################################################################
## graph  showing relation  between Age Range and education  in Enrollment procedure
 
## Plot showing the relation AegRange and Eduction Level
dfageeducation4 <- enrolment4 %>%
  dplyr::count(age= enrolment4$age_range,education=enrolment4$highest_education_level, sort = TRUE) 

ggplot(data=dfageeducation4) + geom_point(aes(x=age , y=education  ,colour=age, size = 4 ))+
  geom_text(aes(x=age,label = n,y=education), size = 3)+
  labs(title = "Education per Age",
       x = "Age-Range",
       y = "highest_education_level")


## Plot showing the relation Gender  and Eduction Level
dfgendereducation4g <- enrolment4 %>%
  dplyr::count(gender= enrolment4$gender,education=enrolment4$highest_education_level, sort = TRUE) 

ggplot(data=dfgendereducation4g) + geom_point(aes(x=gender , y=education  ,colour=gender, size = 4 ))+
  geom_text(aes(x=gender,label = n,y=education), size = 3)+
  labs(title = "Education per gender",
       x = "gender",
       y = "highest_education_level")

###################################################################################
## graph  showing relation  with Employemnet Area   in Enrollment procedure


##   Plot showing the relation between gender vs employment_area
dfemployment4 <- enrolment4 %>%
  dplyr::count(gender= enrolment4$gender,employment=enrolment4$employment_area, sort = TRUE) 
ggplot(data=dfemployment4) + geom_point(aes(x=gender , y=employment  ,colour=gender, size = 4))+
  geom_text(aes(x=gender,label = n,y=employment), size = 3)+
  labs(title = "Employment per Gender",
       x = "Gender",
       y = "Employment's")


##group the employment_area
dfemployment4pie <- enrolment4 %>%
  dplyr::count( employment=enrolment4$employment_area, sort = TRUE) 

## keep the top 5 
dfemployment4pie <- head(dfemployment4pie,5)

library(ggplot2)
## plot the top 5 employemnet area in Pie

ggplot(dfemployment4pie, aes(x = "", y = n, fill = employment)) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")



###################################################################################
## graph  showing relation  with countries that interest for the course 
##  graph  gender vs   country that enrol in a course 
table (enrolment4$country)

dfcountry4 <- enrolment4 %>%
  dplyr::count(gender= enrolment4$gender, country=enrolment4$country, sort = TRUE) 

dfcountry4 = head(dfcountry4,7)

## plot showing the top countries regarding gender that participating in the course
ggplot(dfcountry4 ,aes(x = gender, y = n   , fill = country))+
  geom_bar(stat="identity")+
 # geom_text(aes(label = n), size = 3) +
  labs(title = "Enrolments regarding age/gender",
       x = "Age-Range",
       y = "Number of Ocurances")


###################################################################################
## 2.   Activity  File    
###################################################################################

head(activity4)
table(activity4$step)
## Each sexction (total 3, have sub-sections 1.1, 1.2, ..)


table(activity4$step_number )

## group the activities per section 
dfsteps4 <- activity4 %>%
  dplyr::count(stepcount= str_sub(activity4$step,1,1) , sort = TRUE) 

## plot showing the number of trials in the 3 sections of activities
ggplot(dfsteps4 ,aes(x = stepcount     , y = n   , fill = stepcount        ))+
  geom_bar(stat="identity")+
  geom_text(aes(label = n,y=n), size = 3) +
  labs(title = "Sectoin  Activity",
       x = "Sections of  Steps ",
       y = "Number of Activities")


 
#### plot showing the 3 Section  of Activities 
dfsteps44 <- activity4 %>%
  dplyr::count(step =  activity4$step  , sort = TRUE)

g = ggplot(data=dfsteps44, aes(x=step, y=n))
g1 = g + geom_point() + stat_smooth(linetype=2) +
  xlab("Sections of  Steps") + ylab("Number of ativiries")
print(g1)


#####  Plot to show the  average time and the total time for completed one section 
table(activity4$last_completed_at)
## Remove the rections that are running 
activity4Date <- subset(activity4, last_completed_at != "")
table(activity4Date$last_completed_at)

## iuse of function as.POSIXl to convert the str to date format
# difftime(as.POSIXlt(activity4Date$last_completed_at),as.POSIXlt(activity4Date$first_visited_at), units = "mins")
## using the function difftime to estimate the diference in minutes of the 2 dates
dfstepduration <- data.frame(activity4Date ,difftime =difftime(as.POSIXlt(activity4Date$last_completed_at),as.POSIXlt(activity4Date$first_visited_at), units = "mins"))
head(dfstepduration)
glimpse(dfstepduration)
## added another field ,difftime, keeping the duration in minutes
head(dfstepduration)


# load dplyr library
library("dplyr")                            

par(mfrow=c(1,2))

### the average time in minutes for each section to complete
dfstepdurationinmin   <- dfstepduration %>% group_by(step = str_sub(dfstepduration$step,1,1)) %>%
  summarise_at(vars(difftime),              # Specify column
               list(name =  mean ))  
## round duration
dfstepdurationinmin$name <- round(dfstepdurationinmin$name)

## plot showing the average time for each section to be completed 
ggplot(dfstepdurationinmin ,aes(x = step     , y = name  ,fill=step      ))+
  geom_bar(stat="identity")+
  geom_text(aes(x = step , label =name, y=name), size = 3) +
  labs(title = "Steps Duration",
       x = "Steps set ",
       y = "minutes")


##  total time in hours that  spend to complete each section 
##   You can see the time spend on activities for the 4th set of data 
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
 


###################################################################################
## 3.   Analysis of data regarding the questions vs response  
###################################################################################
table(Questionresp4$quiz_question)
head(Questionresp4)
## to check the question types of questions 
table(Questionresp4$question_type )

## group my data per scetion and response : correct/false answers 
dfQuestionresp4 <- Questionresp4 %>%
  dplyr::count(quiz= str_sub(Questionresp4$quiz_question, 1, 1),response=Questionresp4$correct, sort = TRUE) 
## plot showing the correct/false answers per section 
ggplot(data=dfQuestionresp4) + geom_point(aes(x=quiz  , y=n  ,colour=response, size = 4 ))+
  geom_text(aes(x=quiz ,label = n,y=n    ), size = 3)+
  labs(title = "Question/Response Quiz",
       x = "Quiz-Section ",
       y = "Number of Trialsl")

###################################################################################
## Merge 2 files to extract data regarding the age range and the wrong answers in Questions 
DF_QuestionAnswerJoin = merge(x=enrolment4,y=Questionresp4,by="learner_id")
#keep only those data with wrong answers
DF_QuestionAnswerJoin <- subset(DF_QuestionAnswerJoin, correct =="false")
## group the set of wrong answers by Age
dfQuestionrespFalse <- DF_QuestionAnswerJoin %>%
  dplyr::count(Age=  DF_QuestionAnswerJoin$age_range , sort = TRUE)

## plot the worong answer per age 
ggplot(dfQuestionrespFalse ,aes(x = Age     , y = n  ,fill=Age      ))+
  geom_bar(stat="identity")+
  geom_text(aes(x = Age , label =n, y=n), size = 3) +
  labs(title = "Incorrect answerd Per Age  ",
       x = "Age Range ",
       y = "No. Incorrect answers")


###################################################################################
## 4  graph    Reason for Leaving the course vs year 
###################################################################################

### 
table (leaving4$leaving_reason   )
#table (leaving4$leaving_reason ,  )

dfleavingReason4  <- leaving4 %>%
  dplyr:: count(LeftYear=  format(as.POSIXct(as.Date(leaving4$left_at ),format = "%m/%d/%Y %H:%M:%S") , format = "%Y")  , reason =leaving4$leaving_reason , sort = TRUE)

ggplot(dfleavingReason4 ,aes(x = LeftYear, y = reason  , fill = LeftYear , colour= LeftYear,size = 3   ))+
  #  geom_bar(stat="identity")+
  geom_text(aes(x=LeftYear,label = n,y=reason), size = 3) +
  labs(title = "Leaving Reason from Course",
       x = "Reason",
       y = "Year Left")



###################################################################################
## 5  graph   video vs viewed 
###################################################################################

video4 <- cyber.security.4.video.stats

table(video4$title) ##,video$step_position )



ggplot(video4 ,aes(x = title, y = total_views   , fill = title   ))+
  geom_bar(stat="identity")+
  geom_text(aes(label = total_views ,y=total_views ), size = 3) +
  labs(title = "Video ",
       x = "title",
       y = "Number of views")



###########################################################################
##  end of basic analysis of 4th set of data 
###########################################################################
#############################################################################
##      Analysis with Multiple sets  2,3 ,4 
#############################################################################

glimpse(enrolment2)
glimpse(enrolment3)

## join the enrollments from  set 
data23 = bind_rows(enrolment2,enrolment3)
data = bind_rows(data23,enrolment4)


## join the activities for set of data 2,3,4
activity23 =bind_rows(activity2,activity3)
activity   =bind_rows(activity23,activity4)
#class(activity)
#table(activity$learner_id)

## inner join the enrollments and activities by the learner 
DF_innerjoin = merge(x=data,y=activity,by="learner_id")
## to check the activity for a specific learner
DF_innerjoin[DF_innerjoin$learner_id %in%  "00750a19-05a8-4a3a-bcbf-4f0271673cf4" ,] 
 


##  cleansing of data - remove the unknown 
head(data)
nrow(data)
data <- data  [(!(data$gender  =="Unknown") & !(data$age_range  =="Unknown")),]
nrow(data)
head(data)
## Another findings of not valid data for statistical purposes 
nrow(cyber.security.4.enrolments)
nrow(enrolment4)
## check  detected_country

###################################################################
##      1st plot  of many sets  -   Enrollments By Year 

dfenrolments   <- data %>%
  dplyr::count(Enrolemts=  format(as.POSIXct(as.Date(data$enrolled_at ),format = "%m/%d/%Y %H:%M:%S") , format = "%Y") , sort = TRUE) 


ggplot(dfenrolments ,aes(x = Enrolemts    , y = n   , fill = Enrolemts   ))+
  geom_bar(stat="identity")+
  geom_text(aes(label = n,y=n), size = 3) +
  labs(title = "Enrolments By Year",
       x = "Year",
       y = "Number of Enrolments")


###################################################################
##    2nd duration of enrollment / unenrollment

## fit_1 <- lm(correct ~ question_number  , data = Questionresp4)

dataenrolduration <- data
## keep omnly the records that are completed 
dataenrolduration <- subset(dataenrolduration, unenrolled_at != "")
nrow(dataenrolduration)
## difftime(as.POSIXlt(dataenrolduration$unenrolled_at),as.POSIXlt(dataenrolduration$enrolled_at), units = "days")

dfdataenrolduration <- data.frame(dataenrolduration ,diffdays = difftime(as.POSIXlt(dataenrolduration$unenrolled_at),as.POSIXlt(dataenrolduration$enrolled_at), units = "days"))
head(dfdataenrolduration)
glimpse(dfdataenrolduration)
##  table(dfdataenrolduration$step)
summary(dfdataenrolduration$diffdays)

###  total time in hours is spend to complete the sets  
dfdataenroldurationall   <- dfdataenrolduration %>% group_by(role = role) %>%
  summarise_at(vars(diffdays),              # Specify column
               list(name =   mean ))  

dfdataenroldurationall$name <- round(dfdataenroldurationall$name )



###   111 days . So we can check with the current day if exist learners with much more than 111 days

dfdataenroldurationallsum   <- dfdataenrolduration %>% group_by(role = role) %>%
  summarise_at(vars(diffdays),              # Specify column
               list(name =   sum ))  

###################################################################
##   3rd  number of users per stpes

DF_innerjoin = merge(x=data,y=activity,by="learner_id")
## to check the activituy for a specific learner
DF_innerjoin[DF_innerjoin$learner_id %in%  "00750a19-05a8-4a3a-bcbf-4f0271673cf4" ,]

## I keep only the records/steps  that are running 
DF_innerjoinlearner  <- subset (DF_innerjoin,last_completed_at == "")
DF_innerjoinlearner[DF_innerjoinlearner$learner_id %in%  "1022e13a-177e-4b5d-9069-80b37f6137ac" ,]

-- step
stepcount   <- DF_innerjoinlearner %>%
  dplyr::count(step =  step , sort = TRUE)
## 
ggplot(stepcount ,aes(x = step, y = n   , fill = step   ))+
  geom_bar(stat="identity")+
  geom_text(aes(label = n,y=n), size = 3) +
  labs(title = "Enrolments By Year",
       x = "Year",
       y = "Number of Enrolments")




## model fitting  
 
