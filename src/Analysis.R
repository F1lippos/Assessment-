##  Analysis.R
## 
## Author Pikrides Filippos - 210229134

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
 
## number of rows of enrollments file 
nrow(enrolment4)
glimpse(enrolment4)
head(enrolment4)
summary(enrolment4)
 

###################################################################################
## graph to show relation between Age Range and gender in Enrollment procedure
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
 
dfageeducation4 <- enrolment4 %>%
  dplyr::count(age= enrolment4$age_range,education=enrolment4$highest_education_level, sort = TRUE) 

ggplot(data=dfageeducation4) + geom_point(aes(x=age , y=education  ,colour=age, size = 4 ))+
  geom_text(aes(x=age,label = n,y=education), size = 3)+
  labs(title = "Education per Age",
       x = "Age-Range",
       y = "highest_education_level")

###################################################################################
## graph showing Education per  gender
dfgendereducation4g <- enrolment4 %>%
  dplyr::count(gender= enrolment4$gender,education=enrolment4$highest_education_level, sort = TRUE) 

ggplot(data=dfgendereducation4g) + geom_point(aes(x=gender , y=education  ,colour=gender, size = 4 ))+
  geom_text(aes(x=gender,label = n,y=education), size = 3)+
  labs(title = "Education per gender",
       x = "gender",
       y = "highest_education_level")

###################################################################################
## graph  showing relation  with Employemnet Area in Enrollment procedure


## Plot showing the relation between gender vs employment_area
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
## plot the top 5 employment area in Pie
ggplot(dfemployment4pie, aes(x = "", y = n, fill = employment)) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")



###################################################################################
## graph  showing relation  with countries that interest for the course 
##  graph  gender vs   country that enroll in a course 
table (enrolment4$country)

dfcountry4 <- enrolment4 %>%
  dplyr::count(gender= enrolment4$gender, country=enrolment4$country, sort = TRUE) 

dfcountry4 = head(dfcountry4,7)

## plot showing the top countries regarding gender that participating in the course
ggplot(dfcountry4 ,aes(x = gender, y = n   , fill = country))+
  geom_bar(stat="identity")+
 # geom_text(aes(label = n), size = 3) +
  labs(title = "Enrolments regarding gender Country",
       x = "Gender",
       y = "Number of Ocurances")


## grpah to display teetcted countries . This file is witthout cleansing 
dfcountry4CY <- cyber.security.4.enrolments %>%
  dplyr::count(  country=cyber.security.4.enrolments$detected_country, sort = TRUE) 

dfcountry4CY = head(dfcountry4CY,7)

## plot showing the top countries regarding that participating in the course
ggplot(dfcountry4CY ,aes(x = country    , y = n   , fill = country))+
  geom_bar(stat="identity")+
   geom_text(aes(label = n), size = 3) +
  labs(title = "Enrolments regarding Detected country",
       x = "country",
       y = "Number of  Enrollments")



###################################################################################
## 2.   Activity  File     -- Activities of learners
## Each section (total 3, have sub-sections 1.1, 1.2, ..)
###################################################################################

## some basiv info related with activities
head(activity4)
table(activity4$step)

table(activity4$step_number )

## group the activities per section using the str_sub function
dfsteps4 <- activity4 %>%
  dplyr::count(stepcount= str_sub(activity4$step,1,1) , sort = TRUE) 

## plot showing the number of trials in the 3 sections of activities
ggplot(dfsteps4 ,aes(x = stepcount     , y = n   , fill = stepcount        ))+
  geom_bar(stat="identity")+
  geom_text(aes(label = n,y=n), size = 3) +
  labs(title = "Section  Activity",
       x = "Sections of  Steps ",
       y = "Number of Activities")


 
#### plot showing the 3 Section  of Activities 
dfsteps44 <- activity4 %>%
  dplyr::count(step =  activity4$step  , sort = TRUE)

g = ggplot(data=dfsteps44, aes(x=step, y=n))
g1 = g + geom_point() + stat_smooth(linetype=2) +
  xlab("Sections of  Steps") + ylab("Number of activities")
print(g1)



geom_smooth(formula = y ~ x, method = "lm")
#####  Plot to show the  average time and the total time for completed one section 
table(activity4$last_completed_at)
## Remove the rections that are running 
activity4Date <- subset(activity4, last_completed_at != "")
table(activity4Date$last_completed_at)

## use of function as.POSIXl to convert the str to date format
 
dfstepduration <- data.frame(activity4Date ,difftime =difftime(as.POSIXlt(activity4Date$last_completed_at),as.POSIXlt(activity4Date$first_visited_at), units = "mins"))
head(dfstepduration)
glimpse(dfstepduration)
## added another field ,difftime, keeping the duration in minutes
head(dfstepduration)



par(mfrow=c(1,2))

### the average time in minutes for each section to complete
dfstepdurationinmin   <- dfstepduration %>% group_by(step = str_sub(dfstepduration$step,1,1)) %>%
  summarise_at(vars(difftime),              # Specify column
               list(name =  mean ))  
## round duration
dfstepdurationinmin$name <- round(dfstepdurationinmin$name)
dfstepdurationinmin$name <- as.numeric(dfstepdurationinmin$name)

## plot showing the average time for each section to be completed 
ggplot(dfstepdurationinmin ,aes(x = step     , y = name  ,fill=step      ))+
  geom_bar(stat="identity")+
  geom_text(aes(x = step , label =name, y=name), size = 3) +
  labs(title = "Steps Duration",
       x = "Section set ",
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

## plot the wrong answer per age 
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
       x = "Year Left",
       y = "Reason")



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

## join the enrollments from  set of files  2,3,4
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

## Assign all the data of enrollments into dataAllenrolments
dataAllenrolments  <- data 

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
##      1st plot  of many sets  -   
## Enrollments By Year to check the trend This is based on the files that are loaded 


## format the enroll_at field 
dfenrolments   <- dataAllenrolments %>%
  dplyr::count(Enrolemts=  format(as.POSIXct(as.Date(dataAllenrolments$enrolled_at ),format = "%m/%d/%Y %H:%M:%S") , format = "%Y") , sort = TRUE) 


ggplot(dfenrolments ,aes(x = Enrolemts    , y = n   , fill = Enrolemts   ))+
  geom_bar(stat="identity")+
  geom_text(aes(label = n,y=n), size = 3) +
  labs(title = "Enrolments By Year",
       x = "Year",
       y = "Number of Enrolments")


##   i get a subset of data . Those who enroll in 2017
data2017 <- subset(dataAllenrolments,format(as.POSIXct(as.Date(dataAllenrolments$enrolled_at ),format = "%m/%d/%Y %H:%M:%S") , format = "%Y") == "2017")
data2017$enrolled_at <- as.numeric(format(as.POSIXct(as.Date(data2017$enrolled_at ),format = "%m/%d/%Y %H:%M:%S") , format = "%m" ))

## trend of enrollments by Month for the year 2017
table(data2017$enrolled_at)

 

Enrolments2017 <-   data2017 %>%
  dplyr::count(Enrolemts=  data2017$enrolled_at  , sort = TRUE) 


ggplot(Enrolments2017 ,aes(x =  Enrolemts   , y=n     , fill =  Enrolemts   ))+
  geom_bar(stat="identity")+
  geom_text(aes(x=Enrolemts,label = n ), size = 4) +
  labs(title = "Enrolments   by month of Year 2017",
       x = "Month",
       y = "Number of Enrolments")



###################################################################
##    2nd plot  of merging many  sets  - 
##    Duration of enrollment / unenrollment

 
dataenrolduration <- dataAllenrolments
## keep only the records that are completed 
dataenrolduration <- subset(dataenrolduration, unenrolled_at != "")
nrow(dataenrolduration)
## difftime(as.POSIXlt(dataenrolduration$unenrolled_at),as.POSIXlt(dataenrolduration$enrolled_at), units = "days")
## create a new data frame with a new field diffdays , where is the days of the course for each learner
dfdataenrolduration <- data.frame(dataenrolduration ,diffdays = difftime(as.POSIXlt(dataenrolduration$unenrolled_at),as.POSIXlt(dataenrolduration$enrolled_at), units = "days"))
head(dfdataenrolduration)
glimpse(dfdataenrolduration)
##  table(dfdataenrolduration$step)
summary(dfdataenrolduration$diffdays)

##  total time in hours is spend to complete the sets  
dfdataenroldurationall   <- dfdataenrolduration %>% group_by(role = role) %>%
  summarise_at(vars(diffdays),              # Specify column
               list(name =   mean ))  

dfdataenroldurationall$name <- round(dfdataenroldurationall$name )



##  Average duration is  111 days . 
## So we can check with the current day if exist learners with much more than 111 days

dfdataenroldurationallsum   <- dfdataenrolduration %>% group_by(role = role) %>%
  summarise_at(vars(diffdays),              # Specify column
               list(name =   sum ))  

###################################################################
##   3rd plot  of many sets  
##   number of users per steps

DF_innerjoin = merge(x=data,y=activity,by="learner_id")
## to check the activity for a specific learner
DF_innerjoin[DF_innerjoin$learner_id %in%  "00750a19-05a8-4a3a-bcbf-4f0271673cf4" ,]

## I keep only the records/steps  that are running 
DF_innerjoinlearner  <- subset (DF_innerjoin,last_completed_at == "")
DF_innerjoinlearner[DF_innerjoinlearner$learner_id %in%  "9496898a-7aa4-4b71-ab91-d10c4bd53375" ,]

#step
stepcount   <- DF_innerjoinlearner %>%
  dplyr::count(step =  step , sort = TRUE)
## 
ggplot(stepcount ,aes(x = step, y = n   , fill = step   ))+
  geom_bar(stat="identity")+
  geom_text(aes(label = n,y=n), size = 3) +
  labs(title = "Enrolments By Section",
       x = "Section",
       y = "Number of Enrolments")


###################################################################
##  4 .Number of current users that are attending the course 
Activelearners  <- subset (dataAllenrolments,unenrolled_at     == "")
nrow(dataAllenrolments)


###################################################################
## 5 Other historical in formations 

## to check the activity for a specific learner
DF_innerjoin[DF_innerjoin$learner_id %in%  "00750a19-05a8-4a3a-bcbf-4f0271673cf4" ,] 

 


#################################################################################
##       model fitting   -- Liner Regression model 
#################################################################################
##install.packages("lubridate")
modelenrolment4 <- data
glimpse(modelenrolment4)

## clean the data 
modelenrolment4 <- modelenrolment4  [(!(modelenrolment4$gender  =="Unknown") |  !(modelenrolment4$age_range  =="Unknown")),]
## factor the variables 
modelenrolment4$learner_id <- as.factor(modelenrolment4$learner_id )
modelenrolment4$age_range <- as.numeric(as.factor(modelenrolment4$age_range ))
modelenrolment4$gender <- as.numeric(as.factor(modelenrolment4$gender ))
modelenrolment4$country         <- as.factor(modelenrolment4$country         )

modelenrolment4$enrolled_at<- as.numeric(format(as.POSIXct(as.Date(modelenrolment4$enrolled_at ),format = "%m/%d/%Y %H:%M:%S") , format = "%Y%m") )
## create a train and  validate set data  
ind = sample(2, nrow(modelenrolment4), replace=TRUE, prob=c(0.6, 0.4))
df.traindata = modelenrolment4[ind==1,]
df.validate = modelenrolment4[ind==2,]
## apply the Linear model fitting 
results.lm <- lm( enrolled_at ~ age_range+gender+country ,data=modelenrolment4)
## see the coefficients 
summary(results.lm)

## predict values using the train data 
head(df.traindata,1)
predict(results.lm, df.traindata) ###
## plot the graph of regression fit 
plot(x = modelenrolment4$enrolled_at,             # True values on x-axis
     y = results.lm$fitted.values,               # fitted values on y-axis
     xlab = "True Values",
     ylab = "Model Fitted Values",
     main = "Regression fits of new enrolments")

abline(b = 1, a = 0)    
# Values should fall around this line!

#####################################################################
##  Compare 2 sets of data 4th and 5th set of data 
#####################################################################


##number of enrollments  4 set 
Activelearners4  <- subset (enrolment4All,unenrolled_at     == "")
nrow(Activelearners4)
##number of enrollments  5 set 
##nrow(enrolment5All)
Activelearners5  <- subset (enrolment5All,unenrolled_at     == "")
nrow(Activelearners5)
head(enrolment5All)


##number of leaving the course  4 set 
nrow(leaving4)
##number of leaving the course  5 set 
nrow(leaving5)
## leaving reason 
table(leaving5$leaving_reason)
