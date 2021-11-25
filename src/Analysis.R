#install.packages("ProjectTemplate")
library("ProjectTemplate")
setwd("~/R/CSCFilProject")
load.project()
##Filippos
head(df2)
nrow(df2)
head(df3$learner_id)
nrow(df3)

library(dplyr)
glimpse(df2)
glimpse(df3)

## join the enrollments per week 
data = bind_rows(df2,df3)
## join the activities per week
activity =bind_rows(activity2,activity3)
class(activity)
table(activity$learner_id)

## inner join the enrolmets and activities
DF_innerjoin = merge(x=data,y=activity,by="learner_id")
## to check the activituy for a specific learner
DF_innerjoin[DF_innerjoin$learner_id %in%  "feb41f66-257c-4839-a209-dd4e0f75ecf5" ,] 

## site for plotd : https://sites.harding.edu/fmccown/r/

library(ggplot2)
library( dplyr )

data <- data  [(!(data$gender  =="Unknown") & !(data$age_range  =="Unknown")),]
dfg <- data %>%
  dplyr::count(age= data$age_range,gender=data$gender, sort = TRUE) 

##Enrolments regarding age/gender
ggplot(dfg ,aes(x = age, y = n   , fill = gender))+
  geom_bar(stat="identity")+
  geom_text(aes(label = n,y=n), size = 3) +
  labs(title = "Enrolments regarding age/gender",
       x = "Age-Range",
       y = "Number of Ocurances")

##Enrollments By Year
dfinserttions  <- data %>%
dplyr::count(Enrolemts=  format(as.POSIXct(as.Date(data$enrolled_at ),format = "%m/%d/%Y %H:%M:%S") , format = "%Y") , sort = TRUE) 
ggplot(dfinserttions ,aes(x = Enrolemts, y = n   , fill = Enrolemts   ))+
  geom_bar(stat="identity")+
  geom_text(aes(label = n,y=n), size = 3) +
  labs(title = "Enrollments By Year",
       x = "Year",
       y = "Number of Enrolments")


##Age_range vs Gender in Enrollments
dfageeducation <-data %>%
dplyr::count(age= data$age_range,gender=data$gender, sort = TRUE)
ggplot(data=dfageeducation) + geom_point(aes(x=age, y=gender,colour=age, size =4))+
  geom_text(aes(x=age,label = n, y=gender), size = 3)+
  labs(title = "Age and Gender",
       x = "Age-Range",
       y = "Gender")

##Education per Age
dfageeducation <- data %>%
dplyr::count(age= data$age_range,education=data$highest_education_level, sort = TRUE) 
ggplot(data=dfageeducation) + geom_point(aes(x=age , y=education  ,colour=age, size = 4 ))+
  geom_text(aes(x=age,label = n,y=education), size = 3)+
  labs(title = "Education per Age",
       x = "Age-Range",
       y = "highest_education_level")

##Education per gender
dfageeducation1 <- data %>%
dplyr::count(gender= data$gender,education=data$highest_education_level, sort = TRUE) 
ggplot(data=dfageeducation1) + geom_point(aes(x=gender , y=education  ,colour=gender, size = 4 ))+
  geom_text(aes(x=gender,label = n,y=education), size = 3)+
  labs(title = "Education per Gender",
       x = "Gender",
       y = "highest_education_level")

##Graph of gender vs employment_area
dfageeducation1 <- data %>%
dplyr::count(gender= data$gender,employment=data$employment_area, sort = TRUE) 
ggplot(data=dfageeducation1) + geom_point(aes(x=gender , y=employment  ,colour=gender, size = 4))+
  geom_text(aes(x=gender,label = n,y=employment), size = 3)+
  labs(title = "Employment per Gender",
       x = "Gender",
       y = "Employment's")

##Employment Status based on Employment Area 
dfinserttions  <- data %>%
dplyr::count(employment= data$employment_area,employment=data$employment_status, sort = TRUE) 
ggplot(dfinserttions ,aes(x = employment, y = n  , fill = employment  ))+
  geom_bar(stat="identity")+
  geom_text(aes(label = n,y=n), size = 3) +
  labs(title = "Employment Status",
       x = "Employment Status")

##3333
dfinserttions  <- data %>%
  dplyr::count(employment_= data$employment_area,employment=data$employment_status, sort = TRUE) 
ggplot(dfinserttions ,aes(x = employment, y = n  , fill = employment_  ))+
  geom_bar(stat="identity")+
  geom_text(aes(label = n,y=n), size = 3) +
  labs(title = "Employment Status",
       x = "Employment Status")

##########################################
#Numbers of genders each age
dfenrolment4 <- enrolment4 %>%
  dplyr::count(age = enrolment4$age_range, gender =enrolment4$gender, sort = TRUE)
ggplot(data=dfenrolment4) + geom_point(aes(x=age  , y=n  ,colour=gender, size = 3  )) +
  geom_text(aes(x=age,label = n,y=n), size = 3)  +
  labs(title = "Enrolments regarding age/gender",
       x = "Age-Range",
       y = "Number of Genders")

ga = ggplot(dfenrolment4, aes(x=age, y=n, group=gender)) +
  geom_line(aes(color=gender))+
  geom_point(aes(color=gender))
print(ga)



dfsteps4 <- activity4 %>%
dplyr::count(stepcount= str_sub(activity4$step,1,1) , sort = TRUE)
ggplot(dfsteps4 ,aes(x = stepcount     , y = n   , fill = stepcount        ))+
  geom_bar(stat="identity")+
  geom_text(aes(label = n,y=n), size = 3) +
  labs(title = "Step Activity",
       x = "Steps set ",
       y = "Number of trials")

                  ####
dfsteps44 <- activity4 %>%
dplyr::count(step =  activity4$step  , sort = TRUE)
g = ggplot(data=dfsteps44, aes(x=step, y=n))
g1 = g + geom_point() + stat_smooth(linetype=2) +
  xlab("Displacement") + ylab("Highway steps")
print(g1)



dfsteps4 <- activity4 %>%
  dplyr::count(stepcount= str_sub(activity4$step,1,1) , sort = TRUE)

ggplot(dfsteps4 ,aes(x = stepcount     , y = n   , fill = stepcount        ))+
  geom_bar(stat="identity")+
  geom_text(aes(label = n,y=n), size = 3) +
  labs(title = "Step Activity",
       x = "Steps set ",
       y = "Number of trials")



dfsteps44 <- activity4 %>%
  dplyr::count(step =  activity4$step  , sort = TRUE)

g = ggplot(data=dfsteps44, aes(x=step, y=n))
g1 = g + geom_point() + stat_smooth(linetype=2) +
  xlab("Displacement") + ylab("Highway steps")

print(g1)


dfQuestionresp4 <- Questionresp4 %>%
  dplyr::count(quiz= str_sub(Questionresp4$quiz_question, 1, 1),response=Questionresp4$correct, sort = TRUE)

ggplot(data=dfQuestionresp4) + geom_point(aes(x=quiz  , y=n  ,colour=response, size = 4 ))+
  geom_text(aes(x=quiz ,label = n,y=n    ), size = 3)+
  labs(title = "Quastion/Response of quiz",
       x = "Quiz-Range",
       y = "Number of Trialsl")




dfQuestionresp44 <- Questionresp4 %>%
dplyr::count(quiz=  Questionresp4$quiz_question , sort = TRUE)
gr = ggplot(data=dfQuestionresp44, aes(x=quiz, y=n))
gr1 = g + geom_point() + stat_smooth(linetype=1) +
  xlab("Displacement") + ylab("Question Response")

print(gr1)
