setwd("E:\\study\\IMS Proschool\\Capstone project")
Data_Original<-read.csv("Attrition1.csv")
#View(Data_Original)
head(Data_Original)
summary(Data_Original)


dim(Data_Original)
table(Data_Original$Attrition) 
prop.table(table(Data_Original$Attrition)) ## percentage frequency distribution
str(Data_Original) ## to check type of data

Data_copy=Data_Original
Data_copy=Data_Original[,-c(9,10,22,27)] # removing non significant variable
str(Data_copy)

## converting to factor
Data_copy$Attrition=factor(Data_copy$Attrition)
Data_copy$Education=factor(Data_copy$Education)
Data_copy$EnvironmentSatisfaction=factor(Data_copy$EnvironmentSatisfaction)
Data_copy$JobInvolvement=factor(Data_copy$JobInvolvement)
Data_copy$JobSatisfaction=factor(Data_copy$JobSatisfaction)
Data_copy$PerformanceRating=factor(Data_copy$PerformanceRating)
Data_copy$RelationshipSatisfaction=factor(Data_copy$RelationshipSatisfaction)
Data_copy$WorkLifeBalance=factor(Data_copy$WorkLifeBalance)
Data_copy$StockOptionLevel=factor(Data_copy$StockOptionLevel)

sapply(Data_copy,class) # to check data type

########### Exploratory Data Analysis

##### Univariate Analysis 

summary(Data_copy)  ## Result -- No missing values

boxplot(Data_copy$Age) ## No outliers
hist(Data_copy$Age) ## Normally distributed

##Business Travel
tableBusiness=table(Data_copy$BusinessTravel) 
barplot(tableBusiness, main = "Business Travels Data",col = "red",ylim = c(0,1200)) ## Most of the employees Travel_Rarely
Bus_att_table=table(Data_copy$BusinessTravel,Data_copy$Attrition)
Bus_att_table ## the ratio of 1/0 is Non-Travel>Travel_rarely>Travel_freq

##Dialy Rate
boxplot(Data_copy$DailyRate) # No Outliers
hist(Data_copy$DailyRate) # Unifromlly distributed .. Not a normal distribution

##Department
tableDepartment=table(Data_copy$Department)
barplot(tableDepartment, main = "Department Data",col = "red",ylim = c(0,1000)) #Most e
Dept_att_table=table(Data_copy$Department,Data_copy$Attrition)
Dept_att_table #the ratio of 0/1 is R&D>HR>Sales

#DistanceFromHome
boxplot(Data_copy$DistanceFromHome) # No outliers but have to deal with skewness
hist(Data_copy$DistanceFromHome) # Highly skewed to the right
quantile(Data_copy$DistanceFromHome,c(0.94,0.95,0.96,0.97,0.98,0.99,1))
table(Data_copy$DistanceFromHome>28) # These 28 values are reason for such biasing have to deal with them
## to be noted when preparing model

#Education
tableEducation=table(Data_copy$Education)
barplot(tableEducation, main = "Education",col = "red",ylim = c(0,1000))
edu_att_table=table(Data_copy$Education,Data_copy$Attrition)
edu_att_table #Ratio of 0/1 BelowCollege<Bachelors<College<Masters<Doctors

#EducationField
tableEducationfield=table(Data_copy$EducationField)
barplot(tableEducationfield, main = "Education field Data",col = "red",ylim = c(0,1000))
edufield_att_table=table(Data_copy$EducationField,Data_copy$Attrition)
edufield_att_table #Ratio of 0/1 Human Resources<Other<Technical Degree<Marketing<Medical<Life Sciences

#Environment Satisfaction
tableEnvsatis=table(Data_copy$EnvironmentSatisfaction)
barplot(tableEnvsatis, main = "Environment Satisfaction",col = "red",ylim = c(0,1000))
envsatis_att_table=table(Data_copy$EnvironmentSatisfaction,Data_copy$Attrition)
envsatis_att_table #Ratio of 0/1 <Other 1<2<3<4

tableGender=table(Data_copy$Gender)
barplot(tableGender, main = "Gender",col = "red",ylim = c(0,1000))
gen_att_table=table(Data_copy$Gender,Data_copy$Attrition)
gen_att_table #Ratio of 0/1 Male<Female

boxplot(Data_copy$HourlyRate) #No outlier
hist(Data_copy$HourlyRate) ## Uniform Distribution -- Not normally Distributed

#Job Involvement
tableJobInv=table(Data_copy$JobInvolvement)
barplot(tableJobInv, main = "Job Involvement",col = "red",ylim = c(0,1000))
jobinv_att_table=table(Data_copy$JobInvolvement,Data_copy$Attrition)
jobinv_att_table #Ratio of 0/1 Low<Medium<High<Very High

#JobLevel
tableJoblevel=table(Data_copy$JobLevel)
barplot(tableJoblevel, main = "Job Level",col = "red",ylim = c(0,1000))
joblevel_att_table=table(Data_copy$JobLevel,Data_copy$Attrition)
joblevel_att_table #Ratio of 1/0 4>5>2>3>1

#JobRole
tableJobRole=table(Data_copy$JobRole)
barplot(tableJobRole, main = "Job Role",col = "red",ylim = c(0,1000))
summary(Data_copy$JobRole)
jobrole_att_table=table(Data_copy$JobRole,Data_copy$Attrition)
jobrole_att_table #Ratio of 1/0  Research Director<Manager<Healthcare Representative<Manufacturing Director<Research Scientist<Sales Executive<Human Resources<Laboratory Technician<Sales Representative

tableJobSat=table(Data_copy$JobSatisfaction)
barplot(tableJobInv, main = "Job Satisfaction",col = "red",ylim = c(0,1000))
jobsat_att_table=table(Data_copy$JobSatisfaction,Data_copy$Attrition)
jobsat_att_table #Ratio of 0/1 1<3<2<4


tablemarital=table(Data_copy$MaritalStatus)
barplot(tablemarital, main = "Marital Status",col = "red",ylim = c(0,1000))
marital_att_table=table(Data_copy$MaritalStatus,Data_copy$Attrition)
marital_att_table #Ratio of 1/0 Divorced<Married<Single

###Monthly Income
boxplot(Data_copy$MonthlyIncome)
hist(Data_copy$MonthlyIncome)
summary(Data_copy$MonthlyIncome)
sd(Data_copy$MonthlyIncome) ## deviation is very high... low number of people with high income
quantile(Data_copy$MonthlyIncome,c(0.20,0.40,0.60,0.80,1))
table(Data_copy$MonthlyIncome>9860,Data_copy$Attrition)

#Monthly Rate
boxplot(Data_copy$MonthlyRate) #No Outliers
hist(Data_copy$MonthlyRate) #Uniform Distribution

#Number of companies worked
bx=boxplot(Data_copy$NumCompaniesWorked) #1 Outlier
bx$stats
Data_copy$NumCompaniesWorked<-ifelse(Data_copy$NumCompaniesWorked>=8,8,Data_copy$NumCompaniesWorked)
boxplot(Data_copy$NumCompaniesWorked)
hist(Data_copy$NumCompaniesWorked) # skewed towards right

#Overtime
tableovertime=table(Data_copy$OverTime)
barplot(tableovertime, main = "Over time",col = "red",ylim = c(0,1000))
overtime_att_table=table(Data_copy$OverTime,Data_copy$Attrition)
overtime_att_table #Ratio of 0/1 Yes<No

#perecnt salary hike
boxplot(Data_copy$PercentSalaryHike) # No outliers but have to deal with skewness
hist(Data_copy$PercentSalaryHike) # Data is highly skewed to the right

#Performance rating
tablePerfRate=table(Data_copy$PerformanceRating)
barplot(tablePerfRate, main = "Performance rating",col = "red",ylim = c(0,1400))
perfrating_att_table=table(Data_copy$PerformanceRating,Data_copy$Attrition)
perfrating_att_table #Ratio of 1/0 3<4

#Relatonship Satisfaction
tablerelasat=table(Data_copy$RelationshipSatisfaction)
barplot(tablerelasat, main = "Relationship Satisfaction",col = "red",ylim = c(0,1400))
relation_sat_att_table=table(Data_copy$RelationshipSatisfaction,Data_copy$Attrition)
relation_sat_att_table #Ratio of 1/0 4<2<3<1

#StockOptionLevel
tableStockLevel=table(Data_copy$StockOptionLevel)
barplot(tableStockLevel, main = "Stock Option Level",col = "red",ylim = c(0,1400))
stocklevel_att_table=table(Data_copy$StockOptionLevel,Data_copy$Attrition)
stocklevel_att_table #Ratio of 1/0 2<1<3<0

#Total Working Hours
bx=boxplot(Data_copy$TotalWorkingYears)# Some outliers have to deal with the skewness
bx$stats
table(Data_copy$TotalWorkingYears>=28)
prop.table(table(Data_copy$TotalWorkingYears>=28))
Data_copy$TotalWorkingYears<-ifelse(Data_copy$TotalWorkingYears>=28,28,Data_copy$TotalWorkingYears)
by=boxplot(Data_copy$TotalWorkingYears)
by$stats
hist(Data_copy$TotalWorkingYears) # Data is slighlty skewed to the right
quantile(Data_copy$TotalWorkingYears,c(0.95,0.96,0.97,0.98,0.99,1))

#Training times last year
tableTrainingLastyr=table(Data_copy$TrainingTimesLastYear)
barplot(tableTrainingLastyr, main = "Training times Last Year",col = "red",ylim = c(0,1400))
Trainingtimes_att_table=table(Data_copy$TrainingTimesLastYear,Data_copy$Attrition)
Trainingtimes_att_table #Ratio of 1/0 6<5<1<3<2<4<0

#WorkLifeBalance
tableWorkLifebal=table(Data_copy$WorkLifeBalance)
barplot(tableWorkLifebal, main = "Work Life Balance",col = "red",ylim = c(0,1400))
Work_life_balance_att_table=table(Data_copy$WorkLifeBalance,Data_copy$Attrition)
Work_life_balance_att_table #Ratio of 1/0 3<2<4<1

#Years at company
boxplot(Data_copy$YearsAtCompany)# There are outliers have to deal with the skewness
boxplot(Data_copy$YearsAtCompany)$stats
table(Data_copy$YearsAtCompany>18)
prop.table(table(Data_copy$YearsAtCompany>18))
hist(Data_copy$YearsAtCompany) # Data is highly skewed to the right

#######Years Incurrent role - 7% outlier
boxplot(Data_copy$YearsInCurrentRole)# There are outliers have to deal with the skewness
boxplot(Data_copy$YearsInCurrentRole)$stats
Data_copy$YearsInCurrentRole<-ifelse(Data_copy$YearsInCurrentRole>=14,14,Data_copy$YearsInCurrentRole)
boxplot(Data_copy$YearsInCurrentRole)
hist(Data_copy$YearsInCurrentRole) # Data is skewed to the right

#Years Since Last promotion
boxplot(Data_copy$YearsSinceLastPromotion)# There are outliers which has to be treated for skewness
boxplot(Data_copy$YearsSinceLastPromotion)$stats

hist(Data_copy$YearsSinceLastPromotion) #Highly skewed to the right
quantile(Data_copy$YearsSinceLastPromotion,c(0.95,0.96,0.97,0.98,0.99,1))
table(Data_copy$YearsSinceLastPromotion>7)
prop.table(table(Data_copy$YearsSinceLastPromotion>7))
summary(Data_copy$YearsSinceLastPromotion)
s=sd(Data_copy$YearsSinceLastPromotion)
m=mean(Data_copy$YearsSinceLastPromotion)
m+3*s # mean + 3 sigma values
Data_copy$YearsSinceLastPromotion<-ifelse(Data_copy$YearsSinceLastPromotion>7,7,Data_copy$YearsSinceLastPromotion)
boxplot(Data_copy$YearsSinceLastPromotion)
boxplot(Data_copy$YearsSinceLastPromotion)$stats
#Data_copy$YearsSinceLastPromotion=ifelse(Data_copy$YearsSinceLastPromotion>14,14,Data_copy$YearsSinceLastPromotion)
#summary(Data_copy$YearsSinceLastPromotion)
#boxplot(Data_copy$YearsSinceLastPromotion)

#Years With Current Manager
boxplot(Data_copy$YearsWithCurrManager)# There are outliers which has to be treated for skewness
boxplot(Data_copy$YearsWithCurrManager)$stats
Data_copy$YearsWithCurrManager<-ifelse(Data_copy$YearsWithCurrManager>=14,14,Data_copy$YearsWithCurrManager)
boxplot(Data_copy$YearsWithCurrManager)
hist(Data_copy$YearsWithCurrManager) # skewed to the right


#Bivariate Analysis
Datacheck<-Data_copy[,-c(7,9,12,13,15,22,23,24,3,5,8,10,14,16,20,27)]
cor(Datacheck)
library(corrplot)
corrplot(cor(Datacheck), method="circle")
## The variables showing correlation of 0.70 or greater are
##Total working hours and Monthly Income = 0.772893
##Years in current Role and years at company = 0.758754
##Years in current role and years at current company = 0.769212 
##Years with current manager and years in current company = 0.714365



##########################    MODEL 2 ---- After treating outliers from

###Monthly Income
boxplot(Data_copy$MonthlyIncome)
hist(Data_copy$MonthlyIncome)
summary(Data_copy$MonthlyIncome)
sd(Data_copy$MonthlyIncome) ## deviation is very high... low number of people with high income
quantile(Data_copy$MonthlyIncome,c(0.20,0.40,0.60,0.80,0.90,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99))
table(Data_copy$MonthlyIncome>9860,Data_copy$Attrition)
#capping with 92%
Data_copy$MonthlyIncome<-ifelse(Data_copy$MonthlyIncome>=16368.80,16368.80,Data_copy$MonthlyIncome)
boxplot(Data_copy$MonthlyIncome)
hist(Data_copy$MonthlyIncome)

### Distancefrom home
boxplot(Data_copy$DistanceFromHome) # No outliers but have to deal with skewness
hist(Data_copy$DistanceFromHome) # Highly skewed to the right
quantile(Data_copy$DistanceFromHome,c(0.94,0.95,0.96,0.97,0.98,0.99,1))
table(Data_copy$DistanceFromHome>28) # These 27 values are reason for such biasing have to deal with them
#capping with 96 percentile
Data_copy$DistanceFromHome<-ifelse(Data_copy$DistanceFromHome>=27,27,Data_copy$DistanceFromHome)
boxplot(Data_copy$DistanceFromHome)
hist(Data_copy$DistanceFromHome)


################## ---- LOGISTIC REGRESSION -----------

library(caret)
set.seed(12345)
Data2<-createDataPartition(Data_copy$Attrition,p=0.8,list=FALSE)
TrainData=Data_copy[Data2,]
TestData=Data_copy[-Data2,]

table(TrainData$Attrition) 
prop.table(table(TrainData$Attrition)) ## percentage frequency distribution

table(TestData$Attrition)
prop.table(table(TestData$Attrition))

library(car)
mod<-lm(as.numeric(Attrition) ~ .,data=TrainData)

stpmod=step(mod)
formula(stpmod)
summary(stpmod)

mod1<-glm(as.factor(Attrition) ~ Age + BusinessTravel + DailyRate + DistanceFromHome + 
            EducationField + EnvironmentSatisfaction + JobInvolvement + 
            JobRole + JobSatisfaction + NumCompaniesWorked + OverTime + 
            RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + 
            TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + 
            YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager,family="binomial"(link='logit'),data=TrainData)
summary(mod1) #AIC 744.42 #AIC 742.77 #742.67
str(Data_copy$Attrition)

#checking the probability for each observation by creating a variable names score
TrainData$score=predict(mod1, TrainData,type = "response")
head(TrainData$score)

str(TrainData$Attrition)
#Analyse the confusion matrix and model accuracy
library(caret)
TrainData$Attrition<-as.factor(TrainData$Attrition)
str(TrainData$Attrition)
TrainData$predict1<-as.factor(ifelse(TrainData$score>0.5,1,0))
library(e1071)
str(TrainData$predict1)
str(TrainData$Attrition)
confusionMatrix(TrainData$predict1,TrainData$Attrition) #Not working --- Doubt

# 89.97% Accuracy
# 89.80% Accuracy 
# 89.80% Accuracy 




##Mcfadden test  ----- Doubt what does it depicts?
library(pscl)
  pR2(mod1)
  
##Concordance test
library(InformationValue)
concor<-Concordance(TrainData$Attrition,TrainData$score)  
concor

plotROC(actuals = TrainData$Attrition,predictedScores = as.numeric (fitted(mod1)))
ks_plot(actuals = TrainData$Attrition,predictedScores = as.numeric (fitted(mod1)))
ks_stat(actuals = TrainData$Attrition,predictedScores = as.numeric (fitted(mod1)))

TestData$score2= predict(mod1,TestData,type="response")
View(TestData)


##############---- DECISION TREE ALGORITHM
Data_new<-Data_copy

## Converting categorical variables to dummy variable and then to factor type
##Business Travel
Data_new$BusinessTravel_New<-ifelse(Data_new$BusinessTravel=="Non-Travel",0,Data_new$BusinessTravel)
Data_new$BusinessTravel_New<-ifelse(Data_new$BusinessTravel=="Travel_Rarely",1,Data_new$BusinessTravel_New)
Data_new$BusinessTravel_New<-ifelse(Data_new$BusinessTravel=="Travel_Frequently",2,Data_new$BusinessTravel_New)
View(Data_new)
Data_new$BusinessTravel_New=factor(Data_new$BusinessTravel_New)
str(Data_new$BusinessTravel_New)

## Department
Data_new$Department_New<-ifelse(Data_new$Department=="Sales",1,Data_new$Department)
Data_new$Department_New<-ifelse(Data_new$Department=="Research & Development",2,Data_new$Department_New)
Data_new$Department_New<-ifelse(Data_new$Department=="Human Resources",0,Data_new$Department_New)
View(Data_new)
Data_new$Department_New=factor(Data_new$Department_New)
str(Data_new$Department_New)

##EducationField
#Human Resources<Other<Technical Degree<Marketing<Medical<Life Sciences
#  0              1         2              3         4        5

Data_new$Edufield_New<-ifelse(Data_new$EducationField=="Human Resources",0,Data_new$EducationField)
Data_new$Edufield_New<-ifelse(Data_new$EducationField=="Other",1,Data_new$Edufield_New)
Data_new$Edufield_New<-ifelse(Data_new$EducationField=="Technical Degree",2,Data_new$Edufield_New)
Data_new$Edufield_New<-ifelse(Data_new$EducationField=="Marketing",3,Data_new$Edufield_New)
Data_new$Edufield_New<-ifelse(Data_new$EducationField=="Medical",4,Data_new$Edufield_New)
Data_new$Edufield_New<-ifelse(Data_new$EducationField=="Life Sciences",5,Data_new$Edufield_New)
View(Data_new)
Data_new$Edufield_New=factor(Data_new$Edufield_New)
str(Data_new$Department_New)
table(Data_new$EducationField,Data_new$Edufield_New)
summary(Data_new$EducationField)
summary(Data_new$Edufield_New)

##Gender
Data_new$Gender_New<-ifelse(Data_new$Gender=="Male",1,0)
View(Data_new)
Data_new$Gender_New=factor(Data_new$Gender_New)
str(Data_new$Gender_New)

##Job Role
#Research Director<Manager<Healthcare Representative<Manufacturing Director<Research Scientist<Sales Executive<Human Resources<Laboratory Technician<Sales Representative
#  0                 1         2                                3               4                    5             6               7                    8                     

Data_new$JobRole_New<-ifelse(Data_new$JobRole=="Research Director",0,Data_new$JobRole)
Data_new$JobRole_New<-ifelse(Data_new$JobRole=="Manager",1,Data_new$JobRole_New)
Data_new$JobRole_New<-ifelse(Data_new$JobRole=="Healthcare Representative",2,Data_new$JobRole_New)
Data_new$JobRole_New<-ifelse(Data_new$JobRole=="Manufacturing Director",3,Data_new$JobRole_New)
Data_new$JobRole_New<-ifelse(Data_new$JobRole=="Research Scientist",4,Data_new$JobRole_New)
Data_new$JobRole_New<-ifelse(Data_new$JobRole=="Sales Executive",5,Data_new$JobRole_New)
Data_new$JobRole_New<-ifelse(Data_new$JobRole=="Human Resources",6,Data_new$JobRole_New)
Data_new$JobRole_New<-ifelse(Data_new$JobRole=="Laboratory Technician",7,Data_new$JobRole_New)
Data_new$JobRole_New<-ifelse(Data_new$JobRole=="Sales Representative",8,Data_new$JobRole_New)

View(Data_new)
Data_new$JobRole_New=factor(Data_new$JobRole_New)
str(Data_new$JobRole_New)
table(Data_new$JobRole,Data_new$JobRole_New)
summary(Data_new$JobRole)
summary(Data_new$JobRole_New)

##MaritalStatus
Data_new$MaritalStatus_New<-ifelse(Data_new$MaritalStatus=="Single",0,Data_new$MaritalStatus)
Data_new$MaritalStatus_New<-ifelse(Data_new$MaritalStatus=="Married",1,Data_new$MaritalStatus_New)
Data_new$MaritalStatus_New<-ifelse(Data_new$MaritalStatus=="Divorced",2,Data_new$MaritalStatus_New)
View(Data_new)
Data_new$MaritalStatus_New=factor(Data_new$MaritalStatus_New)
str(Data_new$MaritalStatus_New)

##OverTime
Data_new$OverTime_New<-ifelse(Data_new$OverTime=="Yes",1,0)
View(Data_new)
Data_new$OverTime_New=factor(Data_new$OverTime_New)
str(Data_new$OverTime_New)

## Removing old variables for Business travel,Department,Education Field,Gender,Job Role,Overtime
#Data_new<-Data_new[,-c(3,5,8,10,14,16,20)]
Data_new<-Data_copy
View(Data_new)

### MODEL Building

library(caret)
set.seed(12345)
Data3<-createDataPartition(Data_new$Attrition,p=0.8,list=FALSE)
TrainData1=Data_new[Data3,]
TestData1=Data_new[-Data3,]

table(TrainData1$Attrition) 
prop.table(table(TrainData1$Attrition)) ## percentage frequency distribution

table(TestData1$Attrition)
prop.table(table(TestData1$Attrition))

##
library(party)
library(rpart)
library(rpart.plot)
fit=rpart(Attrition~.,data=TrainData1,method="class",control=rpart.control(minsplit=31,cp=0.01))
rpart.plot(fit)
print(fit)
summary(fit)
prp(fit)
plotcp(fit)
printcp(fit)
#train data

TrainData1$Attrition<-as.factor(TrainData1$Attrition)
TestData1$Attrition<-as.factor(TestData1$Attrition)
str(fit)
predtr$score<-predict(fit,TrainData1$Attrition,type="class")
confusionMatrix(predtr$score,TrainData$Attrition)
table(predtr,TrainData1$Attrition)
((946+89)/(949+89+101+41))*100
#test data
predtest<-predict(fit,TestData1,type="class")
table(predtest,TestData1$Attrition)
((242+4)/(242+4+41+7))*100
library(pROC)
library(e1071)
auctrain<-roc(as.numeric(TrainData1$Attrition), as.numeric(predtr))
print(auctrain)
auctest<-roc(as.numeric(TestData1$Attrition), as.numeric(predtest))
print(auctest)

plot(auctrain, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auctrain$auc[[1]],3)),col = 'blue')
plot(auctest, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auctest$auc[[1]],3)),col = 'blue')

## Logistic Regression model is performing better than Decision Tree model with accuracy of 90.89 % in opposite to 88.27%
################################ random forest

Data_new<-Data_copy
View(Data_new)
summary(Data_new)
dim(Data_new)
library(randomForest)
require(caret) 
library(pROC)
str(Data_new$Attrition)
Data_new$Attrition<-as.numeric(as.character(Data_new$Attrition))
str(Data_new$Attrition)
set.seed(1234)
splitIndex1 <- createDataPartition(Data_new$Attrition, p = .70,list = FALSE, times = 1)
trainSplit1 <- Data_new[ splitIndex1,]
testSplit1 <- Data_new[-splitIndex1,]
print(table(trainSplit1$Attrition))
print(table(testSplit1$Attrition))
str(trainSplit1$Attrition)

prop.table(table(trainSplit1$Attrition))
prop.table(table(testSplit1$Attrition))

modelrf <- randomForest(as.factor(Attrition) ~ . , data = trainSplit1, do.trace=T)
modelrf

str(trainSplit1$Attrition)
importance(modelrf)
#The variable importance plot displays a plot with variables sorted by MeanDecreaseGini
varImpPlot(modelrf)


#trainSplit$Attrition<-as.numeric(trainSplit$Attrition)
#testSplit$Attrition<-as.numeric(testSplit$Attrition)


predrf_tr1 <- predict(modelrf, trainSplit1)
predrf_test1 <- predict(modelrf, testSplit1)


### score prediction using AUC
confusionMatrix(predrf_tr1,trainSplit1$Attrition) #accuracy 100%
confusionMatrix(predrf_test1,testSplit1$Attrition) #accuracy 85%

aucrf_tr <- roc(as.numeric(trainSplit1$Attrition), as.numeric(predrf_tr1),  ci=TRUE)
aucrf_test <- roc(as.numeric(testSplit1$Attrition), as.numeric(predrf_test1),  ci=TRUE)

aucrf_tr
aucrf_test

plot(aucrf_tr, ylim=c(0,1), print.thres=TRUE, main=paste('Random Forest AUC:',round(aucrf_tr$auc[[1]],3)),col = 'blue')
plot(aucrf_test, ylim=c(0,1), print.thres=TRUE, main=paste('Random Forest AUC:',round(aucrf_test$auc[[1]],3)),col = 'blue')

#########################Naive Bayes

#NaiveBayes Model

Data_new1=Data_Original[,-c(9,10,22,27)]

set.seed(1234)
splitIndex2 <- createDataPartition(Data_new1$Attrition, p = .70,list = FALSE, times = 1)
trainSplit2 <- Data_new1[ splitIndex2,]
testSplit2 <- Data_new1[-splitIndex2,]
print(table(trainSplit2$Attrition))
print(table(testSplit2$Attrition))
str(trainSplit2$Attrition)

prop.table(table(trainSplit2$Attrition))
prop.table(table(testSplit2$Attrition))

modelnb1 <- naiveBayes(as.factor(Attrition) ~. , data = trainSplit2)
modelnb1

prednb_tr1 <- predict(modelnb1,trainSplit2)
prednb_test1 <- predict(modelnb1,testSplit2)

### score prediction using AUC
confusionMatrix(prednb_tr1,trainSplit1$Attrition) #79.9% accuracy
confusionMatrix(prednb_test1,testSplit1$Attrition) #75.73% accuracy

############################### KNN ALgorithm
Data_new3=Data_Original[,-c(9,10,22,27)]
Data_new3$Attrition<-as.factor(Data_new3$Attrition)


## converting to factor
Data_new3$Attrition=factor(Data_new3$Attrition)
Data_new3$Education=factor(Data_new3$Education)
Data_new3$JobLevel=factor(Data_new3$JobLevel)
Data_new3$JobRole=factor(Data_new3$JobRole)
Data_new3$EnvironmentSatisfaction=factor(Data_new3$EnvironmentSatisfaction)
Data_new3$JobInvolvement=factor(Data_new3$JobInvolvement)
Data_new3$JobSatisfaction=factor(Data_new3$JobSatisfaction)
Data_new3$PerformanceRating=factor(Data_new3$PerformanceRating)
Data_new3$RelationshipSatisfaction=factor(Data_new3$RelationshipSatisfaction)
Data_new3$WorkLifeBalance=factor(Data_new3$WorkLifeBalance)
Data_new3$StockOptionLevel=factor(Data_new3$StockOptionLevel)

Data_new3$BusinessTravel=factor(Data_new3$BusinessTravel)
Data_new3$Department=factor(Data_new3$Department)
Data_new3$EducationField=factor(Data_new3$EducationField)
Data_new3$OverTime=factor(Data_new3$OverTime)
Data_new3$MaritalStatus=factor(Data_new3$MaritalStatus)
Data_new3$Gender=factor(Data_new3$Gender)

str(Data_new3)

library(dummies)
dummy_df3 = dummy.data.frame(Data_new3[, c('JobRole','Gender','MaritalStatus','OverTime','EducationField','Department','BusinessTravel','Education', 'EnvironmentSatisfaction','JobInvolvement','JobLevel','JobSatisfaction',"PerformanceRating","RelationshipSatisfaction","StockOptionLevel","WorkLifeBalance")])

Data_combined<-Data_new3
Data_combined<-cbind.data.frame(Data_new3,dummy_df3)
View(Data_combined)

Data_combined = Data_combined[, !(names(Data_combined) %in% c('JobRole','Gender','MaritalStatus','OverTime','EducationField','Department','BusinessTravel','Education', 'EnvironmentSatisfaction','JobInvolvement','JobLevel','JobSatisfaction',"PerformanceRating","RelationshipSatisfaction","StockOptionLevel","WorkLifeBalance"))]
str(Data_combined)

X1 = Data_combined[, !(names(Data_combined) %in% c('Attrition'))]
View(X1)
Data_combined_scaled = as.data.frame(scale(X1))
str(Data_combined_scaled)

set.seed(1234)
splitIndex3 <- createDataPartition(Data_new1$Attrition, p = .70,list = FALSE, times = 1)

Data_combined_train <- Data_combined_scaled[splitIndex3,]
Data_combined_test <- Data_combined_scaled[-splitIndex3,]

Data_combined_train_labels <- Data_combined[splitIndex3, 'Attrition']
Data_combined_test_labels <- Data_combined[-splitIndex3, 'Attrition']

## Let's use diff k values (no of NNs) to see how they perform in terms of correct proportion of classification and success rate. 

#install.packages("class")
#library(class)
install.packages("class")
#install.packages("gmodels")
library(gmodels)
library(class)

# Thumb rule to decide on k for k-NN is sqrt(n)/2
k = sqrt(nrow(Data_new1))/2
k


set.seed(400)
ct <- trainControl(method="repeatedcv",repeats = 3)
fit <- train(Attrition ~ ., data = Data_combined, method = "knn", trControl = ct, preProcess = c("center","scale"),tuneLength = 20)
fit

test_pred_rule <-  knn(train = Data_combined_train, test = Data_combined_test, cl = Data_combined_train_labels, k=9)
CrossTable(x=Data_combined_test_labels ,y=test_pred_rule ,prop.chisq = FALSE)
#85.3%