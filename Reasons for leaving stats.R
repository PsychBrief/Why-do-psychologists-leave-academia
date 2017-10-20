#Install and upload the packages "readr" and "knitr"
install.packages("readr")
library(readr)
install.packages("knitr")
library(knitr)
#Download the data from GitHub
GitHub_data<-"https://raw.githubusercontent.com/PsychBrief/Why-do-psychologists-leave-academia/master/MyDataReport_(with_exclusions).csv"
#Read the csv. file
QuestionnaireData<-read.csv(GitHub_data)
#Change the names of the factors
names(QuestionnaireData)<-c("Consent","Age","Work_Location","Sex","Occupation","Other","Academic?","PhD","Postdoc","Assistant_Lecturer","Lecturer","Senior_Lecturer","Reader","Professor","Trainee_Prac_Psych","Prac_Psych","Other","Left_Academia?","Stress","W_L_balance","Truth_of_field","Criticism_culture","Bullying","Pay","Criticised","Replics","Incentives","No_benefit","No_progress","Location","Funding","Other","Comments")
#Turn the integer variable into a factor and give them the correct labels
QuestionnaireData$Stress<-factor(QuestionnaireData$Stress,labels=c("Blank", "Not relevant","Slightly relevant","Moderately relevant","Highly relevant","Extremely relevant"))
QuestionnaireData$W_L_balance<-factor(QuestionnaireData$W_L_balance,labels=c("Blank","Not relevant","Slightly relevant","Moderately relevant","Highly relevant","Extremely relevant"))
QuestionnaireData$Truth_of_field<-factor(QuestionnaireData$Truth_of_field,labels=c("Blank","Not relevant","Slightly relevant","Moderately relevant","Highly relevant","Extremely relevant"))
QuestionnaireData$Criticism_culture<-factor(QuestionnaireData$Criticism_culture,labels=c("Blank","Not relevant","Slightly relevant","Moderately relevant","Highly relevant","Extremely relevant"))
QuestionnaireData$Bullying<-factor(QuestionnaireData$Bullying,labels=c("Blank", "Not relevant","Slightly relevant","Moderately relevant","Highly relevant","Extremely relevant"))
QuestionnaireData$Pay<-factor(QuestionnaireData$Pay,labels=c("Blank","Not relevant","Slightly relevant","Moderately relevant","Highly relevant","Extremely relevant"))
QuestionnaireData$Criticised<-factor(QuestionnaireData$Criticised,labels=c("Blank","Not relevant","Slightly relevant","Moderately relevant","Highly relevant","Extremely relevant"))
QuestionnaireData$Replics<-factor(QuestionnaireData$Replics,labels=c("Blank","Not relevant","Slightly relevant","Moderately relevant","Highly relevant","Extremely relevant"))
QuestionnaireData$Incentives<-factor(QuestionnaireData$Incentives,labels=c("Blank","Not relevant","Slightly relevant","Moderately relevant","Highly relevant","Extremely relevant"))
QuestionnaireData$No_benefit<-factor(QuestionnaireData$No_benefit,labels=c("Blank","Not relevant","Slightly relevant","Moderately relevant","Highly relevant","Extremely relevant"))
QuestionnaireData$No_progress<-factor(QuestionnaireData$No_progress,labels=c("Blank","Not relevant","Slightly relevant","Moderately relevant","Highly relevant","Extremely relevant"))
QuestionnaireData$Location<-factor(QuestionnaireData$Location,labels=c("Blank","Not relevant","Slightly relevant","Moderately relevant","Highly relevant","Extremely relevant"))
QuestionnaireData$Funding<-factor(QuestionnaireData$Funding,labels=c("Blank","Not relevant","Slightly relevant","Moderately relevant","Highly relevant","Extremely relevant"))
#Create a table with the variable Sex on the y and the different reasons for leaving academia on the x
table(QuestionnaireData$Sex, QuestionnaireData$Stress)
table(QuestionnaireData$Sex, QuestionnaireData$W_L_balance)
table(QuestionnaireData$Sex, QuestionnaireData$Truth_of_field)
table(QuestionnaireData$Sex, QuestionnaireData$Criticism_culture)
table(QuestionnaireData$Sex, QuestionnaireData$Bullying)
table(QuestionnaireData$Sex, QuestionnaireData$Pay)
table(QuestionnaireData$Sex, QuestionnaireData$Criticised)
table(QuestionnaireData$Sex, QuestionnaireData$Replics)
table(QuestionnaireData$Sex, QuestionnaireData$Incentives)
table(QuestionnaireData$Sex, QuestionnaireData$No_benefit)
table(QuestionnaireData$Sex, QuestionnaireData$No_progress)
table(QuestionnaireData$Sex, QuestionnaireData$Location)
table(QuestionnaireData$Sex, QuestionnaireData$Funding)