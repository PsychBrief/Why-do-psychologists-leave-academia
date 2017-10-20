#Create a number vector of the scores
Reasons_scores<-c(3.25,3.43,2.82,2.21,1.99,2.83,1.76,1.81,3.55,2.66,2.4,3.19,3.5)
#Create a character vector of the options
Reasons<-c("Stress", "W_L_Balance", "Truth_of_field", "Criticism_culture", "Bullying", "Pay", "Criticised", "Replics", "Incentives", "No_benefit", "No_progress", "Location", "Funding")
#create a data frame of the reasons and reasons scores
ReasonsData<-data.frame(Reasons,Reasons_scores)
#Force the order of the lables for the character vector Reasons
ReasonsData$Reasons=factor(ReasonsData$Reasons, levels=c("Stress", "W_L_Balance", "Truth_of_field", "Criticism_culture", "Bullying", "Pay", "Criticised", "Replics", "Incentives", "No_benefit", "No_progress", "Location", "Funding"))
install.packages("ggplot2")
library(ggplot2)
#Graph object of the data frame with Reasons as the x axis and Reasons_scores as the y
Reasons_graph<-ggplot(ReasonsData,aes(Reasons,Reasons_scores))
#Put bars on my graph object, fill in the bars in blue, give it a title and centre it, put the scores on the bars, change the colour of the label font to white, and change the size of the font for the axis labels.
Reasons_graph+geom_bar(stat="identity",fill = "dodgerblue2")+ ggtitle("Weighted average of reasons for leaving academia (rated from 1-5)")+ theme(plot.title = element_text(hjust = 0.5))+ geom_text(aes(label = Reasons_scores), colour="white", size = 5, hjust = 0.5, vjust = 3, position =     "stack")+ theme(text = element_text(size=14))