library(tidyverse)
library(psych)
#Import data
leave.dat = read.csv(file="./Full transformed data (with exclusions).csv")

#Subset of reasons
leave.dat.sub = select(leave.dat, You.believe.the.stress.of.the.job.is.too.high:You.dislike.the.large.amount.of.effort.required.to.gain.funding)

####Give simpler variable names####
names(leave.dat.sub)[names(leave.dat.sub)=="You.believe.the.stress.of.the.job.is.too.high"] <- "Stress"
names(leave.dat.sub)[names(leave.dat.sub)=="You.want.a.better.work.life.balance"] <- "WL_Balance"
names(leave.dat.sub)[names(leave.dat.sub)=="You.have.experienced.frustration.despair.at.uncertainty.of.how.much.of.the.literature.is.true.due.to.QRP.s..publication.bias..etc."] <- "QRP_Uncertainty"
names(leave.dat.sub)[names(leave.dat.sub)=="You.are.intimidated.by.the.culture.of.criticism"] <- "Criticism"
names(leave.dat.sub)[names(leave.dat.sub)=="You.have.experienced.workplace.bullying.harassment"] <- "Bullying"
names(leave.dat.sub)[names(leave.dat.sub)=="You.believe.the.pay.is.too.low"] <- "Low_Pay"
names(leave.dat.sub)[names(leave.dat.sub)=="You.have.been..in.your.view..harshly.criticized.because.of.your.work"] <- "Harsh_Criticism"
names(leave.dat.sub)[names(leave.dat.sub)=="You.have.experienced.frustration.despair.due.to.being.unable.to.replicate.other.scientist.s.published.studies"] <- "Replication_Frustration"
names(leave.dat.sub)[names(leave.dat.sub)=="You.do.not.believe.the.rewards.incentives.to.stay.are.great.enough.or.are.too.delayed"] <- "Poor_Rewards"
names(leave.dat.sub)[names(leave.dat.sub)=="You.do.not.feel.the.work.you.are.performing.is.a.benefit.to.society"] <- "No_Help_Society"
names(leave.dat.sub)[names(leave.dat.sub)=="You.do.not.feel.the.field.is.progressing"] <- "Field_Not_Progressing"
names(leave.dat.sub)[names(leave.dat.sub)=="You.dislike.the.lack.of.control.over.your.location"] <- "Location"
names(leave.dat.sub)[names(leave.dat.sub)=="You.dislike.the.large.amount.of.effort.required.to.gain.funding"] <- "Funding"

####Create sub-data-frames that are rearranged with reason/score columns####
reason = "Stress"
scores = leave.dat.sub$Stress
Stress.dat= data.frame(reason, scores)

reason = "WL_Balance"
scores = leave.dat.sub$WL_Balance
WL_Balance.dat= data.frame(reason, scores)

reason = "QRP_Uncertainty"
scores = leave.dat.sub$QRP_Uncertainty
QRP_Uncertainty.dat= data.frame(reason, scores)

reason = "Criticism"
scores = leave.dat.sub$Criticism
Criticism.dat= data.frame(reason, scores)

reason = "Bullying"
scores = leave.dat.sub$Bullying
Bullying.dat= data.frame(reason, scores)

reason = "Low_Pay"
scores = leave.dat.sub$Low_Pay
Low_Pay.dat= data.frame(reason, scores)

reason = "Harsh_Criticism"
scores = leave.dat.sub$Harsh_Criticism
Harsh_Criticism.dat= data.frame(reason, scores)

reason = "Replication_Frustration"
scores = leave.dat.sub$Replication_Frustration
Replication_Frustration.dat= data.frame(reason, scores)

reason = "Poor_Rewards"
scores = leave.dat.sub$Poor_Rewards
Poor_Rewards.dat= data.frame(reason, scores)

reason = "No_Help_Society"
scores = leave.dat.sub$No_Help_Society
No_Help_Society.dat= data.frame(reason, scores)

reason = "Field_Not_Progressing"
scores = leave.dat.sub$Field_Not_Progressing
Field_Not_Progressing.dat= data.frame(reason, scores)

reason = "Location"
scores = leave.dat.sub$Location
Location.dat= data.frame(reason, scores)

reason = "Funding"
scores = leave.dat.sub$Funding
Funding.dat= data.frame(reason, scores)

####Merge rearranged sub-data-frames####
plot.dat = rbind(Stress.dat, WL_Balance.dat, QRP_Uncertainty.dat, Criticism.dat, Bullying.dat, Low_Pay.dat, Harsh_Criticism.dat, Replication_Frustration.dat,
                 Poor_Rewards.dat, No_Help_Society.dat, Field_Not_Progressing.dat, Location.dat, Funding.dat)

####Plotting####
#APA theme
apatheme <-
  theme_bw()+                                   
  theme(panel.grid.major = element_blank(),       
        panel.grid.minor = element_blank(),        
        panel.background = element_blank(),     
        panel.border = element_blank(),          
        text=element_text(family="Arial"),       
        legend.title=element_blank(),             
        legend.position= "right",               
        axis.line.x = element_line(color="black"), 
        axis.line.y = element_line(color="black")) 

#Faceted density plot
density.plot = ggplot(data = plot.dat, aes(x = scores))+
  geom_histogram(aes(y=..density..),      
                 binwidth=1,
                 colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+
  facet_wrap(~reason, ncol = 4)+
  labs(x = "Scores (1 = 'Not Relevant'; 5 = 'Extremely Relevant')", y = "Density")+
  apatheme
density.plot

ggsave("density.png", width=6, height=6, unit="in", dpi=300)
#Pirate plot
#Save descriptives by reason
descriptives = describeBy(x = plot.dat$scores, group = plot.dat$reason)

#Save values for groups, their means, and SEs
group = c('Stress', 'WL_Balance', 'QRP_Uncertainty', 'Criticism', 'Bullying', 'Low_Pay', 'Harsh_Criticism', 'Replication_Frustration', 
          'Poor_Rewards', 'No_Help_Society', 'Field_Not_Progressing', 'Location', 'Funding')
means = c(descriptives$Stress$mean, descriptives$WL_Balance$mean, descriptives$QRP_Uncertainty$mean, descriptives$Criticism$mean,
          descriptives$Bullying$mean, descriptives$Low_Pay$mean, descriptives$Harsh_Criticism$mean, descriptives$Replication_Frustration$mean,
          descriptives$Poor_Rewards$mean, descriptives$No_Help_Society$mean, descriptives$Field_Not_Progressing$mean, 
          descriptives$Location$mean, descriptives$Funding$mean)
ses = c(descriptives$Stress$se, descriptives$WL_Balance$se, descriptives$QRP_Uncertainty$se, descriptives$Criticism$se,
        descriptives$Bullying$se, descriptives$Low_Pay$se, descriptives$Harsh_Criticism$se, descriptives$Replication_Frustration$se,
        descriptives$Poor_Rewards$se, descriptives$No_Help_Society$se, descriptives$Field_Not_Progressing$se, 
        descriptives$Location$se, descriptives$Funding$se)

#Merge summary plotting data
pirate.dat = data.frame(group, means, ses)

#Modified apatheme (angled axis labels)
apatheme.mod <-
  theme_bw()+                                   
  theme(panel.grid.major = element_blank(),       
        panel.grid.minor = element_blank(),        
        panel.background = element_blank(),     
        panel.border = element_blank(),          
        text=element_text(family="Arial"),       
        legend.title=element_blank(),             
        legend.position= "right",               
        axis.line.x = element_line(color="black"), 
        axis.text.x  = element_text(angle=75, vjust=0.5),
        axis.line.y = element_line(color="black")) 

#Pirate plot
pirate.plot = ggplot(data = pirate.dat, aes(x = group, y = means))+
  geom_violin(data= plot.dat, aes(x = reason, y = scores), fill="orange")+
  geom_jitter(data= plot.dat, aes(x = reason, y = scores), shape = 1, color = "blue", width = .1, alpha = .15)+
  geom_point(size = 3)+
  geom_errorbar(ymax= means+(1.96*ses), ymin=means+(-1.96*ses), width = 0.25)+
  apatheme.mod+
  labs(x="Reason", y="Score")
pirate.plot

ggsave("pirate.png", width=12, height=6, unit="in", dpi=300)
