library(dplyr)
library(reshape2)
library(ggplot2)

# Load data
leaving <- read.csv("~/Desktop/leaving.csv")

# Convert to percentage
leaving$Not.relevant <- (leaving$Not.relevant/leaving$Total)*100
leaving$Slightly.relevant <- (leaving$Slightly.relevant/leaving$Total)*100
leaving$Moderately.relevant <- (leaving$Moderately.relevant/leaving$Total)*100
leaving$Highly.relevant <- (leaving$Highly.relevant/leaving$Total)*100
leaving$Extremely.relevant <- (leaving$Extremely.relevant/leaving$Total)*100 

# Select variables and convert to long format
leaving_subset <- leaving %>% select(-Total,-Weighted.Average)
leaving_melt <- melt(leaving_subset,variable.name = 'Response', value.name = 'Percentage')

# Plot
ggplot(leaving_melt, aes(Response,Percentage)) + 
  geom_bar(stat = 'identity') + 
  facet_wrap(~Answer.Choices,labeller = labeller(Answer.Choices = label_wrap_gen(40))) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))  + geom_hline(yintercept = 0)

