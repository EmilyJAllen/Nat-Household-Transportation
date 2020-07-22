library(tidyverse)
hh17<- read.csv("C:/Users/emily/Documents/College/3rd_Year/Transportation_Project/data/hhpub.csv") 
hh17$WALK2SAVE<- factor(hh17$WALK2SAVE)
   

head(hh17$WALK2SAVE)
hist(hh17$WALK2SAVE)
hh17$WALK2SAVE

ggplot(hh17,aes(x=WALK2SAVE) )+
  geom_bar(aes(y=(..count..)/sum(..count..)))+
  scale_x_discrete( labels=c("NA", "I don't\n know", "I prefer not \nto answer",
                      "Strongly\n Agree", "Agree", "Neither Agree\n Nor Disagree",
                      "Disagree", "Strongly \nDisagree"))+
  ggtitle("Do you walk to reduce financial burden of travel?")+
  scale_y_continuous(name = "Frequency")



