library(tidyverse)
library(ggplot2)

per17 <- read.csv("C:/Users/emily/Documents/College/3rd_Year/Transportation_Project/data/perpub.csv")
per17$PRMACT<- as.factor(per17$PRMACT)
per17$SAMEPLC<- as.factor(per17$SAMEPLC)
  
head(per17$PRMACT)
###########################
#     UNIVARIATE
###########################
ggplot(per17,aes(x=PRMACT) )+
  geom_bar()+
  scale_x_discrete(labels =  c("I don't know", "I prefer \nnot to answer",
                               "Appropriate \nskip",
                               "Working", "Temporarily\n absent \nfrom a\n job
                               or business",
                               "Looking\n for work/\n unemployed",
                               "A home-\nmaker", "Going to\n school",
                               "Retired", "Something\n else"))+
  ggtitle("What was your primary activity in the last week?")




ggplot(per17%>% filter(SAMEPLC != "-1"), aes(x=SAMEPLC))+
  geom_bar(aes(y=(..count..)/sum(..count..)))+
  scale_x_discrete(labels= c("I dont \nKnow", "Prefer not \nto answer","Personally\n sick",
                             "vacation or \npersonal \nday", "caretaking",
                             "Disabled/ \nhomebound", "work \nat home", 
                             "not \nscheduled \nfor work", "work \naround \nhome",
                             "bad weather", "out of \ncountry", 
                             "no \ntransportation \navailable", "Something \nelse"))+
  ggtitle("What was your reason for no trips? (of those w 0 trips)")


#compare hh income by activity 
ggplot(per17%>% filter(HHFAMINC>0), aes(y=HHFAMINC))+
  geom_boxplot()+
  facet_wrap(~PRMACT, labeller = labeller(PRMACT= c("-8"="I don't know", 
                                                    "-7"="I prefer not to answer",
                                                     "-1"="Appropriate skip",
                                                    "1"="Working", "2"="Temporarily absent \nfrom a job or business",
                                                     "3"="Looking for work/ unemployed",
                                                      "4"="A home-maker","5"= "Going to school",
                                                      "6"="Retired", "97"="Something else")))
  

head(per17$PRMACT)
#####################################
#        BIVARIATE
#####################################

# compare age and activity
ggplot(per17%>% filter(R_AGE>0), aes(y=R_AGE))+
  geom_boxplot()+
  facet_wrap(~PRMACT, labeller = labeller(PRMACT= c("-8"="I don't know", 
                                                    "-7"="I prefer not to answer",
                                                    "-1"="Appropriate skip",
                                                    "1"="Working", "2"="Temporarily absent \nfrom a job or business",
                                                    "3"="Looking for work/ unemployed",
                                                    "4"="A home-maker","5"= "Going to school",
                                                    "6"="Retired", "97"="Something else")))


ggplot(per17%>% filter(R_AGE>0), aes(y=R_AGE))+
  geom_point()+
  facet_wrap(~PRMACT, labeller = labeller(PRMACT= c("-8"="I don't know", 
                                                    "-7"="I prefer not to answer",
                                                    "-1"="Appropriate skip",
                                                    "1"="Working", "2"="Temporarily absent \nfrom a job or business",
                                                    "3"="Looking for work/ unemployed",
                                                    "4"="A home-maker","5"= "Going to school",
                                                    "6"="Retired", "97"="Something else")))


ggplot(per17%>% filter(R_AGE>0), aes(x= PRMACT, y=R_AGE))+
  geom_point()

