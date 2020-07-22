library(tidyverse)

veh17<- read.csv("C:/Users/emily/Documents/College/3rd_Year/Transportation_Project/data/vehpub.csv") 
veh17$HFUEL<- factor(veh17$HFUEL)
head(veh17$HFUEL)
tail(veh17$HFUEL)
summary(veh17$HFUEL)
hist(veh17$HFUEL)

ggplot(veh17%>% filter(veh17$HFUEL!=-1),aes(x=HFUEL) )+
  geom_bar()+
  scale_x_discrete( labels=c("NA", "I don't know",
                             "Biodiesel", "Plug-in \nHybrid", "Electric",
                             "Hybrid", "Some other Fuel"))+
  ggtitle("What kind of hybrid? (Excluding non-hybrids)")



#######compare hh income and fuel type 
#filter out NA, idk
c<- ggplot(veh17 %>% filter(veh17$HHFAMINC>0), aes(y=HHFAMINC))+
  geom_boxplot()+
  facet_wrap(~HFUEL, labeller = labeller(HFUEL= c( "-9"= "NA","-8"="I don't know", "-1"="Appropriate Skip",
                                                  "1"="Biodiesel","2"= "Plug-in \nHybrid", "3"="Electric",
                                                  "4"=  "Hybrid","97"= "Some other Fuel")))

c
head(veh17$HFUEL)

######compare hh income and gas vs non gas
#create gas/nongas grouping var
veh17 <- veh17%>% mutate(gas_grouping= ifelse(HFUEL==-1,1,0))
#filter out IDK and NA
c<- ggplot(veh17 %>% filter(veh17$HFUEL!=-8 & veh17$HFUEL!=-9), aes(y=HHFAMINC))+
  geom_boxplot()+
  facet_wrap(~gas_grouping, labeller = labeller(gas_grouping= c("1"="Gasoline", 
                                                          "0"="Non-gasoline")))
c







#t test 
#filter for appropriate skip vehicles
gas_veh <- veh17%>% filter(HFUEL==-1)
#filter for electric vehicles
elec_veh <- veh17%>% filter(HFUEL==3)

t.test(gas_veh$HHFAMINC, elec_veh$HHFAMINC)
