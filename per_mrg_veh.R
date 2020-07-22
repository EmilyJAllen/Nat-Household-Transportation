library(ggplot2)

per17_veh<- merge(per17, veh17[,c("HOUSEID", "VEHID","HFUEL", "VEHAGE")],
                  by= c("HOUSEID"))

##################################
#       STUDENT FUEL TYPES
###################################
ggplot(per17_veh%>% filter(PRMACT==5), aes(x= as.factor(HFUEL)))+
  geom_bar()+
  scale_x_discrete( labels=c("NA", "I don't know", "Appropriate Skip",
                             "Biodiesel", "Plug-in \nHybrid", "Electric",
                             "Hybrid", "Some other Fuel"))+
  ggtitle("Fuel Type for Students")


table(per17_veh%>% filter(PRMACT==5) %>% 
                           select(HFUEL)) %>% prop.table()
#################################t
#       JOB SEEKER FUEL
#################################
c<- ggplot(per17_veh%>% filter(PRMACT==3), aes(x= HFUEL))+
  geom_bar()+
  scale_x_discrete( labels=c("NA", "Appropriate Skip",
                             "Biodiesel", "Plug-in \nHybrid", "Electric",
                             "Hybrid", "Some other Fuel"))+
  ggtitle("Fuel Type for Job-Seekers")
c
summary(per17_veh%>% filter(PRMACT==3) %>% select(HFUEL))

table(per17_veh%>% filter(PRMACT==3) %>% 
        select(HFUEL)) %>% prop.table()
#################################
#       EMPLOYED FUEL
##################################

c<- ggplot(per17_veh%>% filter(PRMACT==1), aes(x= HFUEL))+
  geom_bar()+
  scale_x_discrete( labels=c("NA", "I don't know", "Appropriate Skip",
                             "Biodiesel", "Plug-in \nHybrid", "Electric",
                             "Hybrid", "Some other Fuel"))+
  ggtitle("Fuel Type for Employed")
c

table(per17_veh%>% filter(PRMACT==1) %>% 
        select(HFUEL)) %>% prop.table()


###################################
#     STUDENT CAR AGE
###################################
c<- ggplot(per17_veh%>% filter(VEHAGE>0), aes(y=VEHAGE))+
  geom_boxplot()+
  facet_wrap(~PRMACT, labeller = labeller(PRMACT= c("-8"="I don't know", 
                                                    "-7"="I prefer not to answer",
                                                    "-1"="Appropriate skip",
                                                    "1"="Working", "2"="Temporarily absent \nfrom a job or business",
                                                    "3"="Looking for work/ unemployed",
                                                    "4"="A home-maker","5"= "Going to school",
                                                    "6"="Retired", "97"="Something else")))
c
c<- ggplot(per17_veh%>% filter(VEHAGE>0)%>%filter(PRMACT==5), aes(y=VEHAGE))+
  geom_boxplot()+
  ggtitle("VEHAGE distribution for students")
c


#   MEAN AGE
per17_veh_gr0<-per17_veh%>% filter(VEHAGE>0)
per17_veh_student<-per17_veh%>% filter(PRMACT==5)
mean(per17_veh_student$VEHAGE)

##################################
#   JOB SEEKER CAR AGE
#################################
per17_veh_jobseeker<-per17_veh%>% filter(PRMACT==3)
mean(per17_veh_jobseeker$VEHAGE)

#######################################
#   EMPLOYED CAR AGE
###################################
per17_veh_employed<-per17_veh%>% filter(PRMACT==3)
mean(per17_veh_jobseeker$VEHAGE)

