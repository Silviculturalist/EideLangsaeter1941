#Diameter distributions

distributions <- readxl::read_xlsx(
  path="G:/My Drive/Erfarenhetstabeller/Eide 1941 material/Eide1941GrunnmaterialComplete.xlsx",
  sheet=3,col_types ="numeric",na="NA")

#Data Handling
distributions <- distributions %>% mutate(Max=case_when(
  Max!="Inf"~Max,
  Max=="Inf"~40)) %>%  #Replace Inf character with 40 in case of max value.
  rowwise() %>% mutate(
    Class=mean(c(Min,Max))
  ) %>% select(-c(Min,Max)) %>% 
  mutate(
    PercentRemaining=PercentRemaining/100,
    PercentFelled=PercentFelled/100,
    PercentHLRemaining=PercentHLRemaining/100,
    PercentHLFelled=PercentHLFelled/100
  ) 

#Check that percent remaining and felled in each class == 1.
distributions %>% 
  group_by(`QMD-class`) %>%
  summarise(
    sum(PercentRemaining,na.rm=TRUE),sum(PercentFelled,na.rm=TRUE)
  )
  

#Look at distribution - animated.
library(gganimate)
library(transformr) #required for line plots.
distributions %>% replace(is.na(.), 0) %>% ggplot(aes(x=Class,y=PercentRemaining))+
  geom_line()+
  labs(title="Percent remaining in 2-cm diameter classes as a function of QMD")+
  ylab("Remaining")+
  xlab("Diameter Class")+
  transition_states(
    `QMD-class`,
    state_length = 1,
    transition_length=2
  )

distributions %>% replace(is.na(.), 0) %>% ggplot(aes(x=Class,y=PercentFelled))+
  geom_line()+
  labs(title="Percent Felled in 2-cm diameter classes as a function of QMD")+
  ylab("Felled")+
  xlab("Diameter Class")+
  transition_states(
    `QMD-class`,
    state_length = 1,
    transition_length=2
  )
