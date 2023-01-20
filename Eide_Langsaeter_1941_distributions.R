#Diameter distributions
library(tidyverse)

distributions <- readxl::read_xlsx(
  path="G:/My Drive/Erfarenhetstabeller/Eide 1941 material/Eide1941GrunnmaterialComplete.xlsx",
  sheet=3,col_types ="numeric",na="NA")

#Data Handling
distributions <- distributions %>% mutate(Max=case_when(
  Max!="Inf"~Max,
  Max=="Inf"~40)) %>%  #Replace Inf character with 40 in case of max value.
  rowwise() %>%
  rename(
    left=Min,
    right=Max
  ) %>% 
  mutate(
    Class=mean(c(left,right))
  ) %>%  
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


#Fit a linear regression to the parameters of a normal distribution to the QMD class.
library(fitdistrplus)

data <- distributions %>% rename(Min=left,Max=right,QMD_class=`QMD-class`) %>% dplyr::select(Min,Max,QMD_class,PercentRemaining)

data <-as.data.frame(data)
norm_fit <- function(QMD,df){
  # Filter data by QMD_class
  data <- df %>% dplyr::filter(QMD_class == QMD)
  
  # Use Sheppard's correction to estimate mean and standard deviation
  mean_est <- sum((data$Min + data$Max)/2 * data$PercentRemaining) / sum(data$PercentRemaining)
  sd_est <- sqrt(sum((((data$Min + data$Max)/2 - mean_est)^2) * data$PercentRemaining) / sum(data$PercentRemaining))
  
  return(c(mean_est, sd_est))
}

# Apply function to each QMD_class and store in a dataframe
param_estimates <- data.frame(QMD_class = unique(data$QMD_class), 
                              mean = sapply(unique(data$QMD_class), norm_fit,df=data)[1,], 
                              sd = sapply(unique(data$QMD_class), norm_fit,df=data)[2,])

# Fit a linear model to relate the parameters to the QMD_class
lm_mean <- lm(mean ~ QMD_class, data = param_estimates)
lm_sd <- lm(sd ~ QMD_class, data = param_estimates)
x<-seq(0,max(data$Max)+3,0.001)
for(i in 1:length(unique(data$QMD_class))){
  df = data[data$"QMD_class"==unique(data$QMD_class)[i],]
  plot(NULL,
       xlim=c(min(data$Min),max(data$Max)),
       ylim=c(0,1)
       )
  for(j in 1:nrow(df)){
    rect(xleft=df$Min-0.5,
         xright=df$Max+0.5,
         ybottom=0,
         ytop=df$PercentRemaining
           )
  }
  rect(xleft=df$Min-0.5,
       xright=df$Max+0.5,
       ybottom=0,
       ytop=pnorm(df$Max+0.5,
                  mean=(-0.2531+0.9817*(unique(data$QMD_class)[i])),
                  sd=(2.0064+0.1282*(unique(data$QMD_class)[i])))-
         pnorm(df$Min-0.5,
               mean=(-0.2531+0.9817*(unique(data$QMD_class)[i])),
               sd=(2.0064+0.1282*(unique(data$QMD_class)[i]))),density = 45
  )
}

