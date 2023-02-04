#Diameter distributions----

# Data import and handling ----
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

# Percent remaining ----
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

#Helper function Percent Remaining
getClassWisePercentAfterFelling <- function(QMD)
{
  ClassMin=seq(0,40,2)
  ClassMax=c(seq(1,40,2),60)
  PercentOfRemaining=pnorm(ClassMax+0.5,
                           mean=(-0.2531+0.9817*QMD),
                           sd=2.0064+0.1282*QMD)-
    pnorm(ifelse(ClassMin!=0,ClassMin-0.5,ClassMin),
          mean=(-0.2531+0.9817*QMD),
          sd=2.0064+0.1282*QMD)
  
  return(
    data.frame(
      ClassMin,
      ClassMax,
      PercentOfRemaining
    )
  )
}


# Percent Felled----
# Apply function to each QMD_class and store in a dataframe
data <- distributions %>% rename(Min=left,Max=right,QMD_class=`QMD-class`) %>% dplyr::select(Min,Max,QMD_class,PercentFelled)

data <-as.data.frame(data)
data[is.na(data$PercentFelled),]$PercentFelled <- 0

norm_fit2 <- function(QMD,df){
  # Filter data by QMD_class
  data <- df %>% dplyr::filter(QMD_class == QMD)
  
  # Use Sheppard's correction to estimate mean and standard deviation
  mean_est <- sum((data$Min + data$Max)/2 * data$PercentFelled) / sum(data$PercentFelled)
  sd_est <- sqrt(sum((((data$Min + data$Max)/2 - mean_est)^2) * data$PercentFelled) / sum(data$PercentFelled))
  
  return(c(mean_est, sd_est))
}

param_estimates <- data.frame(QMD_class = unique(data$QMD_class), 
                              mean = sapply(unique(data$QMD_class), norm_fit2,df=data)[1,], 
                              sd = sapply(unique(data$QMD_class), norm_fit2,df=data)[2,])

# Fit a linear model to relate the parameters to the QMD_class
lm_mean <- lm(mean ~ QMD_class, data = param_estimates)
lm_sd <- lm(sd ~ QMD_class, data = param_estimates)

getClassWisePercentFelled <- function(QMD)
{
  ClassMin=seq(0,40,2) 
  ClassMax=c(seq(1,40,2),60)
  PercentOfRemaining=pnorm(ClassMax+0.5,
                           mean=(-0.6223+0.9930*QMD),
                           sd=2.2371+0.1482*QMD)-
    pnorm(ifelse(ClassMin!=0,ClassMin-0.5,ClassMin),
          mean=(-0.6223+0.9930*QMD),
          sd=2.2371+0.1482*QMD)
  
  return(
    data.frame(
      ClassMin,
      ClassMax,
      PercentOfRemaining
    )
  )
}


## Fit data for height vs HL
distributions %>% ggplot(aes(x=Class,y=PercentHLRemaining,group=`QMD-class`))+
  geom_line(aes(col=as.numeric(rev(`QMD-class`))))+
  geom_function(fun=function(x){predict(HLrelative,newdata=list(Class=x,`QMD-class`=rep(30.5,length(x))),type="response")})+
  geom_function(fun=function(x){predict(HLrelative,newdata=list(Class=x,`QMD-class`=rep(28.5,length(x))),type="response")})+
  geom_function(fun=function(x){predict(HLrelative,newdata=list(Class=x,`QMD-class`=rep(15.5,length(x))),type="response")})+
  geom_function(fun=function(x){predict(HLrelative,newdata=list(Class=x,`QMD-class`=rep(10.5,length(x))),type="response")})+
  geom_function(fun=function(x){predict(HLrelative,newdata=list(Class=x,`QMD-class`=rep(7.5,length(x))),type="response")})+
  geom_function(fun=function(x){predict(HLrelative,newdata=list(Class=x,`QMD-class`=rep(4.5,length(x))),type="response")})



#Heights of remaining trees relative HL----
#Fit Näslund 1936.
# H-1.3 = d^2/((a+bd)^2) for H~D relation for Pine.

relative.height = function(d1, d2, a, b, c, f, g) {
  ((1.3 + d1^2/((a+b*d1)^2)) / (1.3 + (d2+c*d2+f*d2^2)^2/((a+b*(d2+c*d2+f*d2^2))^2)))*g
}

HLrelative = nls(PercentHLRemaining~relative.height(d1=Class,d2=as.numeric(`QMD-class`),a=a,b=b,c=c,f=f,g=g),
                 start=list(a=0.5,b=0.1,c=0.5,f=0.2,g=2),
                 data=distributions,
                 control=nls.control(maxiter = 2000))



RemainingStandClassHeightRelativeHL <- function(QMD,ClassMidPoint)
{
  a = 2.183746  
  b = 0.239257 
  c = 0.614209
  f = -0.005992
  g = 1.142162 
  
  return(
    ((1.3 + ClassMidPoint^2/((a+b*ClassMidPoint)^2)) / (1.3 + (QMD+c*QMD+f*QMD^2)^2/((a+b*(QMD+c*QMD+f*QMD^2))^2)))*g
  )
}


distributions %>% filter(`QMD-class`==20.5) %>% ggplot(aes(x=Class,y=PercentHLRemaining))+
  geom_line()+
  geom_function(fun=function(x){predict(HLrelative,newdata=list(Class=x,`QMD-class`=20.5))})



#Heights of Felled trees relative to HL----
distributions %>% ggplot(aes(x=Class,y=PercentHLFelled,group=`QMD-class`))+
  geom_line(aes(col=as.numeric(rev(`QMD-class`))))

HLrelative = nls(PercentHLFelled~relative.height(d1=Class,d2=as.numeric(`QMD-class`),a=a,b=b,c=c,f=f,g=g),
                 start=list(a=2.5,b=0.27,c=1.22,f=-0.006,g=1.24),
                 data=distributions,
                 control=nls.control(maxiter =10000,minFactor = 1E-12))

distributions %>% filter(`QMD-class`==4.5) %>% ggplot(aes(x=Class,y=PercentHLFelled))+
  geom_line()+
  geom_function(fun=function(x){predict(HLrelative,newdata=list(Class=x,`QMD-class`=4.5))})

FelledStemsClassHeightRelativeHL <- function(QMD,ClassMidPoint)
{
  a = 2.493915
  b = 0.273404
  c = 1.122605
  f = -0.006501
  g = 1.248597 
  
  return(
    ((1.3 + ClassMidPoint^2/((a+b*ClassMidPoint)^2)) / (1.3 + (QMD+c*QMD+f*QMD^2)^2/((a+b*(QMD+c*QMD+f*QMD^2))^2)))*g
  )
}
