#H50 expected mean height at 50 years of age.
#Mean diameter?

#Bonitet A, HL50 = 20, period length 3
#Bonitet B, HL50 = 17, period length 3
#Bonitet C, HL50 = 14, period length 4
#Bonitet D, HL50 = 11, period length 5
#Bonitet E, HL50 = 8, period length 5

#' Produksjonsundersøkelser i granskog.
#'
#' @source Eide, E., Langsæter, A. Produksjonundersøkelser i granskog.
#' Produktionsuntersuchungen von Fichtenwald. Medd. Norske Skogforsøksv.
#' 24. Bind. VII:1.
#'
#' @param ageBRH Age at breast height. (assumed).
#' @param BA Basal area of stand pr. ha. (m^2)
#' @param HL50 Lorey's mean height of the site quality at 50 years of age.
#' @param QMD Quadratic Mean Diameter (cm)
#' @param baRemovedThisPeriod Basal area removed in this period.(m^2)
#' @param baRemovedLastPeriod Basal area removed last period. (m^2)
#' @param baRemovedLastPeriod2 Basal area removed in the period before last period. (m^2)
#'
#' @return ANNUAL basal area increment in coming period.
#' @export
#'
#' @examples
#' #Worked example from p. 371.
#' Eide_Langsaeter_1941_BAI(age = 70,
#' HL50 = 14,
#' BA = 31.5,
#' QMD=19,
#' baRemovedThisPeriod = 3.06,
#' baRemovedLastPeriod = 3.07,
#' baRemovedLastPeriod2 = 3.18)
Eide_Langsaeter_1941_norway_spruce_BAI <- function(age,BA,HL50,QMD,baRemovedThisPeriod,baRemovedLastPeriod,baRemovedLastPeriod2){

## Observe obulus sign is negation in this paper!!
  return(
    3845.4/(age^2) + 14.5343/sqrt(BA) + 0.07517*HL50 + 14.5759/QMD + 0.13731*
      sqrt(((100*((baRemovedThisPeriod/2)+baRemovedLastPeriod+(baRemovedLastPeriod2/2)))/BA))-3.518
  )
}




Eide_Langsaeter_1941_norway_spruce_HL <- function(age,QMDFelled,QMDStanding,BA,baRemovedThisPeriod){
 #heightIncrementPercentage =  (33.3+0.715*(100*QMDFelled/HLStanding)+0.033*(100*baRemovedThisPeriod/BA)-0.029*age)
  return(
    (33.3+0.71512*(100*QMDFelled/QMDStanding)+0.03340*(100*baRemovedThisPeriod/BA)-0.02801*age)
  )
}
 #Increase due to thinning effect set at k=0.2, p. 379.
 ThinningEffect <- (0.2*(100*baRemovedThisPeriod/BA)/100)

 return(
   HLStanding+heightIncrementPercentage + (1+ThinningEffect)
 )

}


## Handling original data ----
library(tidyverse)
EideGrunnmaterial <- readxl::read_xlsx("G:/My Drive/Erfarenhetstabeller/Eide 1941 material/Eide1941GrunnmaterialComplete.xlsx",na = "NA")

EideGrunnmaterial <- EideGrunnmaterial %>% mutate(
  across(
    c(Plot,Region,`Site Quality`),factor
  ),
  across(
    cols=everything(-c(Plot,Region,`Site Quality`),numeric)
  )
)


EideGrunnmaterial %>% ggplot(aes(x=Age,y=`Standing HL m`,group=Plot,color=`Site Quality`))+geom_point()+geom_line()+xlim(c(0,160))


Eide2 <- EideGrunnmaterial %>% mutate(
  HLRel=100*`Delta H of trees also remaining in the next period m`/`Delta HL m`,
  gRel=100*`Basal area removed per ha m2`/(`Standing Basal Area per ha`+`Basal area removed per ha m2`)
  )

Eide2 %>% filter(Plot==1802)

#Chapman Richards
HL ~ asymptote(1-exp(-k*Age))^p


modelA <- nls(data=EideGrunnmaterial[which(EideGrunnmaterial$`Site Quality`=="A"),],formula = `Standing HL m`~asymp*(1-exp(-0.03*Age))^2,start=c(asymp=40))
summary(modelA)
modelB <- nls(data=EideGrunnmaterial[which(EideGrunnmaterial$`Site Quality`=="B"),],formula = `Standing HL m`~asymp*(1-exp(-0.03*Age))^2,start=c(asymp=40))
modelC <- nls(data=EideGrunnmaterial[which(EideGrunnmaterial$`Site Quality`=="C"),],formula = `Standing HL m`~asymp*(1-exp(-0.03*Age))^2,start=c(asymp=40))
modelD <- nls(data=EideGrunnmaterial[which(EideGrunnmaterial$`Site Quality`=="D"),],formula = `Standing HL m`~asymp*(1-exp(-0.03*Age))^2,start=c(asymp=40))
modelE <- nls(data=EideGrunnmaterial[which(EideGrunnmaterial$`Site Quality`=="E"),],formula = `Standing HL m`~asymp*(1-exp(-0.03*Age))^2,start=c(asymp=40))

EideGrunnmaterial %>% ggplot(aes(x=Age,y=`Standing HL m`,group=Plot,color=`Site Quality`))+geom_point()+geom_line()+xlim(c(0,160))+
  geom_function(fun=function(x) predict(modelA,data.frame(Age=x)))+
  geom_function(fun=function(x) predict(modelB,data.frame(Age=x)))+
  geom_function(fun=function(x) predict(modelC,data.frame(Age=x)))+
  geom_function(fun=function(x) predict(modelD,data.frame(Age=x)))+
  geom_function(fun=function(x) predict(modelE,data.frame(Age=x)))+
  ylim(c(0,27))


install.packages("quantreg")
EideGrunnmaterial2 <- EideGrunnmaterial %>% rename(HL=`Standing HL m`)
modelA <- quantreg::nlrq(data=EideGrunnmaterial2[which(EideGrunnmaterial2$`Site Quality`=="A"),],tau = 0.9,formula = HL~asymp*(1-exp(-0.025*Age))^2,start=c(asymp=40))
modelB <- quantreg::nlrq(data=EideGrunnmaterial2[which(EideGrunnmaterial2$`Site Quality`=="B"),],tau = 0.9,formula = HL~asymp*(1-exp(-0.025*Age))^2,start=c(asymp=40))
modelC <- quantreg::nlrq(data=EideGrunnmaterial2[which(EideGrunnmaterial2$`Site Quality`=="C"),],tau = 0.9,formula = HL~asymp*(1-exp(-0.025*Age))^2,start=c(asymp=40))
modelD <- quantreg::nlrq(data=EideGrunnmaterial2[which(EideGrunnmaterial2$`Site Quality`=="D"),],tau = 0.9,formula = HL~asymp*(1-exp(-0.025*Age))^2,start=c(asymp=40))
modelE <- quantreg::nlrq(data=EideGrunnmaterial2[which(EideGrunnmaterial2$`Site Quality`=="E"),],tau = 0.9,formula = HL~asymp*(1-exp(-0.025*Age))^2,start=c(asymp=40))


EideGrunnmaterial2 %>% ggplot(aes(x=Age,y=HL,group=Plot,color=`Site Quality`))+geom_point()+geom_line()+xlim(c(0,160))+
  geom_function(fun=function(x) predict(modelA,data.frame(Age=x)),size=1)+
  geom_function(fun=function(x) predict(modelB,data.frame(Age=x)),size=1)+
  geom_function(fun=function(x) predict(modelC,data.frame(Age=x)),size=1)+
  geom_function(fun=function(x) predict(modelD,data.frame(Age=x)),size=1)+
  geom_function(fun=function(x) predict(modelE,data.frame(Age=x)),size=1)+
  ylim(c(0,27))+
  geom_point(data=data.frame(x=50,y=c(8,11,14,17,20)),aes(x=x,y=y),inherit.aes=FALSE)