#Read the data.
EideBark <- readxl::read_xlsx(path="G:/My Drive/Erfarenhetstabeller/Eide 1941 material/Eide1941GrunnmaterialComplete.xlsx",sheet = 2)

#Height GADA relationship.
Eide_Langsaeter_1941_norway_spruce_HL <- function(age,age2,height)
{
  a= -22.38129
  b= 457.63666
  c= 96.42790 
  j= 2.00000
  B <- b - ((age^j) / height)
  R <- sqrt(
    (B^2) - 2 * a * (c + (age^j))
  )
  
  return(
    (((age2 / age)^j) * height * (b + R) - (age2^j)) / (height * a * (1 - ((age2 / age)^j)) + B + R)
  )
}


#Inspect the data.
plot(EideBark$Age,EideBark$a,col=as.factor(EideBark$`Site Quality`),pch=18)
plot(EideBark$b,EideBark$a,col=as.factor(EideBark$`Site Quality`),pch=18)
plot(EideBark$Age,EideBark$b,col=as.factor(EideBark$`Site Quality`),pch=18)

#Transform the categorical site quality variables to their numeric counterpart
#i.e. height at age 50.
EideBark %>% mutate(
  SQ=case_when(
    `Site Quality`=="A"~20,
    `Site Quality`=="B"~17,
    `Site Quality`=="C"~14,
    `Site Quality`=="D"~11,
    `Site Quality`%in%c("E","F")~8
  )
) %>% 
  group_by(SQ) %>% 
  summarise(
    mean(a),mean(b)
  ) %>% lm(`mean(b)`~SQ,data=.)  #Simple linear regression
#Change to get the other parameter.

#Output relationships.
#a = 2.83706 - 0.07129*SQ
#b = 0.75837 - 0.01666*SQ

#Create a function for this.
#From age and height, get the Site Quality, thereafter calculate diameter thickness.
Eide_Langsaeter_1941_norway_spruce_double_bark_thickness<- function(age,height_m,diameter_cm)
{
  SQ = Eide_Langsaeter_1941_norway_spruce_HL(age,50,height_m)
  a= 2.83706 - 0.07129*SQ
  b= 0.75837 - 0.01666*SQ
  
  return(
    a+b*diameter_cm
  )
}

#Plot results.
#Results look excellent, compare plot 12 in original publication.
data.frame(x=seq(0,42,0.01)) %>% ggplot(aes(x=x)) +
  geom_function(
    fun=function(x) Eide_Langsaeter_1941_norway_spruce_double_bark_thickness(age = 50,height_m = 20,diameter_cm = x),
    aes(color="A")
  )+
  geom_function(
    fun=function(x) Eide_Langsaeter_1941_norway_spruce_double_bark_thickness(age = 50,height_m = 17,diameter_cm = x),
    aes(color="B")
  )+
  geom_function(
    fun=function(x) Eide_Langsaeter_1941_norway_spruce_double_bark_thickness(age = 50,height_m = 14,diameter_cm = x),
    aes(color="C")
  )+
  geom_function(
    fun=function(x) Eide_Langsaeter_1941_norway_spruce_double_bark_thickness(age = 50,height_m = 11,diameter_cm = x),
    aes(color="D")
  )+
  geom_function(
    fun=function(x) Eide_Langsaeter_1941_norway_spruce_double_bark_thickness(age = 50,height_m = 8,diameter_cm = x),
    aes(color="E")
  )
