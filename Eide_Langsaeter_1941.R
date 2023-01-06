#H50 expected mean height at 50 years of age.
#Mean diameter?

#Bonitet A, HL50 = 20, period length 3
#Bonitet B, HL50 = 17, period length 3
#Bonitet C, HL50 = 14, period length 4
#Bonitet D, HL50 = 11, period length 5
#Bonitet E, HL50 = 8, period length 5


#' Pg Basal Area Increment Function for Norway Spruce in Norway.
#'
#' @source Eide, E., Langsæter, A. Produksjonundersøkelser i granskog.
#' Produktionsuntersuchungen von Fichtenwald. Medd. Norske Skogforsøksv.
#' 24. Bind. VII:1.
#'
#' @param age Age at breast height. (assumed).
#' @param age2 Age at end of period.
#' @param BA Basal area of stand pr. ha. (m^2)
#' @param HL50 Lorey's mean height of the site quality at 50 years of age.
#' @param QMD Quadratic Mean Diameter (cm)
#' @param baRemovedThisPeriod Basal area removed in this period.(m^2)
#' @param baRemovedLastPeriod Basal area removed last period. (m^2)
#' @param baRemovedLastPeriod2 Basal area removed in the period before last period. (m^2)
#'
#' @return ANNUAL basal area increment in coming period. (age2-age)
#' @export
#'
#' @examples
#' #Worked example from p. 371.
#' Eide_Langsaeter_1941_norway_spruce_BAI(age = 70,
#' age2=74
#' HL50 = 14,
#' BA = 31.5,
#' QMD=19,
#' baRemovedThisPeriod = 3.06,
#' baRemovedLastPeriod = 3.07,
#' baRemovedLastPeriod2 = 3.18)
Eide_Langsaeter_1941_norway_spruce_BAI <- function(age,BA,HL50,QMD,age2,baRemovedThisPeriod,baRemovedLastPeriod,baRemovedLastPeriod2)
  {

## Observe obulus sign is negation in this paper!!
  return(
    (3845.4/(age^2) + 14.5343/sqrt(BA) + 0.07517*HL50 + 14.5759/QMD + 0.13731*
      sqrt(((100*((baRemovedThisPeriod/2)+baRemovedLastPeriod+(baRemovedLastPeriod2/2)))/BA))-3.518)*(BA/100)*(age2-age) + BA
  )
}

#' GADA parametrised HL development from data in Eide & Langsaeter 1941.
#'
#' @param age Age1 in years.
#' @param age2 Age2 in years.
#' @param height HL in meters.
#'
#' @return HL at age2 (uncorrected for thinning)
#' @export
#'
#' @examples
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



#Correcting HL for thinning operations.
Eide_Langsaeter_1941_norway_spruce_HL_corr <- function(age,QMDFelled,QMDStanding,BAremaining,baRemovedThisPeriod){
  #Tab.15. p. 440, column 11.
  return(
    (33.3+0.71512*(100*QMDFelled/QMDStanding)+0.03340*(100*baRemovedThisPeriod/BAremaining)-0.02801*age)
  )
  # H, height growth according to function.
}

#  #Increase due to thinning effect set at k=0.2, p. 379.
#  ThinningEffect <- function(baRemovedThisPeriod, StandingBA){
#     0.2*(100*baRemovedThisPeriod/BA)/100)
#     HLStanding+heightIncrementPercentage + (1+ThinningEffect)
#  }
#  return(
#    
#  )
# 
# }

#bonitet C alder 62 HL = 
HL1 <- Eide_Langsaeter_1941_norway_spruce_HL(50,62,14)
# alder 66
HL2 <- Eide_Langsaeter_1941_norway_spruce_HL(50,66,14)

#Uncorrected HL increment.
((HL2-HL1)/4)

#removal 18.32


#Diameter- Height relationship
Eide_Langsaeter_1941_norway_spruce_diameter_to_height <- function(QMD)
{
  return(
    -1.929 + 1.41219*QMD - 0.01853*(QMD^2)
  )
}


# Bark thickness.
Eide_Langsaeter_1941_norway_spruce_double_bark_thickness<- function(age,height_m,diameter_cm)
{
  SQ = Eide_Langsaeter_1941_norway_spruce_HL(age,50,height_m)
  a= 2.83706 - 0.07129*SQ
  b= 0.75837 - 0.01666*SQ
  
  return(
    a+b*diameter_cm
  )
}
