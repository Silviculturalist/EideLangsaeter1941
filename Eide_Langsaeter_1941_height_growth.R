# Exploring the height-age relationship from Eide & Langsaeter 1941.

## Load libraries ----
library(tidyverse) # General data handling.


## Load original data ----
EideGrunnmaterial <- readxl::read_xlsx("Eide1941GrunnmaterialComplete.xlsx", na = "NA")

EideGrunnmaterial <- EideGrunnmaterial %>% mutate(
  across(
    c(Plot, Region, `Site Quality`), factor
  ),
  across(
    cols = everything(-c(Plot, Region, `Site Quality`), numeric)
  )
)

EideGrunnmaterial %>% ggplot(aes(x = Age, y = `Standing HL m`, group = Plot, color = `Site Quality`)) +
  geom_point() +
  geom_line() +
  xlim(c(0, 160))


Eide2 <- EideGrunnmaterial %>% mutate(
  HLRel = 100 * `Delta H of trees also remaining in the next period m` / `Delta HL m`,
  gRel = 100 * `Basal area removed per ha m2` / (`Standing Basal Area per ha` + `Basal area removed per ha m2`)
)

EideGrunnmaterial2 <- EideGrunnmaterial %>%
  rename(HL = `Standing HL m`) %>%
  filter(!is.na(HL)) %>%
  mutate(SQ = `Site Quality`)

## Visualisation of material ----
EideGrunnmaterial %>% ggplot(aes(x = Age, y = `Standing HL m`, group = Plot, color = `Site Quality`)) +
  geom_point() +
  geom_line() +
  xlim(c(0, 160)) +
  ylim(c(0, 27))

## Regression ----
# Cieszewski GADA 2005.
# A four parameter GADA function is employed from the 2005 paper.
GADA2005Predict <- function(height, age, age2, a, b, c, j) {
  B <- b - ((age^j) / height)
  R <- sqrt(
    (B^2) - 2 * a * (c + (age^j))
  )

  return(
    (((age2 / age)^j) * height * (b + R) - (age2^j)) / (height * a * (1 - ((age2 / age)^j)) + B + R)
  )
}

# The function is vectorised.
vectGADA2005 <- Vectorize(GADA2005Predict)


# To minimise the negative log-likelihood.
minLogLikGADA <- function(Pars, data) {
  # Preliminaries
  N <- nrow(data)
  Tdiffs <- data$A2 - data$A1
  term1 <- (N / 2) * log(2 * pi * var(Tdiffs))
  sumSqLogDiffs <- sum(log(Tdiffs))
  halfVariance <- var(Tdiffs) / 2

  # Get estimates.
  predicts <- vectGADA2005(data$H1, data$A1, data$A2, a = Pars[1], b = Pars[2], c = Pars[3], j = Pars[4])
  sqPredErrors <- (data$H2 - predicts)^2

  sumSqPredbyTime <- sqPredErrors / (data$A2 - data$A1)
  finalterm <- sum(sumSqPredbyTime)

  returnVal <- (term1 + sumSqLogDiffs / 2 + halfVariance * finalterm)

  # Punish if ..
  # too fast growth in youth.
  # .. for high Site index.
  if (
    (GADA2005Predict(40, 100, 5, a = Pars[1], b = Pars[2], c = Pars[3], j = Pars[4]) > 5) |
      (GADA2005Predict(40, 100, 5, a = Pars[1], b = Pars[2], c = Pars[3], j = Pars[4]) < 0) |
      is.na(GADA2005Predict(40, 100, 5, a = Pars[1], b = Pars[2], c = Pars[3], j = Pars[4]))
  ) {
    returnVal <- returnVal + 999
  }

  # .. for low site index.
  if (
    (GADA2005Predict(12, 100, 5, a = Pars[1], b = Pars[2], c = Pars[3], j = Pars[4]) > 5) |
      (GADA2005Predict(12, 100, 5, a = Pars[1], b = Pars[2], c = Pars[3], j = Pars[4]) < 0) |
      is.na(GADA2005Predict(40, 100, 5, a = Pars[1], b = Pars[2], c = Pars[3], j = Pars[4]))
  ) {
    returnVal <- returnVal + 999
  }

  return(
    returnVal
  )
}


## Data handling----

# Get first measurement ages and heights.
summary1 <- EideGrunnmaterial2 %>%
  group_by(Plot) %>%
  summarise(A1 = min(Age), A2 = Age)

# Get respective heights for measurement occassions.
summary2 <- EideGrunnmaterial2 %>%
  group_by(Plot) %>%
  filter(Age == min(Age)) %>%
  summarise(H1 = HL)

summary3 <- EideGrunnmaterial2 %>%
  group_by(Plot, Age) %>%
  summarise(H2 = HL) %>%
  mutate(A2 = Age) %>%
  select(-Age)
summaryHeights <- summary1 %>%
  left_join(summary2) %>%
  left_join(summary3)

summaryHeights <- summaryHeights %>%
  ungroup() %>%
  filter((A2 - A1) > 0) %>%
  filter(!is.na(A1), !is.na(A2), !is.na(H1), !is.na(H2))


# Standard something wrong..
optim(fn = minLogLikGADA, par = c(-40, 670, 8000, 2.3), data = summaryHeights, lower = c(-200, 0, 0, 2), upper = c(0, 1000, 10000, 3), method = "L-BFGS-B")

# Converges better.
dfoptim::nmkb(fn = minLogLikGADA, par = c(-40, 670, 8000, 2.3), data = summaryHeights, lower = c(-200, 0, 0, 2), upper = c(0, 1000, 10000, 3))

# Plotting Results----
# The original publication has 6 (5) site qualities differentiated by age.
# A : 20 m by 50 y.
# B : 17 m..
# C : 14
# D : 11
# E, F : 8
EideGrunnmaterial2 %>% ggplot(aes(x = Age, y = HL, group = Plot, color = `Site Quality`)) +
  geom_point() +
  geom_line() +
  xlim(c(0, 200)) +
  geom_function(fun = function(x) GADA2005Predict(8, 50, x, -22.38129, 457.63666, 96.42790, 2.00000), aes(color = "E")) +
  geom_function(fun = function(x) GADA2005Predict(11, 50, x, -22.38129, 457.63666, 96.42790, 2.00000), aes(color = "D")) +
  geom_function(fun = function(x) GADA2005Predict(14, 50, x, -22.38129, 457.63666, 96.42790, 2.00000), aes(color = "C")) +
  geom_function(fun = function(x) GADA2005Predict(17, 50, x, -22.38129, 457.63666, 96.42790, 2.00000), aes(color = "B")) +
  geom_function(fun = function(x) GADA2005Predict(20, 50, x, -22.38129, 457.63666, 96.42790, 2.00000), aes(color = "A")) +
  ylim(c(0, 35)) +
  geom_point(data = data.frame(x = 50, y = c(8, 11, 14, 17, 20)), aes(x = x, y = y), inherit.aes = FALSE)


# Evaluate Fit----
summaryHeights <- summaryHeights %>% 
  mutate(
    predicts = GADA2005Predict(height = H1,age=A1,age2 = A2, -22.38129, 457.63666, 96.42790, 2.00000),
    resid = predicts - H2
  )

#Plot the density of the residuals.
summaryHeights %>% ggplot(aes(x=A2-A1,y=resid))+
                            geom_histogram(aes(y=..density..))


#Plot the ECDF
residECDF<-ecdf(summaryHeights$resid)
plot(x=seq(-3,3,0.01),y=residECDF(seq(-3,3,0.01)))
lines(x=seq(-3,3,0.01),y=pnorm(seq(-3,3,0.01),mean=0,sd=1))

#QQ-plot
plot(x=pnorm(seq(-3,3,0.01)),
     y=(residECDF(seq(-3,3,0.01))),
     xlab="Theoretical Normal Quantiles",
     ylab="Empirical Quantiles",
     asp=1,
     ylim = c(0,1),
     xlim = c(0,1)
)
abline(0,1)

#Calculate the RMSE.
summaryHeights %>% summarise(RMSE=sqrt(sum((resid)^2)/n()))



