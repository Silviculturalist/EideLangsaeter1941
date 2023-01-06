### Diameter - height relationship
# Original - h = A + Bd + Cd^2 
# Suggested by original authors : h= A+ Bd + Cd^2 + Dd^3

#Data
plot(EideGrunnmaterial2$`Standing QMD cm`,EideGrunnmaterial2$HL,xlab="Diameter, cm",ylab="Lorey's Mean Height of Stand")

#Linear regression
hdrel <- lm(data=EideGrunnmaterial2,formula = HL~`Standing QMD cm`+ I(`Standing QMD cm`^2))
hdrel2 <- lm(data=EideGrunnmaterial2,formula = HL~`Standing QMD cm`+ I(`Standing QMD cm`^2)+I(`Standing QMD cm`^3))

#Sample data for calc.
df2 = data.frame(`Standing QMD cm`=seq(2.5,40,0.01),check.names = FALSE)

#Add lines to plot.
lines(x=df2$`Standing QMD cm`,y=predict(hdrel,newdata = df2))
lines(x=df2$`Standing QMD cm`,y=predict(hdrel2,newdata = df2),col="red")

anova(hdrel,hdrel2,test="LRT")
#ANOVA suggests improvement in RSS is not significant.

