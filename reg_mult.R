

boston = readxl::read_excel("boston.xlsx")

b = boston[,-1]

#esquisse::esquisser(b)
#esquisse::esquisser(boston)

#adicionando labels
b$chas=as.factor(b$chas)
levels(b$chas)=c("otherwise", "bounds river")

#Transformando log
b$crim = log(b$crim)

#Exibir os dados
summary(b)

#Criar regressaão linear com todas as variaveis
reg.mlt=lm(data=b, medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + lstat)
summary(reg.mlt)

#multicolinearidade
library(rms)
round(vif(reg.mlt),1)

#anomalias
library(car)
influenceIndexPlot(reg.mlt , vars=c("Cook","Studentized","hat"))

#seleção de variáveis
reg.mlt2=step(reg.mlt);summary(reg.mlt2)
influenceIndexPlot(reg.mlt2 , vars=c("Cook","Studentized","hat"))

#previsao
hp$LPRICE_HAT=fitted.values(reg.mlt2)
hp$RES=residuals(reg.mlt2)
hp$EP=hp$RES/hp$LPRICE*100
plot(hp$EP)
grid(col=4)

SQFT=2500; lsqft=log(SQFT)
TAX=1200; ltax=log(TAX)
novo=data.frame(COR="no", LSQFT=lsqft, LTAX=ltax)
lprice.hat=predict(reg.mlt2, novo);lprice.hat
pricenew.hat=exp(lprice.hat);pricenew.hat

anova(reg.mlt2)
