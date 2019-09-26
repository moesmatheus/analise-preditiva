cb=cardbalance[,-1]

#teriamos que analisar variavel por variavel etc. .....
summary(cb)

#rodar o modelo
fit=lm(data = cb, Balance~.)
summary(fit)
cb$bal_hat=fit$fitted.values # valores estimados pelo modelo
cb$res=fit$residuals# vamos utilizar no fim

#verificar multicol.
install.packages("rms")
library(rms)
vif(fit)
#em caso de multicolinearidade devemos "quebrar" essa relação entre variáveis
# em nosso caso deveríamos rodar sem Limit 


#detectar pontos discrepnates ou influentes 
install.packages("car")
influenceIndexPlot(fit , vars=c("Cook","Studentized","hat"))
# analisar os pntos, tentar entender se posso explicar por que?

#seleção de vars
fit2=step(fit)
summary(fit2)
#só por curiosidade vamos eliminar limit
fit3=lm(data = cb,Balance ~ Income +  Rating + Cards + Age + 
         Student)
summary(fit3)










