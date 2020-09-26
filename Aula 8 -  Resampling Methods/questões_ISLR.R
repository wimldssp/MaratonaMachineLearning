library(ISLR) #carrega os banco de dados usados no livro
library(boot) 
library(MASS)
library(dplyr)
# Questão 5

#regressão logística usando o df Default 
glm.fit=glm(default~income + balance, data=Default, family=binomial())
#selcionando o conjunto de treino
set.seed(1)
train=sample(10000,5000)

#regressão usando o conjunto de treino
glm.fit2=glm(default~income + balance, data=Default, subset=train, family=binomial())
#prever default a partir da probabilidde posterior de análise:
coef = coef(glm.fit2)
for (i in 1:10000){
  Default$prob[i]=(exp(coef[1] + Default$income[i]*coef[2] + Default$balance[i]*coef[3]))/(1+exp(coef[1] + Default$income[i]*coef[2] + Default$balance[i]*coef[3]))
  Default$predict[i]= ifelse(Default$prob[i] > 0.5, "Yes", "No")
}
#calcular o erro de teste para o conjunto de validação:
Default$check = ifelse(Default$default==Default$predict, 0, 1)
frac = sum(Default[-train,]$check)/5000
#repetir esse processo três vezes
frac = rep(0,3)
for (j in 1:3){
  train=sample(10000,5000)
  glm.fit=glm(default~income + balance, data=Default, subset=train, family=binomial())
  coef = coef(glm.fit)
    for (i in 1:10000){
      Default$prob[i]=(exp(coef[1] + Default$income[i]*coef[2] + Default$balance[i]*coef[3]))/(1+exp(coef[1] + Default$income[i]*coef[2] + Default$balance[i]*coef[3]))
      Default$predict[i]= ifelse(Default$prob[i] > 0.5, "Yes", "No")
  }
  Default$check = ifelse(Default$default==Default$predict, 0, 1)
  frac[j] = sum(Default[-train,]$check)/5000
}


