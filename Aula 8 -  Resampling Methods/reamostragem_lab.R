#Exemplos livro 'An introduction to statistical learning' de Gareth James et. al
## pgs.191-

# Valida��o Conjunta
#Objetivo: estimar as taxas de erro de diversos modelos lineares. 
#install.packages("ISLR")
library(ISLR)
library(boot)
library(dplyr)
set.seed(1) # marca o processo aleat�rio.
train=sample(392,196)

# seleciona uma amostra aleat�ria para ser o conjunto de treino (train set)
lm.fit=lm(mpg~horsepower, data=Auto, subset=train) #performa uma regress�o linear tendo horsepower como vari�vel independente e mpg como vari�vel dependente. O  banco de dados usado � o Auto, mas apenas as observa��es contidas no conjunto de treino (train) s�o usadas (� um 'atalho' vinculado ao comando sample, n�o funcionar� em todas as situa��es).

#Depois de calcular os coeficientes para a regress�o linear acima, � poss�vel usar o comando predict() para estimar mpg para todas as 392 observa��es. A fun��o mean() � usada para calcular o MSE (mean squared error) para as 196 observa��es do conjunto de valida��o. 
attach(Auto)
mean((mpg-predict(lm.fit, Auto))[-train]^2) # o termo '[-train] informa ao R que somente as observa��es n�o contidas em train ser�o utilizadas 
#calculando o MSE para modelos polinomiais:
lm.fit2=lm(mpg~poly(horsepower, 2), data=Auto, subset=train) #A fun��o poly � usada pra calcular o polin�mio de ordem 2 da vari�vel horsepower
mean((mpg-predict(lm.fit2,Auto))[-train]^2)


lm.fit3=lm(mpg~poly(horsepower, 3), data=Auto, subset=train) 
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

## Valida��o Cruzada Leave-one-out (LOOCV) 
library(boot) 
glm.fit=glm(mpg~horsepower, data=Auto) #regress�o linear usando o comando glm que ser� necess�rio para etapas posteriores
cv.err=cv.glm(Auto, glm.fit)
cv.err$delta #estimativas da valida��o cruzada. Neste caso, os n�meros s�o id�nticos e correspondem a estat�stica LOOCV. 
#Vamos criar agora um loop para avaliar os resultados comparativamente para regress�es polinomiais de ordem 1 a 5. 
cv.error=rep(0,5) #vetor que ir� guardar os resultados
for (i in 1:5) { #loop
  glm.fit=glm(mpg~poly(horsepower,i), data=Auto)
  cv.error[i]=cv.glm(Auto, glm.fit)$delta[1]
}
cv.error
## Quest�o: qual o melhor modelo para an�lise, considerando as estimativas do MSE?

# Valida��o cruzada K-fold
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i), data=Auto)
  cv.error.10[i]=cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10
##Qual a diferen�a entre a valida��o cruzada k-fold e a LOOCV?

## Bootstrap 
#Estimando a precis�o de uma estat�stica de interesse
#criando uma fun��o alpha para estimar alpha:
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)- cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
#O comando abaixo estima essa fun��o para todos os dados no banco de dados Portfolio:
alpha.fn(Portfolio, 1:100)
# O pr�ximo comando usa a fun��o sample para selecionar aleatoriamente 100 observa��es do banco de dados original:
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace=T))
#A fun��o boot automatiza o processo e calcula o desvio-padr�o associado � distribui��o:
boot(Portfolio, alpha.fn, R=1000)

#Estimando a precis�o de um modelo de regress�o linear
boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower, data=data, subset=index)))
boot.fn(Auto, 1:392)
#agora podemos usar boot.fn combinado com sample para criar amostras aleat�rias do nosso banco de dados:
set.seed(1)
boot.fn(Auto, sample(392, 392, replace=T))
#comparando desvios-padr�o dos coeficientes do modelo  linear e do modelo de bootstrap
summary(lm(mpg~horsepower,data=Auto))$coef #dp coeficientes regressao linear

boot(Auto, boot.fn,1000) # dp usando o m�todo de bootstrap
boot(data=Auto, statistic = boot.fn, R=1000)
# A t�cnica de bootstrap n�o se baseia em pressupostos te�ricos sobre a rela��o entre as vari�veis (e seus termos de erro), muitas vezes oferecendo uma estimativa mais precisa dos coeficientes que os modelos de regress�o linear.

#Calculando os coeficientes e o desvio padr�o para o modelo de regress�o polinomial de ordem 2 com bootstrap:
boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2), data=data,subset=index))
set.seed(1)
boot(Auto, boot.fn,1000)

#Comparando os resultados com a regress�o polinomial:
summary(lm(mpg~horsepower + I(horsepower^2), data=Auto))$coef
# Os modelos providenciam coeficientes mais pr�ximos, demosntrando a adequabilidade do modelo quadr�tico