#Exemplos livro 'An introduction to statistical learning' de Gareth James et. al
## pgs.191-

# Validação Conjunta
#Objetivo: estimar as taxas de erro de diversos modelos lineares. 
#install.packages("ISLR")
library(ISLR)
library(boot)
library(dplyr)
set.seed(1) # marca o processo aleatório.
train=sample(392,196)

# seleciona uma amostra aleatória para ser o conjunto de treino (train set)
lm.fit=lm(mpg~horsepower, data=Auto, subset=train) #performa uma regressão linear tendo horsepower como variável independente e mpg como variável dependente. O  banco de dados usado é o Auto, mas apenas as observações contidas no conjunto de treino (train) são usadas (é um 'atalho' vinculado ao comando sample, não funcionará em todas as situações).

#Depois de calcular os coeficientes para a regressão linear acima, é possível usar o comando predict() para estimar mpg para todas as 392 observações. A função mean() é usada para calcular o MSE (mean squared error) para as 196 observações do conjunto de validação. 
attach(Auto)
mean((mpg-predict(lm.fit, Auto))[-train]^2) # o termo '[-train] informa ao R que somente as observações não contidas em train serão utilizadas 
#calculando o MSE para modelos polinomiais:
lm.fit2=lm(mpg~poly(horsepower, 2), data=Auto, subset=train) #A função poly é usada pra calcular o polinômio de ordem 2 da variável horsepower
mean((mpg-predict(lm.fit2,Auto))[-train]^2)


lm.fit3=lm(mpg~poly(horsepower, 3), data=Auto, subset=train) 
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

## Validação Cruzada Leave-one-out (LOOCV) 
library(boot) 
glm.fit=glm(mpg~horsepower, data=Auto) #regressão linear usando o comando glm que será necessário para etapas posteriores
cv.err=cv.glm(Auto, glm.fit)
cv.err$delta #estimativas da validação cruzada. Neste caso, os números são idênticos e correspondem a estatística LOOCV. 
#Vamos criar agora um loop para avaliar os resultados comparativamente para regressões polinomiais de ordem 1 a 5. 
cv.error=rep(0,5) #vetor que irá guardar os resultados
for (i in 1:5) { #loop
  glm.fit=glm(mpg~poly(horsepower,i), data=Auto)
  cv.error[i]=cv.glm(Auto, glm.fit)$delta[1]
}
cv.error
## Questão: qual o melhor modelo para análise, considerando as estimativas do MSE?

# Validação cruzada K-fold
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i), data=Auto)
  cv.error.10[i]=cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10
##Qual a diferença entre a validação cruzada k-fold e a LOOCV?

## Bootstrap 
#Estimando a precisão de uma estatística de interesse
#criando uma função alpha para estimar alpha:
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)- cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
#O comando abaixo estima essa função para todos os dados no banco de dados Portfolio:
alpha.fn(Portfolio, 1:100)
# O próximo comando usa a função sample para selecionar aleatoriamente 100 observações do banco de dados original:
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace=T))
#A função boot automatiza o processo e calcula o desvio-padrão associado à distribuição:
boot(Portfolio, alpha.fn, R=1000)

#Estimando a precisão de um modelo de regressão linear
boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower, data=data, subset=index)))
boot.fn(Auto, 1:392)
#agora podemos usar boot.fn combinado com sample para criar amostras aleatórias do nosso banco de dados:
set.seed(1)
boot.fn(Auto, sample(392, 392, replace=T))
#comparando desvios-padrão dos coeficientes do modelo  linear e do modelo de bootstrap
summary(lm(mpg~horsepower,data=Auto))$coef #dp coeficientes regressao linear

boot(Auto, boot.fn,1000) # dp usando o método de bootstrap
boot(data=Auto, statistic = boot.fn, R=1000)
# A técnica de bootstrap não se baseia em pressupostos teóricos sobre a relação entre as variáveis (e seus termos de erro), muitas vezes oferecendo uma estimativa mais precisa dos coeficientes que os modelos de regressão linear.

#Calculando os coeficientes e o desvio padrão para o modelo de regressão polinomial de ordem 2 com bootstrap:
boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2), data=data,subset=index))
set.seed(1)
boot(Auto, boot.fn,1000)

#Comparando os resultados com a regressão polinomial:
summary(lm(mpg~horsepower + I(horsepower^2), data=Auto))$coef
# Os modelos providenciam coeficientes mais próximos, demosntrando a adequabilidade do modelo quadrático