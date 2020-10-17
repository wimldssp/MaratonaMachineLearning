rm(list = ls())
cat("\014")

# Chapter 8 Lab: Decision Trees

###############################################################################
# Fitting Classification Trees
###############################################################################


# Usaremos árvores de classificação para analisar o dataset Carseats.
library(tree)
library(ISLR)
attach(Carseats)

# Como Sales é uma variável contínua, iremos criar a classe binária High, que
# terá o valor Yes se Sales > 8
High = factor(ifelse(Sales<=8,"No","Yes"))

# Vamos concatenar High ao dataset Carseats
Carseats = data.frame(Carseats,High)

# Ajustar uma árvore de classificação que usará todas as variáveis preditoras
# exceto Sales para prever High
tree.carseats = tree(High~.-Sales,Carseats)

# Listar as variáveis usadas como nós internos, número de nós folha e erro
summary(tree.carseats)

# Exibir a estrutura da árvore com os textos e nomes das categorias
plot(tree.carseats)
text(tree.carseats,pretty=0)

# Exibir as saídas de cada ramo da árvore, o critério de split, 
# o número de observações em cada ramo, o desvio, a predição para o ramo (Yes ou No)
# e a fração de observações em cada ramo com Yes ou No. As folhas são indicadas com *
tree.carseats

# Setar uma semente
set.seed(2)

# Dividir as observações em conjuntos de treino e teste
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train,]
High.test = High[-train]

# Construir a árvore no conjunto de treino e avaliar sua performance no conjunto de teste
tree.carseats = tree(High~.-Sales,Carseats,subset=train)
tree.pred = predict(tree.carseats,Carseats.test,type="class")

# Exibir os resultados de predição
table(tree.pred,High.test)
(104+50)/200


###############################################################################
# Fitting Regression Trees
###############################################################################

library(MASS)

# Setar uma semente
set.seed(1)

# Dividir as observações em conjuntos de treino e teste
train = sample(1:nrow(Boston), nrow(Boston)/2)

# Construir a árvore no conjunto de treino
tree.boston = tree(medv~.,Boston,subset=train)

# Listar as variáveis usadas como nós internos, número de nós folha e erro
summary(tree.boston)

# Exibir a estrutura da árvore com os textos e nomes das categorias
plot(tree.boston)
text(tree.boston,pretty=0)

# Avaliar a performance da árvore original no conjunto de teste
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

###############################################################################
# Bagging and Random Forests
###############################################################################

# A função randomForest() pode ser usada tanto para Bagging quanto Random Forests
library(randomForest)

# Bagging

set.seed(1)

# Construir o modelo no conjunto de treino
# O argumento mtry indica que os 13 preditores devem ser considerados para as
# divisões da árvore
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston

# Avaliar o modelo no conjunto de teste
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

# É possível alterar o número de árvores com o parâmetro ntree
bag.boston = randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

# Random Forest

set.seed(1)

# Construir o modelo no conjunto de treino
# O argumento mtry indica que apenas 6 preditores devem ser considerados para as
# divisões da árvore
rf.boston = randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)

# Importância de cada variável
importance(rf.boston) 
varImpPlot(rf.boston)

###############################################################################
# Boosting
###############################################################################

library(gbm)
set.seed(1)

# Construir o modelo no conjunto de treino
# distribution="gaussian" indica que é um problema de regressão
# n.trees=5000 indica 5000 árvores
# interaction.depth=4 limita a profundidade de cada árvore
boost.boston = gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)

# Mostrar um gráfico e estatísticas de influência relativa
summary(boost.boston)

# Avaliar o modelo no conjunto de teste
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)

# Construir outro modelo no conjunto de treino, variando alguns parâmetros
boost.boston = gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost = predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)

