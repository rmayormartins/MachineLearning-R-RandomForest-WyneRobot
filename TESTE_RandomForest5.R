
##################################################BIBLIOTECAS########################################################

library(caret)
library(datasets)
library(randomForest)



##################################################DATASET########################################################

#vinho <-read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", header = TRUE, sep = ";", dec = ".")
#str(win)
#table(win$quality)

setwd("C:/Program Files/RStudio")
vinho <-read.csv("winequality-white.csv", header = T, sep = ";", dec = ".")
#############################################PREPARACAO DO DATASET########################################################
#INSPEÇÃO DO DATASET
sum(is.na(vinho)) #conta (sum) a busca (is.na) por NA (not available) - vazios, NaN por exemplo

head(vinho, n = 7) #verifica as primeiras linhas do dataset 
#vinho[1,] vc pode acessar uma determinada linha
names(vinho) #as características químicas
#vinho$  vc pode acessar as colunas (o que nos interessa nesse caso está na coluna quality, são os rotulos de qualidade)
table(vinho$quality) #conta quantas notas de 3 até 9 
summary(vinho$quality)
barplot(table(vinho$quality))

#apesar da qualidade já ser uma variavel numerica que podemos classificar, transformamos em uma variavel categorica
#vinho$quality <- as.factor(vinho$quality) 

vinho$quality

###########################################transformando o dataset pra diminuir o a classificar###########################

vinho$voto <- ifelse(vinho$quality < 5, "bad", "good")
vinho$voto[vinho$quality == 5] <- "normal"
vinho$voto[vinho$quality == 6] <- "normal"
vinho$voto <- as.factor(vinho$voto)
str(vinho$voto)

table(vinho$voto)
barplot(table(vinho$voto))

#Divisão do DATASET EM TREINO E TESTE
nrow(vinho)
set.seed(123)
grupos <- sample(2, nrow(vinho), replace = T, prob=c(0.6, 0.4))
train <-vinho[grupos==1,]
test <- vinho[grupos==2,]



#############################################TREINAMENTO########################################################

#usando https://cran.r-project.org/web/packages/randomForest/randomForest.pdf


#CRIAÇÃO DO MODELO
library(randomForest)
#ntree number of trees grown.
#mtry number of predictors sampled for spliting at each node

# A ideia é reduzir a correlação das arvores...
# The idea in random forests ... is to improve the variance reduction of bagging by reducing the correlation between the trees, 
# without increasing the variance too much. 
# This is achieved in the tree-growing process through random selection of the input variables.

#EXECUTANDO O MODELO (TREINO)
model <- randomForest(quality ~ ., data = train, ntree = 10, mtry = 5) 
model <- randomForest(voto ~ . - quality, data = train, ntree = 10, mtry = 5, importance = TRUE, varImpPlot = TRUE, varUsed = TRUE) 

model

#cria modelo com 10 arvores, e cada arvore pode pegar 5 características
#padrão da função randomForest é 500 arvores e os subconjuntos de características é a raiz quadrada do número de características
model$importance

varImpPlot(model, sort=TRUE)
varUsed(model, by.tree=FALSE, count=TRUE)
names(vinho) #as características químicas
#n decreasing order of importance



#Explorando uma arvore
(getTree(model,3,labelVar=TRUE)) #0 se for terminal 
#labelVar usa label para split 
#split qual variavel é o nó, 0 se for terminal, status -1 e 1 Sim ou Nao, prediction o resultado.
nrow(getTree(model,3,labelVar=TRUE)) #extensão da árvore para todas as amostras apresentadas
maior_votacao <- data.frame(getTree(model,3,labelVar=TRUE)) #quem ela votou nas amostras
maior_votacao_1 <- na.omit(maior_votacao$prediction) #tirar os NA
maior_votacao_1
transform(maior_votacao_1) #transformar em um objeto para colocar na tabela
table(maior_votacao_1) #ela julgou mais normais...
table(vinho$quality) #conta quantas notas de 3 até 9 
table(vinho$voto) #conta quantas notas de 3 até 9 




#############################################TESTE########################################################
pred <- predict(model, test)

#############################################DESEMPENHO########################################################

table(test$voto, pred)
library(caret)
confusionMatrix(table(pred, test$voto))

library(gmodels)
CrossTable(test$voto, pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))




