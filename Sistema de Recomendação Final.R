library(recommenderlab)
library(dplyr)
library(ggplot2)                    
library(data.table)
library(reshape2)
library(haven)

setwd <- ("C:/Users/lucas/OneDrive/Área de Trabalho/MBA Data Science & Analytics/TCC/Movie Recommendation")

set.seed(123)

filmes <- read.csv("movies.csv")#, stringsAsFactors = FALSE)
classificacao <- read.csv("ratings.csv")



#Criando um banco de dados somente com a coluna genero:
genero <- as.data.frame(filmes$genres, stringsAsFactors = FALSE)

# Como cada filme pode ter mais de um genero, os mesmos vieram na mesma coluna
#separados por "|", nesse caso estamos dividindo e criando 1 coluna para cada:
genero2 <- as.data.frame(tstrsplit(genero[,1],'[|]', type.convert = TRUE), stringsAsFactors = FALSE)

# Renomeando as colunas de 1 a 10, pois temos filme com no maximo 10 generos:
colnames(genero2) <- c(1:10)

# Criando uma lista de todos os generos disponiveis no banco:

lista_genero <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime", 
                  "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "Musical",
                  "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western")

# Criando uma matrix X com o total de colunas do Banco de genero (18) e com 
#o total de linhas de filmes (incluindo o cabecalho 9743) com valores de 0
#e tambem, incluindo na primeira linha os generos (agora fica um total de 9742 filmes)
#para incluir como cabecalho:
gen_matriz_1 <- matrix(0, 9743,18)
gen_matriz_1[1,] <- lista_genero
colnames(gen_matriz_1) <- lista_genero

# Adicionando na matriz criada, para cada indice (filmes), seus respectivos generos,
#formando uma variavel binaria (1 ou 0):
for (index in 1:nrow(genero2)) {
  for (col in 1:ncol(genero2)) {
    gen_col = which(gen_matriz_1[1,] == genero2[index,col])
    gen_matriz_1[index+1,gen_col] <- 1
  }
}

# Removendo a primeira linha para manter somente os valores binarios de cada
#filme:
gen_matriz_2 <- as.data.frame(gen_matriz_1[-1,], stringsAsFactors=FALSE)

# Convertendo os valores de texto para inteiros:
for (col in 1:ncol(gen_matriz_2)) {
  gen_matriz_2[,col] <- as.integer(gen_matriz_2[,col]) 
} 

# Agora iremos criar uma matriz baseado na base de dados de classificacao
#onde as colunas sao os movieId`s e linhas os userId`s
matriz_classificacao<- dcast(classificacao, userId~movieId, value.var = "rating", na.rm = FALSE)
matriz_classificacao<- as.matrix(matriz_classificacao[,-1]) # removendo coluna userId`s
matriz_classificacao <- as(matriz_classificacao, "realRatingMatrix")
matriz_classificacao

#Collaborative Filtering
#Create an evaluation scheme bypliting the data and specifuing other parameters
evls <- evaluationScheme(matriz_classificacao, method="split", train=0.8,
                         given=12)

evls

trg <- getData(evls, "train")
trg

test_known <- getData(evls, "known")
test_known

test_unknown <- getData(evls, "unknown")
test_unknown

#Create UBCF recommender model with the trainning data
rcmnd_ub_p <- Recommender(trg, "UBCF", parameter = list(normalize = "center", method = "pearson"))
rcmnd_ub_p
rcmnd_ub_c <- Recommender(trg, "UBCF", parameter = list(normalize = "center", method = "cosine"))
rcmnd_ub_c

# Create predictions for the test users
pred_ub_p <- predict(rcmnd_ub_p, test_known, type="ratings")
pred_ub_p

pred_ub_c <- predict(rcmnd_ub_c, test_known, type="ratings")
pred_ub_c

#Evaluate the model accuracy for the unknown set of test users
acc_ub_p <- calcPredictionAccuracy(pred_ub_p, test_unknown)
as.matrix(acc_ub_p)
acc_ub_c <- calcPredictionAccuracy(pred_ub_c, test_unknown)
as.matrix(acc_ub_c)

#Compare the results
as(test_unknown, "matrix")[1:8,1:5]
as(pred_ub_p, "matrix")[1:8,1:5]
as(pred_ub_c, "matrix")[1:8,1:5]
#######

rcmnd_ib_p <- Recommender(trg, "IBCF", parameter = list(normalize = "center", method = "pearson"))
rcmd_ib_c <- Recommender(trg, "IBCF", parameter = list(normalize = "center", method = "cosine"))

pred_ib_p <- predict(rcmnd_ib_p, test_known, type="ratings")
pred_ib_c <- predict(rcmd_ib_c, test_known, type="ratings")
pred_ib_c
pred_ib_p

acc_ib_p <- calcPredictionAccuracy(pred_ib_p, test_unknown)
acc_ib_c <- calcPredictionAccuracy(pred_ib_c, test_unknown)

acc <- rbind(UBCF_Pearson = acc_ub_p, UBCF_Cosine = acc_ub_c, IBCF_Pearson = acc_ib_p, IBCF_Cosine = acc_ib_c)
acc
                  
#Get top recomendations
pred_ub_top <- predict(rcmnd_ub, test_known)
pred_ub_top      

pred_ib_p_top <- predict(rcmnd_ib_p, test_known)                        
pred_ib_p_top
