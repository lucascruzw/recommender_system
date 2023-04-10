#Este Projeto tem por sua finalidade indicar a melhor recomendação para indicação de filmes:

library(recommenderlab)
library(dplyr)
library(ggplot2)                    
library(data.table)
library(reshape2)
library(haven)

setwd <- ("C:/Users/lucas/OneDrive/Área de Trabalho/MBA Data Science & Analytics/TCC/Movie Recommendation")

set.seed(1234)

# Para trabalhar nesse aprendizado em máquina de recomendação de filmes, busquei
# os bancos de dados do IMDB e MovieLens para os filmes e suas respectivas 
# classificações:

filmes <- read.csv("movies.csv")#, stringsAsFactors = FALSE)
classificacao <- read.csv("ratings.csv")

classificacao_rep <- classificacao %>% group_by(userId) %>% count()

# Verificando a estratura dos bancos de dados:
str(filmes)
str(classificacao)

# Verificando um resumo de todo o banco de dados com alguns dados estatisticos:
summary(filmes)
summary(classificacao)

#verificando cabeçalho dos banco de dados:
head(filmes)
head(classificacao)

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

# Olhando a estrutura da Matriz:
str(gen_matriz_2)

# Juntando a matriz criada com os valores binarios para cada genero com a base 
#dos filmes exlcuindo a coluna genero (3 coluna):
localizar <- cbind(filmes[,1:2], gen_matriz_2)
head(localizar)

# Agora iremos criar uma matriz baseado na base de dados de classificacao
#onde as colunas sao os movieId`s e linhas os userId`s
matriz_classificacao_antes <- dcast(classificacao, userId~movieId, value.var = "rating", na.rm = FALSE)
matriz_classificacao_antes <- as.matrix(matriz_classificacao_antes[,-1]) # removendo coluna userId`s
class(matriz_classificacao_antes)
# Como sugere o pacore recommenderlab, convertemos em uma matriz esparsa, pois a maioria dos valores
#sao iguais a zero
matriz_classificacao <- as(matriz_classificacao_antes, "realRatingMatrix")
matriz_classificacao
getRatingMatrix(matriz_classificacao)
identical(as(matriz_classificacao, "matrix"),matriz_classificacao_antes)
#as(matriz_classificacao, "list")
head(as(matriz_classificacao, "data.frame"))

write.csv(as(matriz_classificacao_antes, "matrix"), "C:/Users/lucas/OneDrive/Área de Trabalho/MBA Data Science & Analytics/TCC/Movie Recommendation/matriz.csv")

# Faremos uma visão geral sobre os importantes parametros de modelos de recomendação 
#que nos traz varias opcoes
modelo_recomendacao <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
modelo_recomendacao

# Usando lapply para aplicar a função em cada parametro de modelo de recomendação e trazer
#suas descricoes
lapply(modelo_recomendacao, "[[", "description")

# Como nos estamos trazendo um banco de dados com dados de usuários diversos e suas classificacoes
#para cada filme que cada um assitiu, vamos trabalhar com o modelo de recomendacao 
#baseado em filtragem colaborativa - IBCF 
modelo_recomendacao$UBCF_realRatingMatrix$parameters
modelo_recomendacao$IBCF_realRatingMatrix$parameters

# Filtragem colaborativa envolve uma sugestão de filmes baseado na coleta de preferencias
#de outros usuarios. Portanto, recomendação de filme é dependente de uma realacao de
#similaridade entre dois usuários ou mais. No pacote recommenderlab podemos utilizar a funcao
#"similarity", que vai nos trazer uma matriz de similaridade entre usuários e também filmes:
matriz_similaridade <- similarity(matriz_classificacao[1:10,], method = "cosine", which = "users")
as.matrix(matriz_similaridade)
write.csv(as(matriz_similaridade, "matrix"), "C:/Users/lucas/OneDrive/Área de Trabalho/MBA Data Science & Analytics/TCC/Movie Recommendation/matriz_similaridade.csv")
image(as.matrix(matriz_similaridade), main = "Similaridades de Usuários")

# A correlação de pearson é definida em uma faixa entre -1 e 1, no caso da 
#similaridade para essa biblioteca soma 1 no coeficiente e divide por 2, para
#manter uma faixa entre 0 e 1
similaridade_filme <- similarity(matriz_classificacao[,1:10], method = "cosine", which = "items")
as.matrix(similaridade_filme)
write.csv(as(similaridade_filme, "matrix"), "C:/Users/lucas/OneDrive/Área de Trabalho/MBA Data Science & Analytics/TCC/Movie Recommendation/matriz_sim_filme.csv")

image(as.matrix(similaridade_filme), main = "Similaridades de Filmes")

# Vamos extrair as classificações exclusivas
classificacao_exclusiva <- as.vector(classificacao$rating)
unique(classificacao_exclusiva)
tabela_classificacao <- table(classificacao_exclusiva)
tabela_classificacao


# Vamos explorar os filmes mais visualizados do nosso banco de dados
filmes_visualiz <- colCounts(matriz_classificacao)
tabela_visualiz <- data.frame(filme = names(filmes_visualiz),
                              visualizacao = filmes_visualiz)
tabela_visualiz <- merge(tabela_visualiz, filmes, by.x = "filme", by.y = "movieId")
tabela_visualiz <- tabela_visualiz[order(tabela_visualiz$visualizacao,
                                         decreasing = TRUE), ]
tabela_visualiz <- select(tabela_visualiz, filme, visualizacao, title)


# Vamos vizualizar em gráfico de barras os 6 primeiros filmes mais assistidos
ggplot(tabela_visualiz[1:10, ], aes(x = title, y = visualizacao)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = visualizacao), vjust = -0.3, size = 3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Visualizações dos Top Filmes")

# vamos fazer um mapa de calor com as classificações dos filmes
image(matriz_classificacao[1:25, 1:25], axes = FALSE, main = "Mapa de Calor das primeiras 25 linhas e 25 colunas")


# Preparando nossos dados seguindo 3 passos: Selecionando dados úteis, Normalização e Binarização

# vamos selecionar aqueles usuários que classificaram um filme no mínimo 51 vezes e tambem por visualizacao
filmes_classificacao <- matriz_classificacao[rowCounts(matriz_classificacao) > 35 & rowCounts(matriz_classificacao) < 168, 
                                             colCounts(matriz_classificacao) > 35 & colCounts(matriz_classificacao) < 168]
filmes_classificacao

min_filmes <- quantile(rowCounts(filmes_classificacao), 0.98)
min_usuarios <- quantile(colCounts(filmes_classificacao), 0.98)
image(filmes_classificacao[rowCounts(filmes_classificacao) > min_filmes,
                    colCounts(filmes_classificacao) > min_usuarios],
      main = "Mapa de Calor dos melhores usuarios e filmes")

# Vamos visualizar a distribuição da média de classificação por usuário
media_classificacao <- rowMeans(filmes_classificacao)
qplot(media_classificacao, fill=I("steelblue"), col=I("red")) +
  ggtitle("Distribuição da média de classificação por Usuário")

# Vamos normalizar nossos dados, pois existem casos de alguns usuários onde pode
#existir classificações  muito altas ou muito baixas, isso age como um viés em nosso
#modelo
classificacao_norm <- normalize(filmes_classificacao)
sum(rowMeans(classificacao_norm) > 0.00001)
image(classificacao_norm[rowCounts(classificacao_norm) > min_filmes,
                         colCounts(classificacao_norm) > min_usuarios],
      main = "Classificações Normalizadas dos top usuários")

# No processo final de preparação iremos binarizar os dados. Significa que nossos dados
#tem dois valores discretos, 1 e 0, dos quais irão permitir um sistema de recomendação trabalhar
#de maneira mais eficiente
filme_min_bin <- quantile(rowCounts(filmes_classificacao), 0.95)
usuario_min_bin <- quantile(colCounts(filmes_classificacao), 0.95)
#filme_assistido <- binarize(filmes_classificacao, minRating = 1)

boa_classificacao <- binarize(filmes_classificacao, minRating = 3)
image(boa_classificacao[rowCounts(filmes_classificacao) > filme_min_bin,
                       colCounts(filmes_classificacao) > usuario_min_bin],
      main = "Mapa de Calor dos melhores usuários e filmes")

# Agora vamos desenvolver nosso próprio sistema de recomendação baseado em filtragem
#colaborativa (IBCF no recommenderlab). Este tipo de filtragem colaborativa encontra similaridade
#nos itens baseado em classificações de usuários

#amostra <- sample(x = c(TRUE, FALSE),
#                      size = nrow(filmes_classificacao),
#                      replace = TRUE,
#                      prob = c(0.8, 0.2))
#treino <- filmes_classificacao[amostra, ]
#teste <- filmes_classificacao[!amostra, ]
amostra <- evaluationScheme(filmes_classificacao, method="split", train=0.9, given=12)
treino <- getData(amostra, "train")
treino
teste_known <- getData(amostra, "known")
teste_known
teste_unknown <- getData(amostra, "unknown")
teste_unknown

# Exploramos os vários parametros de nosso sistema de recomendação baseado em filtragem
#colaborativa
sistema_rec <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
sistema_rec$IBCF_realRatingMatrix$parameters

modelo_rec <- Recommender(data = treino, method = "IBCF", list(method = "pearson"))
class(modelo_rec)

# Usando a função getModel() vamos retornar o modelo_rec. Vamos então encontrar a classe
#e dimensões das similaridades da matriz contidas dentro do info_model
info_model <- getModel(modelo_rec)
class(info_model$sim)
dim(info_model$sim)
top_itens <- 20
image(info_model$sim[1:top_itens, 1:top_itens],
      main = "Mapa de Calor das primeiras colunas e linhas")

# Vamos somar as linhas e colunas com as similaridades dos objetos acima
soma_lin <- rowSums(info_model$sim > 0)
table(soma_lin)

soma_col <- colSums(info_model$sim > 0)
qplot(soma_col, fill=I("steelblue"), col=I("red"))+ ggtitle("Distribuição da soma de colunas")

# Vamos criar uma variável chamada top_rec (top recomendações), que será inicializada até 10. Depois
#usaremos o predict() que vai identificar itens similares e irá ranquear os mesmos apropriadamente.
#Cada classificação é usada como peso. Cada peso é multiplicado com similaridades relacionadas. Depois
#tudo será adicionado no final
top_rec <- 10 # numero de recomendações para cada usuário
pred_rec <- predict(object = modelo_rec,
                                     newdata = teste_known,
                                     n = top_rec)
pred_rec

pred <- predict(modelo_rec, teste_known, type="ratings")
pred
usuario_1 <- pred_rec@items[[1]]
filmes_usuario1 <- pred_rec@itemLabels[usuario_1]
filmes_usuario2 <- filmes_usuario1
for (i in 1:10){
  filmes_usuario2[i] <- as.character(subset(filmes,
                                            filmes$movieId == filmes_usuario1[i])$title)
}
filmes_usuario2

matriz_recomendação <- sapply(pred_rec@items,
                              function(x){as.integer(colnames(filmes_classificacao)[x])})
matriz_recomendação[,1:4]


accuracy <- calcPredictionAccuracy(pred, teste_unknown)
accuracy
