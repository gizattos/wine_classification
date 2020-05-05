#### LENDO ARQUIVO E VERIFICANDO SEUS DADOS ####
vinhos_casa <- read.csv2(file = 'dados_vinhos.csv', stringsAsFactors = F, dec = '.')
summary(vinhos_casa)

#### REALIZANDO PRÉ-PROCESSAMENTO####
#### Trocando vírgulas por ponto e transformando o tipo da coluna alcohol e citric.acid - ####
vinhos_casa$citric.acid <- gsub(',','\\.',vinhos_casa$citric.acid)
vinhos_casa$citric.acid <- as.numeric(vinhos_casa$citric.acid)
vinhos_casa$alcohol <- as.numeric(vinhos_casa$alcohol)
summary(vinhos_casa)
vinhos_arrumado <- vinhos_casa

#### Com o dicionário de dados foi visto que o tipo 0 = white e o tipo 1 = red, por isso foi alterado no arquivo para melhor visualização  ####
vinhos_arrumado$type[vinhos_arrumado$type == '0'] <- 'white'
vinhos_arrumado$type[vinhos_arrumado$type == '1'] <- 'red'
summary(vinhos_arrumado)
vinhos_arrumado$type <- toupper(vinhos_arrumado$type)

####  Tratando os valores faltantes na coluna alcohol, para isso foi realizado a substituição pela média do tipo faltante ####
vinhos_arrumado$alcohol[is.na(vinhos_arrumado$alcohol) & (vinhos_arrumado$type == 'RED') ] <- mean(vinhos_arrumado$alcohol[vinhos_arrumado$type=='RED'],na.rm = T)
vinhos_arrumado$alcohol[is.na(vinhos_arrumado$alcohol) & (vinhos_arrumado$type == 'WHITE')] <- mean(vinhos_arrumado$alcohol[vinhos_arrumado$type=='WHITE'],na.rm = T)
summary(vinhos_arrumado)

#### Dividindo a qualidade em bom e ruim, bom>6 e ruim<=6 ####
tabela_cnome <- vinhos_arrumado
tabela_cnome$quality[tabela_cnome$quality <6 ] <-1
tabela_cnome$quality[tabela_cnome$quality >=6] <-2
tabela_cnome$quality<-as.character(tabela_cnome$quality)
tabela_cnome$quality[tabela_cnome$quality =='1'] <- 'ruim'
tabela_cnome$quality[tabela_cnome$quality =='2'] <- 'bom'
tabela_cnome$quality <-as.factor(tabela_cnome$quality)
summary(tabela_cnome$quality)
table(tabela_cnome$quality)

#### Preparando dados para aplicação do Random Forest ####
install.packages('caTools')
library(caTools)
dados_para_modelo <- tabela_cnome
dados_para_modelo$type <- NULL

#### Dividindo modelo em treino e teste ####
valores<-sample.split(dados_para_modelo$quality, SplitRatio = 0.8)
table(valores)
dados_para_modelo$flag <- valores
dado_treino <- dados_para_modelo[dados_para_modelo$flag == T,]
dado_treino$flag <- NULL
dado_teste <- dados_para_modelo[dados_para_modelo$flag == F,]
dado_teste$flag <- NULL

#### Aplicando modelo randomForest para prever a qualidade do vinho ####
install.packages('randomForest')
library(randomForest)
install.packages('caret')
library(caret)

#### Criando modelo ####
modelo_rf <-randomForest(quality ~ ., data = dado_treino)
modelo_rf
predicao <-predict(modelo_rf,newdata = dado_teste[,-12])

#### Visualizando os acertos do modelo  ####
dado_teste$predicao <- predicao
dado_teste
