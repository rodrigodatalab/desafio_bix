# DESAFIO CIENTISTA DE DADOS BIX TECNOLOGIA

# Configurando diretório de trabalho
setwd("C:/JOBS_R/Bix_Tecnologia")
getwd()

# --------------- MEU MODELO A PARTIR DAQUI ----------------

# OTIMIZAÇÃO DO PLANEJAMENTO DE MENUTENÇÃO PARA FROTA 

# DESCRIÇÃO DO WORK FLOW PARA DESENVOLVIMENTO DO MODELO

# Carregando pacotes necessários
library(caret)
library(corrplot)
library(dplyr)
library(ggplot2)
library(naniar)
library(party)
library(randomForest)
library(recipes)
library(visdat)

# carregando e verificando os dados
# Os dados de treino contém 60.000 linhas (registros) e 171 colunas (variáveis). 
# A coluna "class" representa a variável alvo.
# Os valores dessa variavel representam: 
#   "pos" apresenta defeito no sistema de ar (SIM);
#   "neg" apresenta outro tipo de defeito (NÃO);
dfbix_treino <- read.csv("dados/base_vigente_anos_anteriores.csv", 
                         header = TRUE,
                         na.strings = "na",
                         dec = ".",
                         stringsAsFactors = TRUE)

# Os dados de teste contém 16.000 linhas (registros) e 171 colunas (variáveis).
dfbix_teste <- read.csv("dados/base_vigente_2020.csv", 
                         header = TRUE,
                         na.strings = "na",
                         dec = ".",
                         stringsAsFactors = TRUE)

##### LIMPEZA E TRANSFORMAÇÃO ##### 

# checando se Os dados de treino e teste apresentam as mesmas variáveis.
colnames(dfbix_treino) == colnames(dfbix_teste)

# As variaveis foram carregadas corretamente.
str(dfbix_treino)
str(dfbix_teste)

# analisando o carregamento dos dados
# O dataset apresenta muitos valores missing
# o percentual de valores missing é de 8,3% para todo o dataset.
vis_miss(dfbix_treino[,1:30], warn_large_data = FALSE)
vis_miss(dfbix_treino[,31:60], warn_large_data = FALSE)
vis_miss(dfbix_treino[,61:90], warn_large_data = FALSE)
vis_miss(dfbix_treino[,91:120], warn_large_data = FALSE)
vis_miss(dfbix_treino[,121:150], warn_large_data = FALSE)
vis_miss(dfbix_treino[,151:171], warn_large_data = FALSE)
vis_miss(dfbix_treino, warn_large_data = FALSE)

# o dataset também apresenta muitos valores "0"
# esses valores também podem representar dados perdidos.
hist(dfbix_treino[,6])
hist(dfbix_treino[,7])
hist(dfbix_treino[,8])
hist(dfbix_treino[,9])
hist(dfbix_treino[,10])

# vou testar descartar variáveis com muitos valores "0".
# Somando os valores "na" com os valores "0" o dataset perde 40% dos dados.
dfbix_treino <- dfbix_treino %>%
  na_if("0")
vis_miss(dfbix_treino, warn_large_data = FALSE)

# Contanto os valores "na" por coluna e salvando em um novo data frame
# Calculando o percentual de dados faltantes por coluna
valores_missing <- data.frame(sapply(dfbix_treino, function(x) sum(is.na(x))))
colnames(valores_missing) <- "faltantes"
valores_missing$Percen_Faltantes <- sapply(valores_missing, 
                                           function(x) ((x/60000)*100))

# Identificando colunas com menos de 5% de dados faltantes.
# Excluindo dos dados de treino e teste, as colunas com mais de 5% faltantes.
valores_missing <- valores_missing[(valores_missing$Percen_Faltantes<5),]
dfbix_treino <- dfbix_treino[,rownames(valores_missing)]
dfbix_teste <- dfbix_teste[,rownames(valores_missing)]

# Analisando as variáveis, identifiquei que a variavel "cd_000" apresenta 
# valores iguais: O número 1209600 se repete em toda a coluna. 
# Vou remove-lá dos dados de treino e teste.
dfbix_treino$cd_000 <- NULL
dfbix_teste$cd_000 <- NULL

# agora os dados de treino e teste são compostos por 52 variáveis.
View(dfbix_treino)
View(dfbix_teste)

# verificando percentual de valores missing nos dados de treino e teste.
# dados de treino apresentam 2,4% de valores missing.
# dados de teste apresentam 1,1% de valores missing.
vis_miss(dfbix_treino, warn_large_data = FALSE)
vis_miss(dfbix_teste, warn_large_data = FALSE)

# Para diminuir os índices de dados faltantes, 
# a decisão é por imputar a mediana para variáveis numéricas.
# coletando os nomes das colunas para construir o código para imputação.
colnames(dfbix_treino)

# Solução nada elegante (mas que resolve) para imputar a mediana.
# Pesquisar/desenvolver uma função que faça isso  com um comando único.
# Imputação para dados de treino: substituindo valores em branco pela mediana
dfbix_treino$aa_000 <- impute_median(dfbix_treino$aa_000)
dfbix_treino$ag_004 <- impute_median(dfbix_treino$ag_004)
dfbix_treino$ag_005 <- impute_median(dfbix_treino$ag_005)
dfbix_treino$ag_006 <- impute_median(dfbix_treino$ag_006)
dfbix_treino$ah_000 <- impute_median(dfbix_treino$ah_000)
dfbix_treino$an_000 <- impute_median(dfbix_treino$an_000)
dfbix_treino$ao_000 <- impute_median(dfbix_treino$ao_000)
dfbix_treino$ap_000 <- impute_median(dfbix_treino$ap_000)
dfbix_treino$aq_000 <- impute_median(dfbix_treino$aq_000)
dfbix_treino$az_000 <- impute_median(dfbix_treino$az_000)
dfbix_treino$az_001 <- impute_median(dfbix_treino$az_001)
dfbix_treino$az_002 <- impute_median(dfbix_treino$az_002)
dfbix_treino$az_003 <- impute_median(dfbix_treino$az_003)
dfbix_treino$az_004 <- impute_median(dfbix_treino$az_004)
dfbix_treino$az_005 <- impute_median(dfbix_treino$az_005)
dfbix_treino$ba_000 <- impute_median(dfbix_treino$ba_000)
dfbix_treino$ba_001 <- impute_median(dfbix_treino$ba_001)
dfbix_treino$ba_002 <- impute_median(dfbix_treino$ba_002)
dfbix_treino$ba_003 <- impute_median(dfbix_treino$ba_003)
dfbix_treino$ba_004 <- impute_median(dfbix_treino$ba_004)
dfbix_treino$ba_005 <- impute_median(dfbix_treino$ba_005)
dfbix_treino$ba_006 <- impute_median(dfbix_treino$ba_006)
dfbix_treino$bb_000 <- impute_median(dfbix_treino$bb_000)
dfbix_treino$bg_000 <- impute_median(dfbix_treino$bg_000)
dfbix_treino$bh_000 <- impute_median(dfbix_treino$bh_000)
dfbix_treino$bi_000 <- impute_median(dfbix_treino$bi_000)
dfbix_treino$bj_000 <- impute_median(dfbix_treino$bj_000)
dfbix_treino$bs_000 <- impute_median(dfbix_treino$bs_000)
dfbix_treino$bt_000 <- impute_median(dfbix_treino$bt_000)
dfbix_treino$bu_000 <- impute_median(dfbix_treino$bu_000)
dfbix_treino$bv_000 <- impute_median(dfbix_treino$bv_000)
dfbix_treino$by_000 <- impute_median(dfbix_treino$by_000)
dfbix_treino$cb_000 <- impute_median(dfbix_treino$cb_000)
dfbix_treino$ci_000 <- impute_median(dfbix_treino$ci_000)
dfbix_treino$ck_000 <- impute_median(dfbix_treino$ck_000)
dfbix_treino$cn_004 <- impute_median(dfbix_treino$cn_004)
dfbix_treino$cn_005 <- impute_median(dfbix_treino$cn_005)
dfbix_treino$cq_000 <- impute_median(dfbix_treino$cq_000)
dfbix_treino$cs_000 <- impute_median(dfbix_treino$cs_000)
dfbix_treino$cs_001 <- impute_median(dfbix_treino$cs_001)
dfbix_treino$cs_002 <- impute_median(dfbix_treino$cs_002)
dfbix_treino$cs_003 <- impute_median(dfbix_treino$cs_003)
dfbix_treino$cs_004 <- impute_median(dfbix_treino$cs_004)
dfbix_treino$cs_005 <- impute_median(dfbix_treino$cs_005)
dfbix_treino$cs_006 <- impute_median(dfbix_treino$cs_006)
dfbix_treino$dn_000 <- impute_median(dfbix_treino$dn_000)
dfbix_treino$ee_000 <- impute_median(dfbix_treino$ee_000)
dfbix_treino$ee_001 <- impute_median(dfbix_treino$ee_001)
dfbix_treino$ee_002 <- impute_median(dfbix_treino$ee_002)
dfbix_treino$ee_003 <- impute_median(dfbix_treino$ee_003)
dfbix_treino$ee_004 <- impute_median(dfbix_treino$ee_004)

# Imputação para dados de teste: substituindo valores em branco pela mediana
dfbix_teste$aa_000 <- impute_median(dfbix_teste$aa_000)
dfbix_teste$ag_004 <- impute_median(dfbix_teste$ag_004)
dfbix_teste$ag_005 <- impute_median(dfbix_teste$ag_005)
dfbix_teste$ag_006 <- impute_median(dfbix_teste$ag_006)
dfbix_teste$ah_000 <- impute_median(dfbix_teste$ah_000)
dfbix_teste$an_000 <- impute_median(dfbix_teste$an_000)
dfbix_teste$ao_000 <- impute_median(dfbix_teste$ao_000)
dfbix_teste$ap_000 <- impute_median(dfbix_teste$ap_000)
dfbix_teste$aq_000 <- impute_median(dfbix_teste$aq_000)
dfbix_teste$az_000 <- impute_median(dfbix_teste$az_000)
dfbix_teste$az_001 <- impute_median(dfbix_teste$az_001)
dfbix_teste$az_002 <- impute_median(dfbix_teste$az_002)
dfbix_teste$az_003 <- impute_median(dfbix_teste$az_003)
dfbix_teste$az_004 <- impute_median(dfbix_teste$az_004)
dfbix_teste$az_005 <- impute_median(dfbix_teste$az_005)
dfbix_teste$ba_000 <- impute_median(dfbix_teste$ba_000)
dfbix_teste$ba_001 <- impute_median(dfbix_teste$ba_001)
dfbix_teste$ba_002 <- impute_median(dfbix_teste$ba_002)
dfbix_teste$ba_003 <- impute_median(dfbix_teste$ba_003)
dfbix_teste$ba_004 <- impute_median(dfbix_teste$ba_004)
dfbix_teste$ba_005 <- impute_median(dfbix_teste$ba_005)
dfbix_teste$ba_006 <- impute_median(dfbix_teste$ba_006)
dfbix_teste$bb_000 <- impute_median(dfbix_teste$bb_000)
dfbix_teste$bg_000 <- impute_median(dfbix_teste$bg_000)
dfbix_teste$bh_000 <- impute_median(dfbix_teste$bh_000)
dfbix_teste$bi_000 <- impute_median(dfbix_teste$bi_000)
dfbix_teste$bj_000 <- impute_median(dfbix_teste$bj_000)
dfbix_teste$bs_000 <- impute_median(dfbix_teste$bs_000)
dfbix_teste$bt_000 <- impute_median(dfbix_teste$bt_000)
dfbix_teste$bu_000 <- impute_median(dfbix_teste$bu_000)
dfbix_teste$bv_000 <- impute_median(dfbix_teste$bv_000)
dfbix_teste$by_000 <- impute_median(dfbix_teste$by_000)
dfbix_teste$cb_000 <- impute_median(dfbix_teste$cb_000)
dfbix_teste$ci_000 <- impute_median(dfbix_teste$ci_000)
dfbix_teste$ck_000 <- impute_median(dfbix_teste$ck_000)
dfbix_teste$cn_004 <- impute_median(dfbix_teste$cn_004)
dfbix_teste$cn_005 <- impute_median(dfbix_teste$cn_005)
dfbix_teste$cq_000 <- impute_median(dfbix_teste$cq_000)
dfbix_teste$cs_000 <- impute_median(dfbix_teste$cs_000)
dfbix_teste$cs_001 <- impute_median(dfbix_teste$cs_001)
dfbix_teste$cs_002 <- impute_median(dfbix_teste$cs_002)
dfbix_teste$cs_003 <- impute_median(dfbix_teste$cs_003)
dfbix_teste$cs_004 <- impute_median(dfbix_teste$cs_004)
dfbix_teste$cs_005 <- impute_median(dfbix_teste$cs_005)
dfbix_teste$cs_006 <- impute_median(dfbix_teste$cs_006)
dfbix_teste$dn_000 <- impute_median(dfbix_teste$dn_000)
dfbix_teste$ee_000 <- impute_median(dfbix_teste$ee_000)
dfbix_teste$ee_001 <- impute_median(dfbix_teste$ee_001)
dfbix_teste$ee_002 <- impute_median(dfbix_teste$ee_002)
dfbix_teste$ee_003 <- impute_median(dfbix_teste$ee_003)
dfbix_teste$ee_004 <- impute_median(dfbix_teste$ee_004)

# verificando novamente dados faltantes
# Agora a visualização mostra que não existem dados faltantes.
vis_miss(dfbix_treino, warn_large_data = FALSE)
vis_miss(dfbix_teste, warn_large_data = FALSE)

# verificando distribuição dos dados de treino
# os dados não seguem distribuição normal.
# algumas variáveis parecem sofrer com outliers.
boxplot(dfbix_treino[,2:12])
boxplot(dfbix_treino[,13:22])
boxplot(dfbix_treino[,23:32])
boxplot(dfbix_treino[,33:42])
boxplot(dfbix_treino[,43:52])

# Correlação entre variáveis numéricas
# alrgumas variáveis apresentam forte correlação positiva
# outras apresentam correlação positiva fraca.
# não foi observado correlação negativa.
numeric.var <- sapply(dfbix_treino, is.numeric)
corr.matrix <- cor(dfbix_treino[,numeric.var])
corrplot(corr.matrix,
         main="\n\nCorrelação entre Variáveis Numéricas",
         method="number")


# Decisão por padronizar os dados de treino e teste
# Pesquisar/desenvolver uma função que faça isso  com um comando único.
# Padronização dos dados de treino
dfbix_treino$aa_000 <- scale(dfbix_treino$aa_000)
dfbix_treino$ag_004 <- scale(dfbix_treino$ag_004)
dfbix_treino$ag_005 <- scale(dfbix_treino$ag_005)
dfbix_treino$ag_006 <- scale(dfbix_treino$ag_006)
dfbix_treino$ah_000 <- scale(dfbix_treino$ah_000)
dfbix_treino$an_000 <- scale(dfbix_treino$an_000)
dfbix_treino$ao_000 <- scale(dfbix_treino$ao_000)
dfbix_treino$ap_000 <- scale(dfbix_treino$ap_000)
dfbix_treino$aq_000 <- scale(dfbix_treino$aq_000)
dfbix_treino$az_000 <- scale(dfbix_treino$az_000)
dfbix_treino$az_001 <- scale(dfbix_treino$az_001)
dfbix_treino$az_002 <- scale(dfbix_treino$az_002)
dfbix_treino$az_003 <- scale(dfbix_treino$az_003)
dfbix_treino$az_004 <- scale(dfbix_treino$az_004)
dfbix_treino$az_005 <- scale(dfbix_treino$az_005)
dfbix_treino$ba_000 <- scale(dfbix_treino$ba_000)
dfbix_treino$ba_001 <- scale(dfbix_treino$ba_001)
dfbix_treino$ba_002 <- scale(dfbix_treino$ba_002)
dfbix_treino$ba_003 <- scale(dfbix_treino$ba_003)
dfbix_treino$ba_004 <- scale(dfbix_treino$ba_004)
dfbix_treino$ba_005 <- scale(dfbix_treino$ba_005)
dfbix_treino$ba_006 <- scale(dfbix_treino$ba_006)
dfbix_treino$bb_000 <- scale(dfbix_treino$bb_000)
dfbix_treino$bg_000 <- scale(dfbix_treino$bg_000)
dfbix_treino$bh_000 <- scale(dfbix_treino$bh_000)
dfbix_treino$bi_000 <- scale(dfbix_treino$bi_000)
dfbix_treino$bj_000 <- scale(dfbix_treino$bj_000)
dfbix_treino$bs_000 <- scale(dfbix_treino$bs_000)
dfbix_treino$bt_000 <- scale(dfbix_treino$bt_000)
dfbix_treino$bu_000 <- scale(dfbix_treino$bu_000)
dfbix_treino$bv_000 <- scale(dfbix_treino$bv_000)
dfbix_treino$by_000 <- scale(dfbix_treino$by_000)
dfbix_treino$cb_000 <- scale(dfbix_treino$cb_000)
dfbix_treino$ci_000 <- scale(dfbix_treino$ci_000)
dfbix_treino$ck_000 <- scale(dfbix_treino$ck_000)
dfbix_treino$cn_004 <- scale(dfbix_treino$cn_004)
dfbix_treino$cn_005 <- scale(dfbix_treino$cn_005)
dfbix_treino$cq_000 <- scale(dfbix_treino$cq_000)
dfbix_treino$cs_000 <- scale(dfbix_treino$cs_000)
dfbix_treino$cs_001 <- scale(dfbix_treino$cs_001)
dfbix_treino$cs_002 <- scale(dfbix_treino$cs_002)
dfbix_treino$cs_003 <- scale(dfbix_treino$cs_003)
dfbix_treino$cs_004 <- scale(dfbix_treino$cs_004)
dfbix_treino$cs_005 <- scale(dfbix_treino$cs_005)
dfbix_treino$cs_006 <- scale(dfbix_treino$cs_006)
dfbix_treino$dn_000 <- scale(dfbix_treino$dn_000)
dfbix_treino$ee_000 <- scale(dfbix_treino$ee_000)
dfbix_treino$ee_001 <- scale(dfbix_treino$ee_001)
dfbix_treino$ee_002 <- scale(dfbix_treino$ee_002)
dfbix_treino$ee_003 <- scale(dfbix_treino$ee_003)
dfbix_treino$ee_004 <- scale(dfbix_treino$ee_004)

# padronização dos dados de teste
dfbix_teste$aa_000 <- scale(dfbix_teste$aa_000)
dfbix_teste$ag_004 <- scale(dfbix_teste$ag_004)
dfbix_teste$ag_005 <- scale(dfbix_teste$ag_005)
dfbix_teste$ag_006 <- scale(dfbix_teste$ag_006)
dfbix_teste$ah_000 <- scale(dfbix_teste$ah_000)
dfbix_teste$an_000 <- scale(dfbix_teste$an_000)
dfbix_teste$ao_000 <- scale(dfbix_teste$ao_000)
dfbix_teste$ap_000 <- scale(dfbix_teste$ap_000)
dfbix_teste$aq_000 <- scale(dfbix_teste$aq_000)
dfbix_teste$az_000 <- scale(dfbix_teste$az_000)
dfbix_teste$az_001 <- scale(dfbix_teste$az_001)
dfbix_teste$az_002 <- scale(dfbix_teste$az_002)
dfbix_teste$az_003 <- scale(dfbix_teste$az_003)
dfbix_teste$az_004 <- scale(dfbix_teste$az_004)
dfbix_teste$az_005 <- scale(dfbix_teste$az_005)
dfbix_teste$ba_000 <- scale(dfbix_teste$ba_000)
dfbix_teste$ba_001 <- scale(dfbix_teste$ba_001)
dfbix_teste$ba_002 <- scale(dfbix_teste$ba_002)
dfbix_teste$ba_003 <- scale(dfbix_teste$ba_003)
dfbix_teste$ba_004 <- scale(dfbix_teste$ba_004)
dfbix_teste$ba_005 <- scale(dfbix_teste$ba_005)
dfbix_teste$ba_006 <- scale(dfbix_teste$ba_006)
dfbix_teste$bb_000 <- scale(dfbix_teste$bb_000)
dfbix_teste$bg_000 <- scale(dfbix_teste$bg_000)
dfbix_teste$bh_000 <- scale(dfbix_teste$bh_000)
dfbix_teste$bi_000 <- scale(dfbix_teste$bi_000)
dfbix_teste$bj_000 <- scale(dfbix_teste$bj_000)
dfbix_teste$bs_000 <- scale(dfbix_teste$bs_000)
dfbix_teste$bt_000 <- scale(dfbix_teste$bt_000)
dfbix_teste$bu_000 <- scale(dfbix_teste$bu_000)
dfbix_teste$bv_000 <- scale(dfbix_teste$bv_000)
dfbix_teste$by_000 <- scale(dfbix_teste$by_000)
dfbix_teste$cb_000 <- scale(dfbix_teste$cb_000)
dfbix_teste$ci_000 <- scale(dfbix_teste$ci_000)
dfbix_teste$ck_000 <- scale(dfbix_teste$ck_000)
dfbix_teste$cn_004 <- scale(dfbix_teste$cn_004)
dfbix_teste$cn_005 <- scale(dfbix_teste$cn_005)
dfbix_teste$cq_000 <- scale(dfbix_teste$cq_000)
dfbix_teste$cs_000 <- scale(dfbix_teste$cs_000)
dfbix_teste$cs_001 <- scale(dfbix_teste$cs_001)
dfbix_teste$cs_002 <- scale(dfbix_teste$cs_002)
dfbix_teste$cs_003 <- scale(dfbix_teste$cs_003)
dfbix_teste$cs_004 <- scale(dfbix_teste$cs_004)
dfbix_teste$cs_005 <- scale(dfbix_teste$cs_005)
dfbix_teste$cs_006 <- scale(dfbix_teste$cs_006)
dfbix_teste$dn_000 <- scale(dfbix_teste$dn_000)
dfbix_teste$ee_000 <- scale(dfbix_teste$ee_000)
dfbix_teste$ee_001 <- scale(dfbix_teste$ee_001)
dfbix_teste$ee_002 <- scale(dfbix_teste$ee_002)
dfbix_teste$ee_003 <- scale(dfbix_teste$ee_003)
dfbix_teste$ee_004 <- scale(dfbix_teste$ee_004)

# verificando os dados tratados
View(dfbix_treino)
View(dfbix_teste)

# Analisando a proporção dos labels da variável alvo
# Apenas 1,67% para "sim": apresenta defeito no sistema de ar;
# e 98,33% para "não": apresenta defeito no sistema de ar.
table(dfbix_treino$class)
dfbix_treino %>% ggplot(aes(x=class))+geom_bar(stat = "count")

# balancear os dados de treino, usanso metodo: upsample.
set.seed(246)
dfbix_treino <- 
  recipe(class~.,data = dfbix_treino) %>%
  themis::step_upsample(class) %>%
  prep()%>%
  juice()
dfbix_treino %>% ggplot(aes(x=class))+geom_bar(stat = "count")


###### salvando dados tratados para análise com MSAzure ######
# salvndo dados de treino
write.csv(dfbix_treino, 
          file = "dfbix_treino_tratados.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

# salvndo dados de treino
write.csv(dfbix_teste, 
          file = "dfbix_teste_tratados.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")


##### MODELAGEM PREDITIVA ##### 

# Os dados ja estão divididos em treino e teste
# Aproximadamente 88% treino e 12% teste.
dim(dfbix_treino);dim(dfbix_teste)


#### REGRESSÃO LOGÍSTICA ####

# Treinando o modelo de regressão logística Fitting do Modelo
LogModel_V1 <- glm(class ~ ., family=binomial(link="logit"), data=dfbix_treino)
print(summary(LogModel_V1))

# O nível de acurácia alcançou 97,2% para essa versão do modelo.
dfbix_teste$class <- as.character(dfbix_teste$class)
dfbix_teste$class[dfbix_teste$class=="pos"] <- "1"
dfbix_teste$class[dfbix_teste$class=="neg"] <- "0"
fitted.results <- predict(LogModel_V1,newdata=dfbix_teste,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != dfbix_teste$class)
print(paste('Logistic Regression Accuracy',1-misClasificError))

# Matriz de Confusão de Regressão Logística mostra que o modelo esta 
# ligeiramente melhor para prever o "NÃO" (97,2% de acerto). 
# as previsões de "SIM" (90,1% de acerto).
print("Confusion Matrix Para Logistic Regression");table(dfbix_teste$class,
                                                         fitted.results > 0.5)

#### ÁRVORE DE DECISÃO ####

# Visualização da Árvore de Decisão
# Usando variáveis apontadas pela regressão logistica.
tree <- ctree(class ~ aa_000 + ag_005 + ag_006 + ah_000 + an_000 + aq_000 + 
                az_001 + az_002 + ba_000 + ba_001 + ba_002 + bb_000 + bg_000 + 
                bh_000 + bs_000 + bt_000 + bv_000 + by_000 + cb_000 + ci_000 + 
                cn_004 + cn_005 + cq_000 + cs_002 + cs_003 + cs_004 + cs_005 + 
                ee_000 + ee_002, dfbix_treino)
plot(tree, type='simple')

# A variável "ci_000" aparece no topo da árvore para prever o "defeito",
# no sistema de freio da frota.
# Matriz de Confusão da Árvore de Decisão.
pred_tree <- predict(tree, dfbix_teste)
print("Confusion Matrix Para Decision Tree")
table(Predicted = pred_tree, Actual = dfbix_teste$class)

# Precisão da árvore de decisão
# Esse modelo apresentou acurácia ligeiramente maior (97,7%).
# mas foi muito ruim para prever o sim, apenas 45,8%.
B1 <- predict(tree, dfbix_teste)
tab1 <- table(Predicted = B1, Actual = dfbix_teste$class)
tab2 <- table(Predicted = pred_tree, Actual = dfbix_teste$class)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))

##### Random Forest #####
# minha máquina não de conta... kkkk
# Apresentou erro: "cannot allocate vector of size 900.3 Mb"
# Deixo o código, na esperança de funcionar nas podesosas máquinas da Bix!!
rfModel <- randomForest(class ~ ., data = dfbix_treino)
print(rfModel)
plot(rfModel)

# Prevendo valores com dados de teste
pred_rf <- predict(rfModel, testing)

# Confusion Matrix
print("Confusion Matrix Para Random Forest"); table(dfbix_treino$class, 
                                                    pred_rf)

# Variáveis mais importantes
varImpPlot(rfModel, sort=T, n.var = 8, main = 'Top 8 Feature Importance')
