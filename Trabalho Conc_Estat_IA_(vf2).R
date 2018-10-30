

##Conceitos Estatísticos para IA
##Trabalho: Uso e análise dos conceitos e técnicas estatísticas
##Turma 2IA
##prof.: Adelaide Alves de Oliveira
##Grupo:
##Fábio Alex de Queiroz Jacob - RM 330989
##Allan Almeida - RM 330858
##Luca Pasquale - RM 330726


# Base   

# Limpar memória do R
rm(list=ls(all=TRUE))


# BaseWine - instalando pacotes

install.packages("psych")
install.packages("plotly")
install.packages("gmodels")
install.packages("corrgram")
install.packages("lmtest")
install.packages("rpart")
install.packages("randomForest")
install.packages("ROCR")
install.packages("pROC")
install.packages("oddsratio")



# Mostrar até 2 casas decimais
options("scipen" = 2)

# Ler arquivo csv

Vinhos <- read.csv2("C:/BaseWine_Red_e_White2018.csv")
 
# Mostrar a Tabela da Base de Dados
fix(Vinhos)

# Mostrar o formato das variáveis (Tabela da Base de Dados)
str(Vinhos)

# Mmostra as variáveis
names(Vinhos)

attach(Vinhos)

# Frequência Absoluta 
table(as.factor(Vinhos$quality), Vinhos$Vinho, useNA = "ifany")

table(as.factor(Vinhos$quality), Vinhos$Vinho)


# Tabela Cruzada (2-Way Cross Tabulation)
library(gmodels)
CrossTable(as.factor(Vinhos$quality), Vinhos$Vinho) 
 
summary(Vinhos)


mean(Vinhos$quality) # média

median(Vinhos$quality) # médiana

quantile(Vinhos$quality,type=4)  # Quartis

quantile(Vinhos$quality,.65,type=4) # exato percentil

range(Vinhos$quality)  # amplitude

diff(range(Vinhos$quality)) # diferença entre o maior e o menor valor

min(Vinhos$quality)  ## valor mínimo de x

max(Vinhos$quality)  ## valor máximo de x

var(Vinhos$quality) ## para obter a variância

sd(Vinhos$quality)  ## para obter o desvio padrão

CV_quality<-sd(Vinhos$quality)/mean(Vinhos$quality)*100  # para obter o coefiiente de variação
CV_quality


# Comando para gerar em 3 linhas e 4 colunas os Histogramas
par (mfrow=c(3,4))
hist(fixedacidity)
hist(volatileacidity)
hist(citricacid )
hist(residualsugar)
hist(chlorides)
hist(freesulfurdioxide)
hist(totalsulfurdioxide)
hist(density)
hist(pH)
hist(sulphates)
hist(alcohol)
hist(quality)

dev.off()

hist(quality, col=c("blue"), col.main="darkgray", prob=T)
     
attach(Vinhos)

# Comando para gerar em 3 linhas e 4 colunas os Histogramas
par (mfrow=c(3,4))
boxplot(fixedacidity, main='fixedacidity')
boxplot(volatileacidity , main='volatileacidity')
boxplot(citricacid , main='citricacid')
boxplot(residualsugar, main='residualsugar')
boxplot(chlorides, main='chlorides')
boxplot(freesulfurdioxide, main='freesulfurdioxide')
boxplot(totalsulfurdioxide, main='totalsulfurdioxide')
boxplot(density, main='density')
boxplot(pH, main='pH')
boxplot(sulphates, main='sulphates')
boxplot(alcohol, main='alcohol')
boxplot(Vinhos$quality, main='quality')

dev.off()

# Comando para gerar em 1 linha e 1 coluna os Histogramas 
par (mfrow=c(1,1))
boxplot(fixedacidity, main='fixedacidity')
boxplot(volatileacidity , main='volatileacidity')
boxplot(citricacid , main='citricacid')
boxplot(residualsugar, main='residualsugar')
boxplot(chlorides, main='chlorides')
boxplot(freesulfurdioxide, main='freesulfurdioxide')
boxplot(totalsulfurdioxide, main='totalsulfurdioxide')
boxplot(density, main='density')
boxplot(pH, main='pH')
boxplot(sulphates, main='sulphates')
boxplot(alcohol, main='alcohol')
boxplot(Vinhos$quality, main='quality')

dev.off()


boxplot(quality ~ Vinho, main='quality', col=c('red','blue'))

boxplot(fixedacidity ~ Vinho, main='fixedacidity',col=c('red','blue'))
boxplot(volatileacidity ~ Vinho , main='volatileacidity', col=c('red','blue'))
boxplot(citricacid ~ Vinho, main='citricacid', col=c('red','blue'))
boxplot(residualsugar ~ Vinho, main='residualsugar',col=c('red','blue'))
boxplot(chlorides ~ Vinho, main='chlorides', col=c('red','blue'))
boxplot(freesulfurdioxide ~ Vinho, main='freesulfurdioxide', col=c('red','blue'))
boxplot(totalsulfurdioxide ~ Vinho, main='totalsulfurdioxide', col=c('red','blue'))
boxplot(density ~ Vinho, main='density', col=c('red','blue'))
boxplot(pH ~ Vinho, main='pH', col=c('red','blue'))
boxplot(sulphates ~ Vinho, main='sulphates', col=c('red','blue'))
boxplot(alcohol ~ Vinho, main='alcohol', col=c('red','blue'))


# Gráfico de Dispersão (pch=caracter, lwd=largura)

plot(freesulfurdioxide~totalsulfurdioxide)
plot(freesulfurdioxide~totalsulfurdioxide, pch=1, lwd=3)

plot(freesulfurdioxide~totalsulfurdioxide)
abline(v=mean(freesulfurdioxide), col="red")
abline(h=mean(totalsulfurdioxide), col="green")



attach(Vinhos)
Vinhos$fx_redSugar <- cut(residualsugar,breaks=c(0,10,20,30,max(residualsugar)))  
Vinhos$fx_redSugar  
str(Vinhos)
CrossTable( Vinhos$fx_redSugar , Vinhos$Vinho) 


attach(Vinhos)


library(psych)

describe(Vinhos)

# describe
# A data.frame of the relevant statistics:
# item name
# item number
# number of valid cases
# mean
# standard deviation
# trimmed mean (with trim defaulting to .1)
# median (standard or interpolated)
# mad: median absolute deviation (from the median)
# minimum
# maximum
# skew
# kurtosis
# standard error


summary(Vinhos)


## De acordo com a Tabela Cruzada de notas vs vinho, podemos analisar
## que os vinhos brancos têm maiores notas que os vinhos tintos.
## Diante disso, seguiremos as análises com os vinhos brancos (WHITE)
## (Obs.: nesta análise, poderia ser aplicada um teste t)


white <- subset(Vinhos, Vinho=="WHITE", select=c(quality,fixedacidity,volatileacidity,citricacid,residualsugar,
                                                 chlorides,freesulfurdioxide,totalsulfurdioxide,density,pH,
                                                 sulphates,alcohol))


# Estatísticas Descritivas

summary(white)
 
str(white)

attach(white)
 

# Estatísticas Descritivas

par (mfrow=c(3,4))
boxplot(fixedacidity, main='fixedacidity')
boxplot(volatileacidity , main='volatileacidity')
boxplot(citricacid , main='citricacid')
boxplot(residualsugar, main='residualsugar')
boxplot(chlorides, main='chlorides')
boxplot(freesulfurdioxide, main='freesulfurdioxide')
boxplot(totalsulfurdioxide, main='totalsulfurdioxide')
boxplot(density, main='density')
boxplot(pH, main='pH')
boxplot(sulphates, main='sulphates')
boxplot(alcohol, main='alcohol')
boxplot(quality, main='quality')

dev.off()

boxplot.stats(white$residualsugar)


AIQ_residualsugar<-quantile(white$residualsugar,.75,type=2)-quantile(white$residualsugar,.25,type=2)
AIQ_residualsugar

limsup_residualsugar= quantile(white$residualsugar,.75,type=4)+1.5*AIQ_residualsugar
limsup_residualsugar
liminf_residualsugar= quantile(white$residualsugar,.25,type=2)-1.5*AIQ_residualsugar
liminf_residualsugar


# Excluir outliers

plot(quality~residualsugar)

white1<-subset(white, residualsugar<=40)   

fix(white1)

attach(white1)

summary(white1)

plot(residualsugar,alcohol)
abline(v=mean(residualsugar), col="red")
abline(h=mean(alcohol), col="green")


# Matriz de Correlações
matcor <- cor(white1)
print(matcor, digits = 2)


library(corrgram)
corrgram(matcor, type = "cor", lower.panel = panel.shade, upper.panel = panel.pie)

panel.cor <- function(x, y, digits=2, prefix ="", cex.cor,
                      ...)  {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y , use = "pairwise.complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits) [1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor))
    cex <- 0.8/strwidth(txt)
  # abs(r) é para que na saída as correlações ficam proporcionais
  text(0.5, 0.5, txt, cex = cex * abs(r))
}
# pdf(file = "grafico.pdf")
pairs(white1, lower.panel=panel.smooth, upper.panel=panel.cor)

dev.off()



## PCA - Principal Components Analysis


# Padronização ("Normalização") 

# Avaliar início
dados_normalizados = as.data.frame(scale(white1))

names(dados_normalizados)

summary(dados_normalizados)


# Componentes Principais
pca1 <- princomp(white1[complete.cases(white1),], cor=TRUE)
summary(pca1)

# Gráfico pca1
plot(pca1) 


## Análise Fatorial (Pacote PSYCH)
library(psych)

## Componentes Principais
pc <- principal(white1,3,rotate="varimax")  
pc

## Gráfico pc
plot(pc)


str(dados_normalizados)
names(dados_normalizados)

load=loadings(pc)
print(load,sort=TRUE,digits=3,cutoff=0.01)     
plot(load)                                 
identify(load,labels=names(dados_normalizados)) 


# Colocar os nomes dos pontos selecionados na figura ---- para parar, clique com o "command key" (tecla "Esc")

plot(pc,labels=names(dados_normalizados))


# Análise Fatorial (utilizando KMO - Kaiser-Meyer-Olkin)
library(psych)

KMO(matcor)

# Componentes Principais 
fit2 <- princomp(white1[complete.cases(white1),], cor=TRUE)
summary(fit2)

loadings(fit2) # pc loadings

plot(fit2,type="lines", cex=1, col = "dark red") # Gráfico fit2


# Componentes Principais (Histograma)

hist(fit2$scores)

biplot(fit2, cex=0.65)


library(psych)

# Escolhendo os Componentes Principais
fa.parallel (white1, fa="pc", show.legend=FALSE, main = "Eigenvalues dos Componentes Principais")

dev.off()


# Rotação varimax

library(psych)

# Rotação varimax - Componentes Principais

# Extração dos fatores
vinhospca  <- principal(white1, nfactors=4, scores=T, rotate="varimax")
vinhospca  # Resultados 

fator01 = vinhospca$scores[,1]
hist(fator01)

fator02 = vinhospca$scores[,2]
hist(fator02)

fator03 = vinhospca$scores[,3]
hist(fator03)

fator04 = vinhospca$scores[,4]
hist(fator04)

matriz <- cbind(white1,fator01,fator02,fator03,fator04)
fix(matriz)
attach(matriz)

dev.off()


# Padronização 

# Avaliar início
dados_padronizados = as.data.frame(scale(white))

names(dados_padronizados)

summary(dados_padronizados)

# Mostrar até 2 casas decimais
options("scipen" = 2)

View(white)

# Trabalhar com as variáveis 
attach(white)

# Verificando o formato das variáveis
str(white)

# Medidas - Resumo (Estatísticas Descritivas - "white")
summary(white)

#Comando para gerar em 4 linhas e 3 colunas os Histogramas

par (mfrow=c(4,3))
hist(white$fixedacidity)
hist(white$volatileacidity)
hist(white$citricacid)
hist(white$residualsugar)
hist(white$chlorides)
hist(white$freesulfurdioxide)
hist(white$totalsulfurdioxide)
hist(white$density)
hist(white$pH)
hist(white$sulphates)
hist(white$alcohol)
hist(white$quality)

dev.off()

par (mfrow=c(1,1))

boxplot(white$quality~white$residualsugar, main='quality vs residualsugar',col=c('red','blue'))

boxplot(white$quality~white$fixedacidity, main='quality vs fixedacidity',col=c('red','blue'))

boxplot(white$quality~white$alcohol, main='quality vs alcohol',col=c('red','blue'))



# Matriz de Correlações
matcor <- cor(white)
print(matcor, digits = 2)


# Visualizar correlacao

library(corrgram)

corrgram(matcor, type = "cor", lower.panel = panel.shade, upper.panel = panel.pie)

install.packages("corrplot")
library(corrplot)

corrplot::corrplot(matcor, method="circle", order="hclust")


# Gráfico de Dispersao para a associação entre residualsugar e quality
plot (x = white$residualsugar, y = white$quality,
      main = "Gráfico de dispersão",
      xlab = "residualsugar",
      ylab = "quality")


#Gráfico de dispersao para a associação entre fixedacidity e quality
plot (x = white$fixedacidity, y = white$quality,
      main = "Gráfico de dispersão",
      xlab = "fixedacidity",
      ylab = "quality")


#Gráfico de dispersao para a associação entre alcohol e quality
plot (x = white$alcohol, y = white$quality,
      main = "Gráfico de dispersão",
      xlab = "alcohol",
      ylab = "quality")


# Opções de gráficos: Gráfico de dispersao com o plotly

install.packages("plotly")
library(plotly)
plot_ly (x=white$quality  , y=white$residualsugar, type="scatter")
plot_ly (x=white$quality  , y=white$fixedacidity, type="scatter")


#Gráfico de Dispersao com o ggplot2

library(ggplot2)
ggplot (data= white, aes(x=white$quality, y=white$residualsugar )) + 
  geom_point(size=0.4) +
  geom_smooth(method="lm", color ="red", linetype=2) +
  labs(title = "Gráfico de dispersão, qualidade e residualsugar", x="residualsugar", y="qualidade")


attach(white)



# Modelo 1 - Regressão Linear Simples

modelo0 <- lm(quality ~ residualsugar)
summary(modelo0)

modelo1 <- lm(quality ~ residualsugar+fixedacidity)
summary(modelo1)

modelo2 <- lm(quality ~ residualsugar+fixedacidity+alcohol)
summary(modelo2)

install.packages("lattice")
install.packages("latticeExtra")
install.packages("asbio")
install.packages("car")

library(lattice)
library(latticeExtra)
library(asbio)
library(car)

measures <- function(x) {
  L <- list(npar = length(coef(x)),
            dfres = df.residual(x),
            nobs = length(fitted(x)),
            RMSE = summary(x)$sigma,
            R2 = summary(x)$r.squared,
            R2adj = summary(x)$adj.r.squared,
            PRESS = press(x),
            logLik = logLik(x),
            AIC = AIC(x),
            BIC = BIC(x))
  unlist(L)
}

modl <- list(m2 = modelo0, m3 = modelo1, m4 = modelo2)
round(t(sapply(modl, measures)), 3)


forward<-step(modelo1,direction="forward")

forward

summary(forward)

backward<-step(modelo1,direction="backward")
backward
summary(backward)

stepwise<-step(modelo1,direction="both")

stepwise
summary(stepwise)


# Modelo final.
modelo_fim <- lm(quality ~ residualsugar+fixedacidity+alcohol)
summary(modelo_fim)


Val_pred <- predict(modelo_fim,interval = "prediction", level = 0.95) 
fix(Val_pred)


# Intervalo de Confianca - Grafico para Media
fit <- Val_pred[,1] # valores preditos
lower <- Val_pred[,2] # limite inferior
upper <- Val_pred[,3] # limite superior


mse <- mean((white$quality - fit)^2)
sqrt(mse)

erro_usando_media <- mean((white$quality - mean(white$quality))^2)
sqrt(erro_usando_media)


# Grafico Residuo
rs <- resid(modelo_fim)
plot(predict(modelo_fim), rs, xlab = "Preditor Linear",ylab = "Residuos")
abline(h = 0, lty = 2)

attach(white)
white_Final<-cbind(white,Val_pred)

fix(white_Final)



# Modelo 2 - Árvore de Regressão

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

attach(white)
fix(white)

modelo_Valor_tree <- rpart (quality ~ residualsugar+fixedacidity+alcohol, data=white, 
                            cp = 0.001,minsplit = 5,maxdepth=10)


# Faz o Gráfico
rpart.plot(modelo_Valor_tree, type=4, extra=1, under=FALSE, clip.right.labs=TRUE,
           fallen.leaves=FALSE,   digits=2, varlen=-10, faclen=20,
           cex=0.35, tweak=1.7,
           compress=TRUE,
           snip=FALSE)

dev.off()

Val_pred_tree <- predict(modelo_Valor_tree,interval = "prediction", level = 0.95) 
str(Val_pred_tree)


mse_tree <- mean((white$quality - Val_pred_tree)^2)
sqrt(mse_tree)

erro_usando_media <- mean((white$quality - mean(white$quality))^2)
sqrt(erro_usando_media)


# Grafico Residuo
rs <- Val_pred_tree- white$quality
plot(predict(modelo_Valor_tree), rs, xlab = "Com Árvore de Regressão",ylab = "Residuos")
abline(h = 0, lty = 2)

dev.off()



# Modelo 3 - Árvore de Decisão (Técnica de Discriminação)


install.packages("rattle")


attach(white)  

fix(white)

str(white)

names(white)

summary(white)

white$quality <- factor(white$quality)

# Frequência absoluta 
table(white$quality)


#Comando para gerar em 1 linha e 1 coluna os plots
par (mfrow=c(1,1))
plot(white$fixedacidity, white$quality , ylab="white quality",xlab="fixedacidity",col=c('red','darkgreen'))
plot(whitea$volatileacidity , white$quality ,ylab="white quality",xlab="volatileacidity",col=c('red','darkgreen'))
plot(white$citricacid, white$quality, ylab="white quality",xlab="citricacid",col=c('red','darkgreen'))
plot(white$residualsugar, white$quality, ylab="white quality",xlab="residualsugar",col=c('red','darkgreen'))
plot(white$chlorides , white$quality, ylab="white quality",xlab="chlorides",col=c('red','darkgreen'))
plot(white$freesulfurdioxide, white$quality, ylab="white quality",xlab="freesulfurdioxide",col=c('red','darkgreen'))
plot(white$totalsulfurdioxide, white$quality, ylab="white quality",xlab="totalsulfurdioxide",col=c('red','darkgreen'))
plot(white$density, white$quality, ylab="white quality",xlab="density",col=c('red','darkgreen'))
plot(white$pH, white$quality, ylab="white quality",xlab="pH",col=c('red','darkgreen'))
plot(white$sulphates, white$quality, ylab="white quality",xlab="sulphates",col=c('red','darkgreen'))
plot(white$alcohol, white$quality, ylab="white quality",xlab="alcohol",col=c('red','darkgreen'))

dev.off()


# Carrega o pacote: Árvore de Decisão
library(rpart) 
library(rpart.plot) 

# Informações dos Parâmetros do Modelo
## Usa rpart para decision tree

modelo_tree <- rpart (quality ~ residualsugar+fixedacidity+alcohol, data=white, cp = 0.008,minsplit = 250,maxdepth=10)


# Faz o Gráfico
rpart.plot(modelo_tree, type=4, extra=104, under=FALSE, clip.right.labs=TRUE,
           fallen.leaves=FALSE,   digits=2, varlen=-3, faclen=20,
           cex=0.4, tweak=1.7,
           compress=TRUE,
           snip=FALSE)


rpart.plot(modelo_tree, type=2, extra="auto", under=FALSE, clip.right.labs=TRUE,
           fallen.leaves=TRUE,   digits=2, varlen=-3, faclen=15,
           cex=0.4, tweak=1.7,
           compress=TRUE,box.palette="auto",
           snip=FALSE)

print(modelo_tree)

preds_prod <- predict(modelo_tree,white)

preds_cat <- predict(modelo_tree, type = "class",white)

head(preds_cat)

table(preds_cat)



# Modelo 4 - Regressão Logística


# Predict como funcao para trazer a probabilidade da variável quality (0/1)

white.previsto.com.modelo_prob<-predict(modelo_tree,white)



## Predict com tipo 'classe' retorna se a variável quality é boa ou não (ruim)

white.previsto.com.modelo<-predict(modelo_tree,white,type='class')

head(white.previsto.com.modelo)

white.matriz.de.confusao<-table(quality, white.previsto.com.modelo)
white.matriz.de.confusao

diagonal <- diag(white.matriz.de.confusao)
perc.erro <- 1 - sum(diagonal)/sum(white.matriz.de.confusao)
perc.erro


# Plotar regra do Modelo Preditivo

library(rattle)

fancyRpartPlot(modelo_tree, cex=0.60)

fancyRpartPlot(modelo_tree, cex=0.60,  palettes=c("Greys", "Oranges"))


attach(white)

modelo_log<- glm(quality ~ residualsugar+fixedacidity+alcohol,white, family=binomial(link=logit))
summary(modelo_log)

predito<-fitted(modelo_log)

summary(predito)

hist(predito)


# Matriz de Confusão  

white$fx_predito <- cut(predito, breaks=c(0,0.50,1), right=F)

plot(white$fx_predito , white$quality)

dev.off()


# Montar a Matriz de Confusão
MC <- table(white$quality, white$fx_predito , deparse.level = 2)   
show(MC) # mostra os resultados

ACC = sum(diag(MC))/sum(MC) # calcula a acurácia  
show(ACC) # mostra a acurácia  


# Criar variável faixa probabilidade
fx_predito1 <- cut(predito, breaks=c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1), right=F)

# Frequência absoluta
table(fx_predito1,white$quality)

# Frequência relativa
prop.table(table(fx_predito1,white$quality),2)

plot(fx_predito1 , white$quality)


fx_predito2 <- cut(predito, breaks=c(0,0.25,0.50,0.75,1), right=F)

plot(fx_predito2 , white$quality)


#Divisao do Banco de Dados completo em Treinamento e Teste

#definir % de casos de treino
prt <- 2/3

# Amostra de Casos de Treino aleatória
set.seed(2018)
treino <- sample(1:NROW(white), as.integer(prt*NROW(white)))

trainData <- white[treino,]
testData  <- white[-treino,]

prop.table(table(trainData$quality))
prop.table(table(testData$quality))


## Análises

## A variável qualidade (quality) é a variável dependente. 
## As variáveis independentes são todas as outras variáveis (conside-
## ramos as variáveis residualsugar, fixedacidity, alcohol mais especi-
## ficamente).

## O modelo matemático da Regressão Linear e da Regressão Logísitca,
## nos fornece as relações entre elas.

## Através da função "summary",temos os parâmentros do modelo.

## Em relação à qualidade do modelo, pode-se basear no parâmetro R2,
## que se aplica tanto para a Regressão Linear, como para a Árvore de Regressão
## Já para a Árvore de Decisão e Regressão Logística, pode-se basear na
## Matriz de Confunsão.

## Em termos de resultado, temos as seguintes conclusões:

## Baseando-se nos dados, não há um método que abrange todos os melhores
## indicadores para comparação.

## A Regressão Logística é melhor nos pontos Especificidade e 
## valor Preditivo Positivo, enquanto que a Árvore de Decisão é melhor
## em relação à Acurácia, Sensibilidade, Eficiência e
## Valor Preditivo Negativo.
## No caso de termos como critério a Qualidade do Modelo para obtermos
## os vinhos de melhor qualidade, escolheríamos a Regressão Logisitca,
## de acordo com o Valor Preditivo Positivo (não considerando a pequena
## diferença entre os modelos).

## Para as técnicas não supervisionadas, indicaríamos a técnica de 
## Clusters (Clusterização).



