

#Conceitos Estat�sticos para IA
##Trabalho: Uso e an�lise dos conceitos e t�cnicas estat�sticas
##Turma 2IA
##prof.: Adelaide Alves de Oliveira
## 
#Grupo:
##F�bio Alex de Queiroz Jacob - RM 330989
##Allan Almeida - RM 
##Luca Pasquale - RM


# Item 1 - Base (in�cio)  

# Limpar mem�ria do R
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

#

# Mostrar at� 2 casas decimais
options("scipen" = 2)

# Ler arquivo csv

Vinhos <- read.csv2("C:/BaseWine_Red_e_White2018.csv")
 
# Mostrar a Tabela da Base de Dados
fix(Vinhos)

# Mostrar o formato das vari�veis (Tabela da Base de Dados)
str(Vinhos)

# Mmostra as vari�veis
names(Vinhos)

attach(Vinhos)

# Frequ�ncia Absoluta 
table(as.factor(Vinhos$quality), Vinhos$Vinho, useNA = "ifany")

table(as.factor(Vinhos$quality), Vinhos$Vinho)


# Tabula��o Cruzada (2-Way Cross Tabulation)
library(gmodels)
CrossTable(as.factor(Vinhos$quality), Vinhos$Vinho) 
 
summary(Vinhos)

aggregate(Vinhos, by = list(Vinhos$Vinho), FUN = mean)


mean(Vinhos$fixedacidity) # m�dia

median(Vinhos$fixedacidity) # m�diana

quantile(Vinhos$fixedacidity,type=4)  # Quartis

quantile(Vinhos$fixedacidity,.65,type=4) # exato percentil

range(Vinhos$fixedacidity)  # amplitude

diff(range(Vinhos$fixedacidity)) # diferen�a entre o maior e o menor valor

min(Vinhos$fixedacidity)  ## valor m�nimo de x

max(Vinhos$fixedacidity)  ## valor m�ximo de x

var(Vinhos$fixedacidity) ## para obter a vari�ncia

sd(Vinhos$fixedacidity)  ## para obter o desvio padr�o

CV_fixedacidity<-sd(Vinhos$fixedacidity)/mean(Vinhos$fixedacidity)*100  # para obter o coefiiente de varia��o
CV_fixedacidity


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


# Gr�fico de Dispers�o (pch=caracter, lwd=largura)

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
white <- subset(Vinhos, Vinho=="WHITE", select=c(quality,fixedacidity,volatileacidity,citricacid,residualsugar,
                                                 chlorides,freesulfurdioxide,totalsulfurdioxide,density,pH,
                                                 sulphates,alcohol))
# Estat�sticas Descritivas
summary(white)
 
str(white)

attach(white)
 

# Estat�sticas Descritivas

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


# Matriz de Correla��es
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
  # abs(r) � para que na sa�da as correla��es ficam proporcionais
  text(0.5, 0.5, txt, cex = cex * abs(r))
}
# pdf(file = "grafico.pdf")
pairs(white1, lower.panel=panel.smooth, upper.panel=panel.cor)

dev.off()

#------------------------
#### Item 1 - Base (fim) 
#------------------------

# Padroniza��o ("Normaliza��o") 

# Avaliar in�cio
dados_normalizados = as.data.frame(scale(white1))

names(dados_normalizados)

summary(dados_normalizados)


# Componentes Principais
pca1 <- princomp(white1[complete.cases(white1),], cor=TRUE)
summary(pca1)

# Gr�fico pca1
plot(pca1) 


## An�lise Fatorial (Pacote PSYCH)
library(psych)

## Componentes Principais
pc <- principal(white1,3,rotate="varimax")  
pc

## Gr�fico pc
plot(pc)


str(dados_normalizados)
names(dados_normalizados)

load=loadings(pc)
print(load,sort=TRUE,digits=3,cutoff=0.01)     
plot(load)                                 
identify(load,labels=names(dados_normalizados)) 


# Colocar os nomes dos pontos selecionados na figura ---- para parar, clique com o "command key" (tecla "Esc")

plot(pc,labels=names(dados_normalizados))


# An�lise Fatorial (utilizando KMO - Kaiser-Meyer-Olkin)
library(psych)

KMO(matcor)

# Componentes Principais 
fit2 <- princomp(white1[complete.cases(white1),], cor=TRUE)
summary(fit2)

loadings(fit2) # pc loadings

plot(fit2,type="lines", cex=1, col = "dark red") # Gr�fico fit2


# Componentes Principais (Histograma)

hist(fit2$scores)

biplot(fit2, cex=0.65)


library(psych)

# Escolhendo os Componentes Principais
fa.parallel (white1, fa="pc", show.legend=FALSE, main = "Eigenvalues dos Componentes Principais")

dev.off()


# Rota��o varimax

library(psych)

# Rota��o varimax - Componentes Principais

# Extra��o dos fatores
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


#----------
## PCA - Principal Components Analysis
#----------


# Padroniza��o 

# Avaliar in�cio
dados_padronizados = as.data.frame(scale(white))

names(dados_padronizados)

summary(dados_padronizados)

# Mostrar at� 2 casas decimais
options("scipen" = 2)

View(white)

# Trabalhar com as vari�veis 
attach(white)

# Verificando o formato das vari�veis
str(white)

# Medidas - Resumo (Estat�sticas Descritivas - "white")
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



# Matriz de Correla��es
matcor <- cor(white)
print(matcor, digits = 2)


#visualizar correlacao

library(corrgram)

corrgram(matcor, type = "cor", lower.panel = panel.shade, upper.panel = panel.pie)

install.packages("corrplot")
library(corrplot)

corrplot::corrplot(matcor, method="circle", order="hclust")


# Gr�fico de Dispersao para a associa��o entre residualsugar e quality
plot (x = white$residualsugar, y = white$quality,
      main = "Gr�fico de dispers�o",
      xlab = "residualsugar",
      ylab = "quality")


#Gr�fico de dispersao para a associa��o entre fixedacidity e quality
plot (x = white$fixedacidity, y = white$quality,
      main = "Gr�fico de dispers�o",
      xlab = "fixedacidity",
      ylab = "quality")


#Gr�fico de dispersao para a associa��o entre alcohol e quality
plot (x = white$alcohol, y = white$quality,
      main = "Gr�fico de dispers�o",
      xlab = "alcohol",
      ylab = "quality")


# Op��es de gr�ficos: Gr�fico de dispersao com o plotly

install.packages("plotly")
library(plotly)
plot_ly (x=white$quality  , y=white$residualsugar, type="scatter")
plot_ly (x=white$quality  , y=white$fixedacidity, type="scatter")


#Gr�fico de dispersao com o ggplot2

library(ggplot2)
ggplot (data= white, aes(x=white$quality, y=white$residualsugar )) + 
  geom_point(size=0.4) +
  geom_smooth(method="lm", color ="red", linetype=2) +
  labs(title = "Gr�fico de dispers�o, qualidade e residualsugar", x="residualsugar", y="qualidade")


attach(white)



# Modelo 1 - Regress�o Linear Simples

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



# Modelo 2 - �rvore de Regress�o

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

attach(white)
fix(white)

modelo_Valor_tree <- rpart (quality ~ residualsugar+fixedacidity+alcohol, data=white, 
                            cp = 0.001,minsplit = 5,maxdepth=10)


# Faz o Gr�fico
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
plot(predict(modelo_Valor_tree), rs, xlab = "Com �rvore de Regress�o",ylab = "Residuos")
abline(h = 0, lty = 2)

dev.off()



# Modelo 3 - �rvore de Decis�o (T�cnica de Discrimina��o)

#vignette('longintro', package = 'rpart')


install.packages("rattle")


attach(white)  

fix(white)

str(white)

names(white)

summary(white)

white$quality <- factor(white$quality)

# Frequ�ncia absoluta 
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


# Carrega o pacote: �rvore de Decis�o
library(rpart) 
library(rpart.plot) 

# Informa��es dos Par�metros do Modelo
## Usa rpart para decision tree

modelo_tree <- rpart (quality ~ residualsugar+fixedacidity+alcohol, data=white, cp = 0.008,minsplit = 250,maxdepth=10)


# Faz o Gr�fico
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



# Modelo 4 - Regress�o Log�stica


# Predict como funcao para trazer a probabilidade da vari�vel quality (0/1)

white.previsto.com.modelo_prob<-predict(modelo_tree,white)



## Predict com tipo 'classe' retorna se a vari�vel quality � boa ou n�o

white.previsto.com.modelo<-predict(modelo_tree,white,type='class')

head(white.previsto.com.modelo)

white.matriz.de.confusao<-table(quality, white.previsto.com.modelo)
white.matriz.de.confusao

diagonal <- diag(white.matriz.de.confusao)
perc.erro <- 1 - sum(diagonal)/sum(white.matriz.de.confusao)
perc.erro


#Plotar regra do Modelo Preditivo

library(rattle)

fancyRpartPlot(modelo_tree, cex=0.60)

fancyRpartPlot(modelo_tree, cex=0.60,  palettes=c("Greys", "Oranges"))


attach(white)

modelo_log<- glm(quality ~ residualsugar+fixedacidity+alcohol,white, family=binomial(link=logit))
summary(modelo_log)

predito<-fitted(modelo_log)

summary(predito)

hist(predito)


# Matriz de confus�o  

white$fx_predito <- cut(predito, breaks=c(0,0.50,1), right=F)

plot(white$fx_predito , white$quality)

dev.off()

MC <- table(white$quality, white$fx_predito , deparse.level = 2) # montar a matriz de confus�o  
show(MC) # mostra os resultados

ACC = sum(diag(MC))/sum(MC) # calcula a acur�cia  
show(ACC) # mostra a acur�cia  


# Criar vari�vel faixa probabilidade
fx_predito1 <- cut(predito, breaks=c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1), right=F)

# Frequ�ncia absoluta
table(fx_predito1,white$quality)

# Frequ�ncia relativa
prop.table(table(fx_predito1,white$quality),2)

plot(fx_predito1 , white$quality)


fx_predito2 <- cut(predito, breaks=c(0,0.25,0.50,0.75,1), right=F)

plot(fx_predito2 , white$quality)


#Divisao do Banco de Dados completo em Treinamento e Teste

#definir % de casos de treino
prt <- 2/3

# Amostra de Casos de Treino aleat�ria
set.seed(2018)
treino <- sample(1:NROW(white), as.integer(prt*NROW(white)))

trainData <- white[treino,]
testData  <- white[-treino,]

prop.table(table(trainData$quality))
prop.table(table(testData$quality))


#