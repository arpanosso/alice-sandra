remove(list=ls())
library(tidyverse)
#####An?lise em DBA -Alice####
#Pressuposi??o da an?lise de vari?ncia ###
DBA <- read.table("data/altura1920.txt",header=T)

head(DBA) # Vai retornar a primeira parte de um objeto, conseguimos ver o que tem nele.
str(DBA)
DBA <- transform(DBA,bloco = factor(bloco), trat = factor(trat))
str(DBA)

bloco <- factor(DBA$bloco)
trat <- factor(DBA$trat)
yield <- DBA$prod

################ DBA - Delineamento em blocos aumentados ###################

library(agricolae)

##An?lise intrablocos via DAU.test

library(agricolae)
library(MASS)
model <- DAU.test(bloco, trat, yield,
                  method = "tukey",
                  console=TRUE)
model$means

#Analise intrablocos via lm ou aov
lm.DBA <- lm(prod ~ bloco + trat, data=DBA)
anova(lm.DBA)

#Analise intrablocos das testemunhas
lmt.DBA <- lm(prod ~ bloco + trat, data=DBA[DBA$tipo=="teste",])
anova(lmt.DBA)

#Adaptado da Tese do Jo?o Batista Duarte (Prof. UFG)
#An?lise intrablocos com a decomposi??o de trat = TR + teste + TR vs teste
#vari?veis auxiliares
DBA$C <- factor(ifelse(DBA$tipo == "TR",0,DBA$trat))
DBA$X <- factor(ifelse(DBA$tipo == "teste",0,DBA$trat))
head(DBA)

lm1.dba <- lm(
  terms(prod ~ bloco + C + X %in% C, keep.order = TRUE), # modelo
  data = DBA,
  contrasts = list(
    C = contr.sum,
    X = contr.sum,
    bloco = contr.sum)
  )
anova(lm1.dba)

#refere-se a saida da fun??o DAU.test - ANOVA2
lm2.dba <- lm(
  terms(prod ~ C + X%in%C + bloco , keep.order = TRUE), # modelo
  data = DBA, contrasts = list(
    C = contr.sum,
    X = contr.sum,
    bloco = contr.sum))
anova(lm2.dba)


library(lme4)
#Ambos tr e tc aleatorios
lmer.dba <- lmer(prod ~ bloco + (1|trat) , data=DBA)
summary(lmer.dba)

# Solution for Random Effects
ranef(lmer.dba)

# VALOR GENOTÍPICO= (DEP=ESTIMATE) + MÉDIA GERAL
ranef(lmer.dba)$trat + mean(DBA$prod)

#Help Julio Bueno - SEP of BLUPs via lme4
blups <- ranef(lmer.dba, condVar = TRUE, drop=TRUE)
str(blups)
blups.trat <- blups$trat
blups.PEV <- attr(blups.trat,"postVar")
blups.SEP <- sqrt(blups.PEV)
t.blups <- blups.trat/blups.SEP # estatística t

cbind(blups.trat,blups.SEP,t.blups)

#To produce a (list of) "caterpillar plots" of the random effects
blups <- ranef(lmer.dba, condVar = TRUE) #usando o argumento drop n?o plotou
lattice::dotplot(blups)

# tr aleatorios e tc fixos
DBA$X1 <- factor(ifelse(DBA$tipo == "TR",1,0))
head(DBA)

lmer.dba1 <- lmer(prod ~ bloco + C + (1|trat:X1) , data=DBA)
summary(lmer.dba1)
ranef(lmer.dba1)
