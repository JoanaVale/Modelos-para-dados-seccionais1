########################################################################################################
# METODOS QUANTITATIVOS                                                                                #
# Mestrado em Contabilidade e Financas                                                                 #
#                                                                                                      #
# TRABALHO PRATICO I                                                                                   #
#                                                                                                      #
# Instituto Superior de Contabilidade e Administracao do Porto - Politecnico do Porto                  #
# Joana Andreia Machado Vale                                                                           #
########################################################################################################

rm(list = ls())

load("C:/Users/Utilizador/Desktop/Metodos Quantitativos/Trabalho 1/casas.RData.RData")
library(wooldridge)
########################################################################################################
#                                     ALINEA A                                                         #
########################################################################################################
# (a) preco = B0 + B1area + B2idade + u                                                                #
########################################################################################################

### (i) Estime o modelo e discuta os resultados do relatorio de regressao fornecido pelo R

suppressMessages(attach(casas))
fit1 <- lm(preco ~ area + idade)
summary(fit1)


### (ii) Determine um intervalo de confianca de 95% para B1 e comente.

# Límite inferior:
confint(fit1)[2,1]

# Límite superior:
confint(fit1)[2,2]

##  O intervalo de confiança de 95% para B1 é de (86.25451 , 95.68509), isto significa que se construíssemos intervalos
# de confiança a partir de muitas amostras diferentes de mesmo tamanho, 95% deles conteriam efetivamente o coeficiente de
# B1: 86.25451º < B1 < 95.68509º. 

### (iii) Teste unilateral a esquerda =>  H0 : B2 = -1000 vs H1: B2 <-1000

summary(fit1)$coefficients

# valor da estatistica t
(summary(fit1)$coefficients[3,1]-(-1000)) / summary(fit1)$coefficients[3,2]

# guardar o valor da estatistica t na variavel tstat
tstat1 <- (summary(fit1)$coefficients[3,1]-(-1000)) / summary(fit1)$coefficients[3,2]   

# Valor-p:

n1 <- nrow(casas)          # no. de observacoes
k1 <- 2                    # no. de variaveis explicativas
gl1 <- n1-k1-1             # no. de graus de liberdade
pval1 <- pt(tstat1, gl1)     # valor-p
pval1

# Considerando ??=0.05 e ??=0.01:

pval1 < 0.05   # teste a 5%
pval1 < 0.01   # teste a 1%


########################################################################################################
#                                     ALINEA B                                                         #
########################################################################################################
# Adicione ao modelo referido em (a) as variaveis area^2 e idade^2                                     #
########################################################################################################

### i) Estime o modelo obtido e discuta os resultados do relatorio de regressao fornecido pelo R.
suppressMessages(attach(casas))
fit2 <- lm(preco ~ area + idade + I(area^2) + I(idade^2))
summary(fit2)


### ii) Estime o efeito parcial da area no preco
coefficients(fit2)

# 1) menor area
min(area)
coefficients(fit2)[2]+2*coefficients(fit2)[4]*min(area)

# 2) maior area
max(area)
coefficients(fit2)[2]+2*coefficients(fit2)[4]*max(area)

# 3) casa com 2300 pés quadrados

coefficients(fit2)[2]+2*coefficients(fit2)[4]*2300


# concavidade
suppressMessages(attach(casas))
-coefficients(fit2)[2]/(2*coefficients(fit2)[4])
library(effects)
plot(effect("area",fit2))



### iii) Estime o efeito parcial da idade no preço

coefficients(fit2)
# 1) a casa mais antiga da amostra
max(idade)
coefficients(fit2)[3]+2*coefficients(fit2)[5]*max(idade)

# 2) a casa mais nova da amostra
min(idade)
coefficients(fit2)[3]+2*coefficients(fit2)[5]*min(idade)

# 3) uma casa com 20 anos de idade 

coefficients(fit2)[3]+2*coefficients(fit2)[5]*20


# concavidade
-coefficients(fit2)[3]/(2*coefficients(fit2)[5])
library(effects)
plot(effect("idade",fit2))


### iv) Determine um intervalo de confianca de 95% para o efeito parcial da area no preco para
#uma casa com 2300 pes quadrados.

betahat1 <- coefficients(fit2)[2]+4600*coefficients(fit2)[4]

n2 <- nrow(casas)          # no. de observacoes
k2 <- 4                    # no. de variaveis explicativas
gl2 <- n2-k2-1             # no. de graus de liberdade
alpha <- 0.05              # valor do alfa


c1 <- qt(1-alpha/2, gl2)

vcov(fit2)
se1 <- sqrt(vcov(fit2)[2,2]+(4600)^2*vcov(fit2)[4,4]+2*4600*vcov(fit2)[2,4])


betahat1 - c1 * se1
betahat1 + c1 * se1


# O intervalo de confianca de 95% para o efeito parcial da area no preco para
#uma casa com 2300 pes quadrados é de (45.72072;55.71686)


### v) Teste a hipotese de que o efeito parcial da idade no preco, para uma casa com 20 anos
#de idade, é igual ou superior a -1000, contra a hipotese alternativa de que é inferior a -1000

betahat2 <- coefficients(fit2)[3]+2*20*coefficients(fit2)[5]
betahat2

se2 <- sqrt(vcov(fit2)[3,3]+(40)^2*vcov(fit2)[5,5]+2*40*vcov(fit2)[3,5])
se2

tstat2 <- (betahat2 -(-1000))/se2
tstat2

n2 <- nrow(casas)          # no. de observacoes
k2 <- 4                    # no. de variaveis explicativas
gl2 <- n2-k2-1             # no. de graus de liberdade
pval2 <- pt(tstat2, gl2)   # valor-p
pval2

pval2 < 0.05   # teste a 5%
pval2 < 0.01   # teste a 1%



########################################################################################################
#                                     ALINEA C                                                         #
########################################################################################################
# Compare as especificacoes dos dois modelos considerados em (a) e (b) tendo em conta tambem           #
#as analises efetuadas.                                                                                #
########################################################################################################

summary(fit1)$adj.r.squared    # R2 ajustado do modelo 1
summary(fit2)$adj.r.squared    # R2 ajustado do modelo 2

# Critério de Schwarz para o modelo 1:
k1 <- 2                                         # no. de variaveis explicativas
n3 <- nobs(fit1)                                # no. de observacoes
SQR1 <- t(residuals(fit1)) %*% residuals(fit1)  # SQR
BIC1 <- log(SQR1/n3) + ((k1 + 1)/n3)*log(n3)    # BIC do modelo 1
BIC1

# Critério de Schwarz para o modelo 2:

k2 <- 4                                         # no. de variaveis explicativas
n4 <- nobs(fit2)                                # no. de observacoes
SQR2 <- t(residuals(fit2)) %*% residuals(fit2)  # SQR
BIC2 <- log(SQR2/n4) + ((k2 + 1)/n4)*log(n4)    # BIC do modelo 2
BIC2


# Critério de informação de Akaike para o modelo 1:

AIC1 <- log(SQR1/n3) + 2*k1/n3    # AIC do modelo 1
AIC1


# Critério de informação de Akaike para o modelo 2:

AIC2 <- log(SQR2/n4) + 2*k2/n4    # AIC do modelo 2
AIC2


# Mostra a informação em tabela:

suppressMessages(library(stargazer))
stargazer(list(fit1,fit2), type = "text", keep.stat = c("adj.rsq"), add.lines=list( c("BIC", round(BIC1,3), round(BIC2,3) ), c("AIC", round(AIC1,3), round(AIC2,3)) ) )


########################################################################################################
#                                     ALINEA D                                                         #
########################################################################################################
# Determine uma nova variavel dada por preco=1000, que corresponde ao preco da casa em                 #  
# milhares de dolares. Considere o modelo de regressao de log(preco=1000) sobre as 9 variaveis         #
# explicativas.                                                                                        #
########################################################################################################

### i) Compare os histogramas do preco e do log(preco/1000) e comente (use a função hist()).

hist(preco)


fit3 <- lm(log(preco/1000) ~ area + idade + quartos + wcs + dono + pisc + tradi + lar + vista)
hist(log(preco1000))



### ii) Estime o modelo e discuta os resultados do relatorio de regressao fornecido pelo R (nao
## se esqueca de interpretar o valor dos coeficientes).

summary((fit3))



### iii) Adicione ao modelo a variavel de interacao tradi * vista e reestime-o. Interprete a
### introducao desta nova variavel no modelo.

fit4 <- lm(log(preco/1000) ~ area + idade + quartos + wcs + dono + pisc + lar + tradi*vista)
summary((fit4))



### iv) Questiona-se se as casas de estilo tradicional devem ter uma funcao de regressao diferente
###das restantes. Efetue o teste de Chow para responder a esta questao. O que pode concluir?

## H0:??0=0?????1=0?????2=0?????3=0?????4=0

fit5 <- lm(log(preco/1000) ~ area + idade + quartos + wcs)
SQR3 <- sum(residuals(fit5)^2)

#serem tradicionais
fit6 <- lm(log(preco/1000) ~ area + idade + quartos + wcs, subset = (tradi==1))
SQR4 <- sum(residuals(fit6)^2)

# serem modernas
fit7 <- lm(log(preco/1000) ~ area + idade + quartos + wcs, subset = (tradi==0))
SQR5 <- sum(residuals(fit7)^2)

#Calcula-se o valor da estatística F:
n5 <- nobs(fit5)
k2 <- 4
f1 <- ((SQR3 - (SQR4 + SQR5))/(k2+1))/((SQR4 + SQR5)/(n5-2*(k2+1)))
f1

alpha2 <- 0.01
cv1 <- qf(1-alpha2, k2+1, n5-2*(k2+1))
cv1

f1 > cv1



### v) Usando o modelo estimado em (iii), faca uma previsao do preco medio de uma casa de
## estilo tradicional com: 2500 pes quadrados de area, 20 anos de idade, com lareira, 3
## quartos e 2 casas de banho, sem piscina, sem vista para o mar e habitada pelo dono no
## momento da venda, e determine o respetivo IC de 95%.


fit4 <- lm(log(preco/1000) ~ area + idade + quartos + wcs + dono + pisc + tradi + lar + vista + tradi*vista)

data.frame(area=2500, idade=20, quartos=3, wcs=2, dono=1, tradi=1, lar=1, pisc=0, vista=0)

# Previsão do preco medio de uma casa:
predict(fit4, data.frame(area=2500, idade=20, quartos=3, wcs=2, dono=1, tradi=1, lar=1, pisc=0, vista=0))  # previsão do preco medio de uma casa 

# IC 95%:
predict(fit4, data.frame(area=2500, idade=20, quartos=3, wcs=2, dono=1, tradi=1, lar=1, pisc=0, vista=0), interval="confidence")


########################################################################################################
#                                     ALINEA E                                                         #
########################################################################################################
# Defina uma nova variavel, area100, dada por area=100, que corresponde a area da casa em              #
# centenas de pes quadrados, e considere o seguinte modelo de regressao:                               #
########################################################################################################


### i) Estime o modelo e apresente os resultados do relatório de regressão fornecido pelo R.

area100 = area / 100
fit8 <- lm(log(preco) ~ area100 + idade + I(idade^2))
summary(fit8)

# O modelo é: preco = 1.112e+01 + 3.876e-02*area100 - 1.755e-02*idade +  1.734e-04*(idade^2)

# Desta forma, pode-se referir que apenas a idade do imovel contribui para a diminuição do valor/ preço do imóvel.
# No entanto, as outras variaveis, como têm coeficientes positivos aumenta o valor dos mesmo. 
# Neste exercicio, todas as variaveis são estatisticamente significativas a 5% como a 1%. 
# O modelo detem um R^2 de 70.70%, o que significa que com estas variaveis explicativas/dependentes explicam 70.70% do modelo/ variavel independente/preço do imovel.


### ii) Faca um gráfico dos resíduos em função (i) da idade e (ii) da area100. Existe alguma
## evidencia de heteroscedasticidade?

## i) da idade
plot(idade, residuals(fit8))

## ii) da area100
plot(area100, residuals(fit8))



### iii) Use o teste de Breusch-Pagan para averiguar se ha evidencia de heteroscedasticidade ao
## nivel de significancia de 1%.

suppressMessages(library(lmtest))
bptest(fit8)


### iv) Estime a função variância e usando erros-padrão robustos comente o efeito da idade e da 
# area100 na variância do erro.
# V (u|x) = ??2exp(??0 + ??1area100 + ??2idade)

attach(casas)
fit8 <- lm(log(preco) ~ area100 + idade + I(idade^2))
logu2 <- log(residuals(fit8)^2)
logvar <- lm(logu2 ~ area100 + idade)
summary(logvar)
coeftest(logvar, vcov=hccm)
#iv
logvar <- lm(log(residual(fit8)^2)~area100 + idade)
suppressMessages(library(lmtest))
suppressMessages(library(car))
coeftest(logvar, vcov=hccm)


### v) Usando as estimativas das variâncias do erro obtidas na alínea anterior, determina as estimativas WLS
# do modelo referido em (e). 

w <- exp(fitted(logvar))
fit11 <- lm(log(preco) ~ area100 + idade + I(idade^2), weights = 1/w)
summary(fit11)



### vi) Coloque numa tabela as estimativas, erros-padrão e testes t do modelo referido em (e)
# obtidos usando os metodos seguintes:



######### OLS;

#Tabela com os coeficientes, erros-padrão e testes t usuais:

suppressMessages(library(lmtest))
coeftest(fit8)


######### OLS e erros-padrão robustos;

#Tabela com os coeficientes, erros-padrão e testes t usuais:
suppressMessages(library(car))
coeftest(fit8, vcov=hccm)


######### WLS usando as estimativas das variâncias do erro obtidas em (iv) e erros-padrao
# robustos;

#Tabela com os coeficientes, erros-padrão e testes t usuais:
suppressMessages(library(lmtest))
coeftest(fit11)


######### WLS usando as estimativas das vari^ancias do erro obtidas em (iv);

#Tabela com os coeficientes, erros-padrão e testes t usuais:
suppressMessages(library(car))
coeftest(fit11, vcov=hccm)


