################# Regressão Linear Simples em R: correlação e previsão
################# Profa. Alia Garrudo Guirado

# ----------------------------------------------------------------------------------------------------------------

#### O PROBLEMA:
#### Qual o comportamento do preço de determinadas casas segundo suas características (por exemplo: tamanho)?

# ----------------------------------------------------------------------------------------------------------------

# Setando a pasta de trabalho:
setwd("C:/Users/gades/Desktop/DataScience/R/Regressao_Linear_Simples_Correlacao_Previsao")

# Importação dos dados:
library(readxl)
dados <- read_excel("dados.xlsx", col_types = c("numeric", "numeric", "numeric")) # skip pode ser usado para não
                                                                                  # importar alguma coluna 
dados                                                                             # dos dados.

# ----------------------------------------------------------------------------------------------------------------

# Verificando relações entre duas variáveis através de um gráfico de dispersão:
plot(dados$area, dados$preco, main="Diagrama de dispersão", xlab="Área", ylab="Preço das casas", pch=19)

# Correlação entre as variáveis:
cor(dados$area, dados$preco)

# Hipótese nula atrelada ao teste de correlação de Pearson: 
# H0 - A correlação entre as variáveis é igual a zero.
cor.test(dados$area, dados$preco)

# Realizando as análises feitas agora com as variáveis preço e tempo:
plot(dados$tempo, dados$preco, main="Diagrama de dispersão", xlab="Tempo", ylab="Preço das casas", pch=19)
cor.test(dados$tempo, dados$preco)

# ----------------------------------------------------------------------------------------------------------------

# Análise da distribuição dos dados - Boxplot:
boxplot(dados$preco)
summary(dados$preco)

# Analisando as observações outliers:
install.packages("car")
library(car)
Boxplot(dados$preco)
dados$preco[79]

# Quais casas tem valor acima do terceiro quantil (0.75)?
which(dados$preco > quantile(dados$preco, 0.75))

# ----------------------------------------------------------------------------------------------------------------
# EQUAÇÃO DA RETA:

# preco = b0 + b1(area)
mod1 = lm(preco ~ area, data=dados)
mod1

# Calculando o preço médio de uma casa com 70 metros quadrados:
preco_70 = 502347 + 7851*70
preco_70

preco_70_2 = mod1$coefficients[[1]] + mod1$coefficients[[2]]*70
preco_70_2

# Criando um gráfico com o diagrama de dispersão e a reta:
plot(dados$area, dados$preco, main="Diagrama e reta")
abline(mod1, col="red") # abline = acrescentar uma coisa no gráfico.

summary(mod1)

names(summary(mod1))

# Coeficiente de determinação:
summary(mod1)$r.square

# Coeficiente de determinação ajustado:
summary(mod1)$adj.r.square

# preco = b0 + b1(tempo da casa)
mod2 = lm(preco ~ tempo, data=dados)
mod2

summary(mod2)

# preco = b0 + b1(area) + b2(tempo)
mod3 = lm(preco ~ area + tempo, data=dados)
mod3

summary(mod3)

# ----------------------------------------------------------------------------------------------------------------
# SUPOSIÇÕES DO MODELO LINEAR:

# 1. Autocorrelação dos resíduos:
# H0: Os resíduos são não autocorrelacionados. Autocorrelação dos resíduos é igual a 0.
plot(mod1$residuals) 
identify(mod1$residuals, n=2)

# Identificada a observação destoante, caso decida retirá-la:
dados_59 = dados[-59,] # primeiro espaço: linhas, segundo: colunas, -59 = excluir linha 59
dados_59_82 = dados[c(-59,-82),]

# Teste de Durbin Watson
install.packages("lmtest")
library(lmtest)
dwtest(mod1)

# 2. Variância constante, isto é, homocedástica:
plot(mod1$fitted.values, mod1$residuals)

# Teste de Breusch-Pagan
# H0: O modelo é homocedástico
bptest(mod1)

# 3. Normalidade dos erros
plot(mod1,2)

# Teste de Shapiro-Wilk
# H0: Os erros têm distribuição normal
shapiro.test(mod1$residuals)

# ----------------------------------------------------------------------------------------------------------------
# PREVISÃO:

# Criando uma base com áreas que temos interesse em prever:
dados_novos = data.frame(area = c(60,70))

# Prevendo os preços de forma pontual:
predict(mod1, newdata=dados_novos)

# Prevendo os preços de forma intervalar:
predict(mod1, newdata=dados_novos, interval="prediction")

predict(mod1, newdata=dados_novos, interval="confidence")
