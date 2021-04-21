################# Aprofundando em hipóteses e correlações:
################# Prof. Maurício Aniche

##### TESTE DE HIPÓTESES:
### O caso do remédio que abaixa febre.

x1 <- runif(30, 37.9, 38.8)
x1
# Comando runinf() cria uma lista de números aleatórios que vai de um valor mínimo
# até um valor máximo.

x2 <- runif(30, 36.0, 38.2)
x2

# Hipótese nula: Remédio não abaixa a febre.
# Hipótese alternativa: Remédio abaixa febre.

# Teste T de Student -  para conjunto de dados com distribuição normal:
t.test(x1,x2)

# Teste de Wilcoxon - para conjunto de dados sem distribuição normal:
wilcox.test(x1,x2)

##### CORRELAÇÃO:

setwd("C:/Users/gades/Desktop/DataScience/R/Estatistica_II")
nums <- read.csv(file="numeros.csv")
nums

cor(nums$X1, nums$X2)

# A função cor() recebe tanto variáveis, quanto listas.
a <- c(2, 3, 4)
b <- c(8, 6, 4)
cor(a, b)

# A função cor() permite usar tanto o método de Pearson (dados com distribuição
# normal), quanto o método de Spearman (dados sem distribuição normal):
cor(a, b, method="pearson")
cor(a, b, method="spearman")

# Outro exemplo:
b <- c(8, 1, 98)
cor(a, b)

# Exercício:
c <- c(37, 420, 117)
d <- c(29, 8, 3)
cor(c,d)

##### REGRESSÃO LINEAR:
A1 <- c(1,2,3)
A2 <- c(2,4,6)

# A função lm() é usada para obter uma regressão linear.
# ~ significa que uma variável depende da outra.
lm(formula=A2~A1)