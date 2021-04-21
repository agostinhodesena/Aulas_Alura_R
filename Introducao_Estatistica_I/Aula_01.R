################# Primeiros Passos com o R:
################# Prof. Maurício Aniche

##### INTRODUÇÃO:
## Realizando operações matemáticas:
1
1+1
3*8+2/5
(3*7)/4

## Criando Variáveis:
numero <- (3*7)/4
numero
numero*2

## Criando listas:
lista <- c(1, 2, 3, 4, 5, 6)
lista
lista*2
aulas <- c(2,4,4,6,6,6,6,8,8,10)

## Plotando um histograma:
hist(aulas)

## Customizando o histograma:
?hist 
# Ao colocar uma interrogração na frente de uma função, estamos usando o help do
# programa. Ao usar essa ferramenta, descobrimos mais sobre a função, seus atributos e
# etc.
hist(aulas, main=paste("Histograma da frequência de aulas da Escola X"), xlab="Número de aulas", ylab = "Frequência")

##### MÉDIA, MEDIANA E MODA:
numeros <- c(1,2,3,4,5)
mean(numeros)
median(numeros)

lista <- c(2, 3, 7, 8, 1, 3, 4, 8, 22, 67, 19)
mean(lista)
median(lista)

## Verificando se a distribuição de uma variável é normal:
# PS.: É interessante verificar isso porque existem medidas de tendência central que funcionam
# melhor para distribuições normais e outras para distribuições assimétricas.
shapiro.test(numeros)
shapiro.test(lista)

## Sumário de infos:
summary(lista)
summary(numeros)

## Criando uma função para verificar a moda de uma variável:
mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x,ux)))]
}
atividade <- c(1, 2, 2, 2, 2, 3, 3, 4, 5, 5, 6, 7)
mode(atividade)

##### BOXPLOT
lista_de_numeros <- c(1,3,5,6,10,19,23,5,7, 89, 15, 14, 22,23,32,23,37)
summary(lista_de_numeros)
boxplot(lista_de_numeros)

## Salvando o gráfico como imagem:
# R não reconhece \. Reconhece /!
png(file="C:/Users/gades/Desktop/DataScience/R/Estatistica_I/boxplot.png", width=700, height=700)
boxplot(lista_de_numeros)
# O que acontece é que o R gravou o boxplot nesse local. É necessário dizer que o procedimento
# acabou através do comando dev.off:
dev.off()

exercicio <- c(20,21,22,22,23,24,24,26,28)
summary(exercicio)

##### DESVIO-PADRÃO
outra_lista <- c(1,2,3,5,6,7,8,11,2,3,44,55,67,12,34,56)
variancia <- var(outra_lista)

desvio_padrao_em_dois_passos <- sqrt(variancia)
desvio_padrao_em_dois_passos

desvio_padrao = sd(outra_lista)
desvio_padrao

## Extraindo dados de um arquivo CSV:
# Primeiro determinando a pasta onde os arquivos estão localizados
setwd("C:/Users/gades/Desktop/DataScience/R/Estatistica_I")

# Puxando o arquivo CSV para o R
nums <- read.csv(file="numeros.csv")
nums

# O caractere $ serve para indicar uma coluna de uma base:
nums$X1
hist(nums$X2)

##### INTERVALOS DE CONFIANÇA
lizard = c(6.2, 6.6, 7.1, 7.4, 7.6, 7.9, 8, 8.3, 8.4, 8.5, 8.6, 8.8, 8.8, 
           9.1, 9.2, 9.4, 9.4, 9.7, 9.9, 10.2, 10.4, 11.3, 11.9)

## Teste T-Student:
t.test(lizard)
## Mudando o nível de confiança
t.test(lizard, conf.level = 0.9)

## Plotando no histograma uma linha para média e outras para os intervalos de
## confiança:
media <- mean(lizard)
hist(lizard)
abline(v=media, col="blue", lwd=2)
abline(v=8.39, col="red", lwd=4)
abline(v=9.39, col="red", lwd=4)
#lwd = tamanho da linha
