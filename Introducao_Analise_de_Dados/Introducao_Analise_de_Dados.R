################# Regressão Linear Simples em R: correlação e previsão
################# Profa. Alia Garrudo Guirado

# ----------------------------------------------------------------------------------------------------------------

#### O PROBLEMA:
#### Uma empresa de vídeos on-line pediu para analisar os dados sobre os cursos oferecidos por ela.
#### Qual a média de tempo para conclusão dos cursos? Quais cursos são mais acessados?
#### A popularidade de um curso tem a ver com sua duração? 
#### Junção de bases diferentes.

# ----------------------------------------------------------------------------------------------------------------
# IMPORTAÇÃO DOS DADOS:

setwd("C:/Users/gades/Desktop/DataScience/R/Introducao_Analise_de_Dados") # Determinando a pasta 
                                                                          # onde os arquivos estão 
                                                                          # localizados.

library(readxl)
aulas <- read_excel("C:/Users/gades/Desktop/DataScience/R/Introducao_Analise_de_Dados/aulas.xlsx")

# ----------------------------------------------------------------------------------------------------------------
# CONHECENDO A BASE & ANÁLISE QUANTITATIVA:

attach(aulas) # Cria uma cópia das variáveis que estão na base. Dessa forma, elas podem ser chamadas sem o $.
section_id

options(max.print=40000) # Mudando o número de linhas a serem exibidas.
section_id

head(section_id) # Exibindo as primeiras observações

sort(section_id) # Ordenando do menor para o maior

aulas[33137,3] <- 3255 # Corrigindo um valor errado na base.

sort(aulas$section_id)

aulas[33137,3]

unique(aulas$section_id) # Omitindo a repetição de valores

length(unique(aulas$section_id)) # Contagem de valores únicos

table(aulas$section_id) # Criando uma tabela de frequências.

sort(table(aulas$section_id)) # Criando uma tabela de frequências ordenada.

sort(table(aulas$course_id))

# install.packages("plyr") # Biblioteca que tem a função de contar 
library(plyr)
 
auxiliar <- count(aulas, vars="course_id") # Salvando em um objeto a frequencia de cada observação.

popularidade <- auxiliar

write.csv(auxiliar, "popularidade.csv") # Exportando a base para um arquivo .csv

# ----------------------------------------------------------------------------------------------------------------
# GRÁFICOS:
rm(list=ls()) # Limpando o que foi feito anteriormente

duracao <- read.csv("C:/Users/gades/Desktop/DataScience/R/Introducao_Analise_de_Dados/duracao.csv")

duracao <- rename(duracao, replace=c("user_id"="aluno","course_id"="curso","timeToFinish"="dias"))

plot(duracao$dias) # Esse gráfico não está dizendo nada.

jpeg("histograma.jpg") # Exportação de gráfico como imagem
hist(duracao$dias, breaks=20, main="Histograma do Tempo", ylab="Frequência", xlab="Tempo", ylim=c(0,2000), col="blue")
dev.off() # Fechando o comando

# ----------------------------------------------------------------------------------------------------------------
# ANÁLISE ESTATÍSTICA:

# Média:
mean(duracao$dias) # A média é NA = Not Available se a base tem dados faltantes.
mean(duracao$dias, na.rm=T)

# Mediana:
median(duracao$dias) # A mediana é NA = Not Available se a base tem dados faltantes.
median(duracao$dias, na.rm=T)

# Sumário Estatístico:
summary(duracao$dias)

# Proporção de dados faltantes:
dim(duracao)[1] # Número de linhas da base.
(3828/6366)*100

# Números de cursos únicos:
length(unique(duracao$curso))

# Números de alunos únicos:
length(unique(duracao$aluno))

# ----------------------------------------------------------------------------------------------------------------
# FUSÃO E BANCO DE DADOS:

sumario_estatistico <- aggregate(duracao$dias, list(duracao$curso), mean, na.rm=T)

sumario_estatistico

popularidade_e_duracao <- merge(sumario_estatistico, popularidade, by='curso')

sumario_estatistico <- rename(sumario_estatistico, replace=c('Group.1'='curso', 'x'='dias'))

popularidade <- rename(popularidade, c('course_id'='curso', 'freq'='popularidade'))

popularidade_e_duracao <- merge(sumario_estatistico, popularidade, by='curso')

# ----------------------------------------------------------------------------------------------------------------
# GRÁFICO E MODELO LINEAR:

plot(popularidade_e_duracao$dias, popularidade_e_duracao$popularidade)

modelo.linear <- lm(popularidade_e_duracao$popularidade ~ popularidade_e_duracao$dias)

plot(popularidade_e_duracao$dias, popularidade_e_duracao$popularidade)
abline(lm(popularidade_e_duracao$popularidade ~ popularidade_e_duracao$dias))

# ----------------------------------------------------------------------------------------------------------------
# MODELO NÃO LINEAR:

scatter.smooth(popularidade_e_duracao$dias, popularidade_e_duracao$popularidade, pch=21, col='grey')

library('ggplot2')

grafico <- ggplot(popularidade_e_duracao, aes(dias, popularidade))
grafico <- grafico + geom_point() + geom_smooth()
grafico
