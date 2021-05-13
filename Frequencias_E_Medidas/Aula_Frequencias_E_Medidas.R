################# Frequências e Medidas:
################# Prof. Rodrigo Fernando Dias

# ------------------------------------------------------------------------------------------

# BASE DE DADOS: PNAD 2015

# A Pesquisa Nacional por Amostra de Domicílios - PNAD investiga anualmente, de forma permanente, 
# características gerais da população, de educação, trabalho, rendimento e habitação e outras, 
# com periodicidade variável, de acordo com as necessidades de informação para o país, como as 
# características sobre migração, fecundidade, nupcialidade, saúde, segurança alimentar, entre 
# outros temas. O levantamento dessas estatísticas constitui, ao longo dos 49 anos de realização 
# da pesquisa, um importante instrumento para formulação, validação e avaliação de políticas 
# orientadas para o desenvolvimento socioeconômico e a melhoria das condições de vida no Brasil.

# ------------------------------------------------------------------------------------------

# DESCRIÇÃO DAS VARIÁVEIS:

# Renda: Rendimento mensal do trabalho principal para pessoas de 10 anos ou mais de idade;
# Idade: Idade do morador na data de referência em anos;
# Altura: Idade do morador na data de referência em anos (elaboração própria);
# UF:
    # 11 - Rondônia
    # 12 - Acre
    # 13 - Amazonas
    # 14 - Roraima
    # 15 - Pará
    # 16 - Amapá
    # 17 - Tocantins
    # 21 - Maranhão
    # 22 - Piauí
    # 23 - Ceará
    # 24 - Rio Grande do Norte
    # 25 - Paraíba
    # 26 - Pernambuco
    # 27 - Alagoas
    # 28 - Sergipe
    # 29 - Bahia
    # 31 - Minas Gerais
    # 32 - Espírito Santo
    # 33 - Rio de Janeiro
    # 35 - São Paulo
    # 41 - Paraná
    # 42 - Santa Catarina
    # 43 - Rio Grande do Sul
    # 50 - Mato Grosso do Sul
    # 51 - Mato Grosso
    # 52 - Goiás
    # 53 - Distrito Federal;
# Sexo:
    # 0 - Masculino
    # 1 - Feminino;
# Anos de Estudo:
    # 1 - Sem instrução e menos de 1 ano
    # 2 - 1 ano
    # 3 - 2 anos
    # 4 - 3 anos
    # 5 - 4 anos
    # 6 - 5 anos
    # 7 - 6 anos
    # 8 - 7 anos
    # 9 - 8 anos
    # 10 - 9 anos
    # 11 - 10 anos
    # 12 - 11 anos
    # 13 - 12 anos
    # 14 - 13 anos
    # 15 - 14 anos
    # 16 - 15 anos ou mais
    # 17 - Não determinados 
    # " " - Não aplicável;
# Cor: 
    # 0 - Indígena
    # 2 - Branca
    # 4 - Preta
    # 6 - Amarela
    # 8 - Parda
    # 9 - Sem declaração;


# Observação:
# Os seguintes tratamentos foram realizados nos dados originais:
# 1. Foram eliminados os registros onde a Renda era inválida (999 999 999 999);
# 2. Foram eliminados os registros onde a Renda era missing;
# 3. Foram considerados somente os registros das Pessoas de Referência de cada domicílio 
# (responsável pelo domicílio).

# ------------------------------------------------------------------------------------------

## R - INFORMAÇÕES BÁSICAS:
sessionInfo() # Informações da instalação do R.

# Importando bibliotecas:
library(dplyr)

# Acessando a ajuda:
?select
?arrange

# ------------------------------------------------------------------------------------------

## IMPORTANDO DATASET DO PROJETO:
setwd("C:/Users/gades/Desktop/DataScience/R/Frequencias_E_Medidas") # Determinando a pasta 
                                                                    # onde os arquivos estão 
                                                                    # localizados.
dados <- read.csv("dados.csv") # Importando arquivo csv.
View(dados)
head(dados, 5) # Para ver o cabeçalho da base de dados.

# ------------------------------------------------------------------------------------------

## TIPOS DE DADOS:
# A - Variáveis qualitativas ordinais:
# Variáveis que podem ser ordenadas ou hierarquizadas.

select(dados, Anos.de.Estudo) # Selecionando a coluna "Anos de Estudo".
unique(select(dados, Anos.de.Estudo)) # Mostrando os valores únicos da coluna.
arrange(unique(select(dados, Anos.de.Estudo)), Anos.de.Estudo) # Ordenando cardinalmente a coluna.
head(arrange(dados, Anos.de.Estudo), 5) # Ordenando cardinalmente a base de dados conforme 
                                        # "Anos.de.Estudo".
c(arrange(unique(select(dados, Anos.de.Estudo)), Anos.de.Estudo)) # Cria um vetor com as 
                                                                  # informações passadas.

# B - Variáveis qualitativas nominais:
# Variáveis que não podem ser ordenadas ou hierarquizadas.

c(arrange(unique(select(dados, UF)), UF))
c(arrange(unique(select(dados, Sexo)), Sexo))
c(arrange(unique(select(dados, Cor)), Cor))

# C - Variáveis quantitativas discretas:
# Variáveis que representam uma contagem onde os valores possíveis formam um conjunto finito
# ou enumerável.

sprintf('De %s até %s anos.', min(dados$Idade), max(dados$Idade)) # sprintf = uma frase concatenada
                                                                  # com infos de uma função.
library(glue)
glue('De {min(dados$Idade)} até {max(dados$Idade)} anos.') # glue = uma frase concatenada com infos
                                                           # de uma função.

# Observação:
# A variável idade pode ser classificada de três formas distintas:
# 1. QUANTITATIVA DISCRETA - quando representa anos completos (números inteiros);
# 2. QUANTITATIVA CONTÍNUA - quando representa a idade exata, sendo representado por frações de anos; e
# 3. QUALITATIVA ORDINAL - quando representa faixas de idade.

# D - Variáveis quantitativas contínuas:
# Variáveis que representam uma contagem ou mensuração que assumem valores em uma escala contínua
# (números reais).

glue('De {min(dados$Altura)} até {max(dados$Altura)} metros.')

# ------------------------------------------------------------------------------------------

## DISTRIBUIÇÃO DE FREQUÊNCIAS - PARA VARIÁVEIS QUALITATIVAS
# MÉTODO 1:

table(dados$Sexo) # Tabulando a frequência que cada valor aparece nessa variável.
prop.table(table(dados$Sexo))*100 # Tabulando a proporção que cada valor aparece nessa variável.

# Criando uma tabela com o comando cbind com informações de frequência e proporção:
dist_freq_qualitativas <- cbind(freq=table(dados$Sexo), percent=prop.table(table(dados$Sexo))*100)
dist_freq_qualitativas

colnames(dist_freq_qualitativas) <- c('Frequência', 'Porcentagem (%)') # Alterando nome das colunas da matriz.
rownames(dist_freq_qualitativas) <- c('Masculino', 'Feminino') # Alterando nome das linhas da matriz.
dist_freq_qualitativas

# MÉTODO 2:

frequencia <- table(dados$Sexo, dados$Cor)
frequencia

rownames(frequencia) <- c('Masculino', 'Feminino')
colnames(frequencia) <- c('Indígena', 'Branca', 'Preta', 'Amarela', 'Parda')
frequencia <- cbind(frequencia)
frequencia

percentual <- prop.table(frequencia)*100
percentual

# Criando uma tabela com três informações sendo que uma delas é resultado de uma função:
medias <- tapply(dados$Renda, list(dados$Sexo, dados$Cor), mean)
rownames(medias) <- c('Masculino', 'Feminino')
colnames(medias) <- c('Indígena', 'Branca', 'Preta', 'Amarela', 'Parda')
medias

# ------------------------------------------------------------------------------------------

## DISTRIBUIÇÃO DE FREQUÊNCIAS - PARA VARIÁVEIS QUANTITATIVAS
# A - Classes personalizadas:

# Classes de Renda:
# A - Acima de 20 SM
# B - De 10 a 20 SM
# C - De 4 a 10 SM
# D - De 2 a 4 SM
# E - Até 2 SM

# onde SM é o valor do salário mínimo na época. Em nosso caso R$ 788,00 (2015):
# A -  Acima de 15.760
# B -  De 7.880 a 15.760
# C -  De 3.152 a 7.880
# D -  De 1.576 a 3.152
# E -  Até 1.576

min(dados$Renda)
max(dados$Renda)

classes <- c(0,1576,3152,7880,15760,200000)
labels <- c('E', 'D', 'C', 'B', 'A')

frequencia <- table(
    cut(
        x=dados$Renda,
        breaks=classes,
        labels=labels,
        include.lowest = TRUE
    )
)
frequencia

percentual <- prop.table(frequencia)*100
percentual

dist_freq_quantitativas_personalizadas <- cbind('Frequência' = frequencia, 'Porcentagem (%)' = percentual)
dist_freq_quantitativas_personalizadas

# Colocando a tabela na ordem do índice:
dist_freq_quantitativas_personalizadas[
    order(row.names(dist_freq_quantitativas_personalizadas)),
]

# B - Classes de amplitude fixa:
# Passo 1: Definindo o número de classes - Regra de Sturges

n <- nrow(dados) # Número de linhas (0bservações) na base de dados.
n
k <- 1 + (10/3) * log10(n)
k

k <- round(k) # Função de arredondamento.
k # Resultado: 17 classes.

# Passo 2: Criar tabela de frequências.

labels <- c(
    '      0.00 |-|  11,764.70', 
    ' 11,764.70  -|  23,529.40', 
    ' 23,529.40  -|  35,294.10', 
    ' 35,294.10  -|  47,058.80', 
    ' 47,058.80  -|  58,823.50', 
    ' 58,823.50  -|  70,588.20', 
    ' 70,588.20  -|  82,352.90', 
    ' 82,352.90  -|  94,117.60', 
    ' 94,117.60  -| 105,882.00', 
    '105,882.00  -| 117,647.00', 
    '117,647.00  -| 129,412.00', 
    '129,412.00  -| 141,176.00', 
    '141,176.00  -| 152,941.00', 
    '152,941.00  -| 164,706.00', 
    '164,706.00  -| 176,471.00', 
    '176,471.00  -| 188,235.00', 
    '188,235.00  -| 200,000.00'
)

frequencia <- table(
    cut(
        x=dados$Renda,
        breaks=k,
        labels=labels,
        include.lowest = TRUE
    )
)
frequencia

percentual <- prop.table(frequencia)*100
percentual

dist_freq_quantitativas_amplitude_fixa <- cbind('Frequência' = frequencia, 'Porcentagem (%)' = percentual)
dist_freq_quantitativas_amplitude_fixa

# Colocando a tabela na ordem do índice:
dist_freq_quantitativas_amplitude_fixa[
    order(row.names(dist_freq_quantitativas_amplitude_fixa)),
]

# HISTOGRAMA:
# O HISTOGRAMA é a representação gráfica de uma distribuição de frequências. 
# É um gráfico formado por um conjunto de retângulos colocados lado a lado, 
# onde a área de cada retângulo é proporcional à frequência da classe que ele representa.

# options(repr.plot.width = 7, repr.plot.height = 4) # Configurando o tamanho da visualização do histograma

hist(dados$Altura) # Gráfico simples

hist(
    x = dados$Altura,
    breaks = 'Sturges',
    col = 'lightblue',
    main = 'Histograma das Alturas',
    xlab = 'Altura',
    ylab = 'Frequência'
)

# col = cor, main = título, xlab = legenda do eixo x, ylab = legenda do eixo y 

library(ggplot2)
library(ggthemes)

ggplot(dados, aes(x = Altura)) + 
           geom_histogram(binwidth = 0.02, color = "black", alpha = 0.9) + 
           ylab("Frequência") + 
           xlab("Alturas") + 
           ggtitle('Histograma das Alturas') +
           theme(
               plot.title = element_text(size = 14, hjust = 0.5),
               axis.title.y = element_text(size = 12, vjust = +0.2),
               axis.title.x = element_text(size = 12, vjust = -0.2),
               axis.text.y = element_text(size = 9),
               axis.text.x = element_text(size = 9)
           )

# Atribuindo um nome a uma parte da função para não repeti-la mais durante o código:
formatos <- theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title.y = element_text(size = 12, vjust = +0.2),
    axis.title.x = element_text(size = 12, vjust = -0.2),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9)
)

ggplot(dados, aes(x = Altura, y = ..density..)) + 
    geom_histogram(binwidth = 0.02, color = "black", alpha = 0.9) + 
    geom_density(color = 'red') +
    ylab("Frequência") + 
    xlab("Alturas") + 
    ggtitle('Histograma das Alturas') +
    formatos

# ..density.. = plotar a linha que descreve a função densidade de probabilidade.

# Transformando a matriz com a distribuição de frequência da renda em data frame.
# (Fazer isso é importante porque é melhor trabalhar com df no ggplot) 
bar_chart <- data.frame(dist_freq_quantitativas_personalizadas)
bar_chart

ggplot(bar_chart, aes(x = row.names(bar_chart), y = bar_chart$Frequência)) + 
    geom_bar(stat = "identity") + 
    ylab("Frequência") + 
    xlab("Classes de Renda") + 
    ggtitle('Gráfico com as Classes de Renda') +
    formatos


# ------------------------------------------------------------------------------------------

## MEDIDAS DE TENDÊNCIA CENTRAL

# Notas de alunos para 6 matérias:
materias <- c('Matemática', 'Português', 'Inglês', 'Geografia', 'História', 'Física', 'Química')
Fulano <- c(8, 10, 4, 8, 6, 10, 8)
Beltrano <- c(10, 2, 0.5, 1, 3, 9.5, 10)
Sicrano <- c(7.5, 8, 7, 8, 8, 8.5, 7)

# Criando um dataframe com as notas dos alunos:
df <- data.frame(Fulano, Beltrano, Sicrano, row.names = materias)
df

# MÉDIA:
(8 + 10 + 4 + 8 + 6 + 10 + 8)/7
mean(df$Fulano)
mean(dados$Renda)

# Realizando a média de uma variável condicionada a outra:
aggregate(list(Renda = dados$Renda), list(Sexo = dados$Sexo), mean) # A variável que será condição deve entrar como uma lista.

# MEDIANA:
# Forma Manual - n é ÍMPAR:

df
df_fulano <- df[order(df$Fulano), ]
df_fulano

# Obtendo o elemento mediano:
n = nrow(df_fulano)
n

elemento_md <- (n+1)/2
elemento_md

df_fulano[elemento_md, ]

# Com apenas uma função:
median(df$Fulano)

# Forma Manual - n é PAR:
# Criando uma amostra aleatória fixa através do set.seed:
set.seed(101)
sample(nrow(df), 6)
df_beltrano <- df[sample(nrow(df), 6),]
df_beltrano

n <- nrow(df_beltrano)
n

# Ordenando os dados:
df_beltrano <- df_beltrano[order(df_beltrano$Beltrano),]
df_beltrano

elemento_md = n/2
elemento_md

mean(df_beltrano[c(elemento_md, elemento_md+1),]$Beltrano) # $Beltrano = selecionando dados apenas da coluna
                                                           # Beltrano.

median(df_beltrano$Beltrano)

# Obtendo a mediana em nosso dataset:
median(dados$Renda)

# MODA:
df

exemplo_moda <- c(1, 2, 2, 3, 4, 4, 5, 6, 7, 7)
exemplo_moda

freq <- table(exemplo_moda)
freq

freq[freq == max(freq)]

names(freq)[freq == max(freq)]

# Criar uma função:
moda <- function(x) {
    
    freq <- table(x)
    return(names(freq)[freq == max(freq)])
    
}

moda(exemplo_moda)

moda(df$Fulano)

moda(df$Beltrano)

moda(df$Sicrano)

moda(dados$Renda)

moda(dados$Altura)

# RELAÇÃO ENTRE AS MEDIDAS DE TENDÊNCIA CENTRAL (MÉDIA, MEDIANA, MODA):

# Assimetria à direita: Moda > Mediana > Média
# Simétrica: Média = Mediana = Moda
# Assimetria à esquerda: Média > Mediana > Moda

# Avaliando a relação dessas medidas com a variável 'RENDA':
ggplot(dados[dados$Renda < 20000, ], aes(x = Renda, y = ..density..)) + 
    geom_histogram(binwidth = 500) + 
    geom_density(color = 'green')

moda_1 <- as.numeric(moda(dados$Renda)) # as.numeric = transformar números que estão como string em números.
moda_1

mediana <- median(dados$Renda)
mediana

media <- mean(dados$Renda)
media

# Comparativo:
moda_1 < mediana
mediana < media

# Avaliando a relação dessas medidas com a variável 'ALTURA':
ggplot(dados, aes(x = Altura, y = ..density..)) + 
    geom_histogram() + 
    geom_density(color = 'green')

moda_2 <- as.numeric(moda(dados$Altura))
moda_2

mediana <- median(dados$Altura)
mediana

media <- mean(dados$Altura)
media

# Avaliando a relação dessas medidas com a variável 'ANOS DE ESTUDO':
ggplot(dados, aes(x = Anos.de.Estudo, y = ..density..)) + 
    geom_histogram() + 
    geom_density(color = 'green')

moda_3 <- as.numeric(moda(dados$Anos.de.Estudo))
moda_3

mediana <- median(dados$Anos.de.Estudo)
mediana

media = mean(dados$Anos.de.Estudo)
media

# Comparativo:
moda_3 > mediana
mediana > media

# ------------------------------------------------------------------------------------------

## MEDIDAS SEPARATRIZES:
# Quartis:
quantile(dados$Renda, c(0.25, 0.5, 0.75))

decis <- c()
for(i in 1:9){
    decis <- c(decis, i/10)
}
decis

quantile(dados$Renda, decis)

centis <- c()
for(i in 1:99){
    centis <- c(centis, i/100)
}
quantile(dados$Renda, centis)

# Gráfico da frequência acumulada da Idade:
ggplot(data = dados, aes(x = Idade)) + 
    geom_histogram(
        aes(y = cumsum(..count..)/sum(..count..)), 
        bins = 10
    ) + 
    geom_freqpoly(
        aes(y = cumsum(..count..)/sum(..count..)), 
        color = 'green'
    )

decis <- c()
for(i in 1:9){
    decis <- c(decis, i/10)
}
quantile(dados$Idade, decis)

length(dados$Idade[dados$Idade <=40]) / length(dados$Idade) * 100

# BOXPLOT:

sexo = c(
    'Masculino', 
    'Feminino'
)
cor = c(
    'Indígena', 
    'Branca', 
    'Preta', 
    'Amarela', 
    'Parda'
)
anos_de_estudo = c(
    'Sem instrução e menos de 1 ano', 
    '1 ano', 
    '2 anos', 
    '3 anos', 
    '4 anos', 
    '5 anos', 
    '6 anos', 
    '7 anos', 
    '8 anos', 
    '9 anos', 
    '10 anos', 
    '11 anos', 
    '12 anos', 
    '13 anos', 
    '14 anos', 
    '15 anos ou mais', 
    'Não determinados'
)

ggplot(data = dados, aes(x = " ", y = Altura)) + 
    stat_boxplot(geom ='errorbar', width = 0.4) + 
    geom_boxplot(fill = '#3274A1') + 
    coord_flip() +
    ylab("Metros") + 
    xlab("") + 
    ggtitle('Box-plot: Alturas') +
    formatos

ggplot(data = dados, aes(x = Sexo, y = Altura, group = Sexo)) + 
    stat_boxplot(geom ='errorbar', width = 0.4) + 
    geom_boxplot(fill = c('#3274A1', "orange")) + 
    coord_flip() +
    ylab("Metros") + 
    xlab("Sexo") + 
    ggtitle('Box-plot: Alturas vs. Sexo') +
    formatos

# Transformando variável 'SEXO' em uma variável categórica não numérica:
dados$Cat.Sexo <- factor(dados$Sexo)
levels(dados$Cat.Sexo) <- sexo

ggplot(data = dados, aes(x = Cat.Sexo, y = Altura)) + 
    stat_boxplot(geom ='errorbar', width = 0.4) + 
    geom_boxplot(fill = c('#3274A1', "orange")) + 
    coord_flip() +
    ylab("Metros") + 
    xlab("Sexo") + 
    ggtitle('Box-plot: Alturas vs. Sexo') +
    formatos

ggplot(data = dados[dados$Renda < 10000, ], aes(x = " ", y = Renda)) + 
    stat_boxplot(geom ='errorbar', width = 0.4) + 
    geom_boxplot(fill = '#3274A1') + 
    coord_flip() +
    ylab("R$") + 
    xlab("") + 
    ggtitle('Box-plot: Renda') +
    formatos

ggplot(data = dados[dados$Renda < 10000, ], aes(x = Cat.Sexo, y = Renda)) + 
    stat_boxplot(geom ='errorbar', width = 0.4) + 
    geom_boxplot(fill = c('#3274A1', "orange")) + 
    coord_flip() +
    ylab("R$") + 
    xlab("Sexo") + 
    ggtitle('Box-plot: Renda vs. Sexo') +
    formatos

# Transformando variável 'SEXO' em uma variável categórica não numérica:
dados$Cat.Anos.de>Estudo <- factor(dados$Anos.de.Estudo, order=True) # order = dizendo pro R que essa variável
levels(dados$Cat.Anos.de.Estudo) <- anos_de_estudo                   # pode ser ordenada.

ggplot(data = dados, aes(x = " ", y = Anos.de.Estudo)) + 
    stat_boxplot(geom ='errorbar', width = 0.4) + 
    geom_boxplot(fill = '#3274A1') + 
    coord_flip() +
    ylab("Anos") + 
    xlab("") + 
    ggtitle('Box-plot: Anos de Estudo') +
    formatos

ggplot(data = dados, aes(x = Cat.Sexo, y = Anos.de.Estudo)) + 
    stat_boxplot(geom ='errorbar', width = 0.4) + 
    geom_boxplot(fill = c('#3274A1', "orange")) + 
    coord_flip() +
    ylab("Anos") + 
    xlab("Sexo") + 
    ggtitle('Box-plot: Anos de Estudo vs. Sexo') +
    formatos

# ------------------------------------------------------------------------------------------

## MEDIDAS DE DISPERSÃO:
# DESVIO MÉDIO ABSOLUTO

df
summary(df)

notas_fulano <- data.frame(Fulano = df$Fulano, row.names(df))
notas_fulano

nota_media_fulano <- mean(notas_fulano$Fulano)
nota_media_fulano

notas_fulano$Desvio <- notas_fulano$Fulano - nota_media_fulano
notas_fulano

notas_fulano$Desvio.Absoluto <- abs(notas_fulano$Desvio)
notas_fulano

ggplot(data = notas_fulano, aes(x = row.names(notas_fulano), y = Fulano)) + 
    geom_point() + 
    geom_hline(yintercept = mean(notas_fulano$Fulano), color = 'red') + 
    geom_segment(aes(x = 1, y = 10, xend = 1, yend = mean(notas_fulano$Fulano))) + 
    geom_segment(aes(x = 2, y = 8, xend = 2, yend = mean(notas_fulano$Fulano))) + 
    geom_segment(aes(x = 3, y = 6, xend = 3, yend = mean(notas_fulano$Fulano))) + 
    geom_segment(aes(x = 4, y = 4, xend = 4, yend = mean(notas_fulano$Fulano))) + 
    geom_segment(aes(x = 5, y = 8, xend = 5, yend = mean(notas_fulano$Fulano))) + 
    geom_segment(aes(x = 6, y = 10, xend = 6, yend = mean(notas_fulano$Fulano))) + 
    geom_segment(aes(x = 7, y = 8, xend = 7, yend = mean(notas_fulano$Fulano)))

mean(notas_fulano$Desvio.Absoluto)

# VARIÂNCIA:

notas_fulano$Desvio2 <- notas_fulano$Desvio ^ 2
notas_fulano

sum(notas_fulano$Desvio2) / (nrow(notas_fulano) - 1)

variancia <- var(notas_fulano$Fulano)
variancia

# DESVIO-PADRÃO:

sqrt(variancia)

desvio_padrao <- sd(notas_fulano$Fulano)
desvio_padrao

summary(df)

sd(df$Fulano)
sd(df$Sicrano)

# ATIVIDADE
dataset <- data.frame( 
    Sexo = c('H', 'M', 'M', 'M', 'M', 'H', 'H', 'H', 'M', 'M'), 
    Idade = c(53, 72, 54, 27, 30, 40, 58, 32, 44, 51) 
) 

dataset
sd(dataset$Idade)
sd(dataset$Idade[dataset$Sexo == 'M'])
