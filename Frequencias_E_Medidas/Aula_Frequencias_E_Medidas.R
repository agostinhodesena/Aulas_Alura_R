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