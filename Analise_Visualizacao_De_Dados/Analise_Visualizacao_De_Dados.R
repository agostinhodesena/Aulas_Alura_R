################# Análise e visualização de dados: ciência de dados com R
################# Prof. Mauro Alves

# ----------------------------------------------------------------------------------------------------------------

#### O PROBLEMA:
#### Verificar se existem imóveis de interesse para um empresa com base no tamanho e preços de aluguel.

# ----------------------------------------------------------------------------------------------------------------
# IMPORTAÇÃO E CARREGAMENTO DE PACOTES:

install.packages("readr")

library(readr)

# ----------------------------------------------------------------------------------------------------------------
# IMPORTAÇÃO DOS DADOS:

setwd("C:/Users/gades/Desktop/DataScience/R/Analise_Visualizacao_De_Dados") # Determinando a pasta 
                                                                            # onde os arquivos estão 
                                                                            # localizados.

aluguel <- read_delim("aluguel.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# ----------------------------------------------------------------------------------------------------------------
# ENTENDENDO A BASE DE DADOS:

# Visualização da estrutura da base de dados (quantas observações, variáveis, tipos de variáveis):
str(aluguel)

# Visualização das primeiras linhas da base de dados:
head(aluguel)


