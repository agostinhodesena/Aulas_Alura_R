################# Modelagem Estatística com R:
################# Prof. Vinícius Barros Rodrigues

######## PROJETO:
######## O que afeta a qualidade do ar nas cidades? Como essas variáveis se comportam?

### A importância de se planejar para fazer uma boa pergunta é que esse processo
### previne análises com base em correlações espúrias.

# ------------------------------------------------------------------------------------------

# INSTALANDO PACOTES:
install.packages("Ecdat")

# CARREGANDO O PACOTE:
library(Ecdat)

# EXTRAINDO DADOS DO PACOTE:
data(Airq)

# ACESSANDO INFORMAÇÕES SOBRE O BANCO DE DADOS:
names(Airq)

# OUTRA FORMA DE VERIFICAR A BASE:
View(Airq)

# ------------------------------------------------------------------------------------------

# DESCRIÇÃO DAS VARIÁVEIS:

# (Importante fazer a descrição de variáveis para o código ficar claro.
# Outras pessoas quando tiverem acesso a script vão entender de forma mais fácil
# as variáveis do projeto, ou até mesmo que escreveu o código e esqueceu do que se
# trata.)

# Airq: índice de qualidade do ar (quanto menor, melhor);
# Vala: valor das empresas nas cidades (milhares de dólares);
# Rain: quantidade de chuva (em polegadas);
# Coas: posição costeira da cidade (sim ou não - variável binária);
# dens: densidade populacional (milha quadrada);
# medi: renda média per capita (dólares);

# ------------------------------------------------------------------------------------------

# ANÁLISE DESCRITIVA DOS DADOS:
# (Primeira coisa que deve ser feita durante uma análise.)
summary(Airq)

# As variáveis podem ser contínuas ou categóricas (dividadas em categorias).
# A variável resposta ou dependente é a qualidade do ar (airq), as variáveis 
# explicativas ou independentes são as outras (vala, rain, coas, dens, medi). 
# Veremos se essas variáveis impactam de alguma forma na qualidade do ar.

# ------------------------------------------------------------------------------------------

# PLOTANDO AS VARIÁVEIS:
# O primeiro argumento definido como variável y em função da variável x.
plot(airq~vala, data=Airq)
plot(airq~rain, data=Airq)
plot(airq~coas, data=Airq)
plot(airq~dens, data=Airq)
plot(airq~medi, data=Airq)

# ------------------------------------------------------------------------------------------

# CRIANDO UM MODELO ESTATÍSTICO:
# y = f(x) que é equivalente no R a:
# y ~ x1 + x2 + ... + xn
# airq ~ vala + rain + coas + dens + medi

# MONTANDO O MODELO - Regressão Linear:

# Cidades com empresas de alto valor têm qualidade do ar diferente (melhor ou pior)?
m1 <- lm(airq~vala, data=Airq)

# <- equivalente a =;
# lm(), função para realização de regresões lineares
# "airq~vala", variável airq dependente da variável vala
# "data=", informando a base de dados das variáveis

m1
summary(m1)

# A variável vala não foi significativa nesse modelo, isto é, o valor das empresas,
# não influencia ou impacta na qualidade do ar das cidades.

# PLOTANDO A RETA DA REGRESSÃO NO GRÁFICO:
plot(airq~vala, data=Airq)
curve(96.451419 + 0.001969*x, add=TRUE)

# ------------------------------------------------------------------------------------------

# O fato da cidade ser costeira impacta na qualidade do ar?
m2 <- lm(airq~coas, data=Airq)
m2
summary(m2)

# A variável coas foi significativa nesse modelo, isso significa que a posição costeira
# da cidade influencia a qualidade do ar nas cidades. Segundo o resultado do modelo, as
# cidades costeiras têm a qualidade do ar melhor.

# PLOTANDO A RETA DA REGRESSÃO NO GRÁFICO:
plot(airq~coas, data=Airq)
curve(125.333 + 0.29476*x, add=TRUE)

# ------------------------------------------------------------------------------------------

# O nível de renda média per capita afeta a qualidade do ar?
m3 <- lm(airq~medi, data=Airq)
summary(m3)
plot(airq~medi, data=Airq)
curve(9.936e+01 + 5.638e-04*x, add=TRUE)

# A variável medi não foi significativa nesse modelo, isso significa que a renda média
# per capita não influencia a qualidade do ar nas cidades. 

# ------------------------------------------------------------------------------------------

# A quantidade de chuva afeta a qualidade do ar?
m4 <- lm(airq~rain, data=Airq)
summary(m4)
plot(airq~rain, data=Airq)
curve(106.6662 - 0.0545*x, add=TRUE)

# A quantidade de chuva não foi significativa nesse modelo, isso significa que essa variável
# não influencia a qualidade do ar nas cidades. 

# ------------------------------------------------------------------------------------------

# A densidade populacional afeta a qualidade do ar?
m5 <- lm(airq~dens, data=Airq)
summary(m5)
plot(airq~dens, data=Airq)
curve(1.054e+02 - 3.857e-04*x, add=TRUE)

# A densidade populacional não foi significativa nesse modelo, isso significa que 
# essa variável não influencia a qualidade do ar nas cidades. 

# Retas de modelos não significativos são opcionais nos gráficos.

# ------------------------------------------------------------------------------------------

##### MELHORANDO OS GRÁFICOS:

plot(airq~medi, data=Airq, xlab="Renda média per capita", ylab="Qualidade do ar", pch=18,
     col="blue", cex=1.2, cex.lab=1.1, main="Renda Média - 2010")
curve(9.936e+01 + 5.638e-04*x, add=TRUE, col="darkblue", lwd=2, lty=2)
# pch = configuração dos pontos no gráfico (point character);
# col = cor dos pontinhos;
# cex = tamanho dos pontos;
# cex.lab = tamanho da fonte nos eixos;
# lwd = tamanho da linha;
# lty = estilo da linha;
# main = título do gráfico.

plot(airq~coas, data=Airq, xlab="A região é costeira?", ylab="Qualidade do ar", 
     col="lightblue", ylim=c(50,170), cex.lab=1.1, main="Análise da qualidade do ar com base na região costeira")
# ylim = limite do eixo (começa e termina onde);
# c() = concatenação de valores;

#### EXERCÍCIO:
summary(m1)
plot(airq~vala, data=Airq, ylab="Qualidade do ar", 
     xlab="Valor das empresas (em milhões de dólares)", col="blue", pch=18, cex.lab=1.1,
     main="Impacto do valor das empresas sobre qualidade do ar")
curve(96.451419 + 0.001969*x, add=TRUE, col="darkblue", lty=2, lwd=1.5)

# ------------------------------------------------------------------------------------------

##### REGRESSÃO MÚLTIPLA:
mRM1 <- lm(airq~vala+coas, data=Airq)
summary(mRM1)

# Existe um efeito da posição costeira e do valor das empresas na qualidade do ar.
# A qualidade do ar das cidades é afetada tanto pelo valor das empresas quando pela 
# posição costeira das cidades. A qualidade do ar é melhor nas cidades costeiras e 
# pior nas cidades com empresas mais valiosas.

# GRÁFICO DA REGRESSÃO MÚLTIPLA
plot(airq~vala, data=Airq, ylab="Qualidade do ar", 
     xlab="Valor das empresas (em milhões de dólares)", pch=18,
     main="Impacto do valor das empresas sobre qualidade do ar")
curve(1.171e+02 + 1.999e-03*x, add=TRUE, lwd=1.5) # Cidade Costeira
curve(1.171e+02 + 1.999e-03*x+-2.968e+01, add=TRUE, lty=2, lwd=1.5) # Cidade não costeira
legend("bottomright", c("Não costeiras", "Costeiras"), lty=c(1,2), bty="n")
# legend("posição") = colocar legenda e primeiro atributo da função é a posição da legenda;
# bty = estilo de borda; 
# "n" = none.

# ------------------------------------------------------------------------------------------

# REGRESSÃO MÚLTIPLA COM AS VARIÁVEIS "VALA", "COAS" E "DENS"
mRM2 <- lm(airq~vala+coas+dens, data=Airq)
summary(mRM2)

# ------------------------------------------------------------------------------------------

# CONTRASTE DE MODELOS:
# Consiste em comparar um modelo completo com um modelo sem uma variável em questão.
# Modelo A - Completo:
mACompleto <- lm(airq~vala+coas+dens, data=Airq)
summary(mACompleto)

# Modelo B - Sem Dens:
mBSemDens <- lm(airq~vala+coas, data=Airq)
summary(mBSemDens)

# Os modelos são iguais?
anova(mACompleto, mBSemDens)
# Hipótese nula: Não existe diferença entre os modelos.
# Hipótese alternativa: Existe diferença entre os modelos.

# Segundo o teste ANOVA, os modelos comparados não são diferentes, ou seja, a variável
# "Dens" pode ser retirada do modelo.

# ------------------------------------------------------------------------------------------

# GRÁFICO FINAL:

plot(airq~vala, data=Airq, ylab="Qualidade do ar", 
     xlab="Valor das empresas (em milhões de dólares)", pch=18,
     main="Impacto do valor das empresas de cidades costeiras e não costeiras sobre qualidade do ar")
curve(1.171e+02 + 1.999e-03*x, add=TRUE, lwd=1.5) # Cidade Costeira
curve(1.171e+02 + 1.999e-03*x+-2.968e+01, add=TRUE, lty=2, lwd=1.5) # Cidade não costeira
legend("bottomright", c("Não costeiras", "Costeiras"), lty=c(1,2), bty="n")
# legend("posição") = colocar legenda e primeiro atributo da função é a posição da legenda;
# bty = estilo de borda; 
# "n" = none.

# ------------------------------------------------------------------------------------------

# CONCLUSÃO:

# O que afeta a qualidade do ar nas cidades?
# As variáveis que afetaram foram: (a) o valor das empresas; (b) a posição costeira das
# cidades. Quanto maior o valor das empresas, pior a qualidade do ar. Cidades costeiras
# apresentam uma melhor qualidade do ar.