# =============================================================================
# PACOTES
# =============================================================================
# install.packages("tidyverse")
library(tidyverse)

# =============================================================================
# Abrir a base de dados
# =============================================================================
getwd()
path <- "C:/Users/Usuário/Desktop/Projetos/Especialização Analise de dados - Ufrrj/Probabilidade e inferencia estatistica/Atividades/Atividade 1"
  setwd(path)

 dados <- read_csv("dados_politica_publica.csv")
glimpse(dados)

# =============================================================================
# EXERCÍCIO 1 – Espaço Amostral e Eventos
# =============================================================================
# Teoria:
# O espaço amostral é o conjunto de todos os resultados possíveis.
# Um evento é um subconjunto do espaço amostral.
#
# Prática:
# a) Qual é o espaço amostral da variável "Regiao"?
# "Rural
# b) Qual a proporção de famílias que vivem na área Urbana?

# a) 
table(dados$Regiao)

# 1º Maneira
prop_urbana <- mean(dados$Regiao == "Urbana")
prop_urbana


# 2º Maneira
nrow(dados[dados$Regiao == "Urbana",])/nrow(dados)


# 3º Maneira
dados %>% 
  filter(Regiao == "Urbana") %>% 
  nrow() / nrow(dados)


# =============================================================================
# EXERCÍCIO 2 – Probabilidade Clássica
# =============================================================================
# Teoria:
# P(Evento) = Casos Favoráveis / Casos Possíveis
#
# Prática:
# Qual a probabilidade de selecionar aleatoriamente uma família
# cujo responsável seja do sexo feminino?

# 1º Maneira
P_feminino <- mean(dados$SexoResponsavel == "Feminino")
P_feminino
# Resposta: 0.6917

# 2º Maneira
nrow(dados[dados$SexoResponsavel == "Feminino",])/nrow(dados)

# 3º Maneira
dados %>% 
  filter(SexoResponsavel == "Feminino") %>% 
  nrow() / nrow(dados)

# =============================================================================
# EXERCÍCIO 3 – Probabilidade Conjunta
# =============================================================================
# Teoria:
# P(A ∩ B) = Probabilidade de A e B ocorrerem ao mesmo tempo.
# P(Evento) = Casos Favoráveis / Casos Possíveis
#
# Prática:
# Qual a probabilidade de uma família ser vulnerável E ter evasão escolar?

# 1ª maneira
P_vuln_evasao <- mean(dados$Vulneravel == "Sim" & dados$EvasaoEscolar == "Sim")
P_vuln_evasao


# 2ª maneira
dados %>% 
  filter(Vulneravel == "Sim" & EvasaoEscolar == "Sim") %>% 
  nrow()/nrow(dados)


# =============================================================================
# EXERCÍCIO 4 – Probabilidade Condicional
# Calcule a probabilidade de uma família ter acesso à internet
# dado que NÃO é vulnerável.
# =============================================================================

# 1ª maneira
dados %>% 
  filter(TemInternet == "Sim" & Vulneravel == "Não") %>% 
  nrow()

dados %>% 
  filter(Vulneravel == "Não") %>% 
  nrow()

# 4531/5681
# 0.79

# 2ª maneira
P_internet_dado_nao_vuln <- mean(dados$TemInternet[dados$Vulneravel == "Não"] == "Sim")
P_internet_dado_nao_vuln

# =============================================================================
# EXERCÍCIO 5 – Simulação
# =============================================================================

# No material didático, na abordagem frequentista para as 
# probabilidades, você estudou que a probabilidade de um evento é 
# determinada pela sua frequência relativa de ocorrência em um 
# grande número de repetições de um experimento. 

# No experimento a seguir, calcula-se a frequencia relativa para 
# 100.000 repetições do experimento: sortear o sexo do responsável.

# Compare as respostas da frequencia_rel_1, frequencia_rel_2 com
# o cálculo da probabilidade da questão 2. Explique a igualdade e a
# diferença entre os valores obtidos apenas para o caso Feminino.

# Número de repetições do experimento (simulações)
n_simulacoes <- 100000

# frequencia_rel_1 ------------------------------------------------------------

# Criando vetor
amostra_1 <- c(unique(dados$SexoResponsavel))

# Para reprodutibilidade
set.seed(123)

# Sorteando com reposição
sorteios_1 <- sample(amostra_1, 
                   size = n_simulacoes, 
                   replace = TRUE)

# Tabela de frequência absoluta
frequencia_abs_1 <- table(sorteios_1)

# Frequência relativa (probabilidade empírica)
frequencia_rel_1 <- prop.table(frequencia_abs_1)

# frequencia_rel_2 ------------------------------------------------------------

amostra_2 <- c(dados$SexoResponsavel)

# Para reprodutibilidade
set.seed(123)

# Sorteando com reposição
sorteios_2 <- sample(amostra_2,
                     size = n_simulacoes, 
                     replace = TRUE)

# Tabela de frequência absoluta
frequencia_abs_2 <- table(sorteios_2)

# Frequência relativa (probabilidade empírica)
frequencia_rel_2 <- prop.table(frequencia_abs_2)

# Respostas -------------------------------------------------------------------
frequencia_rel_1
frequencia_rel_2
P_feminino

