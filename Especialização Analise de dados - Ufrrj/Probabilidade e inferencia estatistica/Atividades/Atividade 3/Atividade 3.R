# Questões 1 a 5 - Respostas teóricas

# 1. População: conjunto total de elementos que possuem uma característica em comum.
#    Amostra: subconjunto da população usado para fazer inferências sobre ela.

# 2. Usamos amostras porque estudar toda a população é muitas vezes inviável por tempo, custo ou acesso.

# 3. Amostra probabilística: cada elemento tem chance conhecida de ser selecionado.
#    Amostra não probabilística: seleção não aleatória, sem garantia de representatividade.

# 4. Viés de amostragem: erro sistemático causado por seleção inadequada.
#    Exemplo: entrevistar apenas usuários de internet para avaliar acesso digital.

# 5. Erro amostral: diferença entre estatística da amostra e parâmetro da população.
#    Pode ser minimizado com amostras maiores e aleatórias.

# Questão 6 - Análise de amostras

# População
populacao <- c(18, 19, 21, 22, 28, 34, 43, 47, 49, 50, 51, 53, 60, 63, 66)

# a) Média populacional
media_pop <- mean(populacao)
print(paste("Média populacional:", media_pop))

# b) Médias amostrais
amostras <- list(
  A = c(18, 21, 28),
  B = c(47, 49, 50),
  C = c(51, 60, 66),
  D = c(21, 47, 60)
)

medias_amostrais <- sapply(amostras, mean)
print("Médias amostrais:")
print(medias_amostrais)

# c) Erros amostrais
erros <- media_pop - medias_amostrais
print("Erros amostrais:")
print(erros)

# d) Viés de seleção: amostras C e B concentram valores altos, podem ter viés.
# e) Amostra representativa: amostra D tem valores distribuídos, mais próxima da média populacional.
# Questões 7 a 11 - Respostas teóricas

# 7. Distribuição amostral da média: distribuição das médias de várias amostras da mesma população.
#    Serve para estimar a variabilidade da média e construir intervalos de confiança.

# 8. Quanto maior o tamanho da amostra, menor o desvio padrão da distribuição amostral da média.

# 9. Pelo Teorema Central do Limite, a distribuição da média amostral tende a ser normal com n ≥ 30.

# 10. Estimador pontual: valor único que estima um parâmetro populacional.
#     Exemplos: média amostral (μ̂), proporção amostral (p̂), variância amostral (σ̂²).

# 11. Intervalo de confiança de 90%: intervalo que tem 90% de chance de conter o verdadeiro parâmetro populacional.

# Questão 12 - Intervalo de confiança para RendaMensal

# Carrega os dados
dados <- read.csv("dados_politica_publica.csv")

# Remove valores ausentes
renda <- na.omit(dados$RendaMensal)

# Estatísticas básicas
media <- mean(renda)
desvio <- sd(renda)
n <- length(renda)

# Intervalo de confiança de 95%
erro_padrao <- desvio / sqrt(n)
z <- qnorm(0.975)  # valor crítico para 95%
limite_inferior <- media - z * erro_padrao
limite_superior <- media + z * erro_padrao

print(paste("Intervalo de confiança 95% para RendaMensal: [",
            round(limite_inferior, 2), ",", round(limite_superior, 2), "]"))

