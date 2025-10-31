# Questão 1 - Probabilidade e distribuição binomial

# Carrega os dados
dados <- read.csv("dados_politica_publica.csv")

# a) Estima a probabilidade de sucesso (Tem Internet = "Sim")
p_sucesso <- mean(dados$Tem.Internet == "Sim")
print(paste("Probabilidade de sucesso (Tem Internet = 'Sim'):", round(p_sucesso, 3)))

# b) Calcula P(X < 10) e P(12 < X ≤ 15) para n = 20, usando distribuição binomial
n <- 20
# P(X < 10)
p_menor_10 <- pbinom(9, size = n, prob = p_sucesso)
# P(12 < X ≤ 15) = P(X ≤ 15) - P(X ≤ 12)
p_intervalo <- pbinom(15, size = n, prob = p_sucesso) - pbinom(12, size = n, prob = p_sucesso)

print(paste("P(X < 10):", round(p_menor_10, 4)))
print(paste("P(12 < X ≤ 15):", round(p_intervalo, 4)))

# c) Esperança e variância da binomial
esperanca <- n * p_sucesso
variancia <- n * p_sucesso * (1 - p_sucesso)

print(paste("Esperança E[X]:", round(esperanca, 2)))
print(paste("Variância V[X]:", round(variancia, 2)))

# Questão 2 - Probabilidades na normal padrão

# a) P(0 ≤ Z ≤ 1.73)
p_a <- pnorm(1.73) - pnorm(0)

# b) P(Z ≤ -1.73) e P(Z ≥ 1.73)
p_b1 <- pnorm(-1.73)
p_b2 <- 1 - pnorm(1.73)

# c) P(0.47 ≤ Z ≤ 1.73)
p_c <- pnorm(1.73) - pnorm(0.47)

print(paste("a) P(0 ≤ Z ≤ 1.73):", round(p_a, 4)))
print(paste("b) P(Z ≤ -1.73):", round(p_b1, 4)))
print(paste("b) P(Z ≥ 1.73):", round(p_b2, 4)))
print(paste("c) P(0.47 ≤ Z ≤ 1.73):", round(p_c, 4)))

# Questão 3 - Estimativa e probabilidades com variável normal

# a) Estima média (μ̂) e desvio padrão (σ̂)
media_renda <- mean(dados$Renda.Antes, na.rm = TRUE)
desvio_renda <- sd(dados$Renda.Antes, na.rm = TRUE)

print(paste("Média estimada (μ̂):", round(media_renda, 2)))
print(paste("Desvio padrão estimado (σ̂):", round(desvio_renda, 2)))

# b) P(X ≤ 1000)
p_x_menor_1000 <- pnorm(1000, mean = media_renda, sd = desvio_renda)

# c) P(X ≥ 3000)
p_x_maior_3000 <- 1 - pnorm(3000, mean = media_renda, sd = desvio_renda)

print(paste("P(X ≤ 1000):", round(p_x_menor_1000, 4)))
print(paste("P(X ≥ 3000):", round(p_x_maior_3000, 4)))
