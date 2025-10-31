# Questão 1 - Respostas teóricas

# a) Hipótese nula (H0): afirma que não há efeito ou diferença entre grupos.
#    Hipótese alternativa (H1): afirma que há efeito ou diferença.

# b) Erro Tipo I (α): rejeitar H0 quando ela é verdadeira.
#    Erro Tipo II (β): não rejeitar H0 quando ela é falsa.

# c) Valor-p: probabilidade de obter um resultado tão extremo quanto o observado, assumindo H0 verdadeira.
#    Se valor-p < nível de significância, rejeitamos H0.

# d) Nível de significância: probabilidade máxima de cometer erro Tipo I. Exemplo: 5% (α = 0.05).

# e) Estatística do teste: valor calculado a partir dos dados que resume a evidência contra H0.

# Questão 2 - Teste de hipótese para uma amostra

# Carrega os dados
dados <- read.csv("dados_politica_publica.csv")

# Seleciona as 100 primeiras linhas
amostra <- head(dados, 100)

# a) Média amostral
media_amostral <- mean(amostra$RendaMensal, na.rm = TRUE)
print(paste("Média amostral:", round(media_amostral, 2)))

# b, c, d) Teste t para uma amostra
teste <- t.test(x = amostra$RendaMensal, mu = 1500, alternative = "two.sided", conf.level = 0.95)

print(teste)

# e) Teste de normalidade
normalidade <- shapiro.test(amostra$RendaMensal)
print(normalidade)

# Questão 3 - Teste t pareado entre RendaMensal e RendaAntes

# a, b, c) Teste t pareado
teste_pareado <- t.test(x = amostra$RendaMensal, y = amostra$RendaAntes,
                        paired = TRUE, alternative = "two.sided", conf.level = 0.95)

print(teste_pareado)

# d) Teste de normalidade da diferença
diferenca <- amostra$RendaMensal - amostra$RendaAntes
normalidade_dif <- shapiro.test(diferenca)
print(normalidade_dif)
# Questão 4 - Teste Qui-quadrado entre TemInternet e EvasaoEscolar

# Cria tabela de contingência
tabela <- table(amostra$TemInternet, amostra$EvasaoEscolar)

# Executa o teste
teste_chi <- chisq.test(tabela)

print(teste_chi)

# Estatística do teste: teste_chi$statistic
# Valor-p: teste_chi$p.value
# Graus de liberdade: teste_chi$parameter

