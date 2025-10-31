## Atividade 3
# Questão 1 - Teste do Qui-quadrado e Coeficiente de Contingência
# Criar a tabela de contingência com os dados fornecidos
tabela <- matrix(c(50, 30, 20,   # Matutino
                   40, 35, 25,   # Vespertino
                   30, 20, 50),  # Noturno
                 nrow = 3, byrow = TRUE)

# Nomear as linhas e colunas
colnames(tabela) <- c("Boa", "Regular", "Ruim")
rownames(tabela) <- c("Matutino", "Vespertino", "Noturno")
tabela
# Visualizar a tabela

# a) Teste do Qui-quadrado de independência
# Aplicar o teste do qui-quadrado
resultado <- chisq.test(tabela)
# Ver o resultado
print(resultado)
resultado$statistic   # Estatística qui-quadrado (X²)
resultado$p.value     # Valor-p
resultado$parameter   # Graus de liberdade

# Interpretação:
# Se o valor-p < 0.05, rejeitamos H0 e concluímos que há associação entre turno e percepção.
''
## b) Coeficiente de contingência
# Fórmula: C = sqrt(chi² / (chi² + n))
# Coeficiente de contingência
# Estatística do qui-quadrado e tamanho da amostra
X2 <- 25.43344
n <- 300
# Cálculo do coeficiente de contingência
C <- sqrt(X2 / (X2 + n))
C
# Interpretação:
# Valores próximos de 0 indicam associação fraca; próximos de 1 indicam associação forte.
''
# Questão 2 - Correlação e gráfico de dispersão
# a) Gráfico de dispersão
# Número de ônibus por 1.000 habitantes
onibus <- c(0.8, 1.2, 0.5, 1.0, 1.4, 0.6, 1.3, 0.9, 1.1, 0.7)

# Tempo médio de deslocamento em minutos
tempo <- c(54, 46, 60, 50, 43, 58, 44, 52, 48, 56)
# Gráfico de dispersão
plot(onibus, tempo,
     main = "Ônibus por Habitante x Tempo de Deslocamento",
     xlab = "Ônibus por 1.000 Habitantes",
     ylab = "Tempo Médio de Deslocamento (min)",
     pch = 19, col = "blue")

# Adicionar linha de tendência
abline(lm(tempo ~ onibus), col = "red", lwd = 2)
''
# b) Correlação de Pearson
# Dados fornecidos pela atividade
onibus <- c(0.8, 1.2, 0.5, 1.0, 1.4, 0.6, 1.3, 0.9, 1.1, 0.7)
tempo <- c(54, 46, 60, 50, 43, 58, 44, 52, 48, 56)
# Cálculo da correlação de Pearson
correlacao <- cor(onibus, tempo, method = "pearson")
correlacao
# Interpretação:
# Valor negativo indica que mais ônibus está associado a menor tempo de deslocamento.


