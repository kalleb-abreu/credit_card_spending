source("src/utils.R")
source("src/metrics.R")
source("src/plots.R")

required_packages <- c("ggplot2", "corrplot", "car", "MASS", "lmtest", "olsrr")
install_if_missing(required_packages)

# Leitura dos dados
dados <- read.csv("data/cleaned_dataset.csv")

# Definição das variáveis
variaveis_numericas <- c("idade", "transacoes_mensais")
variaveis_ordinais <- c("mes", "ano")
variaveis_nominais <- c("id", "cidade", "estado", "sexo")
variavel_resposta <- "gasto_mensal"

# Tema para gráficos
theme_paper <- theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# Análise de correlação
correlations <- calculate_correlations(dados, variavel_resposta, 
                                     variaveis_numericas, 
                                     variaveis_ordinais,
                                     c("sexo", "estado", "cidade"))
print(correlations, digits = 3)

# Gráficos de dispersão
# plots <- create_scatter_plots(dados, variaveis_numericas, variavel_resposta, theme_paper)
# for(p in plots) print(p)


# Step by step for MLR

# Fit full model
full_model <- lm(gasto_mensal ~ ., data = dados)

# Perform backward elimination
backward_model <- step(full_model, direction = "backward")

# Perform forward selection
null_model <- lm(gasto_mensal ~ 1, data = dados)  # Null model
forward_model <- step(null_model, scope = formula(full_model), direction = "forward")

# Compare models
summary(backward_model)
summary(forward_model)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(backward_model)

# Check multicollinearity
library(car)
vif(backward_model)

# Make predictions
new_data <- data.frame(idade = c(25, 30), transacoes_mensais = c(10, 15))
predict(backward_model, newdata = new_data)
