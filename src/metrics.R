calculate_correlation <- function(data, target, var, method = "pearson") {
  cor(data[[target]], data[[var]], method = method, use = "complete.obs")
}

calculate_nominal_correlation <- function(data, target, var) {
  # Para variáveis binárias, usar correlação point-biserial
  if(length(unique(data[[var]])) == 2) {
    dummy <- as.numeric(data[[var]] == unique(data[[var]])[1])
    return(cor(data[[target]], dummy))
  }
  
  # Para variáveis categóricas, usar coeficiente Eta
  model <- aov(data[[target]] ~ data[[var]])
  ss <- summary(model)[[1]][,"Sum Sq"]
  sqrt(ss[1] / sum(ss))
}

calculate_correlations <- function(data, target, num_vars, ord_vars, nom_vars) {
  # Calcular correlações para cada tipo de variável
  num_cors <- sapply(num_vars, function(var) calculate_correlation(data, target, var))
  ord_cors <- sapply(ord_vars, function(var) calculate_correlation(data, target, var, "kendall"))
  nom_cors <- sapply(nom_vars, function(var) calculate_nominal_correlation(data, target, var))
  
  # Criar tabela de resultados
  data.frame(
    var_name = c(num_vars, ord_vars, nom_vars),
    method = c(rep("Pearson", length(num_vars)), 
              rep("Kendall", length(ord_vars)),
              ifelse(sapply(nom_vars, function(var) length(unique(data[[var]])) == 2),
                    "Point-Biserial", "Eta")),
    correlation = c(num_cors, ord_cors, nom_cors),
    interval = ifelse(c(rep(TRUE, length(num_vars) + length(ord_vars)),
                       sapply(nom_vars, function(var) length(unique(data[[var]])) == 2)),
                     "[-1, +1]", "[0, +1]")
  )
}