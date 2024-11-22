if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
} else {
  library(dplyr)
}

df <- read.csv("data/dataset.csv", sep = ";", dec = ",")

unwanted_cols <- c("limite_total", "limite_disp", "grupo_estabelecimento", 
                  "cidade_estabelecimento", "pais_estabelecimento", 
                  "safra_abertura")
df <- df[!names(df) %in% unwanted_cols]

unique_ids <- unique(df$id)
id_mapping <- data.frame(
  new_id = sprintf("%04d", 1:length(unique_ids)),
  original_id = unique_ids
)

df$id <- id_mapping$new_id[match(df$id, id_mapping$original_id)]
df$safra_abertura <- as.character(df$safra_abertura)
df$cidade <- as.character(df$cidade)
df$estado <- as.factor(df$estado)
df$idade <- as.integer(df$idade)
df$sexo <- as.factor(df$sexo)
df$data <- as.Date(df$data, format = "%d.%m.%Y")
df$valor <- as.numeric(gsub(",", ".", df$valor))

df$mes <- format(df$data, "%m")
df$ano <- format(df$data, "%Y")
df$data <- NULL

df_grouped <- df %>%
  group_by(id, mes, ano) %>%
  summarise(
    safra_abertura = names(sort(table(safra_abertura), decreasing = TRUE))[1],
    cidade = names(sort(table(cidade), decreasing = TRUE))[1],
    estado = names(sort(table(estado), decreasing = TRUE))[1],
    idade = as.integer(names(sort(table(idade), decreasing = TRUE))[1]),
    sexo = names(sort(table(sexo), decreasing = TRUE))[1],
    gasto_mensal = sum(valor, na.rm = TRUE),
    transacoes_mensais = sum(!is.na(valor)),
    .groups = 'drop'
  ) %>%
  arrange(id, ano, mes)

write.csv(df_grouped, "data/cleaned_dataset.csv", row.names = FALSE)