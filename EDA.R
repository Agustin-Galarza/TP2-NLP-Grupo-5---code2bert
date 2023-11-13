library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(tm)
library(tidytext)

train <- readLines("C:/Users/bruno/OneDrive/Carrera/Segundo Cuatrimestre 2023/PLN/train")
train <- data.frame(funciones = lineas, stringsAsFactors = FALSE)
View(train)

extraer_primera_palabra <- function(texto) {
palabras <- strsplit(as.character(texto), " ")[[1]]
primera_palabra <- palabras[1]
return(primera_palabra)
}


train$longitud = nchar(train$funciones)
train$conteo_return = conteo_public <- str_count(train$funciones, "return")
train$primera_palabra <- sapply(train$funciones, extraer_primera_palabra)
train$segunda_palabra = sapply(strsplit(train$funciones, " "), function(x) if(length(x) >= 2) x[2] else NA)
train$tercera_palabra = sapply(strsplit(train$funciones, " "), function(x) if(length(x) >= 3) x[3] else NA)

#Gráficos
train %>% ggplot() + geom_histogram(aes(x=longitud), bins = 15, fill = "skyblue") + labs(title = "Cantidad de caracteres por función")

total_returns = train %>% group_by(conteo_return) %>% summarise(cantidad_total=n())
total_returns %>% ggplot() + geom_bar(aes(x=conteo_return, y=cantidad_total), stat = "identity") + labs(title = "Cantidad de 'return' por función", x = "Total 'return' por función", y = "Frecuencia")

tipos_de_datos = train %>% group_by(segunda_palabra) %>% summarise(cantidad=n()) %>% arrange(desc(cantidad)) %>% head(10)
tipos_de_datos = tipos_de_datos %>% rename("Tipo de dato" = segunda_palabra)
tipos_de_datos %>%  ggplot(aes(x = `Tipo de dato`, y = cantidad, fill = `Tipo de dato`)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Tipo de dato que devuelve la función", x = "Tipo de dato", y = "Cantidad") +
  theme_minimal()

primera_palabra = train %>% group_by(primera_palabra) %>% summarise(cantidad=n()) %>% arrange(desc(cantidad)) %>% head(5)
ggplot(primera_palabra, aes(x = primera_palabra, y = cantidad, fill = primera_palabra)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Frecuencia de la Primera Palabra", x = "Primera Palabra", y = "Cantidad") +
  theme_minimal()


train$cantidad_espacios <- rowSums(sapply(train, function(x) str_count(x, " ")))
espacios = train %>% group_by(cantidad_espacios) %>% summarise(cantidad=n()) %>% arrange(desc(cantidad)) %>% head(10)
espacios %>% ggplot() + geom_bar(aes(x=cantidad_espacios, y=cantidad), stat = "identity")

ggplot(espacios, aes(x = cantidad_espacios, y = cantidad)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Cantidad de espacios por función",
       x = "Cantidad de espacios",
       y = "Frecencia") +
  theme_minimal()

train %>%
  filter(longitud < 2000) %>%
  ggplot(aes(x = longitud, y = cantidad_espacios)) +
  geom_point(color = "dodgerblue", alpha = 0.7, size = 3) +
  labs(
    title = "Relación entre Longitud y Cantidad de Espacios",
    x = "Longitud de la Función",
    y = "Cantidad de Espacios"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )



#Resultados
new_results <- read_csv("new_results.csv")
new_results = new_results %>% select(prediction_weighted_score, c2v_prediction_weighted_score)
View(new_results)

ggplot(new_results, aes(x = prediction_weighted_score, y = c2v_prediction_weighted_score)) +
  geom_point(color = "dodgerblue", alpha = 0.7, size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +  # Línea y=x
  labs(
    title = "Relación entre prediction_weighted_score y c2v_prediction_weighted_score",
    x = "prediction_weighted_score",
    y = "c2v_prediction_weighted_score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )

new_results_a = new_results %>% filter(prediction_weighted_score>c2v_prediction_weighted_score)
new_results_a$diferencia = new_results_a$prediction_weighted_score - new_results_a$c2v_prediction_weighted_score
new_results_a$diferencia %>% mean()

new_results %>% arrange(desc(prediction_weighted_score-c2v_prediction_weighted_score)) %>% View()
new_results %>% filter(new_results$...1==1378)%>% View()


new_results_b = new_results %>% filter(prediction_weighted_score<c2v_prediction_weighted_score)
new_results_b$diferencia = new_results_b$c2v_prediction_weighted_score - new_results_b$prediction_weighted_score
new_results_b$diferencia %>% mean()




ggplot(new_results, aes(x = prediction_weighted_score, y = c2v_prediction_weighted_score)) +
  geom_point(color = "dodgerblue", alpha = 0.7, size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_point(data = subset(new_results, c2v_prediction_weighted_score < prediction_weighted_score),
             aes(x = prediction_weighted_score, y = c2v_prediction_weighted_score),
             color = "darkgreen", size = 3, alpha = 0.7) +
  labs(
    title = "Relación entre prediction_weighted_score y c2v_prediction_weighted_score",
    x = "Prediction Weighted Score",
    y = "C2V Prediction Weighted Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "none",
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )


library(ggplot2)

ggplot(new_results, aes(x = prediction_weighted_score, y = c2v_prediction_weighted_score)) +
  geom_point(color = "dodgerblue", alpha = 0.7, size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_point(data = subset(new_results, c2v_prediction_weighted_score < prediction_weighted_score),
             aes(x = prediction_weighted_score, y = c2v_prediction_weighted_score),
             color = "darkgreen", size = 3, alpha = 0.7) +
  geom_point(data = subset(new_results, c2v_prediction_weighted_score > prediction_weighted_score),
             aes(x = prediction_weighted_score, y = c2v_prediction_weighted_score),
             color = "red", size = 3, alpha = 0.7) +
  labs(
    title = "Relación entre prediction_weighted_score y c2v_prediction_weighted_score",
    x = "Prediction Weighted Score",
    y = "C2V Prediction Weighted Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "none",
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )


###########################################################################

new_results_2 = read_csv("new_results.csv")
View(new_results_2)
new_results_2$conteo_return = str_count(new_results_2$method_str, "return")
new_results_2= new_results_2 %>% mutate(tiene_return = case_when(
  conteo_return == 0 ~ "No tiene return",
  T ~ "Tiene return(s)"
))


new_results_2 %>%
  filter(tiene_return == "No tiene return") %>%
  ggplot(aes(x = "", y = prediction_weighted_score)) +
  geom_boxplot(fill = "skyblue", color = "dodgerblue", alpha = 0.8, width = 0.5) +
  labs(
    title = "Distribución de prediction_weighted_score para observaciones sin return",
    x = NULL,  # Elimina la etiqueta del eje x
    y = "prediction_weighted_score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks.x = element_blank()  # Elimina las marcas del eje x
  )


new_results_2 %>%
  filter(tiene_return == "Tiene return(s)") %>%
  ggplot(aes(x = "", y = prediction_weighted_score)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen", alpha = 0.8, width = 0.5) +
  labs(
    title = "Distribución de prediction_weighted_score para observaciones con return",
    x = NULL,  # Elimina la etiqueta del eje x
    y = "prediction_weighted_score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks.x = element_blank()  # Elimina las marcas del eje x
  )


new_results_2$tipo_de_dato <- str_split(new_results_2$method_str, "\\s+") %>% sapply(function(x) if(length(x) > 1) x[2] else NA)
new_results_2 %>% group_by(tipo_de_dato) %>% summarise(cantidad=n()) %>% arrange(desc(cantidad)) %>% head(5)
new_results_3 = new_results_2 %>% filter(tipo_de_dato %in% c("void","static","int","boolean","float"))

ggplot(new_results_3, aes(x = tipo_de_dato, y = prediction_weighted_score)) +
  geom_boxplot(fill = "lightblue", color = "blue", alpha = 0.8, width = 0.5) +
  labs(
    title = "Boxplots de prediction_weighted_score para cada tipo_de_dato",
    x = "Tipo de Dato",
    y = "prediction_weighted_score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  facet_wrap(~tipo_de_dato, scales = "free_x")
