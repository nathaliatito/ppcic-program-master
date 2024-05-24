library("daltoolbox") 
library("harbinger")
library(readr)
library(ggplot2)
library(zoo)
library(xts)
library(forecast)
library(dplyr)

############## AMADOR ###################

#diretório
path <- "../data/output_ts_atletas/historico/hist_saullo.csv"

df <- read.csv(path)

amostra <- 20

#separando dfs
df_inicio <- subset(df, id <= amostra)
df_fim <- subset(df, id >= (max(df$id) - amostra))

############ INÍCIO ##############

#loop pra suavizar as séries
ids_unique <- unique(df_inicio$id)
df_suavizado <- data.frame()

for (id in ids_unique) {
  serie = df_inicio[df_inicio$id == id,]$velocity
  media_suavizada <- rollapply(serie, width = 30, FUN = mean, by = 30, align = "right", fill = NA, na.rm = TRUE)
  media_suavizada <- media_suavizada[is.finite(media_suavizada)] #removendo possíveis inf
  df_temp <- data.frame(indice = 1:length(media_suavizada), media_janela = media_suavizada)
  df_temp <- na.omit(df_temp)
  df_temp$id <- id
  
  df_suavizado <- bind_rows(df_suavizado, df_temp)
}

#change points
list_cp <- list()
list_cp_first <- list()
model <- hcp_cf_ets()
for (id in ids_unique) {
  data <- df_suavizado[df_suavizado$id == id,]
  if (nrow(data) > 10) { ##modelo não roda pra séries pequenas
    model <- fit(model, data$media_janela)
    detection <- detect(model, data$media_janela)
    prop <- round(nrow(detection |> dplyr::filter(event==TRUE)) / nrow(data),3) #prop qtd cp em relação a série
    prop_first <- round(head(detection |> dplyr::filter(event==TRUE))$idx / nrow(data),3) #prop first cp em relação a série
      list_cp <- append(list_cp, prop)
    list_cp_first <- append(list_cp_first, prop_first)
  }
}

list_cp_numeric_a <- as.numeric(list_cp)
media_a <- mean(list_cp_numeric_a, na.rm = TRUE)
round(media_a,4)

list_cp_first_numeric_a <- as.numeric(list_cp_first)
media_a_first <- mean(list_cp_first_numeric_a, na.rm = TRUE)
round(media_a_first,4)

############## FIM ###################

#loop pra suavizar as séries
ids_unique <- unique(df_fim$id)
df_suavizado <- data.frame()

for (id in ids_unique) {
  serie = df_fim[df_fim$id == id,]$velocity
  media_suavizada <- rollapply(serie, width = 30, FUN = mean, by = 30, align = "right", fill = NA, na.rm = TRUE)
  media_suavizada <- media_suavizada[is.finite(media_suavizada)] #removendo possíveis inf
  df_temp <- data.frame(indice = 1:length(media_suavizada), media_janela = media_suavizada)
  df_temp <- na.omit(df_temp)
  df_temp$id <- id
  
  df_suavizado <- bind_rows(df_suavizado, df_temp)
}

#change points
list_cp <- list()
list_cp_first <- list()
model <- hcp_cf_ets()
for (id in ids_unique) {
  data <- df_suavizado[df_suavizado$id == id,]
  if (nrow(data) > 10) { ##modelo não roda pra séries pequenas
    model <- fit(model, data$media_janela)
    detection <- detect(model, data$media_janela)
    prop <- round(nrow(detection |> dplyr::filter(event==TRUE)) / nrow(data),3)
    prop_first <- round(head(detection |> dplyr::filter(event==TRUE))$idx / nrow(data),3) #prop first cp em relação a série
    list_cp <- append(list_cp, prop)
    list_cp_first <- append(list_cp_first, prop_first)
  }
}

list_cp_numeric_e <- as.numeric(list_cp)
media_e <- mean(list_cp_numeric_e, na.rm = TRUE)
round(media_e,4)

list_cp_first_numeric_e <- as.numeric(list_cp_first)
media_e_first <- mean(list_cp_first_numeric_e, na.rm = TRUE)
round(media_e_first,4)

############## FULL ###################

#loop pra suavizar as séries
ids_unique <- unique(df$id)
df_suavizado <- data.frame()

for (id in ids_unique) {
  serie = df[df$id == id,]$velocity
  media_suavizada <- rollapply(serie, width = 30, FUN = mean, by = 30, align = "right", fill = NA, na.rm = TRUE)
  media_suavizada <- media_suavizada[is.finite(media_suavizada)] #removendo possíveis inf
  df_temp <- data.frame(indice = 1:length(media_suavizada), media_janela = media_suavizada)
  df_temp <- na.omit(df_temp)
  df_temp$id <- id
  
  df_suavizado <- bind_rows(df_suavizado, df_temp)
}

#change points
list_cp <- list()
list_cp_first <- list()
model <- hcp_cf_ets()

for (id in ids_unique) {
  data <- df_suavizado[df_suavizado$id == id,]
  if (nrow(data) > 10) { ##modelo não roda pra séries pequenas
    model <- fit(model, data$media_janela)
    detection <- detect(model, data$media_janela)
    prop <- round(nrow(detection |> dplyr::filter(event==TRUE)) / nrow(data),3)
    list_cp <- append(list_cp, prop)
  }
}

list_cp_numeric_f <- as.numeric(list_cp)
media_f <- mean(list_cp_numeric_f, na.rm = TRUE)
round(media_f,2)

#plot do avanço das props dos pontos de mundança
ggplot(data.frame(x = seq_along(list_cp_numeric_f), y = list_cp_numeric_f), aes(x, y)) +
  geom_line() +
  labs(x = "treinos", y = "pontos de mudança (%)", title = "média de pontos de mudança por treino")