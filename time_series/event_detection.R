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
path <- "../data/output_ts_atletas/amador"

df_a <- data.frame()
id <- 1

nomes <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)

#loop pra ler as séries as series e adicionando a uma lista
for (arquivo in nomes) {
  serie_temporal <- read.csv(arquivo)
  serie_temporal$id <- id
  
  df_a <- bind_rows(df_a, serie_temporal)
  
  id <- id + 1
}

#loop pra suavizar as séries
ids_unique <- unique(df_a$id)
df_suavizado <- data.frame()

for (id in ids_unique) {
  serie = df_a[df_a$id == id,]$velocity
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
  model <- fit(model, data$media_janela)
  detection <- detect(model, data$media_janela)
  prop <- round(nrow(detection |> dplyr::filter(event==TRUE)) / nrow(data),3) #prop qtd cp em relação a série
  prop_first <- round(head(detection |> dplyr::filter(event==TRUE))$idx / nrow(data),3) #prop first cp em relação a série
  list_cp <- append(list_cp, prop)
  list_cp_first <- append(list_cp_first, prop_first)
}

list_cp_numeric_a <- as.numeric(list_cp)
media_a <- mean(list_cp_numeric_a, na.rm = TRUE)
round(media_a,4)

list_cp_first_numeric_a <- as.numeric(list_cp_first)
media_a_first <- mean(list_cp_first_numeric_a, na.rm = TRUE)
round(media_a_first,4)

############## EXPERIENTE ###################

#diretório
path <- "../data/output_ts_atletas/experiente"

df_e <- data.frame()
id <- 1

nomes <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)

#loop pra ler as séries as series e adicionando a uma lista
for (arquivo in nomes) {
  serie_temporal <- read.csv(arquivo)
  serie_temporal$id <- id
  
  df_e <- bind_rows(df_e, serie_temporal)
  
  id <- id + 1
}

#loop pra suavizar as séries
ids_unique <- unique(df_e$id)
df_suavizado <- data.frame()

for (id in ids_unique) {
  serie = df_e[df_e$id == id,]$velocity
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
  model <- fit(model, data$media_janela)
  detection <- detect(model, data$media_janela)
  prop <- round(nrow(detection |> dplyr::filter(event==TRUE)) / nrow(data),3)
  prop_first <- round(head(detection |> dplyr::filter(event==TRUE))$idx / nrow(data),3) #prop first cp em relação a série
  list_cp <- append(list_cp, prop)
  list_cp_first <- append(list_cp_first, prop_first)
}

list_cp_numeric_e <- as.numeric(list_cp)
media_e <- mean(list_cp_numeric_e, na.rm = TRUE)
round(media_e,4)

list_cp_first_numeric_e <- as.numeric(list_cp_first)
media_e_first <- mean(list_cp_first_numeric_e, na.rm = TRUE)
round(media_e_first,4)

######### TESTES ###########

#Mann-Whitney U

#prop tamanho lista
sample1 <- c(list_cp_numeric_a)
sample2 <- c(list_cp_numeric_e)

mannwhitney_test <- wilcox.test(sample1, sample2)

mannwhitney_test

#prop cp first
sample1 <- c(list_cp_first_numeric_a)
sample2 <- c(list_cp_first_numeric_e)

mannwhitney_test <- wilcox.test(sample1, sample2)

mannwhitney_test
