source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #



library(readxl)
library(ggplot2)
library(dplyr)
library(broom)

clientes <- read_excel("relatorio_old_town_road.xlsx", sheet = "infos_clientes")


clientes_convertidos <- clientes %>%
  rename(ClientID = Cli3ntID) %>%
  mutate(
    Height_cm = Height_dm *10,
    Weight_kg = Weight_lbs * 0.45359237
  ) %>%
  select(ClientID, Height_cm, Weight_kg)
head(clientes_convertidos)


## Correlação de Pearson:

teste_pearson <- cor.test(clientes_convertidos$Weight_kg, clientes_convertidos$Height_cm, method = "pearson")
print(teste_pearson)

modelo_regressão <- lm(Height_cm ~ Weight_kg, data = clientes_convertidos)
summary(modelo_regressão)

## Gráfico:


g2 <- ggplot(clientes_convertidos, aes(x = Weight_kg, y = Height_cm)) +
  geom_point(alpha = 0.6, color = "#004F9F") +
  geom_smooth(method = "lm", color = "red", se = FALSE, linewidth = 1) +
  scale_x_continuous(labels =  scales::label_number(decimal.mark = ",", big.mark = ".")) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +
  labs(
    title = "Figura 2: Relação de Peso e Altura dos Clientes",
    x = "Peso(kg)",
    y = "Altura(cm)"
  )+
  theme_estat()

ggsave("peso_x_altura.png", plot = g2, width = 8, height = 6)

print(g2)
  

