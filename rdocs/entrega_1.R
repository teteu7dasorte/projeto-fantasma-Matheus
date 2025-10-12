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

##Análise 1
#carregando padrões da ESTAT:

#Carregando os dados: 

vendas <- read_excel("relatorio_old_town_road.xlsx" , sheet =  "relatorio_vendas")
lojas <- read_excel("relatorio_old_town_road.xlsx" , sheet = "infos_lojas")
produtos <-read_excel("relatorio_old_town_road.xlsx" , sheet = "infos_produtos")
funcionarios <- read_excel("relatorio_old_town_road.xlsx" , sheet = "infos_funcionarios")
cidades <- read_excel("relatorio_old_town_road.xlsx" , sheet = "infos_cidades")
clientes <- read_excel("relatorio_old_town_road.xlsx" , sheet = "infos_clientes")
vendas_info <- read_excel("relatorio_old_town_road.xlsx", sheet = "infos_vendas")


produtos <- produtos %>% rename(ItemID = Ite3ID)
lojas <- lojas %>% rename(StoreID = Stor3ID)
vendas_info <- vendas_info %>% rename(SaleID = Sal3ID)

cotacao <- 5.31
dados_completos <- vendas %>%
  left_join(vendas_info, by = "SaleID") %>%
  left_join(produtos, by = "ItemID") %>%
  left_join(lojas, by = "StoreID")%>%
  mutate(
    Date = as.Date(Date),
    Ano = as.integer(year(Date)),
    Receita = Quantity * UnityPrice / cotacao
  )
  

# Calcular Receita em reais e Agrupar:


Receita <- dados_completos %>%
  group_by(Ano) %>%
  summarise(
    receita_total = sum(Receita, na.rm = TRUE),
    receita_media = mean(Receita, na.rm = TRUE),
    n(),
    .groups = 'drop'
  )
print(Receita)


# Gráfico:

 g1 <- ggplot(Receita,aes(x = Ano, y = receita_media)) +
   geom_line(size = 1, colour = "#A11D21") +
   geom_point(size = 2, colour = "#A11D21")+
   scale_x_continuous(limits = c(1880,1889),
                      breaks = 1880:1889)+
   labs(
     title = "Receita Média das Lojas (1880-1889)",
     x= "Ano",
     y= "Receita Média(R$)"
     ) +
   theme_estat()
ggsave("receita_media_anual.png", plot = g1 , width = 8, height = 6)
print(g1)
