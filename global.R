## global.R ##
library(dplyr)
library(ggplot2)
library(ggthemes)
library(googleVis)
library(scales)
library(RColorBrewer)
library(plotly)
library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(devtools)
library(DT)
library(tidyr)


# Arquivo # ------------------------------------------------------------------------------------------------------------------------------
reembolsos = read.csv("./data/deputados_agrup.csv")
# ----------------------------------------------------------------------------------------------------------------------------------------

# listas ---------------------------------------------------------------------------------------------------------------------------------
br_states = unique(reembolsos$state_code) #ESTADOS
year = unique(reembolsos$year) #ANOS
category = unique(reembolsos$receipt_description)

reembolsos$political_party = gsub("S.PART.", "SEM PARTIDO", reembolsos$political_party, fixed = T) %>%
  gsub("SEM PARTIDO ASSOCIADO", "SEM PARTIDO", ., fixed = T)
parties = unique(reembolsos$political_party)[order(unique(reembolsos$political_party))] #PARTIDO

# ----------------------------------------------------------------------------------------------------------------------------------------

# gasto por deputado ---------------------------------------------------------------------------------------------------------------------
gastos_por_deputado = group_by(reembolsos, year, deputy_name, political_party) %>%
  summarise(., deputados_gpa = sum(value_receipts))
# ----------------------------------------------------------------------------------------------------------------------------------------

#agrupamento por partido ----------------------------------------------------------------------------------------------------------------
gastos_por_partido = group_by(reembolsos, year, political_party) %>%
  summarise(., partido_mgpd = sum(value_receipts)/n_distinct(deputy_name), n_deputados = n_distinct(deputy_name))

max_por_partido = group_by(gastos_por_deputado, year, political_party) %>%
  summarise(., partido_maxgpd = max(deputados_gpa))

partidos = left_join(gastos_por_partido, max_por_partido, by = c("year", "political_party"))
# ----------------------------------------------------------------------------------------------------------------------------------------

#agrupamento por estado ------------------------------------------------------------------------------------------------------------------
estados = filter(reembolsos, state_code != "Vazio") %>%
  group_by(., year, state_code) %>%
  summarise(., estado_mgpd = sum(value_receipts) / n_distinct(deputy_name), n_deputados_estado = n_distinct(deputy_name)) %>%
  mutate(., codUF = paste0("BR-", state_code))
# ----------------------------------------------------------------------------------------------------------------------------------------


# deputado # -----------------------------------------------------------------------------------------------------------------------------
max_receipt = group_by(reembolsos, year, receipt_description) %>%
  summarise(max_receipt = max(max_receipt))

max_receipt = left_join(max_receipt, reembolsos, by = c("year", "receipt_description", "max_receipt"))

max_receipt = select(max_receipt, Year = year, Category = receipt_description, "Top Receipt [R$]" = max_receipt, Name = deputy_name, Party = political_party, 
         State = state_code)

# ----------------------------------------------------------------------------------------------------------------------------------------