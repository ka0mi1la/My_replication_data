library(dplyr)
library(tidyr)
library(survival)
library(ggplot2)
library(survminer)
library(readr)
library(tidyverse)
library(knitr)
library(kableExtra)

df <- read_csv("~/my-r-project/my_final_dataset.csv")

#Описательные статистики
summary(
  df |> 
    select(
      event, event2, sanction, polity2,
      Llog_gdppc, Llog_pop, Llog_imports_gdp_pc, Llog_oil_gas, cold_war,
      relfrac, gmlmidongoing, demobin, sanction_trade,
      sanction_fin, gwf_military, gwf_party, gwf_personal
    )
)

# Корреляция у типов санкций
df |>
  select(
    sanction_trade, sanction_fin
  ) |>
  cor(use = "complete.obs") |>
  round(2)

# Для H1
m1 <- coxph(
  Surv(tstart, tstop, event) ~ sanction
    + polity2
    + tt(polity2)
    + Llog_gdppc 
    + Llog_pop 
    + Llog_imports_gdp_pc
    + Llog_oil_gas 
    + cold_war 
    + relfrac 
    + gmlmidongoing
    + cluster(ccode),
  data = df |> filter(demobin == 0),
  tt = function(x, t, ...) x * log(t + 1)
)
summary(m1)
car::vif(m1)
logLik(m1)


# Для H2
m2 <- coxph(
  Surv(tstart, tstop, event) ~ sanction_fin 
    + sanction_trade
    + polity2 
    + tt(polity2)
    + Llog_gdppc 
    + Llog_pop 
    + Llog_imports_gdp_pc
    + Llog_oil_gas 
    + cold_war 
    + relfrac 
    + gmlmidongoing
    + cluster(ccode),
  data = df |> filter(demobin == 0),
  tt = function(x, t, ...) x * log(t + 1)
)
summary(m2)
logLik(m2)
car::vif(m2)

#Тест Вальда
car::linearHypothesis(
  m2,
  "sanction_fin = sanction_trade"
)


# Для H3
m3 <- coxph(
  Surv(tstart, tstop, event) ~ sanction * anocracy
    + Llog_gdppc 
    + Llog_pop 
    + Llog_imports_gdp_pc
    + Llog_oil_gas 
    + cold_war 
    + gmlmidongoing 
    + relfrac
    + cluster(ccode),
  data = df |> filter(demobin == 0)
)
summary(m3)
car::vif(m3)
logLik(m3)

#Для H4
m4 <- coxph(
  Surv(tstart, tstop, event) ~ sanction_fin * gwf_personal
    + polity2 
    + tt(polity2)
    + Llog_gdppc 
    + Llog_pop 
    + Llog_imports_gdp_pc
    + Llog_oil_gas 
    + cold_war
    + gmlmidongoing
    + cluster(ccode),
  data = df |> filter(demobin == 0),
  tt = function(x, t, ...) x * log(t + 1)
)
summary(m4)
car::vif(m4)
logLik(m4)