library(dplyr)
library(tidyr)
library(survival)
library(readr)
library(tidyverse)
library(GGally)

df <- read_csv("~/my-r-project/my_final_dataset.csv")

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

ggcoef_compare(
  list(`Модель 1` = m1, `Модель 2` = m2, `Модель 3` = m3, `Модель 4` = m4), 
  exponentiate = TRUE,
  point_size = 1,
  interaction_sep = " × ",
  variable_labels = c(
    "sanction" = "Санкции",
    "polity2" = "Тип полит. режима (Polity2)",
    "tt(polity2)" = "Тип полит. режима (Polity2) × log(срок инкумбента)",
    "Llog_gdppc" = "log(ВВП на душу населения), t-1",
    "Llog_pop" = "log(численность населения), t-1",
    "Llog_imports_gdp_pc" = "log(открытость экономики), t-1",
    "Llog_oil_gas" = "log(нефтегазовые доходы), t-1",
    "cold_war" = "Холодная война",
    "relfrac" = "Религиозная фракционализация",
    "gmlmidongoing" = "Межгосударственный конфликт",
    "sanction_fin" = "Фин. санкции",
    "sanction_trade" = "Торговые санкции",
    "anocracy" = "Анократия",
    "sanction × anocracy" = "Санкции × Анократия",
    "gwf_personal" = "Персонал. режим",
    "sanction_fin × gwf_personal" = "Фин. санкции × Персонал. режим"
  )
) +
  labs(
    x = "Отношение рисков (HR)"
  )

