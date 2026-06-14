library(dplyr)
library(tidyr)
library(survival)
library(ggplot2)
library(survminer)
library(readr)
library(tidyverse)

df <- read_csv("~/my-r-project/my_final_dataset.csv")
# Для H1
m1zph <- coxph(
  Surv(tstart, tstop, event) ~ sanction
    + polity2
    + Llog_gdppc 
    + Llog_pop 
    + Llog_imports_gdp_pc
    + Llog_oil_gas 
    + cold_war 
    + relfrac 
    + gmlmidongoing
    + cluster(ccode),
  data = df |> filter(demobin == 0)
)
summary(m1zph)
cox.zph(m1zph)
ggcoxzph(cox.zph(m1zph), point.size = 0.4, ggtheme = theme_bw(),font.main = 8, font.x = 8, font.y = 8)


# Для H2
m2zph <- coxph(
  Surv(tstart, tstop, event) ~ sanction_fin 
    + sanction_trade
    + polity2 
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
summary(m2zph)
cox.zph(m2zph)
ggcoxzph(cox.zph(m2zph), point.size = 0.4, ggtheme = theme_bw(),font.main = 8, font.x = 8, font.y = 8)


# Для H3
m3zph <- coxph(
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
summary(m3zph)
cox.zph(m3zph)
ggcoxzph(cox.zph(m3zph), point.size = 0.4, ggtheme = theme_bw(),font.main = 8, font.x = 8, font.y = 8)


#Для H4
m4zph <- coxph(
  Surv(tstart, tstop, event) ~ sanction_fin * gwf_personal
    + polity2 
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
summary(m4zph)
cox.zph(m4zph)
ggcoxzph(cox.zph(m4zph), point.size = 0.4, ggtheme = theme_bw(),font.main = 8, font.x = 8, font.y = 8)