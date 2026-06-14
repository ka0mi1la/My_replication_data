library(dplyr)
library(tidyr)
library(survival)
library(readr)

df <- read_csv("~/my-r-project/my_final_dataset.csv")

# M1 robust
m1_rob <- coxph(
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
  data = df,
  tt = function(x, t, ...) x * log(t + 1)
)
summary(m1_rob)
car::vif(m1_rob)
logLik(m1_rob)


# M2 robust
m2_rob <- coxph(Surv(tstart, tstop, event) ~
  + sanction_fin
  + gwf_personal
  + gwf_military
  + gwf_party
  + polity2
  + tt(polity2) 
  + Llog_gdppc 
  + Llog_pop 
  + Llog_imports_gdp_pc
  + Llog_oil_gas 
  + relfrac
  + gmlmidongoing 
  + cluster(ccode),
  data = df |> 
    filter(demobin == 0),
  tt = function(x, t, ...) x * log(t + 1))
summary(m2_rob)
car::vif(m2_rob)
logLik(m2_rob)


# M3 robust
m3_rob <- coxph(
  Surv(tstart, tstop, event2) ~ sanction 
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
  data = df,
  tt = function(x, t, ...) x * log(t + 1)
)
summary(m3_rob) 
car::vif(m3_rob)
logLik(m3_rob)