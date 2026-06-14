library(readr)
library(dplyr)
library(stringr)
library(survival)
library(ggplot2)

df <- read_csv("~/my-r-project/my_final_dataset.csv")

df2 <- df |>
  filter(demobin == 0) |>
  mutate(
    sanction_type = case_when(
      sanction == 0 ~ "Без санкций",
      sanction_fin == 1 & sanction_trade == 0 ~ "Финансовые без торговых",
      sanction_fin == 0 & sanction_trade == 1 ~ "Торговые без финансовых",
      sanction_fin == 1 & sanction_trade == 1 ~ "Финансовые и торговые санкции",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(sanction_type))

m <- coxph(
  Surv(tstart, tstop, event) ~
    strata(sanction_type) +
    polity2 +
    Llog_gdppc +
    Llog_pop +
    Llog_imports_gdp_pc +
    Llog_oil_gas +
    cold_war +
    relfrac +
    gmlmidongoing +
    cluster(ccode),
  data = df2
)

s <- summary(survfit(m))

pl <- data.frame(
  years = s$time / 365.25,
  surv = s$surv,
  sanction_type = str_remove(s$strata, "sanction_type=")
)

ggplot(
  pl,
  aes(
    x = years,
    y = surv,
    color = sanction_type,
    linetype = sanction_type
  )
) +
  geom_step(linewidth = 0.8) +
  geom_hline(
    yintercept = 0.5,
    color = "#dadada",
    linewidth = 0.5
  ) +
  scale_color_manual(
    values = c(
      "Без санкций" = "black",
      "Финансовые без торговых" = "#9c9e9e",
      "Торговые без финансовых" = "#696b6b",
      "Финансовые и торговые санкции" = "#434645"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Без санкций" = "solid",
      "Финансовые без торговых" = "longdash",
      "Торговые без финансовых" = "dotted",
      "Финансовые и торговые санкции" = "dotdash"
    )
  ) +
  labs(
    x = "Количество лет у власти",
    y = "Вероятность сохранения власти",
    color = "",
    linetype = ""
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )