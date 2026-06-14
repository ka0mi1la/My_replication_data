library(dplyr)
library(tidyr)
library(countrycode)
library(lubridate)
library(readr)
library(peacesciencer)

load("~/Downloads/my_data.RData")

country_name <- c(
  "31" = "Bahamas",
  "80" = "Belize",
  "110" = "Guyana",
  "115" = "Suriname",
  "260" = "German Federal Republic",
  "265" = "German Democratic Republic",
  "347" = "Kosovo",
  "511" = "Zanzibar",
  "520" = "Somalia",
  "531" = "Eritrea",
  "626" = "South Sudan",
  "678" = "Yemen Arab Republic",
  "680" = "Yemen People's Republic",
  "760" = "Bhutan",
  "781" = "Maldives",
  "817" = "Republic of Vietnam",
  "835" = "Brunei",
  "860" = "East Timor",
  "910" = "Papua New Guinea",
  "940" = "Solomon Islands",
  "950" = "Fiji"
)

mad_vars <- mad |>
  transmute(
    ccode = countrycode(
      countrycode,
      origin = "iso3c",
      destination = "cown",
      custom_match = c("CSK" = 315, "SRB" = 342, "YUG" = 345)
    ),
    year,
    country,
    gdppc,
    pop,
    log_gdppc = log(gdppc),
    log_pop = log(pop * 1000)
  )

oil_vars <- oil |>
  transmute(
    ccode = countrycode(
      iso3numeric,
      origin = "iso3n",
      destination = "cown",
      custom_match = c(
        "200" = 315,
        "278" = 265,
        "280" = 260,
        "688" = 342,
        "714" = 817,
        "720" = 680,
        "886" = 678
      )
    ),
    year,
    log_oil_gas = log(oil_gas_valuePOP_2000 + 1)
  )

sanction_vars <- gsdb2 |>
  mutate(
    ccode = countrycode(
      sanctioned_state_iso3,
      origin = "iso3c",
      destination = "cown",
      custom_match = c(
        "KSV" = 347,
        "SRB" = 342,
        "CSK" = 315,
        "DDR" = 265,
        "SVU" = 365,
        "VDR" = 816,
        "YUG" = 345,
        "RHO" = 552
      )
    )
  ) |>
  filter(!is.na(ccode)) |>
  group_by(ccode, year) |>
  summarise(
    sanction = 1L,
    sanction_trade = as.integer(any(trade == 1, na.rm = TRUE)),
    sanction_fin = as.integer(any(financial == 1, na.rm = TRUE)),
    .groups = "drop"
  )

archigos_vars <- ar |>
  transmute(
    obsid,
    spell_start = eindate,
    spell_end = eoutdate,
    exit,
    fail = as.integer(exit %in% c("Irregular", "Foreign"))
  ) |>
  filter(
    !is.na(spell_start),
    !is.na(spell_end),
    spell_end >= spell_start
  )

my_vars <- create_leaderyears(
  standardize = "cow",
  subset_years = 1960:2014
) |>
  left_join(
    polity_data |> select(polity_annual_ccode, year, polity2),
    by = c("ccode" = "polity_annual_ccode", "year")
  ) |>
  add_gml_mids() |>
  left_join(
    gwf |>
      select(gwf_cowcode, year, gwf_party, gwf_military, gwf_personal),
    by = c("ccode" = "gwf_cowcode", "year")
  ) |>
  add_creg_fractionalization() |>
  add_cow_trade() |>
  mutate(
    cold_war = as.integer(year <= 1991)
  ) |>
  left_join(mad_vars, by = c("ccode", "year")) |>
  mutate(
    country = coalesce(
      na_if(country, ""),
      recode(as.character(ccode), !!!country_name, .default = NA_character_)
    ),
    imports_gdp = (imports * 1000000) / (gdppc * pop * 1000),
    log_imports_gdp_pc = log((imports_gdp * 100) + 1)
  ) |>
  left_join(sanction_vars, by = c("ccode", "year")) |>
  mutate(
    across(
      c(sanction, sanction_trade, sanction_fin),
      ~ replace_na(., 0L)
    )
  ) |>
  left_join(oil_vars, by = c("ccode", "year")) |>
  left_join(archigos_vars, by = "obsid") |>
  distinct(obsid, year, .keep_all = TRUE)

dataset <- my_vars |>
  filter(!is.na(spell_start), !is.na(spell_end)) |>
  mutate(
    int_start = pmax(make_date(year, 1, 1), spell_start),
    int_end = pmin(make_date(year, 12, 31), spell_end),
    tstart = as.integer(int_start - spell_start),
    tstop = as.integer(int_end - spell_start),
    event = as.integer(year == year(spell_end) & fail == 1),
    event2 = as.integer(year == year(spell_end) & exit != "Still in Office"),
    demobin = as.integer(polity2 >= 6),
    anocracy = as.integer(polity2 >= -5 & polity2 <= 5),
    autocracy = as.integer(polity2 <= -6),
    regime3 = case_when(
      !is.na(polity2) & polity2 >= 6 ~ "Democracy",
      !is.na(polity2) & polity2 <= -6 ~ "Autocracy",
      !is.na(polity2) ~ "Anocracy",
      TRUE ~ NA_character_
)
  ) |>
  filter(tstop > tstart) |>
  arrange(obsid, year) |>
  group_by(obsid) |>
  mutate(
    Llog_gdppc = lag(log_gdppc, 1),
    Llog_pop = lag(log_pop, 1),
    Llog_imports_gdp_pc = lag(log_imports_gdp_pc, 1),
    Llog_oil_gas = lag(log_oil_gas, 1)
  ) |>
  ungroup() |>
  mutate(spell_id = dense_rank(obsid)) |>
  arrange(spell_id, year) |>
  group_by(spell_id) |>
  mutate(prev_tstop = lag(tstop)) |>
  filter(is.na(prev_tstop) | tstop > prev_tstop) |>
  ungroup() |>
  select(
    year,
    country,
    ccode,
    obsid,
    leader,
    tstart,
    tstop,
    event,
    event2,
    sanction,
    sanction_fin,
    sanction_trade,
    polity2,
    demobin,
    anocracy,
    autocracy,
    regime3,
    gwf_personal,
    gwf_military,
    gwf_party,
    Llog_gdppc,
    Llog_pop,
    Llog_imports_gdp_pc,
    Llog_oil_gas,
    cold_war,
    relfrac,
    gmlmidongoing
  )

write_csv(dataset, "~/my-r-project/my_final_dataset.csv")
