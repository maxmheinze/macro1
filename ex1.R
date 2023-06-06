
# Header ------------------------------------------------------------------

pacman::p_load(
  tidyverse,
  readxl,
  stargazer
)



# Read in Data ------------------------------------------------------------

pwt <- read_xlsx("pwt1001.xlsx", sheet = "Data")

unconcon <- pwt %>%
  select(countrycode, year, rgdpe, pop) %>%
  filter(year %in% c(1960,1990)) %>%
  group_by(countrycode) %>%
  filter(sum(is.na(rgdpe)) < 1) %>%
  mutate(rgdpe = rgdpe / pop) %>%
  select(-pop) %>%
  mutate(ygrowth = (last(rgdpe) / first(rgdpe))^(1/30) - 1) %>%
  filter(year == 1960) %>%
  select(-year)

unconcon_reg <- lm(ygrowth ~ rgdpe, data = unconcon)

summary(unconcon_reg)  
