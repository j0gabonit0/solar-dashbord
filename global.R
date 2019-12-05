library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(scales)

path <- "/Users/sascha/hackyhack/solar/solar_europe_de.csv"
path_wiki <- "/Users/sascha/hackyhack/solar/wiki_nuts_tidy.csv"
path_slpc <- "/Users/sascha/hackyhack/solar/slpc.csv"

####################################
#3.Schritt - Überflüssige Daten entfernen Windspeed/

solar_europe_de <- read_delim(file = path, delim = ",") %>% # Rohdaten eingelesen
  select(-windspeed_10m) %>% # Windspeed entfernen
  mutate(global_radiation = radiation_direct_horizontal + radiation_diffuse_horizontal,
         solar_watt=global_radiation * ( 0.68 * ((-0.583 * temperature + 115)/100))) %>%
  select(-radiation_direct_horizontal,-radiation_diffuse_horizontal)

####################################
#3.1 Wikipedia NUTS Daten hinzufügen

wiki_nuts <- read_delim(file = path_wiki,delim = ",")
solar_europe_de_nuts <- inner_join(solar_europe_de, wiki_nuts, by = c("country" = "NUTS2"))

#slpc aus https://swbt-netz.de/
slpc <- read_delim(file = path_slpc, delim = ";", skip = 1) %>%
  mutate(date = Datum %>% as.Date("%d.%m.%Y") %>% as.character() %>% substr(6,10) %>% paste0("2019-", .) %>% as.Date()) %>%
  group_by(date) %>%
  mutate(watt = gsub(",", ".", `Wirkleistung [kW]`) %>% as.numeric()) %>%
  summarize(standardlast = sum(watt) * 1000)
