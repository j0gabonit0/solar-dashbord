# install package tidyverse
library(tidyverse)
library(ggplot2)


path <- "/Users/sascha/hackyhack/solar/data/opsd-weather_data-2019-04-09_WerteTemperatur-Einstrahlung/weather_data.csv"

# Fehler in der Ursprungsdatei


#2 Filtering hart nicht richtig funktioniert, ich habe es überall manuell eingefügt

raw_opsd <- read_delim(file = path,delim = ",") # Rohdaten eingelesen
solar_europe <-raw_opsd # Rohdaten in neue Tabelle geschrieben
solar_europe_de <- select(solar_europe,utc_timestamp, starts_with("DE")) %>%
  filter(utc_timestamp >= "2005-12-31 24:00:00") %>%
  distinct(utc_timestamp, .keep_all = TRUE)

solar_europe_tidy <- solar_europe_de %>% 
  pivot_longer(-utc_timestamp, names_to = "country", values_to = "wm2") # Rohdaten umgeändert in drei Spalten
solar_europe_tidy2 <- solar_europe_tidy %>% 
  separate(country, into = c("country","metric"),
           sep = "_", extra = "merge")
  
  solar_europe_tidy3 <- pivot_wider(solar_europe_tidy2, names_from = metric, values_from = wm2)

############
#1.Schritt - Rohdaten in eine neue Tabelle schreiben


raw_opsd <- read_delim(file = path,delim = ",") # Rohdaten 
solar_europe <-raw_opsd # Ursprungstabelle

########################
# 2. Schritt - Umwandeln der Rohdaten in tidydata

solar_europe_de <- solar_europe %>%
  select(utc_timestamp, starts_with("DE")) %>%
  filter(utc_timestamp >= "1989-12-31 24:00:00") %>%
  distinct(utc_timestamp, .keep_all = TRUE) %>%
  pivot_longer(-utc_timestamp, names_to = "country", values_to = "wm2") %>% # Rohdaten umgeändert in drei Spalten
  separate(country, into = c("country","metric"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = metric, values_from = wm2)

write_csv(solar_europe_de,"/Users/sascha/hackyhack/solar/solar_europe_de.csv")  

####################################
#3.Schritt - Überflüssige Daten entfernen Windspeed/

path2 <- "/Users/sascha/hackyhack/solar/solar_europe_de.csv"
solar_europe_de <- read_delim(file = path2,delim = ",") %>% # Rohdaten eingelesen
  select(-windspeed_10m) %>% # Windspeed entfernen
  mutate(global_radiation = radiation_direct_horizontal + radiation_diffuse_horizontal,
         solar_watt  = global_radiation * (0.68 * ((-0.583 * temperature + 115)/100))) %>%
  select(-radiation_direct_horizontal,-radiation_diffuse_horizontal)
####################################
#3.1 Wikipedia NUTS Daten hinzufügen

path_wiki <- "/Users/sascha/hackyhack/solar/wiki_nuts_tidy.csv"
wiki_nuts <- read_delim(file = path_wiki,delim = ",")
solar_europe_de_nuts <- inner_join(solar_europe_de, wiki_nuts, by = c("country" = "NUTS2"))

################################################
#4.Schritt - Durchschnittliche Sonneneinstrahlung vor Ort m2

durchschnitt <- function(startdate, enddate, selected_country) {
  solar_europe_de_nuts %>%
  filter(country == selected_country) %>%
  filter(utc_timestamp >= startdate, utc_timestamp <= enddate) %>%
  summarize(
    rad = mean(global_radiation, na.rm = TRUE),
    max = max(global_radiation),
    min = min(global_radiation[which(global_radiation > 0)]),
    summe = sum()
      )
}
durchschnitt("2014-01-01 06:00:00","2015-01-01 24:00:00","DE11")

############################################################
#5.Schritt - Erzeugte kwH / m2 und erforderliche m2 für Jahresverbrauch

m2 <- function(kwhy, efficency = 0.2, startyear, endyear, selected_country){
  years <- endyear - startyear + 1
  solar_europe_de_nuts %>%
    filter(country == selected_country) %>%
    filter(utc_timestamp >= paste0(startyear, "-01-01 00:00:00"), utc_timestamp <= paste0(endyear, "-12-31 24:00:00")) %>%
    summarise(
      yieldm2 = sum(solar_watt) / years * efficency / 1000, # Die kWh die pro Jahr vom Modul pro m2 erzeugt werden können
      m = kwhy/yieldm2 # die benötigte Fläche für die angegebenen kwH im jahr
           )
}

m2(kwhy = 100,startyear = 2005, endyear = 2015, selected_country = "DE60") # 

 ########################################################################
#6.Schritt - Erzeugte Jahresertrag bei angegebener Dachfläche

kwhyield <-function(startyear,endyear,efficency,selected_country,m2,price){
  years <- endyear - startyear + 1
  solar_europe_de_nuts %>%
    filter(country == selected_country) %>%
    filter(utc_timestamp >= paste0(startyear, "-01-01 00:00:00"), utc_timestamp <= paste0(endyear, "-12-31 24:00:00")) %>%
    summarise(
      yield = sum(solar_watt) / years * efficency * m2 / 1000,
    )    
}
  
kwhyield(startyear = 2009, endyear = 2016,efficency = 0.2, selected_country = "DE60", m2 = 384) # 

###########################################################################################
#7.Schritt ggplot

solar_europe_de_nuts %>%
  filter(country == "DE11") %>%
  filter(utc_timestamp >= startdate, utc_timestamp <= enddate)
  mutate(day = utc_timestamp %>% as.character() %>% substr(6,10)) %>%
  group_by(day) %>%
  summarize(avg = mean(solar_watt, na.rm = TRUE), std = sd(solar_watt, na.rm = TRUE) / sqrt(n())) %>%
  mutate(date = as.Date(paste0("2019-", day))) %>%
  ggplot() + 
    aes(x = date, y = avg) +
    geom_ribbon(aes(ymin = avg - 1.96 * std, ymax = avg + 1.96 * std), fill = "grey70") +
    geom_line()

#####




  
