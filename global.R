library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(scales)
library(dplR)

path <- "/Users/sascha/Nextcloud/17_solar_dashbord/solar_europe_de_nuts.csv"
path_slpc <- "/Users/sascha/Nextcloud/17_solar_dashbord/slpc_c.csv"

solar_europe_de_nuts <- read_delim(file = path,delim = ",")
slpc <- read_delim(file = path_slpc,delim = ",")

