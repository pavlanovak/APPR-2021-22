library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(tmap)
library(shiny)
library(readr)
library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
library(wesanderson)
library(cluster)
library(ggalt)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(mapproj)
library(janitor)
library(gridExtra)
options(gsubfn.engine="R")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")
