# Uvoz potrebnih knjižnic
library(rgdal)
library(rgeos)
library(dplyr)
library(readr)
library(ggplot2)
library(digest)
library(mosaic)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

# Funkcija uvozi.zemljevid(url, ime.zemljevida, pot.zemljevida="",
#                          mapa="../zemljevidi", encoding=NULL, force=FALSE)
#
# Funkcija najprej preveri, ali zemljevid na podani lokaciji že obstaja. Če
# ne obstaja ali če je parameter force nastavljen na TRUE, pobere arhiv z
# navedenega naslova in ga razširi. Nato uvozi zemljevid in ga vrne.
#
# Parametri:
#   * url             Naslov URL, iz katerega naj dobimo arhiv z zemljevidom.
#   * ime.zemljevida  Ime datoteke SHP brez končnice
#   * pot.zemljevida  Pot do mape z datoteko SHP, kakršna je v pobranem arhivu
#                     (privzeto prazna - vrhnja mapa arhiva).
#   * mapa            Pot do mape, kamor naj se shrani zemljevid (privzeto
#                     mapa "../zemljevid")
#   * encoding        Kodiranje znakov v zemljevidu (privzeta vrednost
#                     NULL, da se pretvorba ne opravi).
#   * force           Ali naj se zemljevid v vsakem primeru pobere z navedenega
#                     naslova (privzeta vrednost FALSE).
#
# Vrača:
#   * zemljevid (SpatialPolygonsDataFrame) iz pobranega arhiva
uvozi.zemljevid <- function(url, ime.zemljevida, pot.zemljevida="",
                            mapa="../zemljevidi", encoding=NULL, force=FALSE) {
  zgostitev <- digest(url, algo="sha1")
  map <- paste0(mapa, "/", zgostitev)
  pot <- paste0(map, "/", pot.zemljevida)
  shp <- paste0(pot, "/", ime.zemljevida, ".shp")
  zip <- paste0(map, "/", zgostitev, ".zip")
  if (force || !file.exists(shp)) {
    if (!file.exists(map)) {
      dir.create(map, recursive=TRUE)
    }
    download.file(url, zip)
    unzip(zip, exdir=map)
  }
  re <- paste0("^", gsub("\\.", "\\.", ime.zemljevida), "\\.")
  files <- grep(paste0(re, "[a-z0-9.]*$"),
                grep(paste0(re, ".*$"), dir(pot), value=TRUE),
                value=TRUE, invert=TRUE)
  file.rename(paste0(map, "/", files),
              paste0(map, "/", sapply(strsplit(files, "\\."),
                                      function(x)
                                        paste(c(x[1:(length(x)-1)], tolower(x[length(x)])),
                                              collapse="."))))
  zemljevid <- readOGR(shp, ime.zemljevida)

  if (!is.null(encoding)) {
    loc <- locale(encoding=encoding)
    for (col in names(zemljevid)) {
      if (is.factor(zemljevid[[col]])) {
        zemljevid[[col]] <- zemljevid[[col]] %>% as.character() %>%
          parse_character(locale=loc) %>% factor()
      } else if (is.character(zemljevid[[col]])) {
        zemljevid[[col]] <- zemljevid[[col]] %>% parse_character(locale=loc)
      }
    }
  }
  
  return(zemljevid)
}
library(tmap)

world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]
ggplot(Europe) +
  geom_sf() +
  coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE) +
  aes(fill = 'pink')

lvls <-(Europe$sovereignt)
primerjava <- data.frame(Country = lvls) %>% left_join(tabela1, by = "Country")
manjkajoci <- primerjava[is.na(primerjava$BDPpc), ]


df = data.frame(drzava = manjkajoci$Country, Country = c("Albania", 
                                                         "Andorra",
                                                         "Bosnia and Herzegovina",
                                                         "Belarus",
                                                         "Czechia",
                                                         "Germany (until 1990 former territory of the FRG)",
                                                         "Kosovo",
                                                         "Liechtenstein",
                                                         "Monaco",
                                                         "Moldova",
                                                         "North Macedonia",
                                                         "Montenegro",
                                                         "Russia",
                                                         "San Marino",
                                                         "Serbia",
                                                         "Ukraine",
                                                         "Vatican"))
tabela1 <- tabela1 %>% left_join(df) %>% mutate(Country=ifelse(is.na(drzava), Country, drzava))
tabela1 <- tabela1 %>% select(-drzava)
n2 <- tabela1 %>% group_by(sovereignt = Country) %>% summarise(mean = mean(Value, na.rm = TRUE))
m <- merge(Europe, n2)
ggplot(m) +
  geom_sf() +
  coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE) +
  aes(fill = mean) +
  scale_fill_gradient(low="yellow", high="magenta")


# Primer uvoza zemljevida (slovenske občine)
# obcine <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
#                           pot.zemljevida="OB", encoding="Windows-1250")
