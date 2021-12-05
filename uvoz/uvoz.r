# 2. faza: Uvoz podatkov
sl <- locale("sl", decimal_mark=",", grouping_mark=".")
D <- read_csv("koliko_kupujejo_zadnji3.csv", na=":", col_select = c(-3, -5, -7),
              locale=locale(encoding="Windows-1250"))
BDP <- read_csv("bdp.csv", na="-", col_select = c(7, 8, 9),
                locale=locale(encoding="Windows-1250")) %>% rename(TIME = OBS_VALUE) %>% rename(GEO = TIME_PERIOD)
tretja <- read_csv("tretja_stopnja.csv", na="-", col_select = c(1, 6, 7),
                   locale=locale(encoding="Windows-1250")) %>% rename(Country = LOCATION) %>% rename(Year = TIME)
skupaj <- left_join(D, BDP, by= c("TIME", "GEO")) %>% rename(BDP = OBS_FLAG) %>% mutate(BDP=parse_number(BDP))
link <- "https://en.wikipedia.org/wiki/List_of_countries_by_tertiary_education_attainment"
stran <- html_session(link) %>% read_html()
tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
  .[[1]] %>% html_table(dec=",")

tabela <- tabela[-1,c(1,2,7)] %>% rename(TIME = Year) %>% rename(GEO = Country)

tabela$TIME <- as.numeric(tabela$TIME)

tabela1 <- left_join(skupaj, tabela, by= c("TIME", "GEO"))
colnames(tabela1) <- c("Year", "Country", "Area", "Value", "BDPpc", "Education")

pomozna <- tibble(
  drzava = c(
    "Austria",
    "Belgium",
    "Switzerland",
    "Czechia",
    "Germany (until 1990 former territory of the FRG)",
    "Denmark",
    "Spain",
    "Estonia",
    "Finland",
    "France",
    "United Kingdom",
    "Greece",
    "Hungary",
    "Ireland",
    "Iceland",
    "Italy",
    "Lithuania",
    "Luxembourg",
    "latvia",
    "Netherlands",
    "Norway",
    "Poland",
    "Portugal",
    "Slovakia",
    "Slovenia",
    "Sweden",
    "Turkey"
  ),
  okrajsava = c(
    "AUT",
    "BEL",
    "CHE",
    "CZE",
    "DEU",
    "DNK",
    "ESP",
    "EST",
    "FIN",
    "FRA",
    "GBR",
    "GRC",
    "HUN",
    "IRL",
    "ISL",
    "ITA",
    "LTU",
    "LUX",
    "LVA",
    "NLD",
    "NOR",
    "POL",
    "PRT",
    "SVK",
    "SVN",
    "SWE",
    "TUR"
  )
)
