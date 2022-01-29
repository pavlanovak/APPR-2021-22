# 2. faza: Uvoz podatkov
sl <- locale("sl", decimal_mark=",", grouping_mark=".")
D <- read_csv("podatki/koliko_kupujejo_zadnji3.csv", na=":", col_select = c(-3, -5, -7),
              locale=locale(encoding="Windows-1250"))
BDP <- read_csv("podatki/bdp.csv", na="-", col_select = c(7, 8, 9),
                locale=locale(encoding="Windows-1250")) %>% rename(TIME = OBS_VALUE) %>% rename(GEO = TIME_PERIOD)
tretja <- read_csv("podatki/tretja_stopnja.csv", na="-", col_select = c(1, 6, 7),
                   locale=locale(encoding="Windows-1250")) %>% rename(okrajsava = LOCATION) %>% rename(Year = TIME) %>% rename(Education2 = Value)
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
  Country = c(
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
    "Latvia",
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
tabela1 <- left_join(tabela1, pomozna, by = "Country")
tabela1 <- left_join(tabela1, tretja, by = c("okrajsava", "Year"))
tabela1$Education2 <- round(tabela1$Education2) 
tabela1 <- tabela1 %>% mutate(Education = ifelse(is.na(Education), Education2, Education)) %>% select(-Education2, -okrajsava)


koliko_vsega <- read_csv("podatki/po_izobr_koliko_vsega.csv", na=":", col_select = c(-3, -5, -7),
                              locale=locale(encoding="Windows-1250")) %>% rename(Year = TIME) %>% rename(Country = GEO) %>% rename(Stopnja = IND_TYPE) %>% rename(Koliko_vseh_nakupov_je_opravila_ta_skupina = Value)
kaj_so <- read_csv("podatki/po_izobrazbi_kaj.csv", na=":", col_select = c(-5, -7),
                   locale=locale(encoding="Windows-1250")) %>% pivot_wider(names_from = "INDIC_IS", values_from = "Value") %>% rename(Year = TIME) %>% rename(Country = GEO) %>% rename(Stopnja = IND_TYPE)
tabela2 <- left_join(kaj_so, koliko_vsega, by = c("Country", "Year", "Stopnja"))

tabela3 <- read_csv("podatki/kaj_kupujejo1.csv", na=":", col_select=c(-4,-5,-7), 
                                   locale=locale(encoding="Windows-1250")) %>% pivot_wider(names_from = "INDIC_IS", values_from = "Value") %>% rename(Year = TIME) %>% rename(Country = GEO)


tabela2_c <- janitor::clean_names(tabela2)
tabela3_c <- janitor::clean_names(tabela3)


