# 3. faza: Vizualizacija podatkov
#Delež ljudi glede na BDP, ki je v zadnjih 3 mesecih opravilo spletni nakup
graf1 <- ggplot(tabela1) + aes(x = BDPpc, y = Value, color = Education) + geom_point() + xlab("BDP na prebivalca") + ylab("Delež ljudi, ki je v zadnjih 3 mesecih opravil spletni nakup") + ggtitle("Delež ljudi glede na BDP na prebivalca, ki je opravil spletni nakup v zadnjih 3 mesecih")
"Delež ljudi glede na delež ljudi s tretjo st izobrazbe, ki je opravilo spletni nakup v zadnjih 3 mesecih"
graf2 <- ggplot(tabela1) + aes(x = Education, y = Value, color = Area ) + geom_point() + xlab("Delež ljudi s tretjo stopnjo izobrazbe") + ylab("Delež ljudi, ki je v zadnjih 3 mesecih opravil spletni nakup") + ggtitle("Delež ljudi glede na delež ljudi s tretjo stopnjo izobrazbe, ki je opravilo spletni nakup v zadnjih 3 mesecih") + scale_color_discrete(name="Predel", labels=c("Živeči v mestih", "Živeči na ruralnih območjih"))


#tabela2 %>%
#  ggplot(
#    mapping = aes(x = tabela2)
#  ) +
#  geom_histogram() 
#

graf3 <- tabela1 %>%
  ggplot(
    mapping = aes(group= Country, x = Country, y = Value)
  ) +
  geom_boxplot() + 
  xlab("Država") + ylab("Kolikšen delež ljudi je v zadnjih 3 mesecih kupil nekaj preko spleta")


n <- tabela1 %>% dplyr::group_by(Country) %>% dplyr::summarise(mean = mean(Value, na.rm = TRUE))
v <- tabela1 %>% dplyr::group_by(Country) %>% dplyr::summarise(BDPmean = mean(BDPpc, na.rm = TRUE))
n$BDP_mean <- v$BDPmean


graf4 <- ggplot(data = tabela1, aes(x = Year, y = Value, color = Country)) +
  geom_line() + xlab("Leto") + ylab("Delež ljudi, ki je v zadnjih 3 mesecih kupil nekaj prek spleta") + ggtitle("Spreminjanje odstotka ljudi po državah, ki so nakupovali prek spleta, čez leta") + labs(color = "Država")

graf5 <- ggplot(data = tabela1, aes(x = Year, y = Value, color = Area)) +
  geom_line() +
  facet_wrap(facets = vars(Country)) + xlab('Leto') + ylab("Delež ljudi, ki je v zadnjih 3 mesecih kupil nekaj prek spleta") + ggtitle("Spreminjanje odstotka ljudi po državah, ki so nakupovali prek spleta, čez leta") + scale_color_discrete(name="Predel", labels=c("Živeči v mestih", "Živeči na ruralnih območjih"))


graf6 <- ggplot(data = tabela2, mapping = aes(x = Year, y = Koliko_vseh_nakupov_je_opravila_ta_skupina, color = Stopnja)) +
  geom_line() +
  facet_wrap(facets =  vars(Country)) + xlab('Leto') + ylab("Kolikšen delež vseh nakupov je opravila določena skupin") + ggtitle("Spreminjanje odstotka ljudi po državah in doseženi izobrazbi, ki so nakupovali prek spleta, čez leta") + scale_color_discrete(name="Dosežena stopnja izobrazbe", labels=c("Posamezniki z visoko formalno izobrazbo", "Posamezniki s srednjo formalno izobrazbo", "Posamezniki brez ali z nizko izobrazbo", "Študenti"))


graf7 <- ggplot(data=n, aes(x=Country, y=mean, fill = BDP_mean)) +
  geom_bar(stat="identity") + coord_flip() + scale_fill_gradient(low="pink", high="magenta")+ xlab('povprečen odstotek ljudi, ki nakupuje prek spleta v tej državi') + ylab('Država') + labs(fill = "Povprečni BDP per capita") + ggtitle("Odstotek ljudi, ki nakupuje prek spleta glede na BDP")


spr <- tabela1
spr["Education"] <- as.integer(unlist(spr["Education"]))
izobrazba <- spr %>% dplyr::group_by(Country) %>% dplyr::summarise(Edmean = mean(Education, na.rm = TRUE))


n$Edmean <- izobrazba$Edmean
graf8 <- ggplot(data=n, aes(x=Country, y=mean, fill = Edmean)) +
  geom_bar(stat="identity") + coord_flip() + scale_fill_gradient(low="yellow", high="red")+ xlab('povprečen odstotek ljudi, ki nakupuje prek spleta v tej državi') + ylab('Država') + labs(fill = "Povprečen delež ljudi s tretjo stopnjo izobrazbe") + ggtitle("Odstotek ljudi, ki nakupuje prek spleta glede na odstotek ljudi z doseženo tretjo stopnjo izobrazbe")



tabela3_c[,3] <- as.integer(as.character(unlist(tabela3_c[,3])))
tabela3_c[,4] <- as.integer(unlist(tabela3_c[,4]))
tabela3_c[,5] <- as.integer(unlist(tabela3_c[,5]))
tabela3_c[,6] <- as.integer(unlist(tabela3_c[,6]))
tabela3_c[,7] <- as.integer(unlist(tabela3_c[,7]))

ttt <- tabela3_c %>% dplyr::filter(year == 2019, country == 'Slovenia') %>% pivot_longer(c(3, 4, 5, 6, 7), names_to = 'type', values_to =  "value")
graf9 <- ggplot(data = ttt, mapping = aes(x = country, y = value, fill = type)) +
  geom_bar(width = 1, stat = 'identity') + coord_polar("y", start=0) + xlab('') + ylab('') + ggtitle("Kaj kupujejo ljudje v posamezni državi?") + scale_fill_discrete(name = "Vrsta kupljenega izdelka", labels = c("Knjige, revije, učenje,...", "Obleke, šport", "Filmi, glasba", "Gospodinjski pripomočki", "Potovanja, hoteli,..."))

tabela2_c[,4] <- as.integer(unlist(tabela2_c[,4]))
tabela2_c[,5] <- as.integer(unlist(tabela2_c[,5]))
fff1 <- tabela2_c %>% dplyr::filter(year == 2019, country == 'United Kingdom', stopnja == 'Individuals with high formal education') %>% pivot_longer(c(4, 5), names_to = 'type', values_to = 'value')
graf10 <- ggplot(data = fff1, mapping = aes(x = country, y = value, fill = type)) +
  geom_bar(width = 1, stat = 'identity') + coord_polar("y", start=0) + ggtitle("Kaj kupujejo ljudje z doseženo visoko izobrazbo v posamezni državi?") + xlab('') + ylab('') + scale_fill_discrete(name = 'Vrsta kupljenega izdelka', labels = c('Knjige, revije, učenje,...', "Obleke, šport"))


fff2 <- tabela2_c %>% dplyr::filter(year == 2019, country == 'United Kingdom', stopnja == 'Individuals with no or low formal education') %>% pivot_longer(c(4, 5), names_to = 'type', values_to = 'value')
graf11 <- ggplot(data = fff2, mapping = aes(x = country, y = value, fill = type)) +
  geom_bar(width = 1, stat = 'identity') + coord_polar("y", start=0) + ggtitle("Kaj kupujejo ljudje brez izobrazbe ali nizko izobrazbo v posamezni državi?") + xlab('') + ylab('') + scale_fill_discrete(name = 'Vrsta kupljenega izdelka', labels = c('Knjige, revije, učenje,...', "Obleke, šport"))





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
tabela1 <- tabela1 %>% left_join(df) %>% dplyr::mutate(Country=ifelse(is.na(drzava), Country, drzava))
tabela1 <- tabela1 %>% dplyr::select(-drzava)
n2 <- tabela1 %>% dplyr::group_by(sovereignt = Country) %>% dplyr::summarise(mean = mean(Value, na.rm = TRUE))
m <- merge(Europe, n2)
ggplot(m) +
  geom_sf() +
  coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE) +
  aes(fill = mean) +
  scale_fill_gradient(low="yellow", high="magenta") + ggtitle('Katera država v povprečju največ spletno nakupuje?') + labs(fill = "Odstotek ljudi,\nki je v zadnjih 3 mesecih\nspletno nakupoval\n(povprečno)")




