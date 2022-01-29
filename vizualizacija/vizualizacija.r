# 3. faza: Vizualizacija podatkov
#Delež ljudi glede na BDP, ki je v zadnjih 3 mesecih opravilo spletni nakup
ggplot(tabela1) + aes(x = BDPpc, y = Value, color = Education) + geom_point() + xlab("BDP na prebivalca") + ylab("Delež ljudi, ki je v zadnjih 3 mesecih opravil spletni nakup") + ggtitle("Delež ljudi glede na BDP na prebivalca, ki je opravil spletni nakup v zadnjih 3 mesecih")
"Delež ljudi glede na delež ljudi s tretjo st izobrazbe, ki je opravilo spletni nakup v zadnjih 3 mesecih"
ggplot(tabela1) + aes(x = Education, y = Value, color = Area ) + geom_point() + xlab("Delež ljudi s tretjo stopnjo izobrazbe") + ylab("Delež ljudi, ki je v zadnjih 3 mesecih opravil spletni nakup") + ggtitle("Delež ljudi glede na delež ljudi s tretjo stopnjo izobrazbe, ki je opravilo spletni nakup v zadnjih 3 mesecih") + scale_color_manual(values=wes_palette(n=3, name="GrandBudapest2")) 


#tabela2 %>%
#  ggplot(
#    mapping = aes(x = tabela2)
#  ) +
#  geom_histogram() 
#

tabela1 %>%
  ggplot(
    mapping = aes(group= Country, x = Country, y = Value)
  ) +
  geom_boxplot()


n <- tabela1 %>% group_by(Country) %>% summarise(mean = mean(Value, na.rm = TRUE))
v <- tabela1 %>% group_by(Country) %>% summarise(BDPmean = mean(BDPpc))
n$BDP_mean <- v$BDPmean

n %>%
  ggplot(
    mapping = aes(x = Country, fill = mean)
  ) +
 geom_bar() 

ggplot(data = tabela1, aes(x = Year, y = Value, color = Country)) +
  geom_line()

ggplot(data = tabela1, aes(x = Year, y = Value, color = Area)) +
  geom_line() +
  facet_wrap(facets = vars(Country))


ggplot(data = tabela2, mapping = aes(x = Year, y = Koliko_vseh_nakupov_je_opravila_ta_skupina, color = Stopnja)) +
  geom_line() +
  facet_wrap(facets =  vars(Country))

ggplot(data=n, aes(x=Country, y=mean, fill = BDP_mean)) +
  geom_bar(stat="identity") + coord_flip() + scale_fill_gradient(low="pink", high="magenta")

tabela3[,3] <- as.integer(as.character(unlist(tabela3[,3])))
tabela3[,4] <- as.integer(unlist(tabela3[,4]))
tabela3[,5] <- as.integer(unlist(tabela3[,5]))
tabela3[,6] <- as.integer(unlist(tabela3[,6]))
tabela3[,7] <- as.integer(unlist(tabela3[,7]))

ttt <- tabela3 %>% dplyr::filter(year == 2019, country == 'Slovenia') %>% pivot_longer(c(3, 4, 5, 6, 7), names_to = 'type', values_to =  "value")
ggplot(data = ttt, mapping = aes(x = country, y = value, fill = type)) +
  geom_bar(width = 1, stat = 'identity') + coord_polar("y", start=0) 

tabela2[,4] <- as.integer(unlist(tabela2[,4]))
tabela2[,5] <- as.integer(unlist(tabela2[,5]))
fff1 <- tabela2 %>% dplyr::filter(year == 2019, country == 'United Kingdom', stopnja == 'Individuals with high formal education') %>% pivot_longer(c(4, 5), names_to = 'type', values_to = 'value')
ggplot(data = fff1, mapping = aes(x = country, y = value, fill = type)) +
  geom_bar(width = 1, stat = 'identity') + coord_polar("y", start=0) 


fff2 <- tabela2 %>% dplyr::filter(year == 2019, country == 'United Kingdom', stopnja == 'Individuals with no or low formal education') %>% pivot_longer(c(4, 5), names_to = 'type', values_to = 'value')
ggplot(data = fff2, mapping = aes(x = country, y = value, fill = type)) +
  geom_bar(width = 1, stat = 'identity') + coord_polar("y", start=0) 









