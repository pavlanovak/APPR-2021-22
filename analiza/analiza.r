# 4. faza: Napredna analiza podatkov

obrisi = function(podatki, hc = TRUE, od = 2, do = NULL) {
  n = nrow(podatki)
  if (is.null(do)) {
    do = n - 1
  }
  
  razdalje = dist(podatki)
  
  k.obrisi = tibble()
  for (k in od:do) {
    if (hc) {
      o.k = hclust(razdalje) %>%
        cutree(k) %>%
        silhouette(razdalje)
    } else {
      set.seed(42) # zato, da so rezultati ponovljivi
      o.k = kmeans(podatki, k)$cluster %>%
        silhouette(razdalje)
    }
    k.obrisi = k.obrisi %>% bind_rows(
      tibble(
        k = rep(k, n),
        obrisi = o.k[, "sil_width"]
      )
    )
  }
  k.obrisi$k = as.ordered(k.obrisi$k)
  
  k.obrisi
}

obrisi.povprecje = function(k.obrisi) {
  k.obrisi.povprecje = k.obrisi %>%
    group_by(k) %>%
    summarize(obrisi = mean(obrisi))
}

obrisi.k = function(k.obrisi) {
  obrisi.povprecje(k.obrisi) %>%
    filter(obrisi == max(obrisi)) %>%
    summarize(k = min(k)) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

diagram.obrisi = function(k.obrisi) {
  ggplot() +
    geom_boxplot(
      data = k.obrisi,
      mapping = aes(x = k, y = obrisi)
    ) +
    geom_point(
      data = obrisi.povprecje(k.obrisi),
      mapping = aes(x = k, y = obrisi),
      color = "red"
    ) +
    geom_line(
      data = obrisi.povprecje(k.obrisi),
      mapping = aes(x = as.integer(k), y = obrisi),
      color = "red"
    ) +
    geom_point(
      data = obrisi.povprecje(k.obrisi) %>%
        filter(obrisi == max(obrisi)) %>%
        filter(k == min(k)),
      mapping = aes(x = k, y = obrisi),
      color = "blue"
    ) +
    xlab("število skupin (k)") +
    ylab("obrisi (povprečje obrisov)") +
    ggtitle(paste("Maksimalno povprečje obrisov pri k =", obrisi.k(k.obrisi))) +
    theme_classic()
}

nakupi = tabela2 %>% dplyr::select(
  Year, Country, Stopnja, Koliko_vseh_nakupov_je_opravila_ta_skupina
) %>%
  filter(
    Year == 2019
  ) %>%
  pivot_wider(
    names_from = Stopnja,
    values_from = Koliko_vseh_nakupov_je_opravila_ta_skupina
  ) %>%
  dplyr::select(-Year)
nakupi[5, 1] = 'Germany'
nakupi[38, 1] = 'Kosovo'

#dendrogram, hierarhično razvrščanje v skupine
drzave = nakupi[, 1] %>% unlist()
razdalje = nakupi[, -1] %>% dist()
dendrogram = razdalje %>% hclust(method = "ward.D")
plot(
  dendrogram,
  labels = drzave,
  ylab = "višina",
  main = NULL
)


hc.kolena = function(dendrogram, od = 1, do = NULL, eps = 0.5) {
  # število primerov in nastavitev parametra do
  n = length(dendrogram$height) + 1
  if (is.null(do)) {
    do = n - 1
  }
  # k.visina je tabela s štirimi stolpci
  # (1) k, število skupin
  # (2) višina združevanja
  # (3) sprememba višine pri združevanju
  # (4) koleno: ali je točka koleno?
  k.visina = tibble(
    k = as.ordered(od:do),
    visina = dendrogram$height[do:od]
  ) %>%
    # sprememba višine
    mutate(
      dvisina = visina - lag(visina)
    ) %>%
    # ali se je intenziteta spremembe dovolj spremenila?
    mutate(
      koleno = lead(dvisina) - dvisina > eps
    )
  k.visina
}

# iz tabele k.visina vrne seznam vrednosti k,
# pri katerih opazujemo koleno
hc.kolena.k = function(k.visina) {
  k.visina %>%
    filter(koleno) %>%
    dplyr::select(k) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

# izračunamo tabelo s koleni za dendrogram
r = hc.kolena(dendrogram)

# narišemo diagram višin združevanja
diagram.kolena = function(k.visina) {
  k.visina %>% ggplot() +
    geom_point(
      mapping = aes(x = k, y = visina),
      color = "red"
    ) +
    geom_line(
      mapping = aes(x = as.integer(k), y = visina),
      color = "red"
    ) +
    geom_point(
      data = k.visina %>% filter(koleno),
      mapping = aes(x = k, y = visina),
      color = "blue", size = 2
    ) +
    ggtitle(paste("Kolena:", paste(hc.kolena.k(k.visina), collapse = ", "))) +
    xlab("število skupin (k)") +
    ylab("razdalja pri združevanju skupin") +
    theme_classic()
}
diagram.kolena(r)

diagram.skupine = function(podatki, oznake, skupine, k) {
  podatki = podatki %>%
    bind_cols(skupine) %>%
    rename(skupina = ...4)
  
  d = podatki %>%
    ggplot(
      mapping = aes(
        x = x, y = y, color = skupina
      )
    ) +
    geom_point() +
    geom_label(label = oznake, size = 2) +
    scale_color_hue() +
    theme_classic()
  
  for (i in 1:k) {
    d = d + geom_encircle(
      data = podatki %>%
        filter(skupina == i)
    )
  }
  d
}


#k-ti voditelji

b <- transform(nakupi, No_or_low= as.numeric(unlist(nakupi[,2])), 
          Med = as.numeric(unlist(nakupi[,3])),
          High = as.numeric(unlist(nakupi[,4])),
          Students = as.numeric(unlist(nakupi[,5])))%>% dplyr::select(Students, No_or_low, Med, High)
skupine = b[, -1] %>%
  kmeans(centers = 3) %>%
  getElement("cluster") %>%
  as.ordered()

print(skupine)
r.hc = nakupi[, -1] %>% obrisi(hc = TRUE)
r.km = nakupi[, -1] %>% obrisi(hc = FALSE)

diagram.obrisi(r.hc)
diagram.obrisi(r.km)

#Optimalno število skupin je torej 2 ali 4.

drzave.x.y =
  as_tibble(razdalje %>% cmdscale(k = 2)) %>%
  bind_cols(drzave) %>%
  dplyr::select(drzava = ...3, x = V1, y = V2)

k = obrisi.k(r.hc)
skupine = nakupi[, -1] %>%
  dist() %>%
  hclust(method = "ward.D") %>%
  cutree(k = k) %>%
  as.ordered()
diagram.skupine(drzave.x.y, drzave.x.y$drzava, skupine, k)

k = obrisi.k(r.km)
set.seed(42) # ne pozabimo na ponovljivost rezultatov
skupine = nakupi[, -1] %>%
  kmeans(centers = k) %>%
  getElement("cluster") %>%
  as.ordered()
diagram.skupine(drzave.x.y, drzave.x.y$drzava, skupine, k)

set.seed(42)
skupine = nakupi[, -1] %>%
  kmeans(centers = 4) %>%
  getElement("cluster") %>%
  as.ordered()
diagram.skupine(drzave.x.y, drzave.x.y$drzava, skupine, 2)


svet.sp = readOGR("zemljevidi/TM_WORLD_BORDERS-0.3.shp", "TM_WORLD_BORDERS-0.3")
svet.sp = gBuffer(svet.sp, byid = TRUE, width = 0)

svet.map = svet.sp %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
svet.centroidi = read_csv("zemljevidi/drzave-centroidi.csv")
evropske.drzave = tibble(
  drzava = c(
    "Albania", "Andorra", "Armenia",
    "Austria", "Azerbaijan", "Belarus",
    "Belgium", "Bosnia and Herzegovina",
    "Bulgaria", "Croatia", "Cyprus",
    "Czechia", "Denmark", "Estonia",
    "Finland", "France", "Georgia",
    "Germany", "Greece", "Hungary",
    "Iceland", "Ireland", "Italy",
    "Kazakhstan", "Latvia",
    "Liechtenstein", "Lithuania",
    "Luxembourg", "Malta", "Moldova",
    "Monaco", "Montenegro",
    "Netherlands", "North Macedonia",
    "Norway", "Poland", "Portugal",
    "Romania", "Russia", "San Marino",
    "Serbia", "Slovakia", "Slovenia",
    "Spain", "Sweden", "Switzerland",
    "Turkey", "Ukraine", "United Kingdom",
    "Holy See (Vatican City)"
  )
)


evropa.izsek = as(extent(-25, 60, 30, 75), "SpatialPolygons")
sp::proj4string(evropa.izsek) <- sp::proj4string(svet.sp)

evropske.drzave = tibble(
  drzava = c(
    "Albania", "Andorra", "Armenia",
    "Austria", "Azerbaijan", "Belarus",
    "Belgium", "Bosnia and Herzegovina",
    "Bulgaria", "Croatia", "Cyprus",
    "Czechia", "Denmark", "Estonia",
    "Finland", "France", "Georgia",
    "Germany", "Greece", "Hungary",
    "Iceland", "Ireland", "Italy",
    "Kazakhstan", "Latvia",
    "Liechtenstein", "Lithuania",
    "Luxembourg", "Malta", "Moldova",
    "Monaco", "Montenegro",
    "Netherlands", "North Macedonia",
    "Norway", "Poland", "Portugal",
    "Romania", "Russia", "San Marino",
    "Serbia", "Slovakia", "Slovenia",
    "Spain", "Sweden", "Switzerland",
    "Turkey", "Ukraine", "United Kingdom",
    "Holy See (Vatican City)"
  )
)

# Evropa se po zemljepisni dolžini razteza
# od -25 do 60, po širini pa od 30 do 75
evropa.izsek = as(extent(-25, 60, 30, 75), "SpatialPolygons")
sp::proj4string(evropa.izsek) <- sp::proj4string(svet.sp)
colnames(evropske.drzave)[1] <- "NAME"
evropa.poligoni = svet.sp %>% crop(evropa.izsek) %>% fortify() %>% tibble() %>%
left_join(
  evropske.drzave,
  by = "NAME"
)
colnames(svet.centroidi)[1] <- "NAME"
evropa.centroidi = evropske.drzave %>%
  left_join(
    svet.centroidi,
    by = "NAME"
  )


colnames(evropa.poligoni)[12] <- "drzava"
colnames(evropa.centroidi)[1] <- "drzava"

prostorski.diagram.skupine = function(drzave, skupine, k) {
  drzave %>%
    bind_cols(skupine) %>%
    dplyr::select(drzava = ...1, skupina = ...2) %>%
    left_join(
      evropa.poligoni,
      by = "drzava"
    ) %>%
    ggplot() +
    geom_polygon(
      mapping = aes(long, lat, group = group, fill = skupina),
      color = "grey"
    ) +
    scale_fill_brewer() +
    coord_map() +
    xlim(-25, 50) +
    theme_classic() +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}

set.seed(42)
skupine = nakupi[, -1] %>%
  kmeans(centers = 2) %>%
  getElement("cluster") %>%
  as.ordered()

prostorski.diagram.skupine(drzave, skupine, 2)

k = dendrogram %>%
  hc.kolena %>%
  hc.kolena.k() %>%
  getElement(1)

skupine = dendrogram %>%
  cutree(k = 4) %>%
  as.ordered()

prostorski.diagram.skupine(drzave, skupine, k)

podatki.ucni <- tabela1 %>%
  filter(
    Year == 2019
  ) %>%
  dplyr::select(-Year, - Area)
podatki.ucni <- podatki.ucni[-c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 62, 64, 66, 68), ]

podatki.ucni <- podatki.ucni[(-c(32, 33, 34)),]
g <- ggplot(podatki.ucni, aes(x=BDPpc, y=Value)) + geom_point()
g + geom_smooth(method="lm")

lin <- lm(data=podatki.ucni, Value ~ BDPpc + Education)
lin
napovedi <- predict(lin)

kv <- lm(data=podatki.ucni, Value ~ Education + I(BDPpc^2))

mls <- loess(data=podatki.ucni, Value ~ BDPpc + Education)


sapply(list(lin, kv, mls), function(x) mean((x$residuals^2)))




podatki.ucni <- transform(podatki.ucni, Education = as.numeric(as.character(Education)))
k <- 5
formula <- Value ~ BDPpc

napaka.cv <- function(podatki, k, formula) {
  n <- nrow(podatki)
  r <- sample(1:n)
  
  razrez <- cut(1:n, k, labels = FALSE)
  
  razbitje <- split(r, razrez)
  
  pp.napovedi = rep(0, n)
  for (i in 1:length(razbitje)) {
    #Naučimo se modela na množici S/Si
    model = podatki[ -razbitje[[i]], ] %>% lm(formula = formula)
    #Naučen model uporabimo za napovedi na Si
    pp.napovedi[ razbitje[[i]] ] = predict(object = model, newdata = podatki[ razbitje [[i]], ] )
    
  }
  
  #model so učni podatki in ostalo ( razbitje[[i]]) so testni podatki. 
  
  napaka <- mean((pp.napovedi - podatki$Value)^2)
  return(napaka)
}
napaka.cv(podatki.ucni, 5, formula)

formule <- c(Value ~ BDPpc,
             Value ~ BDPpc + I(BDPpc^2),
             Value ~ BDPpc + I(BDPpc^2) + I(BDPpc^3),
             Value ~ BDPpc + I(BDPpc^2) + I(BDPpc^3) + I(BDPpc^4),
             Value ~ BDPpc + I(BDPpc^2) + I(BDPpc^3) + I(BDPpc^4) +I(BDPpc^5))
napake <- rep(0, 5)
for (i in 1:5){
  formula <- formule[[i]]
  napaka <- napaka.cv(podatki.ucni, 5, formula) 
  napake[i] <- napaka
}
which.min(napake)
lin.model <- lm(data = podatki, formula = Value ~ BDPpc + I(BDPpc^2))


napaka_regresije = function(podatki, model) {
  podatki %>%
    bind_cols(Value.hat = predict(model, podatki)) %>%
    mutate(
      izguba = (Value - Value.hat) ^ 2
    ) %>%
    dplyr::select(izguba) %>%
    unlist() %>%
    mean()
}

log.model = glm(
  Value ~ BDPpc + I(BDPpc^2),
  data = podatki.ucni, family = "binomial"
)
print(log.model)

library(ranger)
library(janitor)
p.ucni <- janitor::clean_names(podatki.ucni)
tabela2 <- janitor::clean_names(tabela2)
set.seed(42)
ng.reg.model = ranger(value ~ bd_ppc + I(bd_ppc^2), p.ucni)
print(ng.reg.model)




ng <- ranger(value ~ bd_ppc, p.ucni)

ucenje = function(podatki, formula, algoritem) {
  switch(
    algoritem,
    lin.reg = lm(formula, data = podatki),
    ng = ranger(formula, data = podatki)
  )
}

napovedi = function(podatki, model, algoritem) {
  switch(
    algoritem,
    lin.reg = predict(model, podatki),
    ng = predict(model, podatki)$predictions
  )
}

napaka_regresije = function(podatki, model, algoritem) {
  podatki %>%
    bind_cols(value.hat = napovedi(podatki, model, algoritem)) %>%
    mutate(
      izguba = (value - value.hat) ^ 2
    ) %>%
    dplyr::select(izguba) %>%
    unlist() %>%
    mean()
}

napaka_razvrscanja = function(podatki, model, algoritem) {
  podatki %>%
    bind_cols(value.hat = napovedi(podatki, model, algoritem)) %>%
    mutate(
      izguba = (value != value.hat)
    ) %>%
    dplyr::select(izguba) %>%
    unlist() %>%
    mean()
}





lin.model = p.ucni %>% ucenje(value ~ bd_ppc, "lin.reg")
p.ucni %>% napaka_regresije(lin.model, "lin.reg")

set.seed(42)
ng.reg.model = p.ucni %>% ucenje(value ~ bd_ppc, "ng")
print(p.ucni %>% napaka_regresije(ng.reg.model, "ng"))

k <- 5
formula2 <- value ~ bd_ppc

napaka.cv2 <- function(podatki, k, formula) {
  n <- nrow(podatki)
  r <- sample(1:n)
  
  razrez <- cut(1:n, k, labels = FALSE)
  
  razbitje <- split(r, razrez)
  
  pp.napovedi = rep(0, n)
  for (i in 1:length(razbitje)) {
    #Naučimo se modela na množici S/Si
    model = podatki[ -razbitje[[i]], ] %>% ranger(formula = formula)
    #Naučen model uporabimo za napovedi na Si
    pp.napovedi[ razbitje[[i]] ] = predict(object = model, data = podatki[ razbitje [[i]], ])$predictions
    
  }
  
  #model so učni podatki in ostalo ( razbitje[[i]]) so testni podatki. 
  
  napaka <- mean((pp.napovedi - podatki$value)^2)
  return(napaka)
}
napaka.cv(p.ucni, 5, formula2)

formule <- c(Value ~ BDPpc,
             Value ~ BDPpc + I(BDPpc^2),
             Value ~ BDPpc + I(BDPpc^2) + I(BDPpc^3),
             Value ~ BDPpc + I(BDPpc^2) + I(BDPpc^3) + I(BDPpc^4),
             Value ~ BDPpc + I(BDPpc^2) + I(BDPpc^3) + I(BDPpc^4) +I(BDPpc^5))
napake <- rep(0, 5)
for (i in 1:5){
  formula <- formule[[i]]
  napaka <- napaka.cv(podatki.ucni, 5, formula) 
  napake[i] <- napaka
}
which.min(napake)
lin.model <- lm(data = podatki, formula = Value ~ BDPpc + I(BDPpc^2))

library(iml)

# Pripravimo le napovedne spremenljivke,
# kot jih rabi funkcija Predictor$new

X = p.ucni %>% dplyr::select(bd_ppc, education)

# Funkciji Predictor$new moramo povedati
# kako napovedujemo z modelom naključnih gozdov 

pfun = function(model, newdata) {
  predict(model, data = newdata, predict.all = FALSE)$predictions
}

# pripravimo najprej objekt razreda Prediktor
# prvi argument funkcije je model,
# drugi in tretji so podatki o napovednih
# in ciljni spremenljivki,
# zadnji pa funkcija za napovedovanje

reg.pred = Predictor$new(
  ng.reg.model,
  data = X, y = p.ucni$value,
  predict.fun = pfun
)

# na koncu uporabimo funkcijo FeatureImp$new
reg.moci = FeatureImp$new(reg.pred, loss = "mse")

plot(reg.moci)


slo <- tabela1 %>% filter(Country == "Slovenia")
slo <- slo[-c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32),]
slo <- slo[-c(1, 2),]

data(EuStockMarkets)
EuStockMarkets
CAC <- slo[,4]
CACs <- slo[,c(1,4)]
CACs %>% ggplot() +
  geom_line(
    mapping = aes(x = Year, y = Value),
    color = "pink"
  )
library(ranger)
Lag <- function(x, n){
  (c(rep(NA, n), x)[1 : length(x)] )
}
naredi.df <- function(x){data.frame(Value = x,
                                    Value1 = Lag(x, 1),
                                    Value2 = Lag(x, 2) ,
                                    Value3 = Lag(x, 3),
                                    Value4 = Lag(x, 4)
)
}
df <- naredi.df(CAC$Value)
model.bi = ranger(Value ~ Value1 + Value2 + Value3 + Value4, data=df %>% drop_na())
n <- nrow(df)
for (i in 1:5){
  df <- naredi.df(c(df$Value, NA))
  napoved = predict(model.bi,  data = df[n + i, ] )$predictions
  df[n+i, 1] = napoved
}
napovedi = df[(n+1):(n+5), 1]




