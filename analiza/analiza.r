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

nakupi = tabela2 %>% select(
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



