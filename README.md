# Analiza podatkov s programom R - 2021/22

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Tematika
Za projektno nalogo bom analizirala spletno prodajo - koliko in kaj ljudje iz držav EU kupujejo prek spleta in to primerjala z BDP-jem po državah in stopnjo stopnjo izobrazbe v državah. Ogledala si bom tudi, kolikšen procent ljudi, ki živijo v mestih, je v zadnjih 3 mesecih kupovalo prek spleta in kolikšen procent takih je na ruralnih območjih.

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).
