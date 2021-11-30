# Analiza podatkov s programom R - 2021/22

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Tematika
Za projektno nalogo bom analizirala spletno prodajo med leti 2004-2019 - koliko in kaj ljudje iz držav EU kupujejo prek spleta in to primerjala z BDP-jem po državah in stopnjo izobrazbe v državah. Ogledala si bom tudi, kolikšen procent ljudi, ki živijo v mestih, je v zadnjih 3 mesecih kupovalo prek spleta in kolikšen procent takih je na ruralnih območjih in pa primerjala kaj ljudje kupujejo prek spleta in česa največ glede na stopnjo izobrazbe posameznikov.
V ta namen bom imela tri tabele.
Tabela BDP v primerjavi s procentom ljudi, ki so v zadnjih treh mesecih spletno nakupovali. Atributi(stolpci) so leto(integer), država(character), predel(factor)(ali je z vasi ali mesta), delež ljudi (ki so v zadnjih 3 mesecih opravili spletni nakup) (double), realni BDP per capita(double) in delež ljudi s tretjo stopnjo izobrazbe po državi. Do tabele bom prišla iz dveh CSV datotek, ki sem ju dobila iz spletne strani www.eurostat.com, in ene html datoteke oziroma wiki razpredelnice (https://en.wikipedia.org/wiki/List_of_countries_by_tertiary_education_attainment)
Druga tabela bosta združeni dve CSV datoteki, kjer bodo stolpci prav tako leto(integer), država(character) nato pa dosežena stopnja izobrazbe(character), stolpec, ki bo vseboval delež ljudi te stopnje izobr., ki so v zadnjih 12 mesecih prek spleta naročili knjigo, revijo, časopis ali pripomoček za učenje(double), stolpec delež ljudi, ki so v zadnjih 12 mesecih kupili obleke ali športne pripomočke(double) ter stolpec, ki bo vseboval podatke o tem, kolikšen delež vseh nakupov so naredili pripadniki te stopnje izobrazbe(double).

Zadnja bo 3. tabela, ki bo predelana CSV tabela in bo predstavljala nasploh kaj ljudje kupujejo in v kakšnih deležih. Stolpci bodo leto(integer), država(character), vrsta dobrine(character) in delež, ki ga predstavlja ta dobrina med vsemi spletnimi nakupi(double)

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
