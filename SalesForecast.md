Sales Forecast
================

Librarys laden & Datensatz einlesen
-----------------------------------

``` r
library(tidyverse)
library(readxl)
library(prophet)
library(data.table)
library(dplyr)
library(ggplot2)

data <- read.csv("~/R 3.5 Files/Seminararbeit Business Analytics/Code/Brantner/data.csv")
```

EDA
---

``` r
#Übersicht
#head(data, n =2)
#895227 Zeilen
nrow(data)
```

    ## [1] 895227

``` r
#11 Spalten
ncol(data)
```

    ## [1] 11

EDA
---

``` r
#Filter auf Filiale
df7 <- data %>%
  filter(KundenNr == "7")

#Aggregieren auf Artikelgruppe und verkaufte Einheiten
ag <- aggregate(df7$VerkaufteMenge, by=list(ArtikelGruppe=df7$Artikelgruppe), FUN=sum)
ag$x <- as.numeric(ag$x)
ag$ArtikelGruppe <- as.factor(ag$ArtikelGruppe)

#Plotten der Verkaufszahlen
ggplot(ag, aes(ArtikelGruppe)) + 
  geom_bar(aes(weight = x), colour="black") + coord_flip() +
  labs(title = "Verkaufszahlen der Artikelgruppen") +
  ylab("Verkaufte Einheiten") + theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
```

![](SalesForecast_files/figure-markdown_github/unnamed-chunk-3-1.png)
