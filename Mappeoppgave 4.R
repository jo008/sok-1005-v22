# Mappeoppgave 4

# Jobbet sammen med Sander Nicolai Jørgensen

rm(list=ls())

library(rvest)
library(tidyverse)
library(rlist)
library(janitor)
library(purrr)

# Lager flere verdier med url til de forskjellige timeplanene mine
sok_1005 <- read_html("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&week=1-20&View=list")

sok_1006 <- read_html("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1006-1&week=1-20&View=list")

sok_1016 <- read_html("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1016-1&week=1-20&View=list")

# Setter de sammen i en lisre
emner <- list(sok_1005,sok_1006,sok_1016)

# Bygger opp likens som scarpe_timeplan gjør
scrape <- function(emner) {
  df <- html_nodes(emner, "table") %>%#Tabell per uke
  html_table(., fill=TRUE) %>%  #Koden blir til en liste
  list.stack(.) #Gjør om listen til et dataframe 
  
  colnames(df) <- df[1,] #Gjør slik at den første raden blir et variabel navn
  
  liste <- df %>% filter(!Dato=="Dato") %>% #Fjerner dato kolonnen
    
  #Lager et skille mellom dag og dato formatet
  separate(Dato, into = c("Dag", "Dato"), sep = "(?<=[A-Za-z])(?=[0-9])") 
  
  liste$Dato <- as.Date(liste$Dato, format="%d.%m.%Y") #Dette blir "dag, måned, år" format
  liste$Uke <- strftime(liste$Dato, format = "%V") #Legger til ukenummer
  
  #Her endrer jeg til min ønsket rekkefølge for å få det mere oversiktlig
  rekkefølge <- c("Uke", "Dag", "Dato", "Tid", "Emnekode", "Rom", "Lærer")
  liste <- liste[, rekkefølge]
  return(liste)
}

timeplan <- map(emner, scrape) %>% bind_rows()
timeplan

#Kilder
#http://www.sthda.com/english/wiki/reordering-data-frame-columns-in-r
#https://purrr.tidyverse.org #cheatsheet