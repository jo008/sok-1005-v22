# Laster ned nødvendige pakker
library(tidyverse)
library(data.table)
library(globe)
library(zoo)
library(ggplot2)
library(readr)
library(cowplot)

## Oppgave 1
#Benytt R og last ned dataene fra “Lower-Troposphere” og lag et ggplot som ligner figuren på nettsiden. I datasettet er det variabelen Globe som er benyttet. Merk at dataene inneholder noe “unødig” informasjon i slutten av fila som du ikke skal ha med. Når du skriver kode som leser data, forsøk å gjøre koden så generell at den også fungerer neste måned når det er kommet en ny rad inn på slutten av datafila.

#For å beregne et 13-måneders glidende-gjennomsnitt (“centered moving-average”) kan du f.eks benytte funksjonen zoo::rollmean(). “Zoo” er R biblioteket, og “rollmean” er navnet på funksjonen. Gjør ditt beste for å få plottet til å se bra og selvforklarende ut. Det må ha en tittel, og meningsfulle titler på akser.

# Laster ned filen 
# kan hende du må legge til 2 etter table
uahncdc_lt_6_0 <- read_table("http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")

# filtrere bort unnødvendig data jeg ikke trenger, legger til 2022 slik at når ny data kommer så fungerer det
tfaere <- uahncdc_lt_6_0 %>%
  filter(Year <= 2022) %>%
  group_by(Globe) 
#%>%group_by(Year)

# beregner et 13-måneders glidende-gjennomsnitt 
tfaere %>%
  mutate(mnd_gjsnitt = rollmean(Globe, k=13, fill = NA))

# sjekker hvilken classe variablene mine er på, finner ut at de ikke 
class(tfaere$Globe)
class(tfaere$Year)

# konverterer variablene til numerisk verdi
tfaere$Globe <- as.numeric(tfaere$Globe)
tfaere$Year <- as.numeric(tfaere$Year)

str(tfaere)

tfaere %>%
  ggplot(aes(x=Year, y=Globe)) + 
  geom_point(alpha = 0.3, color="dark blue") +
  geom_line(color="dark blue") +
  geom_smooth(color="red", orientation = "mnd_gjsnitt", span=0.09, se = FALSE) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  scale_y_continuous(limits = c(-0.7, 0.9),
                     breaks = scales::breaks_width(0.1)) +
  scale_x_continuous(limits = c(1979, 2022),
                     breaks = scales::breaks_width(1)) +
  labs(title="Satellittbasert temperatur på klodens nedre atmosfære",
       subtitle = "Temperaturer fra 1979-2021",
       x ="År",
       y = "Gjennomsnittlig grader (celsius)") +
  theme_bw()

tfaere %>%
  ggplot(aes(x=Year, y=Globe)) + 
  geom_point(alpha = 0.3, color="dark blue") +
  geom_line(alpha = 0.2, color="dark blue") +
  #geom_smooth(aes(y = tfaere, col = "Hei")) +
  #geom_hline(yintercept = 0, alpha = 0.5) +
  #scale_y_continuous(name = "Temperatur", limits = c(-0.7, 0.9),
                     #breaks = scales::breaks_width(0.1)) +
  #scale_x_continuous(breaks = scales::breaks_width(2)) +
  labs(title="Satellittbasert temperatur på klodens nedre atmosfære",
       x ="År",
       y = "Gjennomsnittlig grader (celsius)") 
  #theme_bw()

# Oppgave 2
#På nettsiden finner vi lenker til temperaturdata på fire nivå i atmosfæren. I denne oppgaven skal du skrive kode som leser alle fire datasettene, og slår dem sammen til et datasett. Fra disse sammenslåtte dataene skal vi trekke ut variabelen NoPol som er temperatur fra 60 til 90 grader nord. Lag et ggplot som viser temperaturen fra disse fire nivåene samtidig, og legg til et femte nivå som er gjennomsnittet av de fire nivåene i atmosfæren. Gjør ditt beste for å få plottet til å se bra og selvforklarende ut. Det må ha en tittel og meningsfulle titler på akser og kategorier.
#Når du skriver kode som leser data, forsøk å gjøre koden så generell at den også fungerer neste måned når det er kommet en ny rad inn på slutten av datafila. Forsøk også å gjøre koden din så effektiv at du ikke kopierer all kode fra et atmosfærisk nivå til det neste, dvs at koden din blir 4 ganger så stor som nødvendig.

# mutate: date og moving_average

# Nedre troposfære
uahncdc_lt_6_0 <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")

# Filterer bort data vi ikke trenger, og grupperer med NoPol som er den variabelen vi skal fokusere på 
nedre_tfaere <- uahncdc_lt_6_0 %>%
  filter(Year <= 2022) %>%
  select(Year, NoPol) %>%
  rename(NoPol_nedre_tfaere = NoPol, Years = Year)
  #group_by(NoPol) 

nedre_tfaere$NoPol_nedre_tfaere <- as.numeric(nedre_tfaere$NoPol_nedre_tfaere)
nedre_tfaere$Years <- as.numeric(nedre_tfaere$Years)

nedre_tfaere %>% 
  ggplot(aes(x=Years, y=NoPol_nedre_tfaere)) +
  geom_point() + 
  scale_y_continuous(limits = c(-2.5, 2.2),
                     breaks = scales::breaks_width(0.2)) +
  scale_x_continuous(limits = c(1979, 2022),
                     breaks = scales::breaks_width(2))

# Midt troposfære
uahncdc_mt_6_0 <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt")

midt_tfaere <- uahncdc_mt_6_0 %>%
  filter(Year <= 2022) %>%
  select(Year, NoPol) %>%
  rename(NoPol_midt_tfaere = NoPol)
  #group_by(NoPol) 

midt_tfaere$NoPol_midt_tfaere <- as.numeric(midt_tfaere$NoPol_midt_tfaere)
midt_tfaere$Year <- as.numeric(midt_tfaere$Year)

midt_tfaere %>% 
  ggplot(aes(x=Year, y=NoPol_midt_tfaere)) +
  geom_point() + 
  scale_y_continuous(limits = c(-0.01, 2),
                     breaks = scales::breaks_width(0.2)) +
  scale_x_continuous(limits = c(1979, 2022),
                     breaks = scales::breaks_width(2))

# Tropopause
uahncdc_tp_6_0 <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt")

tropopause <- uahncdc_tp_6_0 %>%
  filter(Year <= 2022) %>%
  select(Year, NoPol) %>%
  rename(NoPol_tropopause = NoPol)
  #group_by(NoPol) 

tropopause$NoPol_tropopause <- as.numeric(tropopause$NoPol_tropopause)
tropopause$Year <- as.numeric(tropopause$Year)

tropopause %>% 
  ggplot(aes(x=Year, y=NoPol_tropopause)) +
  geom_point() + 
  scale_y_continuous(limits = c(-0.01, 5),
                     breaks = scales::breaks_width(0.2)) +
  scale_x_continuous(limits = c(1979, 2022),
                     breaks = scales::breaks_width(2))

# Nedre stratosfære
uahncdc_ls_6_0 <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt")

nedre_ssfaere <- uahncdc_ls_6_0 %>%
  filter(Year <= 2022) %>%
  select(Year, NoPol) %>%
  rename(NoPol_nedre_ssfaere = NoPol)
  #group_by(NoPol) 

nedre_ssfaere$NoPol_nedre_ssfaere <- as.numeric(nedre_ssfaere$NoPol_nedre_ssfaere)
nedre_ssfaere$Year <- as.numeric(nedre_ssfaere$Year)

nedre_ssfaere %>% 
  ggplot(aes(x=Year, y=NoPol_nedre_ssfaere)) +
  geom_point() + 
  scale_y_continuous(limits = c(-9, 10),
                     breaks = scales::breaks_width(1)) +
  scale_x_continuous(limits = c(1979, 2022),
                     breaks = scales::breaks_width(2))

plot_grid(nedre_ssfaere, nedre_tfaere, ncol = 2, labels = "AUTO") 



# Binder datasettene sammen
total <- cbind(nedre_tfaere, midt_tfaere, tropopause, nedre_ssfaere) %>%
# funket ikke å bruke unique eller distinct for å fjerne kolonnene som var duplisert. Bruker select
  select(Years, NoPol_nedre_ssfaere, NoPol_tropopause, NoPol_midt_tfaere, NoPol_nedre_tfaere)

gjsnitt <- rbind(nedre_tfaere, midt_tfaere, tropopause, nedre_ssfaere)
#select(Year, NoPol_nedre_ssfaere, NoPol_tropopause, NoPol_midt_tfaere, NoPol_nedre_tfaere) %>% 

total %>% 
  #pivot_longer(NoPol_nedre_tfaere:NoPol_nedre_ssfaere, 
               #names_to = "Temperatur", values_to = "Verdi") %>%
  #pivot_longer(-Year, names_to="Temperatur", values_to="verdi") %>% 
  #ggplot(aes(x = NoPol_nedre_ssfaere, y = Year) +
  ggplot(aes(x=Year, y=NoPol_tropopause)) +
  geom_point() + 
  scale_y_continuous(limits = c(-0.7, 0.9),
                     breaks = scales::breaks_width(0.1)) +
  scale_x_continuous(limits = c(1979, 2022),
                     breaks = scales::breaks_width(2))
  

test %>%
  filter(country %in% c("Norway","Sweden")) %>% 
  ggplot(aes(x=år, y=eksportverdi, col=land)) +
  geom_line() +
  labs(title="Eksport av varer og tjenester \n (nominelle tall)",
       x =" ",
       y = "milliarder US$") +
  theme_bw()
    

    tfaere %>%
      ggplot(aes(x=Year, y=Globe)) + 
      geom_point(alpha = 0.3, color="dark blue") +
      geom_line(color="dark blue") +
      geom_smooth(color="red", orientation = "mnd_gjsnitt", span=0.09, se = FALSE) +
      geom_hline(yintercept = 0, alpha = 0.5) +
      scale_y_continuous(limits = c(-0.7, 0.9),
                         breaks = scales::breaks_width(0.1)) +
      scale_x_continuous(limits = c(1979, 2022),
                         breaks = scales::breaks_width(1)) +
      labs(title="Satellittbasert temperatur på klodens nedre atmosfære",
           subtitle = "Temperaturer fra 1979-2021",
           x ="År",
           y = "Gjennomsnittlig grader (celsius)") +
      theme_bw()


    df <- data.frame(x = c(0, 5, 10, 15), y = c(2.2, 3.8, 4.6, 7.6),z = c(4.5, 6.8, 9.3, 10.5))
    
    ggplot(df, aes(x)) + 
      geom_line(aes(y = y, colour = "y")) +   
      geom_line(aes(y = z, colour = "z"))

    dframe %>%
      filter(country=="Norway") %>% 
      select(år, import, eksport) %>% 
      pivot_longer(-år, names_to="aktivitet", values_to="verdi") %>% 
      ggplot(aes(x=år, y=verdi, col=aktivitet)) +
      geom_line() +
      scale_color_manual(values=c("dark red", "dark blue")) +
      labs(title="Norsk eksport og import av varer og tjenester \n (nominelle tall)",
           x =" ",
           y = "milliarder US$") +
      theme_bw()
    
    kpi %>%
      rename(KPI_2015=value) %>%
      select(dato, KPI_2010, KPI_2015) %>% 
      pivot_longer(-dato,
                   names_to = "KPI",
                   values_to = "indeks") %>% 
      ggplot(aes(x=dato, y=indeks, col=KPI)) +
      geom_line() +
      labs(title="Konsumprisindeks - KPI",
           x =" ",
           y = "Totalindeks") +
      theme_bw()