# Setup
# Laster ned nødvenige pakker
library(tidyverse)
library(rvest)
library(ggplot2)

# Oppgave 1

# Lager en variabel med informasjon fra nettsiden
html <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132")

# Omgjør gjør dataen til en tabell
verdi <- html_table(html_nodes(html, "table")[[1]], header = TRUE)

# Sjekker hva navnene er på tabellen slik at de kan endres
names(verdi)

# Endrer navnene før jeg fortsetter med å endre resten
verdi <-
  select(verdi, Modell = `Modell (temp. varierte fra 0° til -10°)`, Wltp = `WLTP-tall`, Stopp = `STOPP`, Avvik = `Avvik`) 

# Her fjerner jeg de kolonnene som har "x" i seg, siden den informasjonen ikke trengs
# Bruker ! for å få den motsatte effekten, fjerner jeg den så velger den kun "x" verdiene
verdi <- verdi[!grepl("x",verdi$Avvik),]

# Her sepererer jeg Wltp kolonnen slik at den får to verdier, en med Wltp og en med kWh. Tar også å gjør om komma til punktum i kwh og Avvik siden de ikke blir numerisk pga kommaet.
verdi <-
separate(verdi, col=Wltp, into=c("Wltp","kWh"), sep="/") %>%
  mutate(Avvik = str_replace(Avvik, ",", "."), 
         kWh = str_replace(kWh, ",", "."))

# Nå må jeg rydde opp i variabelen, f.eks fjerne km og % i kolonnene. Samtidig så gjør jeg slik at verdiene er numerisk og ikke en "character"
verdi$Wltp <- gsub("km","",as.character(verdi$Wltp)) %>% as.numeric(verdi$Wltp)
verdi$kWh <- gsub("kWh","",as.character(verdi$kWh)) %>% as.numeric(verdi$kWh)
verdi$Stopp <- gsub("km","",as.character(verdi$Stopp)) %>% as.numeric(verdi$Stopp)
verdi$Avvik <- gsub("%","",as.character(verdi$Avvik)) %>% as.numeric(verdi$Avvik)

# gjør om til str
str(verdi)

# Nå kan jeg begynne å plotte

plot <- verdi %>% 
  ggplot(aes(x=Wltp,y=Stopp)) + 
  geom_point() +
  geom_abline(col  ="red") + 
  scale_y_continuous(limits = c(200,600),
                     breaks = scales::breaks_width(100)) +
  scale_x_continuous(limits = c(200,600),
                     breaks = scales::breaks_width(100)) +
  labs(title="Forholdet mellom kjørelengden til elektriske biler",
       #subtitle = "Wltp er den oppgitte kjørelengden",
       x ="Wltp (den oppgitte kjørelengden)",
       y = "Stopp (faktisk kjørelengde)") +
  theme_bw()

plot

# Oppgave 2

# Her legger vi til lm() funksjonen til koden 
lm(Stopp ~ Wltp, data = verdi)

# Her er plot nr 2, med lm funksjonen
plot2 <- plot +
  geom_smooth(method = lm)

plot2

# Siden ingen av merkene når opp til forventet kjørelengde, så bruker vi lm funksjonen. Den brukes for å finne sammenhengen (i dette tilfellet) mellom flere avhengige variabler. Med den nye linja vi har fått så kan vi tolke det slik. Siden den blå linjen er lavere enn den røde linjen, så ser vi at den blå linjen er nærmere punktene. Det betyr at når wltp øker så er den mer lik den oppgitte kjørelengden på de elektriske bilene. Vi ser at når vi kjører den første linjen (lm(Stopp ~ Wltp, data = verdi) så kommer det opp at wltp er på ca. 0.9 og at koeffisienter er på ca. -26.6. Koeffisienter er den forventedene gjennomsnittsverdien av Y når X=0. Siden intercept er et negativt tall, så betyr det at den sanne rekkevidden er mindre enn de har reklmaert for.

# Kilder
# https://www.rdocumentation.org/packages/rvest/versions/0.3.2/topics/html_table
# https://r-lang.com/grepl-in-r/
# https://www.quora.com/What-does-mean-in-R/answer/Daniel-Findley?ch=10&oid=8348653&share=61428e94&target_type=answer 
# https://datascience.stackexchange.com/questions/15589/remove-part-of-string-in-r
# https://www.theanalysisfactor.com/interpreting-the-intercept-in-a-regression-model/
