library(tidyverse)
library(readxl)
library(dplyr)
library(sf) #Simple Features. This is the map-maker
library(viridis)  #Nicer colors
library(ggthemes) #Nice theme for maps
library(ggrepel)
library(lintr)
library(dbplyr)

###MAP CODING
#Import Shp file and plot
shpfile <- "Brussels Real Estate/geo.be/municipality_3812.shp" #path to file
emptymap <- st_read(dsn = shpfile) #read the shp format into R
plot(emptymap$geometry) #plot the geometry column
emptymap <- emptymap %>%
  rename(Commune = namefre) #rename column so we can do the leftjoin

###DATA CODING
#Importing Municipality Date and first filter
housing_brussels <- "Brussels Real Estate/export.csv"
dat_original <- read.csv(housing_brussels)

#ORIGINAL DAT & rename columns and values
dat <-
  dat_original %>%
    rename(
      Year = Année,
      Building.type = Type.de.bâtiment,
      Sqm = Superficie.du.terrain,
      Number.of.sales = Nombre.de.ventes,
      Median.price = Prix.médian,
      X1st.quartile = X1er.quartile.du.prix,
      X3rd.quartile = X3ème.quartile.du.prix
       ) %>%
    select(-Trimestre) %>%
    mutate(Commune_short = Commune)

#Add shortened Names
dat$Commune_short <- as.character(dat$Commune_short)
dat$Commune_short[dat$Commune_short == "Berchem-Sainte-Agathe"] <-
  "Berchem"
dat$Commune_short[dat$Commune_short == "Forest (Bruxelles-Capitale)"] <-
  "Forest"
dat$Commune_short[dat$Commune_short == "Molenbeek-Saint-Jean"] <-
  "Molenbeek"
dat$Commune_short[dat$Commune_short == "Saint-Josse-ten-Noode"] <-
  "St-Josse"
dat$Commune_short[dat$Commune_short == "Watermael-Boitsfort"] <-
  "Boitsfort"
dat$Commune_short[dat$Commune_short == "Bruxelles"] <- "Bxl"
dat$Commune_short[dat$Commune_short == "Woluwe-Saint-Pierre"] <-
  "WSP"
dat$Commune_short[dat$Commune_short == "Woluwe-Saint-Lambert"] <-
  "WSL"
dat$Commune_short <- as.factor(dat$Commune_short)


##CLEANING
#Translate Values
dat$Building.type <- as.character(dat$Building.type)
dat$Building.type[dat$Building.type ==
  "Maisons avec 2 ou 3 façades (type fermé + type demi-fermé)"] <-
  "Terraced or Semi-terraced"
dat$Building.type[dat$Building.type ==
  "Appartements"] <-
  "Apartments"
dat$Building.type <- as.factor(dat$Building.type)

#Filter out uneeded values
dat <-
  dat %>%
    filter(
      !Building.type %in% c("Maisons avec 4 ou plus de façades (type ouvert)") &
      !Sqm %in% c("300-599m²", "600-999m²", "1000-1499m²", ">=1500m²")
          )
dat <- 
  dat[!grepl("2è", dat$Semestre),] #Remove 2nd semestre
dat <- dat %>% select(-Semestre)

###ALL CLEANING HAS BEEN COMPLETE. NOW LETS CREATE OUR DATAFRAMES

###DF 2022 apartments no nas
dat_2022_appart_nonas <-
  dat %>% filter(Year == 2022 &
                   Building.type == "Apartments" & !is.na(Median.price))
###DF all years Apartments no nas
dat_allyears_appart_nonas <-
  dat %>% filter(Building.type == "Apartments" & !is.na(Median.price))

###DF % Median Price increase Apartments
dat_pcincrease_appart <- dat %>% select(Commune,Commune_short,Year,Building.type,Sqm,Median.price) %>%
  filter(Building.type=="Apartments" & Sqm=="Superficie inconnue") %>% 
  pivot_wider(names_from = Year,values_from = Median.price)
colnames(dat_pcincrease_appart)[5:17] <- paste("year_",colnames(dat_pcincrease_appart)[5:17],"")
dat_pcincrease_appart <- dat_pcincrease_appart %>% mutate('increase2010_2022'=as.integer((`year_ 2022 `/`year_ 2010 `-1)*100),'increase2015_2022'=as.integer((`year_ 2022 `/`year_ 2017 `-1)*100),,'increase2020_2022'=as.integer((`year_ 2022 `/`year_ 2020 `-1)*100))



###MERGE DATA AND MAP CODING
dat_2022_appart_nonas_merged <-
  left_join(emptymap, dat_2022_appart_nonas, by = "Commune") %>%  filter(!is.na(Year))
dat_allyears_appart_nonas_merged <-
  left_join(emptymap, dat_2022_appart_nonas, by = "Commune") %>%  filter(!is.na(Year))
dat_pcincrease_appart_merged <-
  left_join(emptymap, dat_pcincrease_appart, by = "Commune") %>% filter(!is.na(increase2010_2022))


#Create the Map/Plot
###MAP Dat_2022
theme_set(theme_map(base_size = 20)) #Set the map theme
ggplot(dat_2022_appart_nonas_merged) +
  geom_sf(aes(fill = Median.price)) +
  coord_sf(datum = NA) +
  scale_fill_distiller(palette = "RdYlGn",
                       name = "EUR",
                       direction = 1) +
  geom_sf_label(aes(label = Commune_short), label.size = NA) +
  labs(title = "Median Price of Apartments by Commune in Brussels",
       subtitle = "",
       caption = "Data: Median Price 2022") +
  theme(legend.position = c(0.95, 0.6),
        legend.key.size = unit(5, "mm"))


###Bar chart of Median Price houses in 2021
dat %>% filter(Year %in% c(2021) &
                  Sqm == "100-299m²" &
                  Building.type == "Terraced or Semi-terraced") %>%
        ggplot(aes(x = reorder(Commune, Median.price), y = Median.price)) +
                geom_bar(stat = "identity") +
                coord_flip() +
                theme_base()


###MAP Dat_increase 2010 2022
theme_set(theme_map(base_size = 20)) #Set the map theme
ggplot(dat_pcincrease_appart_merged) +
  geom_sf(aes(fill = increase2010_2022)) +
  coord_sf(datum = NA) +
  scale_fill_distiller(palette = "RdYlGn",
                       name = "% increase",
                       direction = 1) +
  geom_sf_label(aes(label = Commune_short), label.size = NA) +
  labs(title = "Percentage increase in Median Price of Apartments by Commune in Brussels",
       subtitle = "",
       caption = "Data: Median Price increase from 2010 to 2022") +
  theme(legend.position = c(0.95, 0.6),
        legend.key.size = unit(5, "mm"))

###MAP Dat_increase 2015 2022
ggplot(dat_pcincrease_appart_merged) +
  geom_sf(aes(fill = increase2015_2022)) +
  coord_sf(datum = NA) +
  scale_fill_distiller(palette = "RdYlGn",
                       name = "% increase",
                       direction = 1) +
  geom_sf_label(aes(label = Commune_short), label.size = NA) +
  labs(title = "Percentage increase in Median Price of Apartments by Commune in Brussels",
       subtitle = "",
       caption = "Data: Median Price increase from 2015 to 2022") +
  theme(legend.position = c(0.95, 0.6),
        legend.key.size = unit(5, "mm"))

###MAP Dat_increase 2020 2022
ggplot(dat_pcincrease_appart_merged) +
  geom_sf(aes(fill = increase2020_2022)) +
  coord_sf(datum = NA) +
  scale_fill_distiller(palette = "RdYlGn",
                       name = "% increase",
                       direction = 1) +
  geom_sf_label(aes(label = Commune_short), label.size = NA) +
  labs(title = "Percentage increase in Median Price of Apartments by Commune in Brussels",
       subtitle = "",
       caption = "Data: Median Price increase from 2020 to 2022") +
  theme(legend.position = c(0.95, 0.6),
        legend.key.size = unit(5, "mm"))

###LINE Dat_all communes
dat_allyears_appart_nonas %>% filter(Commune_short %in% c("St-Josse","Ixelles","WSP","Forest","Saint-Gilles","Uccle")) %>% 
ggplot(aes(x=as.factor(Year),y=Median.price,group=Commune_short,color=Commune_short)) + 
  geom_line() +theme_base()
