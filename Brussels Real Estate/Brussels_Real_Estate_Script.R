library(tidyverse)
library(readxl)
library(dplyr)
library(sf) #Simple Features. This is the map-maker
library(viridis)  #Nicer colors
library(ggthemes) #Nice theme for maps

###MAP CODING
#Import Shp file and plot
shpfile <- "Brussels Real Estate/geo.be/municipality_3812.shp" #path to file 
emptymap <- st_read(dsn = shpfile) #read the shp format into R
plot(emptymap$geometry) #plot the geometry column 
emptymap <-  emptymap %>% rename(Commune=namefre) #rename column so we can do the leftjoin

###DATA CODING
getwd()

#Importing Municipality Date and first filter
housing_brussels <- "Brussels Real Estate/export.csv"
dat_original <- read.csv(housing_brussels)

#ORIGINAL DAT & rename columns and values
dat <-
  dat_original %>% rename(
    Year = Année,
    Building.type = Type.de.bâtiment,
    Sqm = Superficie.du.terrain,
    Number.of.sales = Nombre.de.ventes,
    Median.price = Prix.médian,
    X1st.quartile = X1er.quartile.du.prix,
    X3rd.quartile = X3ème.quartile.du.prix
  ) %>% select(-Trimestre)

#Translate Values
dat$Building.type <- as.character(dat$Building.type)
dat$Building.type[dat$Building.type == "Maisons avec 2 ou 3 façades (type fermé + type demi-fermé)"] <- "Terraced or Semi-terraced"
dat$Building.type[dat$Building.type == "Appartements"] <- "Apartments"
dat$Building.type <- as.factor(dat$Building.type)

#Filter out uneeded values
dat <- dat %>% filter(!Building.type %in% c("Maisons avec 4 ou plus de façades (type ouvert)"))
dat <- dat %>% filter(!Sqm %in% c("300-599m²","600-999m²","1000-1499m²",">=1500m²"))

#Create the DFs to be used by the merge
dat_2022_appart_nonas <- dat %>% filter(Year==2022 & Building.type=="Apartments" & !is.na(Median.price))
#2022 data on apartments, median price

###MERGE DATA AND MAP CODING
dat_merged <- left_join(emptymap,dat_2022_appart_nonas, by= "Commune")
dat_merged_filtered <- dat_merged %>%  filter(!is.na(Year) )


theme_set(theme_map(base_size = 20)) #Set the map theme

#Creat the Map/Plot
ggplot(dat_merged_filtered)+
  geom_sf(aes(fill=Median.price)) +
  coord_sf(datum = NA)+
  scale_fill_distiller(palette = "RdYlGn", name="EUR",direction = 1) +
  geom_sf_label(aes(label = Commune)) +
  labs(
    title = "Median Price of Apartments by Commune in Brussels",
    subtitle = "",
    caption = "Data: Median Price 2022") +
  theme(legend.position = c(0.95, 0.6),legend.key.size = unit(5,"mm") ) 




##NEED TO Create 2021-+Quater Dates

#Basic Plot of Uccle, Ixelles and Foret Prices for appartments
dat_uccixlfor <- dat %>% filter(Commune %in% c("Uccle","Forest (Bruxelles-Capitale)","Ixelles") & Building.type=="Apartments")

#Bar chart of Median Price apartments in 2022
dat %>%  filter(Year %in% c(2022) & Sqm == "Superficie inconnue" & Building.type=="Apartments") %>%  ggplot(aes(x = reorder(Commune,Median.price), y = Median.price))   + geom_bar(stat="identity") + coord_flip()

#Bar chart of Median Price houses in 2022
dat %>%  filter(Year %in% c(2022) & Sqm == "100-299m²" & Building.type=="Terraced or Semi-terraced") %>%  ggplot(aes(x = reorder(Commune,Median.price), y = Median.price))   + geom_bar(stat="identity") + coord_flip()

#Bar chart of Median Price houses in 2022
dat %>%  filter( Sqm == "100-299m²" & Building.type=="Terraced or Semi-terraced" & Commune=="Auderghem" )%>%  ggplot(aes(x = reorder(Year,Median.price), y = Median.price))   + geom_bar(stat="identity")


dat_test <- dat %>% filter(Year==2021 & Commune=="Ixelles")
dat_allcom_appart %>% filter(Année==2022) %>% slice_min(Prix.médian)
dat_allcom_appart %>% filter(Année==2022) %>% slice_max(Nombre.de.ventes)
