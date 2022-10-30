# install.packages(c("tidyverse","readxl","dplyr","sf","viridis","ggthemes","ggrepel","lintr","dbplyr","scales"))

library(tidyverse)
library(readxl)
library(dplyr)
library(sf) #Simple Features. This is the map-maker
library(viridis)  #Nicer colors
library(ggthemes) #Nice theme for maps
library(ggrepel)
library(lintr)
library(dbplyr)
library(scales)


###MAP CODING - Import Shp file and plot

shpfile <- "Brussels Real Estate/geo.be/municipality_3812.shp"
emptymap <- st_read(dsn = shpfile) #read the shp format into R
plot(emptymap$geometry) #plot the geometry column
emptymap <- emptymap %>%
  rename(Commune = namefre) #rename column so we can do the leftjoin

###DATA CODING
#Importing Municipality Date and first filter

df_raw <- read.csv("Brussels Real Estate/export.csv")

### DATA WRANGLING
#rename columns and values
df <-
  df_raw %>%
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
df$Commune_short <- as.character(df$Commune_short)
df$Commune_short[df$Commune_short == "Berchem-Sainte-Agathe"] <- "Berchem"
df$Commune_short[df$Commune_short == "Forest (Bruxelles-Capitale)"] <- "Forest"
df$Commune_short[df$Commune_short == "Molenbeek-Saint-Jean"] <- "Molenbeek"
df$Commune_short[df$Commune_short == "Saint-Josse-ten-Noode"] <- "St-Josse"
df$Commune_short[df$Commune_short == "Watermael-Boitsfort"] <- "Boitsfort"
df$Commune_short[df$Commune_short == "Bruxelles"] <- "Bxl"
df$Commune_short[df$Commune_short == "Woluwe-Saint-Pierre"] <- "WSP"
df$Commune_short[df$Commune_short == "Woluwe-Saint-Lambert"] <- "WSL"
df$Commune_short <- as.factor(df$Commune_short)


#Translate Values
df$Building.type <- as.character(df$Building.type)
df$Building.type[df$Building.type == "Maisons avec 2 ou 3 façades (type fermé + type demi-fermé)"] <- "Terraced or Semi-terraced"
df$Building.type[df$Building.type == "Appartements"] <- "Apartments"
df$Building.type <- as.factor(df$Building.type)

#Filter out uneeded values
df <-
  df %>%
    filter(
      !Building.type %in% c("Maisons avec 4 ou plus de façades (type ouvert)") &
      !Sqm %in% c("300-599m²", "600-999m²", "1000-1499m²", ">=1500m²")
          )
df <- 
  df[!grepl("2è", df$Semestre),] #Remove 2nd semestre
df <- df %>% select(-Semestre)


###Model creation

#DF 2022 apartments no nas
df_2022_appart <-
  df %>%
  filter(Year == 2022 & Building.type == "Apartments" & !is.na(Median.price))

df_2022_appart <-
  left_join(emptymap, df_2022_appart, by = "Commune") %>%
  filter(!is.na(Year))
  
    
#DF all years Apartments no nas
df_appart <-
  df %>% filter(Building.type == "Apartments" & !is.na(Median.price))

df_appart <-
  left_join(emptymap, df_appart, by = "Commune") %>%
  filter(!is.na(Year))


#DF % Median Price increase Apartments
df_pcinc_appart <- df %>% 
  select(Commune,Commune_short,Year,Building.type,Sqm,Median.price) %>%
  filter(Building.type=="Apartments" & Sqm=="Superficie inconnue") %>% 
  pivot_wider(names_from = Year,values_from = Median.price)

colnames(df_pcinc_appart)[5:17] <- paste("year_",colnames(df_pcinc_appart)[5:17],"")
df_pcinc_appart <- df_pcinc_appart %>% 
  mutate('increase2010_2022'=as.integer(((`year_ 2022 `-`year_ 2010 `)/`year_ 2022 `)*100),
         'increase2015_2022'=as.integer(((`year_ 2022 `-`year_ 2015 `)/`year_ 2022 `)*100),
         'increase2020_2022'=as.integer(((`year_ 2022 `-`year_ 2020 `)/`year_ 2022 `)*100)
         )

df_pcinc_appart <-
  left_join(emptymap, df_pcinc_appart, by = "Commune") %>%
  filter(!is.na(increase2010_2022))


###Create Visuals
#MAP Df_2022
theme_set(theme_map(base_size = 20)) #Set the map theme
ggplot(df_2022_appart) +
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


#Bar chart of Median Price houses in 2021
df %>% filter(Year %in% c(2021) &
                  Sqm == "100-299m²" &
                  Building.type == "Terraced or Semi-terraced") %>%
        ggplot(aes(x = reorder(Commune, Median.price), y = Median.price)) +
                geom_bar(stat = "identity") +
                coord_flip() +
                theme_base()


#MAP Dat_increase 2010 2022
theme_set(theme_map(base_size = 20)) #Set the map theme
ggplot(df_pcinc_appart) +
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

#MAP Dat_increase 2015 2022
ggplot(df_pcinc_appart) +
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

#MAP Dat_increase 2020 2022
ggplot(df_pcinc_appart) +
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

#LINE Dat_all communes
df_appart %>% 
  filter(Commune_short %in% c("St-Josse","Ixelles","WSP","Forest","Saint-Gilles","Uccle")) %>%
  ggplot(aes(x=as.factor(Year),y=Median.price,group=Commune_short,color=Commune_short)) +
  geom_line() +
  theme_base() +
  scale_y_continuous(name="Average Price", labels=dollar_format(suffix="€",prefix="")) +
  scale_x_discrete(name="Year")+
  ggthemes::theme_economist()
  
  
###Adding Taux d'interets

df_intRates_raw <- read.csv("Brussels Real Estate/MIRCCO_30102022155824566.csv")

df_intRates <- df_intRates_raw %>% 
  select(-Flags,-Flag.Codes,-MIRCCO_AREA,-MIRCCO_SECTOR,-Secteur,-MIRCCO_INSTRUMENT,-MIRCCO_MATURITY,-FREQUENCY,-Fréquence) %>%
  filter(Région=="Belgique",Instrument=="Inférieur à 1 million d'euros")

#notes-need to clean dates then split df into 3 tables by grouping maturity


####MIR: Taux d'intérêt sur les nouveaux crédits  https://stat.nbb.be/Index.aspx?DataSetCode=MIRCCO&lang=fr#
##C:\Users\danie\OneDrive\Documents\Data Science\projects\solidboom\Brussels Real Estate MIRCCO

####https://data.oecd.org/price/producer-price-indices-ppi.htm
###https://data.oecd.org/interest/short-term-interest-rates.htm