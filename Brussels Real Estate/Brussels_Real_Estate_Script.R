# install.packages(c("tidyverse","readxl","dplyr","sf","viridis","ggthemes","ggrepel","lintr","dbplyr","scales"))

library(tidyverse)
library(readxl)
library(sf) #Simple Features. This is the map-maker
library(viridis)  #Nicer colors
library(ggthemes) #Nice theme for maps
#library(ggrepel)
#library(lintr)
#library(dbplyr)
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
#Year Semestre Column
df$Year_Semestre <- paste(df$Year, substr(df$Semestre,1,1), sep="_")
df<- df %>%
  select(-Year,-Semestre)





###Model creation

#DF 2021 apartments no nas
df_2021_appart <-
  df %>%
  filter(Year_Semestre == "2021_1" & Building.type == "Apartments" & !is.na(Median.price))

df_2021_appart <-
  left_join(emptymap, df_2021_appart, by = "Commune") %>%
  filter(!is.na(Year_Semestre))
  
    
#DF all years Apartments no nas
df_appart <-
  df %>% filter(Building.type == "Apartments" & !is.na(Median.price))

df_appart <-
  left_join(emptymap, df_appart, by = "Commune") %>%
  filter(!is.na(Year_Semestre))


#DF % Median Price increase Apartments
df_pcinc_appart <- df %>% 
  select(Commune,Commune_short,Year_Semestre,Building.type,Sqm,Median.price) %>%
  filter(Building.type=="Apartments" & Sqm=="Superficie inconnue") %>% 
  pivot_wider(names_from = Year_Semestre,values_from = Median.price)

colnames(df_pcinc_appart)[5:29] <- paste("year_",colnames(df_pcinc_appart)[5:29],"")
df_pcinc_appart <- df_pcinc_appart %>% 
  mutate('increase2010_2022'=as.integer(((`year_ 2022_1 `-`year_ 2010_1 `)/`year_ 2022_1 `)*100),
         'increase2015_2022'=as.integer(((`year_ 2022_1 `-`year_ 2015_1 `)/`year_ 2022_1 `)*100),
         'increase2020_2022'=as.integer(((`year_ 2022_1 `-`year_ 2020_1 `)/`year_ 2022_1 `)*100)
         )

df_pcinc_appart <-
  left_join(emptymap, df_pcinc_appart, by = "Commune") %>%
  filter(!is.na(increase2010_2022))


#DF 2021 house no nas
df_2021_house <-
  df %>%
  filter(Year_Semestre == "2021_1" &
           grepl('Terraced', Building.type) &
           grepl("100",Sqm) &
           !is.na(Median.price))

df_2021_house <-
  left_join(emptymap, df_2021_house, by = "Commune") %>%
  filter(!is.na(Year_Semestre))



###Create Visuals
#MAP Df_2021 Apartments
theme_set(theme_map(base_size = 20)) #Set the map theme
ggplot(df_2021_appart) +
  geom_sf(aes(fill = Median.price)) +
  coord_sf(datum = NA) +
  scale_fill_distiller(palette = "RdYlGn",
                       name = "EUR",
                       direction = 1) +
  geom_sf_label(aes(label = Commune_short), label.size = NA) +
  labs(title = "Median Price of Apartments by Commune in Brussels",
       subtitle = "",
       caption = "Data: Median Price 1st Semestre 2021") +
  theme(legend.position = c(0.95, 0.6),
        legend.key.size = unit(5, "mm"))

#conclusion: South and East are the most expensive areas for apartments with west being cheapest
#Map Df_2021
theme_set(theme_map(base_size = 20)) #Set the map theme
ggplot(df_2021_house) +
  geom_sf(aes(fill = Median.price)) +
  coord_sf(datum = NA) +
  scale_fill_distiller(palette = "RdYlGn",
                       name = "EUR",
                       direction = 1) +
  geom_sf_label(aes(label = Commune_short), label.size = NA) +
  labs(title = "Median Price of Houses (100-300sqm) by Commune in Brussels",
       subtitle = "",
       caption = "Data: Median Price 1st Semestre 2021") +
  theme(legend.position = c(0.95, 0.6),
        legend.key.size = unit(5, "mm"))
#conclusion: South and East most expensive for average house while Ixelles is by far the most popular



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
#conclusion WSP one of the most expensive has not see big increase

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
#conclusion starting to see increas in the north

#LINE Dat_all communes
df_appart %>% 
  filter(Commune_short %in% c("Jette","Ixelles","WSP","Anderlecht","St-Josse") & grepl("_1",Year_Semestre)) %>%
  ggplot(aes(x=as.factor(Year_Semestre),y=Median.price,group=Commune_short,color=Commune_short)) +
  geom_line(size=1.5) +
  theme_base() +
  scale_y_continuous(name="Average Price", labels=dollar_format(suffix="€",prefix="")) +
  scale_x_discrete(name="Year")+
  ggthemes::theme_economist()
#gap between south and east is the same if not getting bigger. we also see Ixelles since 2107 increasing a great deal  
  
###MODEL1 -Adding Taux d'interets

df_intRates_raw <- read.csv("Brussels Real Estate/MIRCCO_30102022155824566.csv")

df_intRates <- df_intRates_raw %>% 
  filter(Région=="Belgique",Instrument=="Inférieur à 1 million d'euros") %>%
  select(-Région ,-Flags,-Flag.Codes,-MIRCCO_AREA,-MIRCCO_SECTOR,-Secteur,-MIRCCO_INSTRUMENT,-MIRCCO_MATURITY,-FREQUENCY,-Fréquence,-Temps)

df_intRates <- df_intRates %>% separate(TIME, sep = "-", into = c("Year","Semestre"))
df_intRates$Semestre<-as.integer(df_intRates$Semestre)
df_intRates$Semestre[df_intRates$Semestre<7] <- 1
df_intRates$Semestre[df_intRates$Semestre>6] <- 2
df_intRates$Year_Semestre <- paste(df_intRates$Year, df_intRates$Semestre, sep="_")
df_intRates <- df_intRates %>%
  group_by(Year_Semestre,Maturité) %>%
  summarise(Median_Value=median(Value))


df_intRates %>% 
  filter(grepl("_2",Year_Semestre))%>%
  ggplot(aes(x=Year_Semestre,y=Median_Value,group=Maturité,color=Maturité))+
  geom_line()+
  theme_minimal()
#conclusion interest rates have been steadily decreasing until this year

df_lr <- df %>%
  filter(Sqm=="Superficie inconnue" & Building.type=="Apartments")%>%
  select(-Building.type,-Sqm)

df_lm <-
  full_join(df_lr, filter(df_intRates,str_detect(Maturité,"supérieure à 5 ans")), by = "Year_Semestre")


mod1<-aov(Median.price ~ Commune_short,df_lm)
summary(mod1)
#t.test(Median.price ~ Commune_short,df_lm) #need just 2 groupings

#model=lm(Median.price ~ Median_Value + Commune_short,filter(df_lm,!Commune_short %in% c("Evere","Bxl","Schaerbeek","Berchem","Anderlecht","Koekelberg","Molenbeek","Saint-Gilles","Ganshoren","Jette","St-Josse")))
model=lm(Median.price ~ Median_Value + Commune_short,df_lm)

anova(model)
summary(model)
hist(model$residuals)
plot(model)


model2=lm(Median.price ~ Median_Value,filter(df_lm,Commune_short=="Ixelles"))

plot(model)
summary(model2)

df_lm %>%
  filter(Commune_short=="Ixelles") %>%
  ggplot(aes(x=Median_Value,y=Median.price))+
  geom_point() +
  theme_base()

#notes- read up and watch videos on R to understand how to read staticial values. 
##bring in next meausre


####MIR: Taux d'intérêt sur les nouveaux crédits  https://stat.nbb.be/Index.aspx?DataSetCode=MIRCCO&lang=fr#

####https://data.oecd.org/price/producer-price-indices-ppi.htm
###https://data.oecd.org/interest/short-term-interest-rates.htm