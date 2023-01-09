library(tidyverse)
library(sf)        #Simple Features. This is the map-maker
library(ggthemes)  #Nice theme for maps
library(scales)    #to get the euro symbol
library(grid)      #to add graphs on one page
library(gridExtra) #to add graphs on one page

###Spatial Coding - Import Shp file and plot

shpfile <- "Brussels Real Estate/geo.be/municipality_3812.shp"
emptymap <- st_read(dsn = shpfile) #read the shp format into R
plot(emptymap$geometry) #plot the geometry column
emptymap <- emptymap %>%
  rename(Commune = namefre) #rename column so we can do the leftjoin

###DATA CODING
#Importing Municipality Date and first filter

df_raw <- read.csv("Brussels Real Estate/bestat_sales_brussels_20230106.csv")

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

#Filter out larger properties
df <-
  df %>%
    filter(
      !Building.type %in% c("Maisons avec 4 ou plus de façades (type ouvert)") &
      !Sqm %in% c("300-599m²", "600-999m²", "1000-1499m²", ">=1500m²")
          )
#Combine columns Year and Semestre
df$Year_Semestre <- paste(df$Year, substr(df$Semestre,1,1), sep="_")
df<- df %>%
  select(-Year,-Semestre)



###Plot creation

#P1 2022_1 apartments
P1 <-
  df %>%
  filter(Year_Semestre == "2022_1" & Building.type == "Apartments" & !is.na(Median.price))
P1 <- left_join(emptymap,P1, by = "Commune") %>%
  filter(!is.na(Year_Semestre)) |> select(tgid,everything())
  
#P2 All years Apartments
P2 <-
  df %>% filter(Building.type == "Apartments" & !is.na(Median.price))
P2 <- left_join(emptymap, P2, by = "Commune") %>%
  filter(!is.na(Year_Semestre))

#P3 % Median Price increase Apartments
df_pcinc_appart <- df %>% 
  select(Commune,Commune_short,Year_Semestre,Building.type,Sqm,Median.price) %>%
  filter(Building.type=="Apartments" & Sqm=="Superficie inconnue") %>% 
  pivot_wider(names_from = Year_Semestre,values_from = Median.price)

colnames(df_pcinc_appart)[5:29] <- paste("year_",colnames(df_pcinc_appart)[5:29],"")
df_pcinc_appart <- df_pcinc_appart %>% 
  mutate('increase2010_2022'=as.integer(((`year_ 2022_1 `-`year_ 2010_1 `)/`year_ 2022_1 `)*100),
         'increase2016_2022'=as.integer(((`year_ 2022_1 `-`year_ 2016_1 `)/`year_ 2022_1 `)*100),
         'increase2020_2022'=as.integer(((`year_ 2022_1 `-`year_ 2020_1 `)/`year_ 2022_1 `)*100),
         'increase2010_2016'=as.integer(((`year_ 2015_1 `-`year_ 2010_1 `)/`year_ 2016_1 `)*100),
         'increase2015_2020'=as.integer(((`year_ 2020_1 `-`year_ 2015_1 `)/`year_ 2020_1 `)*100)
         )

P3 <-
  left_join(emptymap, df_pcinc_appart, by = "Commune") %>%
  filter(!is.na(increase2010_2022))


#P4 2022 house
P4 <-
  df %>%
  filter(Year_Semestre == "2022_1" &
           grepl('Terraced', Building.type) &
           grepl("100",Sqm) &
           !is.na(Median.price))
P4 <- 
  left_join(emptymap, P4, by = "Commune") %>%
  filter(!is.na(Year_Semestre))



###Create Visuals
#Blank plot
P1_visual <- ggplot(P1) +
  geom_sf(aes(fill = Commune)) +
  coord_sf(datum = NA) +
  geom_sf_label(aes(label = Commune_short), label.size = NA) +
    theme(plot.title = element_text(hjust=0.5,size=20),
        plot.subtitle = element_text(hjust = 0.5,size = 15),
        plot.caption = element_text(hjust = 0.5,size = 10),
        legend.position = "none",
        panel.background = element_rect(fill="lightgrey"))
P1_visual




#MAP Df_2022 Apartments
theme_set(theme_map(base_size = 20)) #Set the map theme

P1_visual <- ggplot(P1) +
  geom_sf(aes(fill = Median.price)) +
  coord_sf(datum = NA) +
  scale_fill_distiller(type = "seq",
                       palette = "RdYlGn",
                       name = "Euros",
                       direction = 1,
                       na.value = "grey",
                       guide = guide_legend (reverse=TRUE),
                       labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  geom_sf_label(aes(label = Commune_short), label.size = NA) +
  labs( subtitle = "Apartments",
        caption = "Data: Belgian national statistical office") +
  theme(legend.position = c(0.01, 0.01),
        legend.key.size = unit(5, "mm"),
        legend.text = element_text(size = 7),
        legend.title = element_text(size=10,face="bold"),
        legend.background = element_rect(fill = 'transparent'),
        plot.title = element_text(hjust=0.5,size=20),
        plot.subtitle = element_text(hjust = 0.5,size = 15),
        plot.caption = element_text(hjust = 0.5,size = 10),
        panel.background = element_rect(fill="lightgrey"))
P1_visual

#conclusion: South and East are the most expensive areas for apartments with west being cheapest
#Map Df_2022 House
theme_set(theme_map(base_size = 20)) #Set the map theme
P4_visual <- ggplot(P4) +
  geom_sf(aes(fill = Median.price)) +
  coord_sf(datum = NA) +
  scale_fill_distiller(type = "seq",
                       palette = "RdYlGn",
                       name = "Euros",
                       direction = 1,
                       na.value = "grey",
                       guide = guide_legend (reverse=TRUE),
                       labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  geom_sf_label(aes(label = Commune_short), label.size = NA) +
  labs(subtitle = "Houses (100-299sqm)",
       caption = "Data: Belgian national statistical office") +
  theme(legend.position = c(0.01, 0.01),
        legend.key.size = unit(5, "mm"),
        legend.text = element_text(size = 7),
        legend.background = element_rect(fill='transparent'),
        legend.title = element_text(size=10,face="bold"),
        plot.title = element_text(hjust=0.5,size=20),
        plot.caption = element_text(hjust = 0.5,size = 10),
        plot.subtitle = element_text(hjust = 0.5,size = 15),
        panel.background = element_rect(fill="lightgrey"))

P4_visual

#conclusion: South and East most expensive for average house while Ixelles is by far the most popular


#MAP Dat_increase 2010 2022
theme_set(theme_map(base_size = 20)) #Set the map theme
P3_2010_2022_visual <- ggplot(P3) +
  geom_sf(aes(fill = increase2010_2022)) +
  coord_sf(datum = NA) +
  scale_fill_distiller(type = "seq",
                       palette = "RdYlGn",
                       name = "%",
                       direction = 1,
                       na.value = "grey",
                       guide = guide_legend (reverse=TRUE),
                       labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  geom_sf_label(aes(label = Commune_short), label.size = NA) +
  labs(subtitle = "2010 to 2022",
       title = "% increase in Median Sale Price\nof Apartments by Brussels' Commune") +
  theme(legend.position = c(0.01, 0.01),
        legend.key.size = unit(5, "mm"),
        legend.title = element_text(size=15,face="bold"),
        legend.background = element_rect(fill = "transparent"),
        plot.title = element_text(hjust=0.5,size=17),
        plot.subtitle = element_text(hjust = 0.5,size = 15),
        plot.caption = element_text(hjust = 0.5,size = 10),
        panel.background = element_rect(fill="snow"))

#conclusion WSP one of the most expensive has not see big increase

#MAP Dat_increase 2016 2022
P3_2016_2022_visual <- ggplot(P3) +
  geom_sf(aes(fill = increase2016_2022)) +
  coord_sf(datum = NA) +
  scale_fill_distiller(type = "seq",
                       palette = "RdYlGn",
                       limits=c(-1,40),
                       name = "%",
                       direction = 1,
                       na.value = "grey",
                       guide = guide_legend (reverse=TRUE),
                       labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  geom_sf_label(aes(label = Commune_short), label.size = NA) +
  labs( subtitle = "2016 to 2022")+
       # caption = "Data: Median Price increase from 2016 to 2022") +
  theme(legend.position = c(0.01, 0.01),
        legend.key.size = unit(5, "mm"),
        legend.title = element_text(size=10,face="bold"),
        legend.text = element_text(size = 7),
        legend.background = element_rect(fill = "transparent"),
        plot.title = element_text(hjust=0.5,size=20),
        plot.subtitle = element_text(hjust = 0.5,size = 15),
        plot.caption = element_text(hjust = 0.5,size = 10),
        panel.background = element_rect(fill="snow"))



#MAP Dat_increase 2010 2016
P3_2010_2016_visual <- ggplot(P3) +
  geom_sf(aes(fill = increase2010_2016)) +
  coord_sf(datum = NA) +
  scale_fill_distiller(type = "seq",
                       palette = "RdYlGn",
                       limits=c(-1,40),
                       name = "%",
                       direction = 1,
                       na.value = "grey",
                       guide = guide_legend (reverse=TRUE),
                       labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  geom_sf_label(aes(label = Commune_short), label.size = NA) +
  labs(subtitle = "2010 to 2016")+
       # caption = "Data: Median Price increase from 2010 to 2016") +
  theme(legend.position = c(0.01, 0.01),
        legend.key.size = unit(5, "mm"),
        legend.title = element_text(size=10,face="bold"),
        legend.text = element_text(size = 7),
        legend.background = element_rect(fill = "transparent"),
        plot.title = element_text(hjust=0.5,size=20),
        plot.subtitle = element_text(hjust = 0.5,size = 15),
        plot.caption = element_text(hjust = 0.5,size = 10),
        panel.background = element_rect(fill="snow"))

#conclusion starting to see increas in the north


perc_inc_grid1 <- grid.arrange(P3_2010_2016_visual,
             P3_2016_2022_visual,
                      nrow=1,
             top = textGrob("% increase in Median Sale Price\nof Apartments by Brussels' Commune",gp=gpar(fontsize=17,font=3))
             )

P3_2010_2022_visual

grid_2022 <- grid.arrange(P1_visual,
                              P4_visual,
                              nrow=1,
                              top = textGrob("Median Sale Price 1st Semestre 2022\nby Brussels' Commune\n",gp=gpar(fontsize=17,font=3))
)

# ####Below I am working on, i would like to create a bar chart of all communes showing median price as well as lines for quartiles
# 
# pivot_apartments <- df %>% filter(Building.type == "Apartments" &
#                                     !is.na(Median.price) &
#                                     Year_Semestre == "2022_1") |> 
#                           select("Commune_short","Median.price","X1st.quartile","X3rd.quartile") |> 
#                           pivot_longer(!Commune_short,names_to = "Value_Type",values_to = "Price") 
# 
# pivot_apartments |> filter(Commune_short=="Anderlecht" | Commune_short=="WSP") |>  
# ggplot(aes(x=reorder(Commune_short,Price),y=Price, fill=Value_Type))+
#   geom_bar(stat = "identity",position = "dodge")+
#   theme_base()
# 
# #LINE Dat_all communes
# P2 %>% 
#   filter(Commune_short %in% c("Jette","Ixelles","WSP","Anderlecht","St-Josse") & grepl("_1",Year_Semestre)) %>%
#   ggplot(aes(x=as.factor(Year_Semestre),y=Median.price,group=Commune_short,color=Commune_short)) +
#   geom_line(size=1.5) +
#   theme_base() +
#   scale_y_continuous(name="Average Price", labels=dollar_format(suffix="€",prefix="")) +
#   scale_x_discrete(name="Year")+
#   ggthemes::theme_economist()
# 
# P2
# 
# #gap between south and east is the same if not getting bigger. we also see Ixelles since 2107 increasing a great deal  
# 
# 
# view(P1)
  
###MODEL1 -Adding Taux d'interets

df_intRates_raw <- read.csv("Brussels Real Estate/MIRCCO_30102022155824566.csv")

df_intRates <- df_intRates_raw %>% 
  filter(Région=="Belgique",Instrument=="Inférieur à 1 million d'euros", !startsWith(Maturité,"Taux")) %>%
  select(-Région ,-Flags,-Flag.Codes,-MIRCCO_AREA,-MIRCCO_SECTOR,-Secteur,-MIRCCO_INSTRUMENT,-MIRCCO_MATURITY,-FREQUENCY,-Fréquence,-Temps) |> 
  rename(InterestRate = Maturité)

df_intRates <- df_intRates %>% separate(TIME, sep = "-", into = c("Year","Semestre"))
df_intRates$Semestre<-as.integer(df_intRates$Semestre)
df_intRates$Semestre[df_intRates$Semestre<7] <- 1
df_intRates$Semestre[df_intRates$Semestre>6] <- 2
df_intRates$Year_Semestre <- paste(df_intRates$Year, df_intRates$Semestre, sep="_")
df_intRates <- df_intRates %>%
  group_by(Year_Semestre,InterestRate) %>%
  summarise(Median_Value=median(Value))

df_intRates$InterestRate[df_intRates$InterestRate == "Fixation initiale du taux d'une durée supérieure à 5 ans"] <- "Fixed rate for longer than 5 years"
df_intRates$InterestRate[df_intRates$InterestRate == "Fixation initiale du taux d'une durée supérieure à 1 an et inférieure à 5 ans"] <- "Fixed rate between 1 and 5 years"



df_intRates %>% 
  filter(grepl("_2",Year_Semestre))%>%
  ggplot(aes(x=Year_Semestre,y=Median_Value,group=InterestRate,color=InterestRate))+
  geom_line()+
  theme_minimal()
#conclusion interest rates have been steadily decreasing until this year
####MIR: Taux d'intérêt sur les nouveaux crédits  https://stat.nbb.be/Index.aspx?DataSetCode=MIRCCO&lang=fr#
