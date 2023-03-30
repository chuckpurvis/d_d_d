########################################################
#  dorling_election2020_south.r
#  2020 Presidential Election
#  Dorling county-level cartogram and
#  ggplot2 overlapping graduated symbols map
#   States = AR, LA, MS, AL, GA, SC, NC
#    - March 26, 2023 --
########################################################
# install.packages("cartogram")
# install.packages("crsuggest")
library(tidyverse)
library(sf)
library(cartogram)
# library(maptools)
# library(rgdal)
library(crsuggest)

setwd("~/Desktop/Density_Diversity_Democracy")

# Load my master county-level elections/demography file.

# n=3,115. Does not include Puerto Rico or most of Alaska!

master5 <- readRDS("usa_county_density_diversity_democracy_master.Rds")

########################################################################
library(tigris)
states1 <- tigris::states(year=2020,cb=TRUE) %>% 
           filter(STUSPS %in% c("AL","MS","GA","KY","NC","SC","TN","VA")) %>% 
           st_transform(32136)

temp1 <- master5 %>% 
  filter(STUSPS %in% c("AL","MS","GA","KY","NC","SC","TN","VA"))
#  filter(STATE_NAME=="New York")

suggest_crs(temp1) # get a listing of suggested coordinate references for this area
suggest_top_crs(temp1) # top suggested for this area

temp1_proj <- st_transform(temp1,32136) # Tennessee, NAD83

# Dorling cartogram

carto1 <- cartogram_dorling(temp1_proj, k=0.7, "plurality_abs")

ggplot() +
  geom_sf(data=temp1_proj, alpha=0.4, aes(fill=winner)) + 
  geom_sf(data=states1, fill=NA, alpha=1.0, color="black", linewidth=0.4) + 
  geom_sf(data=carto1, alpha=0.8, aes(fill=winner)) +  #, show.legend=FALSE)
  scale_fill_manual(values=c("blue", "red")) +
  theme_void() +

  labs(title = "2020 Presidential Election Results: Southern States",
       subtitle = "Size of Circle is Plurality of Winner in County",
       caption = "Created: CPurvis; Data sources: Census 2020 (via tidycensus), MIT Election Lab",
       fill = "Plurality of Votes by County") + 
  theme(
    legend.title = element_text(color="blue", size=8, face="bold"), 
    legend.text  = element_text(color="blue", size=7),
    plot.title   = element_text(color="blue", size=15, face="bold"),
    plot.subtitle= element_text(color="blue", size=14, face="bold"),
    plot.caption = element_text(color="blue",size=10,face="italic"))

setwd("~/Desktop/Density_Diversity_Democracy")

ggsave("south_county_presid_election_2020_2.png", plot=last_plot(),
       dpi=300,width=8,height=7) 

########################################################################
# ggplot2 graduated symbol map

centers1 <- sf::st_centroid(temp1_proj) %>%  # This is used for the graduated symbols
  arrange(desc(plurality_abs))          # sort by plurality in descending order

ggplot(data=temp1_proj) +
  geom_sf(data=temp1_proj, alpha=0.4, aes(fill=winner)) +  #, show.legend=FALSE) + 
  scale_fill_manual(values=c("blue", "red")) +
  geom_sf(data=states1, fill=NA, alpha=1.0, color="black", linewidth=0.4) + 
  theme_void() + 
  geom_sf(data=centers1, alpha=0.8, aes(fill=winner, size=plurality_abs),
          show.legend=FALSE,
          #  pch = 21   # filled circle "plotting character"
          shape = 21 # filled circle shape (newer version of pch)
  ) + 
  scale_size_area(max_size=12) +
  
  labs(title = "2020 Presidential Election Results: Midwestern States",
       subtitle = "Size of Circle is Plurality of Winner in County",
       caption = "Created: CPurvis; Data sources: Census 2020 (via tidycensus), MIT Election Lab",
       fill = "Plurality of Votes by County") + 
  theme(
    legend.title = element_text(color="blue", size=8, face="bold"), 
    legend.text  = element_text(color="blue", size=7),
    plot.title   = element_text(color="blue", size=15, face="bold"),
    plot.subtitle= element_text(color="blue", size=14, face="bold"),
    plot.caption = element_text(color="blue",size=10,face="italic"))

setwd("~/Desktop/Density_Diversity_Democracy")

ggsave("south_county_presid_election_2020_1_projected.png", plot=last_plot(),
       dpi=300,width=8,height=7) 

#########################################################################################
# Simple choropleth map, based on plurality of democratic or republican

ggplot(data=temp1_proj) +
  geom_sf(data=temp1_proj, alpha=0.4, aes(fill=winner)) +  #, show.legend=FALSE) + 
  scale_fill_manual(values=c("blue", "red")) +
  geom_sf(data=states1, fill=NA, alpha=1.0, color="black", linewidth=0.3) + 
  theme_void() + 
#  geom_sf(data=centers1, alpha=0.8, aes(fill=winner, size=plurality_abs),
#          show.legend=FALSE,
#          #  pch = 21   # filled circle "plotting character"
#          shape = 21 # filled circle shape (newer version of pch)
#  ) + 
  scale_size_area(max_size=30) +
  
  labs(title = "2020 Presidential Election Results: Midwestern States",
#       subtitle = "Size of Circle is Plurality of Winner in County",
       caption = "Created: CPurvis; Data sources: Census 2020 (via tidycensus), MIT Election Lab",
       fill = "Plurality of Votes by County") + 
  theme(
    legend.title = element_text(color="blue", size=8, face="bold"), 
    legend.text  = element_text(color="blue", size=7),
    plot.title   = element_text(color="blue", size=15, face="bold"),
    plot.subtitle= element_text(color="blue", size=14, face="bold"),
    plot.caption = element_text(color="blue",size=10,face="italic"))

setwd("~/Desktop/Density_Diversity_Democracy")

ggsave("south_county_presid_election_2020_0_choropleth_projected.png", plot=last_plot(),
       dpi=300,width=8,height=7)   
#########################################################################################
tempx <- temp1_proj %>% 
  sf::st_drop_geometry() %>%  # drop the geometry so we can write a nice csv file!
  select (GEOID, STATE_NAME, NAME.y, landarea_sqmi, totpop_sqmi,
          Diversity20, density_grp, TotalPop20, Hispanic20, NH_White20,
          NH_Black20, NH_AIAN20, NH_Asian20, NH_NHOPI20, NH_Other20,
          NH_Multi20, NH_AsianPI20, NH_Other320,
          Share_White20,Share_Black20,Share_AsianPI20,Share_Other320,
          Share_Hisp20,
          totalvotes, democrat, republican, other3,
          shr_demo, shr_rep, shr_oth3, winner, plurality_abs)   
write.csv(tempx, file="south_county_presid_election_data.csv")

########################################################################################
