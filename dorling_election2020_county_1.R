########################################################
#  dorling_election2020_county_1.r
#  Dorling county-level cartograms of 2020 Presidential Election
#   by States & State Groups
#    - March 23, 2023 --
########################################################
install.packages("cartogram")
install.packages("crsuggest")
library(tidyverse)
library(sf)
library(cartogram)
library(maptools)
library(rgdal)
library(crsuggest)
setwd("~/Desktop/Density_Diversity_Democracy")

# Load my master county-level elections/demography file.

# n=3,115. Does not include Puerto Rico or most of Alaska!

master5 <- readRDS("usa_county_density_diversity_democracy_master.Rds")

########################################################################
temp1 <- master5 %>% 
  filter(STATE_NAME=="California")

suggest_crs(temp1) # get a listing of suggested coordinate references for this area
suggest_top_crs(temp1) # top suggested for California

temp1_proj <- st_transform(temp1,3310) # California Albers, NAD83(2011)

carto1 <- cartogram_dorling(temp1_proj,"plurality_abs")

ggplot() +
  geom_sf(data=temp1_proj, alpha=0.4, aes(fill=winner)) + 
  geom_sf(data=carto1, alpha=0.8, aes(fill=winner)) +  #, show.legend=FALSE)
  scale_fill_manual(values=c("blue", "red")) +
  theme_void() +

  labs(title = "2020 Presidential Election Results by California County",
       subtitle = "Size of Circle is Plurality of Winner in County",
       caption = "Created: CPurvis; Data sources: Census 2020 (via tidycensus), MIT Election Lab",
       fill = "Plurality of Votes by County") + 
  theme(
    legend.title = element_text(color="blue", size=8, face="bold"), 
    legend.text  = element_text(color="blue", size=7),
    plot.title   = element_text(color="blue", size=15, face="bold"),
    plot.subtitle= element_text(color="blue", size=14, face="bold"),
    plot.caption = element_text(color="blue",size=12,face="italic"))

setwd("~/Desktop/Density_Diversity_Democracy")

ggsave("california_county_presid_election_2020_2.png", plot=last_plot(),
       dpi=300,width=8,height=7) 

########################################################################
temp2 <- master5 %>% 
  filter(STATE_NAME=="Texas")

suggest_top_crs(temp2) # get a listing of suggested coordinate references for this area

temp2_proj <- st_transform(temp2,3083) # Texas Centric Albers Equal Area, NAD83(2011)

carto2 <- cartogram_dorling(temp2_proj,"plurality_abs")

ggplot() +
  geom_sf(data=temp2_proj, alpha=0.4, aes(fill=winner)) + 
  geom_sf(data=carto2, alpha=0.8, aes(fill=winner)) +  #, show.legend=FALSE)
  scale_fill_manual(values=c("blue", "red")) +
  theme_void() +
  
  labs(title = "2020 Presidential Election Results by Texas County",
       subtitle = "Size of Circle is Plurality of Winner in County",
       caption = "Created: CPurvis; Data sources: Census 2020 (via tidycensus), MIT Election Lab",
       fill = "Plurality of Votes by County") + 
  theme(
    legend.title = element_text(color="blue", size=8, face="bold"), 
    legend.text  = element_text(color="blue", size=7),
    plot.title   = element_text(color="blue", size=15, face="bold"),
    plot.subtitle= element_text(color="blue", size=14, face="bold"),
    plot.caption = element_text(color="blue",size=12,face="italic"))

setwd("~/Desktop/Density_Diversity_Democracy")

ggsave("texas_county_presid_election_2020_2.png", plot=last_plot(),
       dpi=300,width=8,height=7)   
########################################################################
temp3 <- master5 %>% 
  filter(STUSPS %in% c("AZ","CO","NM","UT"))

suggest_top_crs(temp3) # get a listing of suggested coordinate references for this area

temp3_proj <- st_transform(temp3,32114) # New Mexico West, NAD83(2011)

carto3 <- cartogram_dorling(temp3_proj,"plurality_abs")

ggplot() +
  geom_sf(data=temp3_proj, alpha=0.4, aes(fill=winner)) + 
  geom_sf(data=carto3, alpha=0.8, aes(fill=winner)) +  #, show.legend=FALSE)
  scale_fill_manual(values=c("blue", "red")) +
  theme_void() +
  
  labs(title = "2020 Presidential Election Results by Counties in the Four Corner States",
       subtitle = "Size of Circle is Plurality of Winner in County",
       caption = "Created: CPurvis; Data sources: Census 2020 (via tidycensus), MIT Election Lab",
       fill = "Plurality of Votes by County") + 
  theme(
    legend.title = element_text(color="blue", size=8, face="bold"), 
    legend.text  = element_text(color="blue", size=7),
    plot.title   = element_text(color="blue", size=15, face="bold"),
    plot.subtitle= element_text(color="blue", size=14, face="bold"),
    plot.caption = element_text(color="blue",size=12,face="italic"))

setwd("~/Desktop/Density_Diversity_Democracy")

ggsave("four_corner_county_presid_election_2020_2.png", plot=last_plot(),
       dpi=300,width=8,height=7)   