########################################################
#  dorling_election2020_newyork.r
#  2020 Presidential Election
#  Dorling county-level cartogram and
#  ggplot2 overlapping graduated symbols map
#   State = New York
#    - March 24, 2023 --
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
temp1 <- master5 %>% 
  filter(STATE_NAME=="New York")

suggest_crs(temp1) # get a listing of suggested coordinate references for this area
suggest_top_crs(temp1) # top suggested for this area

temp1_proj <- st_transform(temp1,32128) # Pennsylvania North, NAD83

carto1 <- cartogram_dorling(temp1_proj,"plurality_abs")

ggplot() +
  geom_sf(data=temp1_proj, alpha=0.4, aes(fill=winner)) + 
  geom_sf(data=carto1, alpha=0.8, aes(fill=winner)) +  #, show.legend=FALSE)
  scale_fill_manual(values=c("blue", "red")) +
  theme_void() +

  labs(title = "2020 Presidential Election Results by New York County",
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

ggsave("newyork_county_presid_election_2020_2.png", plot=last_plot(),
       dpi=300,width=8,height=7) 

########################################################################
# ggplot2 graduated symbol map

centers1 <- sf::st_centroid(temp1_proj) %>%  # This is used for the graduated symbols
  arrange(desc(plurality_abs))          # sort by plurality in descending order

ggplot(data=temp1_proj) +
  geom_sf(data=temp1_proj, alpha=0.4, aes(fill=winner)) +  #, show.legend=FALSE) + 
  scale_fill_manual(values=c("blue", "red")) +
  theme_void() + 
  geom_sf(data=centers1, alpha=0.8, aes(fill=winner, size=plurality_abs),
          show.legend=FALSE,
          #  pch = 21   # filled circle "plotting character"
          shape = 21 # filled circle shape (newer version of pch)
  ) + 
  scale_size_area(max_size=30) +
  
  labs(title = "2020 Presidential Election Results by New York County",
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

ggsave("newyork_county_presid_election_2020_1_projected.png", plot=last_plot(),
       dpi=300,width=8,height=7)   
#########################################################################################
