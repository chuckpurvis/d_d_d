###################################################################
# PL94171_USA_County_Density.r  "Density and Diversity"
#  Use the R-package TIDYCENSUS to extract PL 94-171 
#   data from the Year 2020 Decennial Censuses
#  Entire USA, COUNTY
#     -- March 15, 2023 --
###################################################################
# install the current working version of tidycensus
# remotes::install_github("walkerke/tidycensus")
# install.packages("tidycensus")

# Load relevant libraries into each R-session.

library(tidyverse)
library(tidycensus)

# load the variable list for 2020 into a dataframe
varlist20 <- load_variables(2020,"pl",cache=FALSE)

selvars20  <- c(TotalPop20   = "P2_001N",   # Total Population
                Hispanic20   = "P2_002N",   # Hispanic or Latino
                NH_White20   = "P2_005N",   # Non-Hispanic, White alone
                NH_Black20   = "P2_006N",   # Non-Hispanic, Black or African American alone
                NH_AIAN20    = "P2_007N",   # Non-Hispanic, American Indian, Alaskan Native alone
                NH_Asian20   = "P2_008N",   # Non-Hispanic, Asian alone
                NH_NHOPI20   = "P2_009N",   # Non-Hispanic, Native Hawaiian, Other Pac Islander alone
                NH_Other20   = "P2_010N",   # Non-Hispanic, Other race alone
                NH_Multi20   = "P2_011N",   # Non-Hispanic, Two-or-More Races
                
                HousingUnits20  = "H1_001N", # Total Housing Units
                Occ_DU20        = "H1_002N", # Occupied Housing Units
                Vacant_DU20     = "H1_003N") # Vacant Housing Units
###############################################################################################
# Get selected PL94-171 variables for USA counties, Census 2020,
#   and calculate population density variables
usa_county  <- get_decennial(year=2020,  sumfile="pl", 
                                geography = "county",  # state="CA",
                                geometry=TRUE, keep_geo_vars=TRUE,
                                show_call = TRUE,output="wide", 
                                variables = selvars20) %>% 
                     replace(is.na(.),0) %>%  # replace all NA with 0
                     dplyr::mutate(totpop = TotalPop20,
                                   landarea_sqmi = ALAND / 2589988.1,
                                   totpop_sqmi  = totpop / landarea_sqmi) %>% 
                     replace(is.na(.),0)  %>%  # replace all NA with 0
                     dplyr::  mutate(density_grp = case_when(
                               totpop_sqmi < 500 ~    "1.rural",
                               totpop_sqmi < 1000 ~   "2.rural-suburb",
                               totpop_sqmi < 6000 ~   "3.suburb-disperse",
                               totpop_sqmi < 10000 ~  "4.suburb-dense",
                               totpop_sqmi < 20000 ~  "5.urban",
                               totpop_sqmi >= 20000 ~ "6.urban core",
                               TRUE ~ "0.missing data")) %>% 
                    dplyr:: relocate(totpop:density_grp, .after= NAME.y)

# tidyverse / dplyr script to calculate a shannon diversity index. mutate-at-work!!
usa_county <- mutate(usa_county,
                     
# Calculate two new variables for a five-group analysis
  NH_AsianPI20     = NH_Asian20 + NH_NHOPI20,
  NH_Other320      = NH_AIAN20  + NH_Other20 + NH_Multi20,
                     
# Share of Population by Five Race/Ethnicity Categories
  Share_White20    = NH_White20 / TotalPop20,
  Share_Black20    = NH_Black20 / TotalPop20,
  Share_AsianPI20  = NH_AsianPI20 / TotalPop20,
  Share_Other320   = NH_Other320   / TotalPop20,
  Share_Hisp20     = Hispanic20 / TotalPop20,
                     
# This is preferred if there are Shares == 0.000 
  Ln_G120 = ifelse(Share_White20 > 0, log(Share_White20) * Share_White20,0),
  Ln_G220 = ifelse(Share_Black20 > 0, log(Share_Black20) * Share_Black20,0),
  Ln_G320 = ifelse(Share_AsianPI20 > 0, log(Share_AsianPI20) * Share_AsianPI20,0),
  Ln_G420 = ifelse(Share_Other320 > 0, log(Share_Other320) * Share_Other320,0),
  Ln_G520 = ifelse(Share_Hisp20 > 0, log(Share_Hisp20) * Share_Hisp20,0),
                     
# Calculate a Shannon Diversity Index, based on the Five Race/Ethnic Categories
#   Multiplying the Diversity Index by 100 is optional
#   It now ranges from 0.0 (only one kind of race/ethnicity) 
#      to 1.0 (totally equal)
                     
Diversity20 = 100 * ((Ln_G120 + Ln_G220 + Ln_G320 + Ln_G420 + Ln_G520)/(log(0.2)))) %>% 
    dplyr::relocate(Diversity20, .after= totpop_sqmi) # move the resulting column....
# End of Mutate Step (Create Diversity Variables!)

usa_county_x <- usa_county %>% 
   sf::st_drop_geometry() # drop the geometry so we can write a nice csv file!

setwd("~/Desktop/pl94171/output")

write.csv(usa_county_x,  "census2020_pl94171_usa_county_density.csv")
######################################################################
install.packages("ggthemes")
install.packages("ggThemeAssist")
library(ggplot2)
library(ggthemes)
library(ggThemeAssist)

# Delete Puerto Rico from the scatterplot
usa_county2 <- usa_county %>% 
    filter(STATEFP != "72")

# Scatterplot of USA Counties by Diversity by Density!
ggplot(usa_county2, aes(x=totpop_sqmi, y=Diversity20, color=density_grp)) +
       geom_point(shape=18, size=2) +
       scale_x_continuous(trans="log10") +
   #    theme_bw() +
       ggtitle("United States Counties by Population Density\nby Racial / Ethnic Diversity: Census 2020") +
theme(
  plot.title=element_text(hjust=0.1, color="blue", size=10, face="bold"),
  plot.caption=element_text(color="blue",size=8,face="italic")) +
  
       labs(color='Density Group') + 
       xlab("Population Density (Persons per Square Mile)") +
       ylab("Racial/Ethnic Diversity (Shannon Index)") +
       labs(caption="Data Source: Census 2020, PL 94-171, via tidycensus")
    
ggsave("usa_county_density_diversity_plot1.png", plot=last_plot(),
       dpi=300,width=8,height=7)

########################################################################
# Bubble chart: USA County by Race/Ethnicity Diversity Index
#               by Population Density,
#     color of symbol = Density Group
#     size of symbol  = Total Population in 1000s.
########################################################################
#  Add a new variable, population in 1000s, using either DPLYR or Base R

usa_county <- usa_county %>% 
                mutate(totpopm = totpop / 1000.) %>% 
                filter(STATEFP != "72")

# usa_county$totpop000 <- usa_county$totpop / 1000.
########################################################################

ggplot(usa_county, aes(x=totpop_sqmi, y=Diversity20, size=totpopm, 
                         color=density_grp)) +
  geom_point(alpha=0.4,shape=19) +
  scale_x_continuous(trans="log10", labels = scales::comma) +
#  scale_x_continuous(labels = scales::comma) +
  scale_size(range = c(0.5, 24), name="Population (1000s)", labels = scales::comma) +
 # theme_bw() +
#  ggtitle("Figure 1\nDensity and Diverity in USA Counties:2020\nby Population Density by Racial / Ethnic Diversity") +
  
  theme(
    legend.title = element_text(color="tomato", size=8, face="bold"), 
    legend.text  = element_text(color="tomato", size=7),
    plot.title   = element_text(color="blue", size=14, face="bold"),
    plot.subtitle= element_text(color="blue", size=12, face="bold"),
    plot.caption = element_text(color="blue",size=8,face="italic"),
    axis.title   = element_text(color="blue",size = 9,face = "bold")) +

  # Add in annotations for San Francisco and Los Angeles!!
#  annotate(geom = "curve", x = 10000, y = 60, xend = 18000, yend = 83, 
#    curvature = .4, arrow = arrow(length = unit(2, "mm"))) +
#  annotate(geom = "text", x = 3500, y = 59, label = "San Francisco", 
#           hjust = "left") + 

#  annotate(geom = "curve", x = 1000, y = 70, xend = 2000, yend = 78, 
#           curvature = .4, arrow = arrow(length = unit(2, "mm"))) +
#  annotate(geom = "text", x = 800, y = 69, label = "Los Angeles", 
#           hjust = "left") +  

  labs(color='Density Group') + 
  xlab("Population Density (Persons per Square Mile)") +
  ylab("Racial/Ethnic Diversity (Shannon Index)") +
  labs(title="Figure 1\nDensity and Diversity in USA Counties: 2020",
       subtitle="by Population Density by Racial / Ethnic Diversity",
       caption="Data Source: Census 2020, PL 94-171, via tidycensus")
  
ggsave("usa_county_density_diversity_bubble_plot1.png", plot=last_plot(),
       dpi=300,width=8,height=7)

################################################################

ggplot(calif_county, aes(x=totpop_sqmi, y=Diversity20, size=totpopm, 
                         color=density_grp)) +
  geom_point(alpha=0.8,shape=19) +
  scale_x_continuous(trans="log10") + 
  
## extra characteristics of this bubblechart using ggThemeAssist addin:
  
  theme(
    plot.subtitle = element_text(size = 11, colour = "blue"), 
    axis.title = element_text(size = 9,face = "bold"), 
    plot.title = element_text(size = 14, face = "bold", colour = "blue"),
    panel.background = element_rect(fill = "gray84")) +
  
  labs(title = "Density and Diversity: California Counties in 2020",
    x = "Density (Total Population per Square Mile)",
    y = "Racial/Ethnic Diversity Index (Shannon Index)",
    colour = "Density Group", 
    size = "Total Population (1000s)",
    subtitle = "Population Density (persons per square mile) and Racial/Ethnic Diversity",
    caption = "Created by: Chuck Purvis, Hayward, CA, using Census 2020, PL 94-171, via tidycensus")

ggsave("county_density_diversity_bubble_2.png", plot=last_plot(),
       dpi=200)

###########################################################