########################################################
#  election2020_county_1.r
#  Import the 2020 US Presidential Election Results
#    by US County from the MIT Election Data site.
#  Merge with 2020 Census data on diversity, density;
#  Produce various scatterplots / bubblecharts / maps
#    - March 18-22, 2023 --
########################################################
library(tidyverse)
library(crsuggest)
library(sf)
# library(ggplot2)

setwd("~/Desktop/Density_Diversity_Democracy/dataverse_files")

# This is the original file from the MIT Election Labs.....
#  n=72,617 records
master1 <- read.csv ("countypres_2000-2020.csv")

temp1 <- master1 %>% 
  filter(year==2020) # save just the 2020 data.
  
temp2 <- temp1 %>% 
  select (-candidate,-office,-version) # delete some extra columns

temp3 <- pivot_wider(temp2,names_from=c(party),
                              values_from=c(candidatevotes))

# The following step summarizes subtotals within county ('mode'), to county-level.
# "mode" subtotals include categories such as EARLY VOTE, PROVISIONAL, ELECTION DAY,
#         ABSENTEE

temp4 <- temp3 %>% 
         group_by(year,state,state_po,county_name,county_fips,totalvotes) %>% 
  
         replace(is.na(.),0) %>%  # replace all NA with 0
  
         summarize(democrat    = sum(DEMOCRAT),
                   republican  = sum(REPUBLICAN),
                   other       = sum(OTHER),
                   libertarian = sum(LIBERTARIAN),
                   green       = sum(GREEN))
###############################################################################
#  Check the Data at State-Level....

statesum <- temp4 %>% 
        group_by(state) %>% 
        summarize(democrat = sum(democrat),
                  republican = sum(republican),
                  other = sum(other),
                  libertarian = sum(libertarian),
                  green = sum(green),
                  total = sum(totalvotes))

statesum2 <- statesum %>% 
      mutate(checktotal = democrat + republican + other + libertarian + green,
             demo_plurality = democrat - republican,
             other3 = other + libertarian + green,
             shr_demo = democrat / total,
             shr_rep  = republican / total,
             shr_oth3 = other3 / total)

setwd("~/Desktop/Density_Diversity_Democracy")
write.csv (statesum2,"usa_county_summed2state_presidential_2020.csv")

# Create a new master 2020 county-level results file!

master2 <- temp4 %>% 
  mutate(checktotal = democrat + republican + other + libertarian + green,
         demo_plurality = democrat - republican,
         other3 = other + libertarian + green,
         total = totalvotes,
         shr_demo = democrat / total,
         shr_rep  = republican / total,
         shr_oth3 = other3 / total,
         winner = case_when(
                   demo_plurality >= 0 ~ "democrat",
                   demo_plurality <  0 ~ "republican"),
         plurality_abs = abs(demo_plurality)
         )

# Update one cell to give correct county_fips for District of Columbia!
master2 <- master2 %>% 
     mutate(county_fips = replace(county_fips, county_name=="DISTRICT OF COLUMBIA",11001))

setwd("~/Desktop/Density_Diversity_Democracy")
write.csv (master2,"usa_county_presidential_2020_edited.csv")
 
#############################################################################
# Merge together the PL94-171 county-level table 
#  Created in my script PL94171_USA_County_Density.R
#   It has N=3,221 and the master2 (election results) is N=3155
#############################################################################
#create a new variable, for joining purposes!!
# Use the census file WITH geometry, for mapping purposes!
usa_county$county_fips <- as.numeric(usa_county$GEOID)

master3 <- left_join(usa_county, master2, by="county_fips")

# The MIT electionlab data does not include Alaskan Voting Districts or 
#   Puerto Rico municipios.
check1 <- master3 %>% 
     filter(is.na(totalvotes)) # This returns records for PR and Alaska

check2 <- master3 %>% 
     filter(!is.na(totalvotes)) # This returns records for 49 states + DC

master4 <- check2 %>%     # keep selected columns, only
  select (GEOID,STATE_NAME,NAME.y,totpop,landarea_sqmi,totpop_sqmi,
          Diversity20,density_grp,
          totalvotes, democrat, republican, other3,
          winner, plurality_abs) 

setwd("~/Desktop/Density_Diversity_Democracy")
write.csv(master4,  "usa_county_density_diversity_democracy_master.csv")

# Save my master check2 file to hard drive!!
# Use either saveRDS or save function to save an R file, 
#  and readRDS or load function to load R files into workapce

# I think "RDS" means R Data Single dataframe....

saveRDS(check2,  "usa_county_density_diversity_democracy_master.Rds")

# Load the RDS (single datafame save R-file) into workspace
master5 <- readRDS("usa_county_density_diversity_democracy_master.Rds")

###################################################################################
# Create scatterplots of density, diversity, 2020 presidential election plurality

## 
library(ggplot2)

# Try labeling very specific bubbles!!!!
# learning this technique from this web page:
# http://r-graph-gallery.com/web-scatterplot-corruption-and-human-development.html
########################################################################################
install.packages("ggrepel")
library(ggrepel)

mycounties <- c("Queens County, New York","Kings County, New York",
                "New York County, New York","Bronx County, New York",
                "Richmond County, New York",
                "Los Angeles County, California","San Francisco County, California",
                "Cook County, Illinois", "Hudson County, New Jersey",
                "Suffolk County, Massachusetts", "District of Columbia, District of Columbia",
                "Philadelphia County, Pennsylvania")
master5 <- master5 %>% 
    mutate(mylabels = ifelse(NAME.y %in% mycounties, NAME.y,"")) %>% 
    mutate(mylabels2 = case_when(
             mylabels=="Queens County, New York"  ~ "Queens",
             mylabels=="Kings County, New York"   ~ "Brooklyn",
             mylabels=="New York County, New York"~ "Manhattan",
             mylabels=="Bronx County, New York"   ~ "Bronx",
             mylabels=="Richmond County, New York"~ "Staten Island",
             mylabels=="Los Angeles County, California"  ~ "Los Angeles",
             mylabels=="San Francisco County, California"~ "San Francisco",
             mylabels=="Cook County, Illinois" ~ "Chicago (Cook Co)",
             mylabels=="Hudson County, New Jersey" ~ "Hudson Co, NJ",
             mylabels=="Suffolk County, Massachusetts" ~ "Boston (Suffolk Co)",
             mylabels=="District of Columbia, District of Columbia" ~ "Washington, DC",
             mylabels=="Philadelphia County, Pennsylvania" ~ "Philadelphia",
             mylabels=="" ~ ""))
# In hindsight, it would be easier just to create the new labels with case_when
#   and a lookup of GEOID to produce labels, e.g., Brooklyn, Manhattan, etc.
##################################################################################
# Figure 1: Density, Diversity, Plurality: All Counties, n=3115
ggplot(check2, aes(x=totpop_sqmi, y=Diversity20, size=plurality_abs, 
                       color=winner)) +
  geom_point(alpha=0.3,shape=19) +
  scale_x_continuous(trans="log10", labels = scales::comma) +
  scale_size(range = c(0.5, 24), name="Plurality of 2020 Votes", labels = scales::comma) +
  scale_color_manual(values = c("blue", "red")) +
  theme(
    legend.title = element_text(color="tomato", size=8, face="bold"), 
    legend.text  = element_text(color="tomato", size=7),
    plot.title   = element_text(color="blue", size=14, face="bold"),
    plot.subtitle= element_text(color="blue", size=11, face="bold"),
    plot.caption = element_text(color="blue",size=8,face="italic"),
    axis.title   = element_text(color="blue",size = 9,face = "bold")) +
  
 annotate(geom = "text", x = 0.1, y = 98, hjust = "left",
          label = "All US Counties (n=3,115)") +
                
  labs(color='Winner of County') + 
  xlab("Population Density (Persons per Square Mile)") +
  ylab("Racial/Ethnic Diversity (Shannon Index)") +
  labs(title="Figure 1\nDemocracy, Density and Diversity in USA Counties: 2020",
       subtitle="Plurality (2020 Pres Election) by Population Density by Racial / Ethnic Diversity",
       caption="Data Source: Census 2020, PL 94-171, via tidycensus; MIT Election Labs")

setwd("~/Desktop/Density_Diversity_Democracy")

ggsave("usa_county_democracy_density_diversity_bubble_plot1.png", plot=last_plot(),
       dpi=300,width=8,height=7)

#################################################################################
#  Figure 2
 # Filter (select) only the most densely populated US Counties.....

check3 <- master5 %>%  
  filter(totpop_sqmi > 1000.)

ggplot(check3, aes(x=totpop_sqmi, y=Diversity20, size=plurality_abs, 
                   color=winner)) +
  geom_point(alpha=0.3,shape=19) +
  scale_x_continuous(trans="log10", labels = scales::comma) +
  scale_size(range = c(0.5, 24), name="Plurality of 2020 Votes", labels = scales::comma) +
  scale_color_manual(values = c("blue", "red")) +
  
  geom_text_repel(aes(label=mylabels2), color="black",size=11/.pt,
                  point.padding=0.1, box.padding=0.6, 
                  min.segment.length = 0, max.overlaps = 1000,
                  seed = 7654) + 
  
  theme(
    legend.title = element_text(color="tomato", size=8, face="bold"), 
    legend.text  = element_text(color="tomato", size=7),
    plot.title   = element_text(color="blue", size=14, face="bold"),
    plot.subtitle= element_text(color="blue", size=11, face="bold"),
    plot.caption = element_text(color="blue",size=8,face="italic"),
    axis.title   = element_text(color="blue",size = 9,face = "bold")) +
  
  annotate(geom = "text", x = 1000, y = 98, hjust = "left",
           label = "Just US Counties with > 1000 Population Density (n=154)") +
  
  labs(color='Winner of County') + 
  xlab("Population Density (Persons per Square Mile)") +
  ylab("Racial/Ethnic Diversity (Shannon Index)") +
  labs(title="Figure 2\nDemocracy, Density and Diversity in USA Counties: 2020",
       subtitle="Plurality (2020 Pres Election) by Population Density by Racial / Ethnic Diversity",
       caption="Data Source: Census 2020, PL 94-171, via tidycensus; MIT Election Labs")

setwd("~/Desktop/Density_Diversity_Democracy")

ggsave("usa_county_democracy_density_diversity_bubble_plot2_labeled.png", plot=last_plot(),
       dpi=300,width=8,height=7)

#################################################################################
# Figure 3
# Filter (select) just US counties with greater than National Average Diversity Index

check4 <- master5 %>%  
  filter(Diversity20 > 75.)

ggplot(check4, aes(x=totpop_sqmi, y=Diversity20, size=plurality_abs, 
                   color=winner)) +
  geom_point(alpha=0.3,shape=19) +
  scale_x_continuous(trans="log10", labels = scales::comma) +
  scale_size(range = c(0.5, 24), name="Plurality of 2020 Votes", labels = scales::comma) +
  scale_color_manual(values = c("blue", "red")) +
  
  geom_text_repel(aes(label=mylabels2), color="black",size=11/.pt,
                  point.padding=0.1, box.padding=0.6, 
                  min.segment.length = 0, max.overlaps = 10000,
                  seed = 7654) +  
  
  theme(
    legend.title = element_text(color="tomato", size=8, face="bold"), 
    legend.text  = element_text(color="tomato", size=7),
    plot.title   = element_text(color="blue", size=14, face="bold"),
    plot.subtitle= element_text(color="blue", size=11, face="bold"),
    plot.caption = element_text(color="blue",size=8,face="italic"),
    axis.title   = element_text(color="blue",size = 9,face = "bold")) +
  
  annotate(geom = "text", x = 0.1, y = 98, hjust = "left",
           label = "Just US Counties with > 75.0 Diversity Index (n=112)") +
  
  labs(color='Winner of County') + 
  xlab("Population Density (Persons per Square Mile)") +
  ylab("Racial/Ethnic Diversity (Shannon Index)") +
  labs(title="Figure 3\nDemocracy, Density and Diversity in USA Counties: 2020",
       subtitle="Plurality (2020 Pres Election) by Population Density by Racial / Ethnic Diversity",
       caption="Data Source: Census 2020, PL 94-171, via tidycensus; MIT Election Labs")

setwd("~/Desktop/Density_Diversity_Democracy")

ggsave("usa_county_democracy_density_diversity_bubble_plot3_labeled.png", plot=last_plot(),
       dpi=300,width=8,height=7)

#################################################################################
# Figure 4
# Filter (select) just the LOW DENSITY and LOW DIVERSITY US Counties.....

check5 <- check2 %>%  
  filter(Diversity20 <= 75.) %>% 
  filter(totpop_sqmi <= 1000.)

ggplot(check5, aes(x=totpop_sqmi, y=Diversity20, size=plurality_abs, 
                   color=winner)) +
  geom_point(alpha=0.3,shape=19) +
  scale_x_continuous(trans="log10", labels = scales::comma) +
  scale_size(range = c(0.5, 12), name="Plurality of 2020 Votes", labels = scales::comma) +
  scale_color_manual(values = c("blue", "red")) +
  theme(
    legend.title = element_text(color="tomato", size=8, face="bold"), 
    legend.text  = element_text(color="tomato", size=7),
    plot.title   = element_text(color="blue", size=14, face="bold"),
    plot.subtitle= element_text(color="blue", size=11, face="bold"),
    plot.caption = element_text(color="blue",size=8,face="italic"),
    axis.title   = element_text(color="blue",size = 9,face = "bold")) +
  
  annotate(geom = "text", x = 0.1, y = 74, hjust = "left",
           label = "Low Diversity + Low Density Counties (n=2,921)") +
  
  labs(color='Winner of County') + 
  xlab("Population Density (Persons per Square Mile)") +
  ylab("Racial/Ethnic Diversity (Shannon Index)") +
  labs(title="Figure 4\nDemocracy, Density and Diversity in USA Counties: 2020",
       subtitle="Plurality (2020 Pres Election) by Population Density by Racial / Ethnic Diversity",
       caption="Data Source: Census 2020, PL 94-171, via tidycensus; MIT Election Labs")

setwd("~/Desktop/Density_Diversity_Democracy")

ggsave("usa_county_democracy_density_diversity_bubble_plot4.png", plot=last_plot(),
       dpi=300,width=8,height=7)

#####################################################################################
# Map #1 - California

check2 <- readRDS("usa_county_density_diversity_democracy_master.Rds")

temp_ca <- check2 %>% 
  filter(STUSPS=="CA")

# suggest_crs(temp_ca) # get a listing of suggested coordinate references for this area
# suggest_top_crs(temp_ca) # top suggested for California
temp_ca <- st_transform(temp_ca,3310) # California Albers, NAD83(2011)

center_ca <- sf::st_centroid(temp_ca) %>%  # This is used for the graduated symbols
          arrange(desc(plurality_abs)) # sort by plurality in descending order,
                                       # or big circles may hide small circles!!
      #   arrange(plurality_abs)

ggplot(data=temp_ca) +
  geom_sf(data=temp_ca, alpha=0.4, aes(fill=winner)) +  #, show.legend=FALSE) + 
  scale_fill_manual(values=c("blue", "red")) +
  theme_void() + 
  geom_sf(data=center_ca, alpha=0.8, aes(fill=winner, size=plurality_abs),
          show.legend=FALSE,
  #  pch = 21   # filled circle "plotting character"
     shape = 21 # filled circle shape (newer version of pch)
  ) + 
  scale_size_area(max_size=30) +

#  scale_size(range = c(0, 30)) +  # This works same as max_size=30
#  scale_fill_manual(values=c("purple4", "salmon4")) +
#  scale_color_manual(values = c("cyan", "magenta")) +
  
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
  
  ggsave("california_county_presid_election_2020_1_diamond.png", plot=last_plot(),
         dpi=300,width=8,height=7)  
  
###############################################################################
# Texas Map!
  temp_tx <- check2 %>% 
    filter(STUSPS=="TX")

# suggest_crs(temp_ca) # get a listing of suggested coordinate references for this area
# suggest_top_crs(temp_ca) # top suggested for California
  temp_tx <- st_transform(temp_tx,3083) # Texas Centric Albers Equal Area, NAD83(2011)
  
  centers_tx <- sf::st_centroid(temp_tx) %>%  # This is used for the graduated symbols
    arrange(desc(plurality_abs))              # sort by plurality in descending order

  ggplot(data=temp_tx) +
    geom_sf(data=temp_tx, alpha=0.4, aes(fill=winner)) +  #, show.legend=FALSE) + 
    scale_fill_manual(values=c("blue", "red")) +
    theme_void() + 
    geom_sf(data=centers_tx, alpha=0.8, aes(fill=winner, size=plurality_abs),
            show.legend=FALSE,
            #  pch = 21   # filled circle "plotting character"
            shape = 21 # filled circle shape (newer version of pch)
    ) + 
    scale_size_area(max_size=30) +

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
  
  ggsave("texas_county_presid_election_2020_1_projected.png", plot=last_plot(),
         dpi=300,width=8,height=7)   
#########################################################################################
# Florida map!
  temp_fl <- check2 %>% 
    filter(STUSPS=="FL")
  
  centers_fl <- sf::st_centroid(temp_fl) %>%  # This is used for the graduated symbols
    arrange(desc(plurality_abs))              # sort by plurality in descending order
  
  ggplot(data=temp_fl) +
    geom_sf(data=temp_fl, alpha=0.4, aes(fill=winner)) +  #, show.legend=FALSE) + 
    scale_fill_manual(values=c("blue", "red")) +
    theme_void() + 
    geom_sf(data=centers_fl, alpha=0.8, aes(fill=winner, size=plurality_abs),
            show.legend=FALSE,
            #  pch = 21   # filled circle "plotting character"
            shape = 21 # filled circle shape (newer version of pch)
    ) + 
    scale_size_area(max_size=30) +
    
    labs(title = "2020 Presidential Election Results by Florida County",
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
  
  ggsave("florida_county_presid_election_2020_1.png", plot=last_plot(),
         dpi=300,width=8,height=7)   
#########################################################################################
# New England map!
  temp_ne <- check2 %>% 
    filter(STUSPS %in% c("CT","MA","ME","NH","RI","VT"))
  
  centers_ne <- sf::st_centroid(temp_ne) %>%  # This is used for the graduated symbols
    arrange(desc(plurality_abs))              # sort by plurality in descending order
  
  ggplot(data=temp_ne) +
    geom_sf(data=temp_ne, alpha=0.4, aes(fill=winner)) +  #, show.legend=FALSE) + 
    scale_fill_manual(values=c("blue", "red")) +
    theme_void() + 
    geom_sf(data=centers_ne, alpha=0.8, aes(fill=winner, size=plurality_abs),
            show.legend=FALSE,
            #  pch = 21   # filled circle "plotting character"
            shape = 21 # filled circle shape (newer version of pch)
    ) + 
    scale_size_area(max_size=30) +
    
    labs(title = "2020 Presidential Election Results by New England County",
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
  
  ggsave("newengland_county_presid_election_2020_1.png", plot=last_plot(),
         dpi=300,width=8,height=7)   
#########################################################################################
