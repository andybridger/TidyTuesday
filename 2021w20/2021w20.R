#TidyTuesday
#Week 20
#Broadband usage and access in the states I went to college
#data wrangling code from @nrennie35

#load libraries
install.packages("tigris")
library(tidyverse)
library(tigris)
library(patchwork)
library(scales)

#import Roboto font
sysfonts::font_add_google('Roboto')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 400)

#load data
tuesdata <- tidytuesdayR::tt_load('2021-05-11')

broadband <- tuesdata$broadband
broadband_zip <- tuesdata$broadband_zip

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#check avg broadband availability by state
state_broadband <- broadband %>% group_by(ST)
state_broadband <- state_broadband %>% summarise(avg = mean(as.numeric(`BROADBAND AVAILABILITY PER FCC`), na.rm=T))

#set up data for MA
MA_data <- filter(broadband, ST == "MA")
MA_data$COUNTYFP <- substrRight(as.character(MA_data$`COUNTY ID`), 3)
tracts_max <- tracts(state = 'MA')
ggtract_MA<-fortify(tracts_max, region = "GEOID") 
ggtract_MA<-left_join(ggtract_MA, MA_data, by=c("COUNTYFP")) 

#set up data for KS
KS_data <- filter(broadband, ST == "KS")
KS_data$COUNTYFP <- substrRight(as.character(KS_data$`COUNTY ID`), 3)
tracts_min <- tracts(state = 'KS')
ggtract_KS<-fortify(tracts_min, region = "GEOID") 
ggtract_KS<-left_join(ggtract_KS, KS_data, by=c("COUNTYFP")) 

#plot for MA
p1 <- ggplot() +
  geom_sf(data = ggtract_MA, aes(fill=as.numeric(`BROADBAND AVAILABILITY PER FCC`)), colour = "#061541", size =0.25) +
  coord_sf(datum = NA)+
  scale_fill_viridis_c(limits=c(0,1), 
                       breaks=c(0,1, 0.5), labels=c("0", "100%", "50%"),
                       guide = guide_colourbar(title.position = "top"))+
  theme_minimal(base_size=10)+
  theme(plot.subtitle = element_text(size=9),
        plot.title=element_text(size=14))+
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               reverse = FALSE,
                               barheight = unit(10,"lines"),
                               barwidth = unit(0.5, "lines")))+
  labs(fill="",
       title = "Massachusetts")

#plot for KS
p2 <- ggplot() +
  geom_sf(data = ggtract_KS, aes(fill=as.numeric(`BROADBAND AVAILABILITY PER FCC`)), colour = "#061541", size =0.25) +
  coord_sf(datum = NA)+
  scale_fill_viridis_c(limits=c(0,1), 
                       breaks=c(0,1, 0.5), labels=c("0", "100%", "50%"),
                       guide = guide_colourbar(title.position = "top"))+
  theme_minimal(base_size=10)+
  theme(plot.subtitle = element_text(size=9),
        plot.title=element_text(size=14))+
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               reverse = FALSE,
                               barheight = unit(10,"lines"),
                               barwidth = unit(0.5, "lines")))+
  labs(fill="",
       title = "Kansas")

#join plots together
p3 <- p1 / p2  +
  plot_annotation(
    title = "Broadband Access in the States I Went to College\n",
    subtitle = "Data are the percentage of people in each county with access to fixed terrestrial broadband at speeds of\n25 Mbps/3 Mbp in 2017. The majority of people in all Massachusetts counties have access to broadband.\nIn most counties in Massachusetts, 100% of people have access to broadband. In contrast, the majority\nof people in many rural counties in Kansas do NOT have access to broadband.\n", 
    caption = '\n#TidyTuesday Week 20 | Viz: @AndyBridger | Data: The Verge & Microsoft'
  )  &
  theme(panel.background = element_rect(fill = "white", colour="white"),
        plot.background = element_rect(fill = "white", colour="white"),
        plot.title = element_text(colour = "#440154", size=20, face="bold", hjust = 0.5, family="Roboto"),
        plot.subtitle = element_text(colour = "#440154", size=12, hjust = 0.0, family="Roboto", lineheight = 1.1),
        plot.caption = element_text(colour = "#440154", size=8, face="bold", hjust = 0, family="Roboto"))
p3

#save plot
ggsave('charts/2021w20.png', p3, width = 8, height = 12, unit = 'in', dpi = 400)


#############
#USAGE
#############


#plot for MA
p1 <- ggplot() +
  geom_sf(data = ggtract_MA, aes(fill=as.numeric(`BROADBAND USAGE`)), colour = "#061541", size =0.25) +
  coord_sf(datum = NA)+
  scale_fill_viridis_c(limits=c(0,1), 
                       breaks=c(0,1, 0.5), labels=c("0", "100%", "50%"),
                       guide = guide_colourbar(title.position = "top"))+
  theme_minimal(base_size=10)+
  theme(plot.subtitle = element_text(size=9),
        plot.title=element_text(size=14))+
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               reverse = FALSE,
                               barheight = unit(10,"lines"),
                               barwidth = unit(0.5, "lines")))+
  labs(fill="",
       title = "Massachusetts")

#plot for KS
p2 <- ggplot() +
  geom_sf(data = ggtract_KS, aes(fill=as.numeric(`BROADBAND USAGE`)), colour = "#061541", size =0.25) +
  coord_sf(datum = NA)+
  scale_fill_viridis_c(limits=c(0,1), 
                       breaks=c(0,1, 0.5), labels=c("0", "100%", "50%"),
                       guide = guide_colourbar(title.position = "top"))+
  theme_minimal(base_size=10)+
  theme(plot.subtitle = element_text(size=9),
        plot.title=element_text(size=14))+
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               reverse = FALSE,
                               barheight = unit(10,"lines"),
                               barwidth = unit(0.5, "lines")))+
  labs(fill="",
       title = "Kansas")

#join plots together
p4 <- p1 / p2  +
  plot_annotation(
    title = "Broadband Usage in the States I Went to College\n",
    subtitle = "Data are the percentage of people in each county that used fixed terrestrial broadband at speeds of\n25 Mbps/3 Mbp in 2019.", 
    caption = '\n#TidyTuesday Week 20 | Viz: @AndyBridger | Data: The Verge & Microsoft'
  )  &
  theme(panel.background = element_rect(fill = "white", colour="white"),
        plot.background = element_rect(fill = "white", colour="white"),
        plot.title = element_text(colour = "#440154", size=20, face="bold", hjust = 0.5, family="Roboto"),
        plot.subtitle = element_text(colour = "#440154", size=12, hjust = 0.0, family="Roboto", lineheight = 1.1),
        plot.caption = element_text(colour = "#440154", size=8, face="bold", hjust = 0, family="Roboto"))
p4

#save plot
ggsave('charts/2021w20_usage.png', p4, width = 8, height = 12, unit = 'in', dpi = 400)
