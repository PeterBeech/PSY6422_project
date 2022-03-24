#Load packages with renv
install.packages("renv")
renv::restore()

#  install.packages("tidyverse")
#  install.packages("ggplot2")
#  install.packages("here")
#  install.packages("dplyr")
#  install.packages("readr")
#  install.packages("showtext", dependencies = TRUE)
  
  library(tidyverse)
  library(ggplot2)
  library(here)
  library(readr)
  library(showtext)
  library(dplyr)
 
  
#Load new font 
font_add_google(name = "Bebas Neue", family = "Bebas Neue")
showtext_auto()

#Clear variable list
rm(list=ls()) 

#Read raw data files and combine
combinedData <- list.files(path = here("data", "rawdata"),                #Identify all CSV files in /data/rawdata
                           pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                                                    #Store all files in list
  bind_rows                                                               #Combine datasets into a single dataset
  
combinedData_FileName = paste(here("data"), "/phs_2019_combined.csv", sep = "") #Define location to save combined dataset as .csv
write.csv(combinedData, combinedData_FileName)                            #Write dataset to new .csv in /data

#Obtain data to plot hero playtime
  heroData <- combinedData[combinedData$hero != "All Heroes", ]           #Remove rows with "All Heroes" in hero column
  playTimeData <- heroData[heroData$stat_name == "Time Played", ] %>%     #Remove rows without "Time Played" in stat_name column
      group_by(hero) %>%                                              #Sum playtime for each hero
      summarise(play_time = sum(stat_amount)) %>% 
      ungroup() %>% 
      arrange(., desc(play_time)) %>%                                     #Arrange data by descending value of play_time
      mutate(play_time = play_time/3600) %>%                                 #Convert play_time from seconds to hours
      mutate(rounded_play_time = round(play_time)) %>% 
      transform(rounded_play_time = as.character(rounded_play_time))

#Aesthetics for graph
#Define colour associated with each hero and add to playTimeData df as column hero_colour
heroColourLocation <- here("data", "hero_colours.csv")
heroColourData <- read.csv(file = heroColourLocation, encoding = "UTF-8")
colnames(heroColourData)[1] <- gsub('^.........','',colnames(heroColourData)[1])
i = 0
for (h in playTimeData$hero){
  i = i + 1
  hrow <- which(heroColourData$hero == h)
  playTimeData$hero_colour[i] <- paste(heroColourData$hex_value[hrow])
}
#colours obtained from related ingame colour, altered to increase visibility

#Define aesthetic variables
backgroundColour = "white"
textColour = "#5c5c5c"
textColourFaded = "#5c5c5c8a"
barLabelSize = 11


#Which data to plot
nheros <- 15:1                                                            #Define number of heroes to show on graph
hpt <- ggplot(playTimeData[nheros,], aes(x = hero, y = play_time, fill = hero))     #Assign plot data to variable

i = 0
hourLabel <- c(0)
for (i in nheros){
  hourLabel[i] <- "HOURS"
  if (playTimeData$play_time[i] > 99){
    hourLabel[i] <- "  HOURS"
  }
}
  
#Plot graph
graph <- hpt +  
  geom_bar(stat = "identity", mapping = aes(x = hero, y = playTimeData$play_time[1]), fill = "#e6e6e6") +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = rounded_play_time), y = 2, family = "Bebas Neue", colour = "white", size = barLabelSize, hjust = 0) +
  geom_text(aes(label = rev(hourLabel)), y = 2, family = "Bebas Neue", colour = "white", size = 6.3, hjust = -0.80, vjust = 0.85) +
  scale_x_discrete(limits = playTimeData$hero[nheros]) +
  scale_y_continuous(limits = c(0, (playTimeData$play_time[1] + 20)), expand = c(0, 0)) +
  xlab("HERO") +
  ylab("PLAY TIME (hours)") +
  labs(title = "Overwatch League Character Usage", subtitle = "by playtime during OWL 2019 stages 1-3", size = 40) +
  theme_bw() +
  coord_flip() +
  scale_fill_manual(values = playTimeData$hero_colour, limits = playTimeData$hero) +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = backgroundColour),
        plot.background = element_rect(fill = backgroundColour),
        panel.background = element_rect(fill = backgroundColour),
        text = element_text(family = "Bebas Neue", colour = textColour),
        plot.title = element_text(size = 35, colour = "#505050"),
        plot.subtitle = element_text(size = 25, colour = textColourFaded),
        axis.title.x = element_text(size = 30, margin = margin(t=20), colour = textColourFaded, hjust = 0.42),
        axis.title.y = element_text(size = 40, margin = margin(r=19), colour = "#5c5c5c4f"),
        axis.ticks.x = element_line(colour = textColourFaded, size = 0.8),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.ticks.length.y = unit(0.3, "cm"),
        axis.text.x = element_text(colour = textColourFaded, size = 16, angle = 55, vjust = 0.9, hjust = 0.95),
        axis.text.y = element_text(colour = textColour, size = 24))
            
windows()
print(graph)
dev.print(file = here("figs", "PlayTimeGraph.png"), device = png, width = 900, height = 900)            
