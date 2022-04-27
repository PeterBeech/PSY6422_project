#Load packages with renv
#install.packages("renv")
library(renv)
renv::restore()

#  install.packages("tidyverse")
#  install.packages("ggplot2")
#  install.packages("here")
#  install.packages("dplyr")
#  install.packages("readr")
#  install.packages("showtext", dependencies = TRUE)
#  install.packages("gganimate")
#  install.packages("gifski")

#Import packages
library(tidyverse)
library(ggplot2)
library(here)
library(readr)
library(showtext)
library(gganimate)
library(gifski)

#Clear variable list
rm(list=ls()) 

#Read raw data files and combine into a single csv file
combinedData <- list.files(path = here("data", "rawdata"),   
                           pattern = "*.csv", full.names = TRUE) %>%                                      
  lapply(read_csv) %>% 
  bind_rows()

#Save combined data set
combinedData_FileName = paste(here("data"), "/phs_2019_combined.csv", sep = "")   
write.csv(combinedData, combinedData_FileName)               

#Obtain data to plot hero playtime
heroData <- combinedData[combinedData$hero != "All Heroes", ]         #Remove rows with "All Heroes" in hero column

playTimeData <- heroData[heroData$stat_name == "Time Played", ] %>%   #Remove rows without "Time Played" in stat_name column
  group_by(hero) %>%                                                #Sum playtime for each hero
  summarise(play_time = sum(stat_amount)) %>% 
  ungroup()

#Creates the function clean_pt; which, reorders and converts play time data into hours      
clean_pt <- function(df){  
    df %>%      
    arrange(., desc(play_time)) %>%                                   #Arrange data by descending value of play_time
    mutate(play_time = play_time/3600) %>%                            #Convert play_time from seconds to hours and round to the nearest whole number
    mutate(rounded_play_time = round(play_time)) %>% 
    transform(rounded_play_time = as.character(rounded_play_time))
}

playTimeData <- clean_pt(playTimeData) 
     
#Aesthetics for graph
  #Define colour associated with each hero and add to playTimeData df as column hero_colour
heroColourLocation <- here("data", "hero_colours.csv")
heroColourData <- read.csv(file = heroColourLocation, encoding = "UTF-8")
colnames(heroColourData)[1] <- gsub('^.........','',colnames(heroColourData)[1])   #Fix error with imported column name

#Creates the function add_colour; which, for each hero in the dataframe, writes colour hex value to a column called hero_colour
add_colour <- function(df){
i = 0
  for (h in df$hero){ 
    i = i + 1
    hrow <- which(heroColourData$hero == h)
    df$hero_colour[i] <- paste(heroColourData$hex_value[hrow])
  }
  df
}

playTimeData <- add_colour(playTimeData)

#Load new custom font for showtext package
font_add_google(name = "Bebas Neue", family = "Bebas Neue")
showtext_auto()

#Define aesthetic variables
backgroundColour = "white"
textColour = "#5c5c5c"
textColourFaded = "#5c5c5c8a"
barLabelSize = 11


#Define data to plot
nheros <- 15:1   #Define number of heroes to show on graph
hpt <- ggplot(playTimeData[nheros,], aes(x = hero, y = play_time, fill = hero))   #Assign plot data to variable

#Align text bar label with numerical bar label. Two labels are used to allow font size difference
i = 0
hourLabel <- c(0)
for (i in nheros){   #If playtime has more than 2 digits, increases spacing of HOURS label to align with numerical bar label
  hourLabel[i] <- "HOURS"
  if (playTimeData$play_time[i] > 99){
    hourLabel[i] <- "  HOURS"
  }
}
  
#Plot graph and customize various plot aesthetics
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

#Save plot to figs folder
windows(width = 900, height = 900)   #Open windows graphics device
print(graph)
dev.print(file = here("figs", "PlayTimeGraph.png"), device = png, width = 900, height = 900)       

#Possible alternate fix for mac operating systems to preserve font
  #quartz(width = 900, height = 900)
  #quartz.save(file = here("figs", "PlayTimeGraph.png"), type = "png", dpi = 300)



#Obtain data to plot play time by season from combined dataset using previously created functions
stagePlayTimeData <- heroData[heroData$stat_name == "Time Played", ] %>% 
  group_by(stage, hero) %>% 
  summarise(play_time = sum(stat_amount)) %>% 
  ungroup() %>% 
  clean_pt() %>% 
  add_colour()

#Calculate total playtime in each season
stageTime <- heroData[heroData$stat_name == "Time Played", ] %>% 
  group_by(stage) %>% 
  summarise(play_time = sum(stat_amount)) %>% 
  ungroup() %>% 
  clean_pt()

#Divide hero play times by total playtime for the corresponding season, giving a percentage of total season play time the hero was used
j = 0
for (s in stagePlayTimeData$stage){
  j = j + 1
    srow <- which(stageTime$stage == s)
    stagePlayTimeData$percent_play_time[j] = (stagePlayTimeData$play_time[j] / stageTime$play_time[srow]) * 100 * 6
}

#Define data to plot
shpt <- ggplot(stagePlayTimeData, aes(x = hero, y = percent_play_time, fill = hero))

#Plot graph and customize various plot aesthetics
staticAnimGraph <- shpt +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_x_discrete(limits = rev, expand = c(0, 0)) +
  xlab("HERO") +
  ylab("PLAY TIME (hours)") +
  labs(title = "Overwatch League Character Usage", subtitle = ("playtime during {closest_state}"), size = 40) +
  theme_bw() +
  coord_flip() +
  scale_fill_manual(values = playTimeData$hero_colour, limits = playTimeData$hero) +
  geom_vline(xintercept = seq(1.5, length = 32), lwd = 2, colour = "white") +
  geom_vline(xintercept = seq(-1.5, length = 32), lwd = 2, colour = "white") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.line = element_line(colour = backgroundColour),
        plot.background = element_rect(fill = backgroundColour),
        panel.background = element_rect(fill = "#e6e6e6"),
        text = element_text(family = "Bebas Neue", colour = textColour),
        plot.title = element_text(size = 30, colour = "#505050"),
        plot.subtitle = element_text(size = 20, colour = textColourFaded),
        axis.title.x = element_text(size = 30, margin = margin(t=20), colour = textColourFaded, hjust = 0.42),
        axis.title.y = element_text(size = 40, margin = margin(r=19), colour = "#5c5c5c4f"),
        axis.ticks.x = element_line(colour = textColourFaded, size = 0.8),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.ticks.length.y = unit(0.3, "cm"),
        axis.text.x = element_text(colour = textColourFaded, size = 16, angle = 55, vjust = 0.9, hjust = 0.95),
        axis.text.y = element_text(colour = textColour, size = 20)) +
        transition_states(stage, transition_length = 2, state_length = 3)

#Render the animated graph as gif and save to figs folder
animate(staticAnimGraph, 200, fps = 20, width = 900, height = 1100,
        renderer = gifski_renderer(here("figs", "SeasonPlayTimeGraph.gif")))
