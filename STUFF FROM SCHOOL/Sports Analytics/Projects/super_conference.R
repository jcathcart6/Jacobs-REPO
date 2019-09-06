location_data <- read.csv("sup_conf.csv")


library(dplyr)
library(ggrepel)
location_data2 <- location_data %>% 
        group_by(city, state_name, lat,lng) %>%
        filter(city %in% c("Chestnut Hill", "Greenville", "Durham", "Tallahassee", "Atlanta", "Louisville", "Miami", 
                           "Chapel Hill", "Raleigh", "Pittsburgh", "Syracuse","Charlottesville", "Blacksburg", "Winston-Salem",
                           "Notre Dame", "Champaign", "Bloomington", "Iowa City", "College Park", "Ann Arbor", "East Lansing", 
                           "Minneapolis", "Lincoln", "Evanston", "Columbus", "State College", "West Lafayette", "New Brunswick",
                           "Madison", "Waco", "Ames", "Lawrence", "Manhattan", "Norman", "Stillwater", "Fort Worth", "Austin", "Lubbock", 
                           "Morgantown", "Tucson", "Tempe", "Berkeley","Tuscaloosa", "Fayetteville","Los Angeles", "Auburn","Gainesville",
                           "Athens","Denver", "Eugene", "Corvallis", "Los Angeles", "Lexington", "Baton Rouge" , "Oxford", 	"Starkville", "Columbia",
                          "Stanford", "Salt Lake City", "Seattle", "Pullman" ,	"Knoxville", "College Station", "Nashville", "Provo"))   %>%
        
    filter(state_name %in% c("Washington", "North Carolina", "Virginia", "Texas", "Oklahoma", "California", "New York", "South Carolina", "Louisiana", "Oregon", "Massachusetts", "Michigan", "Wisconson",
                                 "Arizona", "Utah", "Colorado", "Kansas", "Nebraska", "Minnesota", "Iowa", "Missouri", "Arkansas", "Illinois", "Indiana", "Kentucky", "Tennesee", "Alabama", "Florida", "Georgia"
                                  ,"West Virginia", "Ohio", "New York", "Pennsylvania", "Maryland", "New Jersey")) 
location_data2




team_loc <- read.csv("team_loc.csv")
team_loc

library(cluster)

loc_clust <- kmeans(team_loc[,4:5],4,nstart = 10)
loc_clust

clusplot(team_loc[,c(4:5)], loc_clust$cluster, color=T, lwd=2)

team_loc$conf <- factor(loc_clust$cluster)
team_loc$conf

library(ggplot2)
library(maps)
states <- map_data("state")
states


ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "black") + 
  geom_point(data = team_loc, aes(x= lng , y = lat, color = conf)) +
  coord_fixed(1.3) + 
  geom_text(data = team_loc, aes(x= lng, y = lat, label = team_name), color = "red", 
            size = 2.5, nudge_x = 0.5) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y =element_blank())





team_loc_final <- read.csv("conf_div.csv")
team_loc_final$conf <- factor(team_loc_final$conf)
team_loc_final$division <- factor(team_loc_final$division)

x <- 0
y <- 0
z <- 0
g <- 0
for (variable in team_loc_final$conf) {
  if (variable == "1") {
    x <- x+1
  } else if ( variable == "2") {
    y <- y + 1
  } else if ( variable == "3") {
    z <- z + 1
  } else {
    g <- g + 1
  }
  
}

print(c(x,y,z,g))

ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill = "white") + 
  geom_point(data = team_loc_final, aes(x= lng , y = lat, color = conf)) +
  coord_fixed(1.3) + 
  geom_text(data = team_loc_final, aes(x= lng, y = lat, label = team_name), 
            size = 2.5, nudge_x = .5, nudge_y = .5, color = "black") +
  ggtitle("Map of New Super Conferences") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y =element_blank())

conf1_teams <- filter(team_loc_final, conf == 1)
states1 <- filter(states, region %in% c("michigan", 
                                       "indiana", 
                                       "ohio",
                                       "kentucky",
                                       "tennessee", 
                                       "georgia",
                                       "alabama",
                                       "florida") )
ggplot(data = states1) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill = "white") + 
  geom_point(data = conf1_teams, aes(x= lng , y = lat, color = division)) +
  coord_fixed(1.3) + 
  geom_text(data = conf1_teams, aes(x= lng, y = lat, label = team_name), 
            size = 2.5, nudge_x = .5, nudge_y = .5) +
  ggtitle("Division Map of Conference 1") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y =element_blank())


states2 <- filter(states, region %in% c("washington",
                                        "oregon",
                                        "california",
                                        "arizona",
                                        "new mexico",
                                        "nevada",
                                        "colorado",
                                        "texas",
                                        "utah"))
conf2_teams <- filter(team_loc_final, conf == 2)
ggplot(data = states2) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill = "white") + 
  geom_point(data = conf2_teams, aes(x= lng , y = lat, color = division)) +
  coord_fixed(1.3) + 
  geom_text(data = conf2_teams, aes(x= lng, y = lat, label = team_name), 
            size = 2.5, nudge_x = .5, nudge_y = .5) +
  ggtitle("Division Map of Conference 2") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y =element_blank())

states3 <- filter(states, region %in% c("nebraska",
                                        "minnesota",
                                        "wisconsin",
                                        "iowa",
                                        "illinois",
                                        "kansas",
                                        "missouri",
                                        "mississippi",
                                        "louisiana",
                                        "oklahoma",
                                        "arkansas"))


conf3_teams <- filter(team_loc_final, conf == 3)
ggplot(data = states3) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill = "white") + 
  geom_point(data = conf3_teams, aes(x= lng , y = lat, color = division)) +
  coord_fixed(1.3) + 
  geom_text(data = conf3_teams, aes(x= lng, y = lat, label = team_name), 
            size = 2.5, nudge_x = .5, nudge_y = .5) +
  ggtitle("Division Map of Conference 3") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y =element_blank())


states4 <- filter(states, region %in% c("south carolina",
                                         "virginia",
                                         "west virginia",
                                         "pennsylvania",
                                         "new york",
                                         "massachusetts",
                                         "rhode island",
                                        "north carolina",
                                        "maryland",
                                        "new jersey",
                                        "cconnecticut"))
conf4_teams <- filter(team_loc_final, conf == 4)
ggplot(data = states4) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill = "white") + 
  geom_point(data = conf4_teams, aes(x= lng , y = lat, color = division)) +
  coord_fixed(1.3) + 
  geom_text(data = conf4_teams, aes(x= lng, y = lat, label = team_name), 
            size = 2.5, nudge_x = .5, nudge_y = .5) +
  ggtitle("Division Map of Conference 4 ") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y =element_blank())

  





