library(dplyr)
install.packages("tree")
library(tree)

snap_count_2017 <- read.csv("Intro to sports analytics /Data/snap_count_2017.csv")
snap_count_2016 <- read.csv("Intro to sports analytics /Data/snap_count_2016.csv")
snap_count_2015 <- read.csv("Intro to sports analytics /Data/snap_count_2015.csv")
snap_count_2014 <- read.csv("Intro to sports analytics /Data/snap_count_2014.csv")
snap_count_2013 <- read.csv("Intro to sports analytics /Data/snap_count_2013.csv")
snap_count_2012 <- read.csv("Intro to sports analytics /Data/snap_count_2012.csv")


snap_count <- rbind(snap_count_2017,snap_count_2016, snap_count_2015, snap_count_2014, snap_count_2013, snap_count_2012)
View(snap_count)

RawData10yrs <- Raw_Data_10yrs_v3_1_

FantasyData <- merge(snap_count, RawData10yrs, by = c("Name", "Year")) %>%
  group_by(FantPos)

names(FantasyData)
model_tree1 <-tree(FantasyData$PPR~ SnapsPerGame + TargetSnapPercentage + SnapsPlayedPercentage + Age, data = FantasyData)
                    View(FantasyData)
View(RawData10yrs)


plot(model_tree1)
text(model_tree1)
