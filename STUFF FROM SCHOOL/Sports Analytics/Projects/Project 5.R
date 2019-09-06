library(dplyr)
library(readxl)

pass_16_17 <- read_xlsx("pass_16_17.xlsx")
win_pct_16_17 <- read_xlsx("win_pct_16.xlsx")
turn_gain_16_17 <- read_xlsx("turn_gain_16.xlsx")
rushing_16_17 <- read_xlsx("rushing_16_17.xlsx")
win_pct_17_18 <- read_xlsx("win_pct_17_18.xlsx")
rushing_17_18 <- read_xlsx("rushing_1718.xlsx")
turn_gain_17_18 <- read_xlsx("turnover_gain_17_18.xlsx")
pass_17_18 <- read_xlsx("passing17_18.xlsx")
weight <- read.csv("PercentDiffMean.csv")

superset_16_17 <- merge(pass_16_17, win_pct_16_17, by = "Name") %>%
  merge(turn_gain_16_17, by = "Name") %>%
  merge(rushing_16_17, by = "Name")
  
superset_17_18 <- merge(pass_17_18, win_pct_17_18, by = "Name") %>%
  merge(turn_gain_17_18, by = "Name") %>%
  merge(rushing_17_18, by = "Name")
names(superset_17_18) <- c("Name" ,    "PassTD" ,  "Pct" ,     "TurnGain" ,"RushYds" , "RushTD")

superset_16_17 <- mutate(superset_16_17, rankscore = Pct * (.94* PassTD  + .92 *TurnGain + .74 * RushYds + 1.1 * RushTD))
superset_17_18 <- mutate(superset_17_18, rankscore = Pct * (.94* PassTD  + .92 *TurnGain + .74 * RushYds + 1.1 * RushTD))

superset_16_17
write.csv(superset_16_17, "superset_16_17.csv")
write.csv(superset_17_18, "superset_17_18.csv")


