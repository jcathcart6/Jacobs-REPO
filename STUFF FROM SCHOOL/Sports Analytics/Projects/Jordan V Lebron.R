library(readxl)
library()
lebronR <- read_excel("Intro to sports analytics /Data/James.xlsx" , sheet = 1)

jordanP <- read_excel("Intro to sports analytics /Data/Jordan.xlsx", sheet = "playoffs")

lebronP <- read_excel("Intro to sports analytics /Data/James.xlsx" , sheet = 2)

jordanR <- read_excel("Intro to sports analytics /Data/Jordan.xlsx", sheet = 1)

avgPlayer <- read_csv("Intro to sports analytics /Data/avgPlayerInfo.csv")

View(avgPlayer)

lebron_OffenseSubScores <- summarize(lebronR, 
                                    R_FGM = mean(lebronR$FGM) * avgPlayer$`FG Scalar`, 
                                    R_FGP = mean(lebronR$`FG%`/100),
                                    R_3PM = mean(lebronR$`3PM`) * avgPlayer$`3P Scalar`, 
                                    R_3PP = mean(lebronR$`3P%`/100),
                                    R_FTP = mean(lebronR$`FT%`/100),
                                    R_FTM = mean(lebronR$FTM) * avgPlayer$`FT Scalar`,
                                    P_FGM = mean(lebronP$FGM) * avgPlayer$`FG Scalar`,
                                    P_FGP = mean(lebronP$`FG%`/100),
                                    P_3PM = mean(lebronP$`3PM`) * avgPlayer$`3P Scalar`, 
                                    P_3PP = mean(lebronP$`3P%`/100),
                                    P_FTP = mean(lebronP$`FT%`/100),
                                    P_FTM = mean(lebronP$FTM) * avgPlayer$`FT Scalar`
                                    )
lebron_OffenseSubScores %>%
  mutate(SuperScore = )

print(lebron_OffenseSubScores_P)
print(lebron_OffenseSubScores)


Lebron_R_FGM <- mean(lebronR$FGM) * avgPlayer$`FG Scalar` 
Lebron_P_FGM <- mean(lebronP$FGM) * avgPlayer$`FG Scalar`
Lebron_R_FGP <- mean(lebronR$`FG%`/100) 
Lebron_R_3PM <- mean(lebronR$`3PM`) * avgPlayer$`3P Scalar`
Lebron_R_3PP <- mean(lebronR$`3P%`/100)
Lebron_P_FGP <- mean(lebronP$`FG%`/100)
Lebron_P_3PM <- mean(lebronP$`3PM`) * avgPlayer$`3P Scalar`
Lebron_P_3PP <- mean(lebronP$`3P%`/100)
Lebron_R_FTM <- mean(lebronR$FTM) * avgPlayer$`FT Scalar`
Lebron_P_FTM <- mean(lebronR$FTM) * avgPlayer$`FT Scalar`
Lebron_R_FTP <- mean(lebronR$`FT%`/100)
Lebron_P_FTP <- mean(lebronP$`FT%`/100)
Scoring_Metric <- (Lebron_R_FGM * Lebron_R_FGP) + (Lebron_R_3PM * Lebron_R_3PP) + (Lebron_R_FTM * Lebron_R_FTP) +
  (1.25 * ( (Lebron_P_FGM * Lebron_P_FGP) + (Lebron_P_3PM * Lebron_P_3PP)))
   
   
Jordan_R_FGM <- mean(jordanR$FGM)
Jordan_P_FGM <- mean(jordanP$FGM)
Jordan_R_FGP <- mean(jordanR$FGP/100)
Jordan_R_3PM <- mean(jordanR$`3PM`)
Jordan_R_3PP <- mean(jordanR$`3PP`/100)
Jordan_P_FGP <- mean(jordanP$`FG%`/100)
Jordan_P_3PM <- mean(jordanP$`3PM`)
Jordan_P_3PP <- mean(jordanP$`3P%`/100)
Jordan_R_FTM <- mean(jordanR$FTM)
Jordan_P_FTM <- mean(jordanR$FTM)
Jordan_R_FTP <- mean(jordanR$`FTP`/100)
Jordan_P_FTP <- mean(jordanP$`FT%`/100)
Jordan_Scoring_Metric <- (Jordan_R_FGM * Jordan_R_FGP) + (Jordan_R_3PM * Jordan_R_3PP) + (Jordan_R_FTM * Jordan_R_FTP) +
  (1.25 * ( (Jordan_P_FGM * Jordan_P_FGP) + (Jordan_P_3PM * Jordan_P_3PP)))

print(Jordan_Scoring)
print(Lebron_Scoring)




P_FGM = mean(lebronP$FGM) * avgPlayer$`FG Scalar`, 
P_FGP = mean(lebronP$`FG%`/100),
P_3PM = mean(lebronP$`3PM`) * avgPlayer$`3P Scalar`, 
P_3PP = mean(lebronP$`3P%`/100),
P_FTP = mean(lebronP$`FT%`/100),
