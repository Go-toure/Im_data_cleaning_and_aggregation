setwd("C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/IM_raw/")

library(tidyverse)
library(lubridate)
library(tidyjson)
library(rjson)

# DF <- read_csv("IM_DRC.json")
DF <- read_csv("COG_SIA_H1.csv")
 # DF$Country[is.na(DF$Country)] <- "CIV"
# DF$Country[DF$Country == "NA"] <- "CIV"
# mydata <- fromJSON(file="IM_DRC.json")
# DF <- as.data.frame(mydata)
GF <- DF |>
  # mutate(Region = case_when(
  #   Country == "BWT" ~ District,
  #   TRUE ~ Region)) |>
  # mutate(District = case_when(
  #   Country == "ZMB" ~ District,
  #   TRUE ~ District))
  
  #for BWT
   #mutate(Region = District) |> 
   #FOR ZMB add this -> District = District,)
  filter(Type_Monitoring == "EndProcess", `Response` !="n/a",`roundNumber` !="n/a", Total_U5_Present !="n/a") |>
  select(`Country`, Region , District, `Response`, `roundNumber`, `Type_Monitoring`,`date_monitored`, `HH_count`, `Total_U5_Present`, `TotalFM`, `sum_missed_children`, `Total_Absent`, `Total_refusal` , `HH[1]/Total_U5_Present_HH`, `HH[2]/Total_U5_Present_HH`, `HH[3]/Total_U5_Present_HH`, `HH[4]/Total_U5_Present_HH`, `HH[5]/Total_U5_Present_HH`, `HH[6]/Total_U5_Present_HH`, `HH[7]/Total_U5_Present_HH`, `HH[8]/Total_U5_Present_HH`, `HH[9]/Total_U5_Present_HH`, `HH[10]/Total_U5_Present_HH`, `HH[1]/U5_Vac_FM_HH`, `HH[2]/U5_Vac_FM_HH`, `HH[3]/U5_Vac_FM_HH`, `HH[4]/U5_Vac_FM_HH`, `HH[5]/U5_Vac_FM_HH`, `HH[6]/U5_Vac_FM_HH`, `HH[7]/U5_Vac_FM_HH`, `HH[8]/U5_Vac_FM_HH`, `HH[9]/U5_Vac_FM_HH`, `HH[10]/U5_Vac_FM_HH`, `HH[1]/group1/Tot_child_Absent_HH`, `HH[2]/group1/Tot_child_Absent_HH`, `HH[3]/group1/Tot_child_Absent_HH`, `HH[4]/group1/Tot_child_Absent_HH`, `HH[5]/group1/Tot_child_Absent_HH`, `HH[6]/group1/Tot_child_Absent_HH`, `HH[7]/group1/Tot_child_Absent_HH`, `HH[8]/group1/Tot_child_Absent_HH`, `HH[9]/group1/Tot_child_Absent_HH`, `HH[10]/group1/Tot_child_Absent_HH`, `HH[1]/group1/Tot_child_NC_HH`, `HH[2]/group1/Tot_child_NC_HH`, `HH[3]/group1/Tot_child_NC_HH`, `HH[4]/group1/Tot_child_NC_HH`, `HH[5]/group1/Tot_child_NC_HH`, `HH[6]/group1/Tot_child_NC_HH`, `HH[7]/group1/Tot_child_NC_HH`, `HH[8]/group1/Tot_child_NC_HH`, `HH[9]/group1/Tot_child_NC_HH`, `HH[10]/group1/Tot_child_NC_HH`, `HH[1]/group1/Tot_child_NotVisited_HH`, `HH[2]/group1/Tot_child_NotVisited_HH`, `HH[3]/group1/Tot_child_NotVisited_HH`, `HH[4]/group1/Tot_child_NotVisited_HH`, `HH[5]/group1/Tot_child_NotVisited_HH`, `HH[6]/group1/Tot_child_NotVisited_HH`, `HH[7]/group1/Tot_child_NotVisited_HH`, `HH[8]/group1/Tot_child_NotVisited_HH`, `HH[9]/group1/Tot_child_NotVisited_HH`, `HH[10]/group1/Tot_child_NotVisited_HH`, `HH[1]/group1/Tot_child_NotRevisited`, `HH[2]/group1/Tot_child_NotRevisited`, `HH[3]/group1/Tot_child_NotRevisited`, `HH[4]/group1/Tot_child_NotRevisited`, `HH[5]/group1/Tot_child_NotRevisited`, `HH[6]/group1/Tot_child_NotRevisited`, `HH[7]/group1/Tot_child_NotRevisited`, `HH[8]/group1/Tot_child_NotRevisited`, `HH[9]/group1/Tot_child_NotRevisited`, `HH[10]/group1/Tot_child_NotRevisited`, `HH[1]/group1/Tot_child_Asleep_HH`, `HH[2]/group1/Tot_child_Asleep_HH`, `HH[3]/group1/Tot_child_Asleep_HH`, `HH[4]/group1/Tot_child_Asleep_HH`, `HH[5]/group1/Tot_child_Asleep_HH`, `HH[6]/group1/Tot_child_Asleep_HH`, `HH[7]/group1/Tot_child_Asleep_HH`, `HH[8]/group1/Tot_child_Asleep_HH`, `HH[9]/group1/Tot_child_Asleep_HH`, `HH[10]/group1/Tot_child_Asleep_HH`, `HH[1]/group1/Tot_child_VaccinatedRoutine`, `HH[2]/group1/Tot_child_VaccinatedRoutine`, `HH[3]/group1/Tot_child_VaccinatedRoutine`, `HH[4]/group1/Tot_child_VaccinatedRoutine`, `HH[5]/group1/Tot_child_VaccinatedRoutine`, `HH[6]/group1/Tot_child_VaccinatedRoutine`, `HH[7]/group1/Tot_child_VaccinatedRoutine`, `HH[8]/group1/Tot_child_VaccinatedRoutine`, `HH[9]/group1/Tot_child_VaccinatedRoutine`, `HH[10]/group1/Tot_child_VaccinatedRoutine`,`HH[1]/group1/Tot_child_Others_HH`, `HH[2]/group1/Tot_child_Others_HH`, `HH[3]/group1/Tot_child_Others_HH`, `HH[4]/group1/Tot_child_Others_HH`, `HH[5]/group1/Tot_child_Others_HH`, `HH[6]/group1/Tot_child_Others_HH`, `HH[7]/group1/Tot_child_Others_HH`, `HH[8]/group1/Tot_child_Others_HH`, `HH[9]/group1/Tot_child_Others_HH`, `HH[10]/group1/Tot_child_Others_HH`, `HH[1]/Parent_Caregive_Inform_HH`, `HH[2]/Parent_Caregive_Inform_HH`, `HH[3]/Parent_Caregive_Inform_HH`, `HH[4]/Parent_Caregive_Inform_HH`, `HH[5]/Parent_Caregive_Inform_HH`, `HH[6]/Parent_Caregive_Inform_HH`, `HH[7]/Parent_Caregive_Inform_HH`, `HH[8]/Parent_Caregive_Inform_HH`, `HH[9]/Parent_Caregive_Inform_HH`, `HH[10]/Parent_Caregive_Inform_HH`)

# write_csv(GF,"C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/CIV_IM_.csv") 

#ABSENT

GF$`HH[1]/group1/Tot_child_Absent_HH`[GF$`HH[1]/group1/Tot_child_Absent_HH` == "n/a"] <- "0"
GF$`HH[2]/group1/Tot_child_Absent_HH`[GF$`HH[2]/group1/Tot_child_Absent_HH` == "n/a"] <- "0"
GF$`HH[3]/group1/Tot_child_Absent_HH`[GF$`HH[3]/group1/Tot_child_Absent_HH` == "n/a"] <- "0"
GF$`HH[4]/group1/Tot_child_Absent_HH`[GF$`HH[4]/group1/Tot_child_Absent_HH` == "n/a"] <- "0"
GF$`HH[5]/group1/Tot_child_Absent_HH`[GF$`HH[5]/group1/Tot_child_Absent_HH` == "n/a"] <- "0"
GF$`HH[6]/group1/Tot_child_Absent_HH`[GF$`HH[6]/group1/Tot_child_Absent_HH` == "n/a"] <- "0"
GF$`HH[7]/group1/Tot_child_Absent_HH`[GF$`HH[7]/group1/Tot_child_Absent_HH` == "n/a"] <- "0"
GF$`HH[8]/group1/Tot_child_Absent_HH`[GF$`HH[8]/group1/Tot_child_Absent_HH` == "n/a"] <- "0"
GF$`HH[9]/group1/Tot_child_Absent_HH`[GF$`HH[9]/group1/Tot_child_Absent_HH` == "n/a"] <- "0"
GF$`HH[10]/group1/Tot_child_Absent_HH`[GF$`HH[10]/group1/Tot_child_Absent_HH` == "n/a"] <- "0"

#REFUS

GF$`HH[1]/group1/Tot_child_NC_HH`[GF$`HH[1]/group1/Tot_child_NC_HH` == "n/a"] <- "0"
GF$`HH[2]/group1/Tot_child_NC_HH`[GF$`HH[2]/group1/Tot_child_NC_HH` == "n/a"] <- "0"
GF$`HH[3]/group1/Tot_child_NC_HH`[GF$`HH[3]/group1/Tot_child_NC_HH` == "n/a"] <- "0"
GF$`HH[4]/group1/Tot_child_NC_HH`[GF$`HH[4]/group1/Tot_child_NC_HH` == "n/a"] <- "0"
GF$`HH[5]/group1/Tot_child_NC_HH`[GF$`HH[5]/group1/Tot_child_NC_HH` == "n/a"] <- "0"
GF$`HH[6]/group1/Tot_child_NC_HH`[GF$`HH[6]/group1/Tot_child_NC_HH` == "n/a"] <- "0"
GF$`HH[7]/group1/Tot_child_NC_HH`[GF$`HH[7]/group1/Tot_child_NC_HH` == "n/a"] <- "0"
GF$`HH[8]/group1/Tot_child_NC_HH`[GF$`HH[8]/group1/Tot_child_NC_HH` == "n/a"] <- "0"
GF$`HH[9]/group1/Tot_child_NC_HH`[GF$`HH[9]/group1/Tot_child_NC_HH` == "n/a"] <- "0"
GF$`HH[10]/group1/Tot_child_NC_HH`[GF$`HH[10]/group1/Tot_child_NC_HH` == "n/a"] <- "0"

#hh_not_visited

GF$`HH[1]/group1/Tot_child_NotVisited_HH`[GF$`HH[1]/group1/Tot_child_NotVisited_HH` == "n/a"] <- "0"
GF$`HH[2]/group1/Tot_child_NotVisited_HH`[GF$`HH[2]/group1/Tot_child_NotVisited_HH` == "n/a"] <- "0"
GF$`HH[3]/group1/Tot_child_NotVisited_HH`[GF$`HH[3]/group1/Tot_child_NotVisited_HH` == "n/a"] <- "0"
GF$`HH[4]/group1/Tot_child_NotVisited_HH`[GF$`HH[4]/group1/Tot_child_NotVisited_HH` == "n/a"] <- "0"
GF$`HH[5]/group1/Tot_child_NotVisited_HH`[GF$`HH[5]/group1/Tot_child_NotVisited_HH` == "n/a"] <- "0"
GF$`HH[6]/group1/Tot_child_NotVisited_HH`[GF$`HH[6]/group1/Tot_child_NotVisited_HH` == "n/a"] <- "0"
GF$`HH[7]/group1/Tot_child_NotVisited_HH`[GF$`HH[7]/group1/Tot_child_NotVisited_HH` == "n/a"] <- "0"
GF$`HH[8]/group1/Tot_child_NotVisited_HH`[GF$`HH[8]/group1/Tot_child_NotVisited_HH` == "n/a"] <- "0"
GF$`HH[9]/group1/Tot_child_NotVisited_HH`[GF$`HH[9]/group1/Tot_child_NotVisited_HH` == "n/a"] <- "0"
GF$`HH[10]/group1/Tot_child_NotVisited_HH`[GF$`HH[10]/group1/Tot_child_NotVisited_HH` == "n/a"] <- "0"

#hh_not_revisited

GF$`HH[1]/group1/Tot_child_NotRevisited`[GF$`HH[1]/group1/Tot_child_NotRevisited` == "n/a"] <- "0"
GF$`HH[2]/group1/Tot_child_NotRevisited`[GF$`HH[2]/group1/Tot_child_NotRevisited` == "n/a"] <- "0"
GF$`HH[3]/group1/Tot_child_NotRevisited`[GF$`HH[3]/group1/Tot_child_NotRevisited` == "n/a"] <- "0"
GF$`HH[4]/group1/Tot_child_NotRevisited`[GF$`HH[4]/group1/Tot_child_NotRevisited` == "n/a"] <- "0"
GF$`HH[5]/group1/Tot_child_NotRevisited`[GF$`HH[5]/group1/Tot_child_NotRevisited` == "n/a"] <- "0"
GF$`HH[6]/group1/Tot_child_NotRevisited`[GF$`HH[6]/group1/Tot_child_NotRevisited` == "n/a"] <- "0"
GF$`HH[7]/group1/Tot_child_NotRevisited`[GF$`HH[7]/group1/Tot_child_NotRevisited` == "n/a"] <- "0"
GF$`HH[8]/group1/Tot_child_NotRevisited`[GF$`HH[8]/group1/Tot_child_NotRevisited` == "n/a"] <- "0"
GF$`HH[9]/group1/Tot_child_NotRevisited`[GF$`HH[9]/group1/Tot_child_NotRevisited` == "n/a"] <- "0"
GF$`HH[10]/group1/Tot_child_NotRevisited`[GF$`HH[10]/group1/Tot_child_NotRevisited` == "n/a"] <- "0"

#hh_child_sleep
GF$`HH[1]/group1/Tot_child_Asleep_HH`[GF$`HH[1]/group1/Tot_child_Asleep_HH` == "n/a"] <- "0"
GF$`HH[2]/group1/Tot_child_Asleep_HH`[GF$`HH[2]/group1/Tot_child_Asleep_HH` == "n/a"] <- "0"
GF$`HH[3]/group1/Tot_child_Asleep_HH`[GF$`HH[3]/group1/Tot_child_Asleep_HH` == "n/a"] <- "0"
GF$`HH[4]/group1/Tot_child_Asleep_HH`[GF$`HH[4]/group1/Tot_child_Asleep_HH` == "n/a"] <- "0"
GF$`HH[5]/group1/Tot_child_Asleep_HH`[GF$`HH[5]/group1/Tot_child_Asleep_HH` == "n/a"] <- "0"
GF$`HH[6]/group1/Tot_child_Asleep_HH`[GF$`HH[6]/group1/Tot_child_Asleep_HH` == "n/a"] <- "0"
GF$`HH[7]/group1/Tot_child_Asleep_HH`[GF$`HH[7]/group1/Tot_child_Asleep_HH` == "n/a"] <- "0"
GF$`HH[8]/group1/Tot_child_Asleep_HH`[GF$`HH[8]/group1/Tot_child_Asleep_HH` == "n/a"] <- "0"
GF$`HH[9]/group1/Tot_child_Asleep_HH`[GF$`HH[9]/group1/Tot_child_Asleep_HH` == "n/a"] <- "0"
GF$`HH[10]/group1/Tot_child_Asleep_HH`[GF$`HH[10]/group1/Tot_child_Asleep_HH` == "n/a"] <- "0"

#hh_VaccinatedRoutine

GF$`HH[1]/group1/Tot_child_VaccinatedRoutine`[GF$`HH[1]/group1/Tot_child_VaccinatedRoutine` == "n/a"] <- "0"
GF$`HH[2]/group1/Tot_child_VaccinatedRoutine`[GF$`HH[2]/group1/Tot_child_VaccinatedRoutine` == "n/a"] <- "0"
GF$`HH[3]/group1/Tot_child_VaccinatedRoutine`[GF$`HH[3]/group1/Tot_child_VaccinatedRoutine` == "n/a"] <- "0"
GF$`HH[4]/group1/Tot_child_VaccinatedRoutine`[GF$`HH[4]/group1/Tot_child_VaccinatedRoutine` == "n/a"] <- "0"
GF$`HH[5]/group1/Tot_child_VaccinatedRoutine`[GF$`HH[5]/group1/Tot_child_VaccinatedRoutine` == "n/a"] <- "0"
GF$`HH[6]/group1/Tot_child_VaccinatedRoutine`[GF$`HH[6]/group1/Tot_child_VaccinatedRoutine` == "n/a"] <- "0"
GF$`HH[7]/group1/Tot_child_VaccinatedRoutine`[GF$`HH[7]/group1/Tot_child_VaccinatedRoutine` == "n/a"] <- "0"
GF$`HH[8]/group1/Tot_child_VaccinatedRoutine`[GF$`HH[8]/group1/Tot_child_VaccinatedRoutine` == "n/a"] <- "0"
GF$`HH[9]/group1/Tot_child_VaccinatedRoutine`[GF$`HH[9]/group1/Tot_child_VaccinatedRoutine` == "n/a"] <- "0"
GF$`HH[10]/group1/Tot_child_VaccinatedRoutine`[GF$`HH[10]/group1/Tot_child_VaccinatedRoutine` == "n/a"] <- "0"

#hh_other_reasons_non_vaccinated

GF$`HH[1]/group1/Tot_child_Others_HH`[GF$`HH[1]/group1/Tot_child_Others_HH` == "n/a"] <- "0"
GF$`HH[2]/group1/Tot_child_Others_HH`[GF$`HH[2]/group1/Tot_child_Others_HH` == "n/a"] <- "0"
GF$`HH[3]/group1/Tot_child_Others_HH`[GF$`HH[3]/group1/Tot_child_Others_HH` == "n/a"] <- "0"
GF$`HH[4]/group1/Tot_child_Others_HH`[GF$`HH[4]/group1/Tot_child_Others_HH` == "n/a"] <- "0"
GF$`HH[5]/group1/Tot_child_Others_HH`[GF$`HH[5]/group1/Tot_child_Others_HH` == "n/a"] <- "0"
GF$`HH[6]/group1/Tot_child_Others_HH`[GF$`HH[6]/group1/Tot_child_Others_HH` == "n/a"] <- "0"
GF$`HH[7]/group1/Tot_child_Others_HH`[GF$`HH[7]/group1/Tot_child_Others_HH` == "n/a"] <- "0"
GF$`HH[8]/group1/Tot_child_Others_HH`[GF$`HH[8]/group1/Tot_child_Others_HH` == "n/a"] <- "0"
GF$`HH[9]/group1/Tot_child_Others_HH`[GF$`HH[9]/group1/Tot_child_Others_HH` == "n/a"] <- "0"
GF$`HH[10]/group1/Tot_child_Others_HH`[GF$`HH[10]/group1/Tot_child_Others_HH` == "n/a"] <- "0"
#HH_CAREGIVER8INFORM
GF$`HH[1]/Parent_Caregive_Inform_HH`[GF$`HH[1]/Parent_Caregive_Inform_HH` == "Y"] <- 1
GF$`HH[1]/Parent_Caregive_Inform_HH`[GF$`HH[1]/Parent_Caregive_Inform_HH` == "Yes"] <- 1
GF$`HH[1]/Parent_Caregive_Inform_HH`[GF$`HH[1]/Parent_Caregive_Inform_HH` == "1"] <- 1
GF$`HH[1]/Parent_Caregive_Inform_HH`[GF$`HH[1]/Parent_Caregive_Inform_HH` == "N"] <- 0
GF$`HH[1]/Parent_Caregive_Inform_HH`[GF$`HH[1]/Parent_Caregive_Inform_HH` == "No"] <- 0
GF$`HH[1]/Parent_Caregive_Inform_HH`[GF$`HH[1]/Parent_Caregive_Inform_HH` == "0"] <- 0
GF$`HH[2]/Parent_Caregive_Inform_HH`[GF$`HH[2]/Parent_Caregive_Inform_HH` == "Y"] <- 1
GF$`HH[2]/Parent_Caregive_Inform_HH`[GF$`HH[2]/Parent_Caregive_Inform_HH` == "Yes"] <- 1
GF$`HH[2]/Parent_Caregive_Inform_HH`[GF$`HH[2]/Parent_Caregive_Inform_HH` == "1"] <- 1
GF$`HH[2]/Parent_Caregive_Inform_HH`[GF$`HH[2]/Parent_Caregive_Inform_HH` == "N"] <- 0
GF$`HH[2]/Parent_Caregive_Inform_HH`[GF$`HH[2]/Parent_Caregive_Inform_HH` == "No"] <- 0
GF$`HH[2]/Parent_Caregive_Inform_HH`[GF$`HH[2]/Parent_Caregive_Inform_HH` == "0"] <- 0
GF$`HH[3]/Parent_Caregive_Inform_HH`[GF$`HH[3]/Parent_Caregive_Inform_HH` == "Y"] <- 1
GF$`HH[3]/Parent_Caregive_Inform_HH`[GF$`HH[3]/Parent_Caregive_Inform_HH` == "Yes"] <- 1
GF$`HH[3]/Parent_Caregive_Inform_HH`[GF$`HH[3]/Parent_Caregive_Inform_HH` == "1"] <- 1
GF$`HH[3]/Parent_Caregive_Inform_HH`[GF$`HH[3]/Parent_Caregive_Inform_HH` == "N"] <- 0
GF$`HH[3]/Parent_Caregive_Inform_HH`[GF$`HH[3]/Parent_Caregive_Inform_HH` == "No"] <- 0
GF$`HH[3]/Parent_Caregive_Inform_HH`[GF$`HH[3]/Parent_Caregive_Inform_HH` == "0"] <- 0
GF$`HH[4]/Parent_Caregive_Inform_HH`[GF$`HH[4]/Parent_Caregive_Inform_HH` == "Y"] <- 1
GF$`HH[4]/Parent_Caregive_Inform_HH`[GF$`HH[4]/Parent_Caregive_Inform_HH` == "Yes"] <- 1
GF$`HH[4]/Parent_Caregive_Inform_HH`[GF$`HH[4]/Parent_Caregive_Inform_HH` == "1"] <- 1
GF$`HH[4]/Parent_Caregive_Inform_HH`[GF$`HH[4]/Parent_Caregive_Inform_HH` == "N"] <- 0
GF$`HH[4]/Parent_Caregive_Inform_HH`[GF$`HH[4]/Parent_Caregive_Inform_HH` == "No"] <- 0
GF$`HH[4]/Parent_Caregive_Inform_HH`[GF$`HH[4]/Parent_Caregive_Inform_HH` == "0"] <- 0
GF$`HH[5]/Parent_Caregive_Inform_HH`[GF$`HH[5]/Parent_Caregive_Inform_HH` == "Y"] <- 1
GF$`HH[5]/Parent_Caregive_Inform_HH`[GF$`HH[5]/Parent_Caregive_Inform_HH` == "Yes"] <- 1
GF$`HH[5]/Parent_Caregive_Inform_HH`[GF$`HH[5]/Parent_Caregive_Inform_HH` == "1"] <- 1
GF$`HH[5]/Parent_Caregive_Inform_HH`[GF$`HH[5]/Parent_Caregive_Inform_HH` == "N"] <- 0
GF$`HH[5]/Parent_Caregive_Inform_HH`[GF$`HH[5]/Parent_Caregive_Inform_HH` == "No"] <- 0
GF$`HH[5]/Parent_Caregive_Inform_HH`[GF$`HH[5]/Parent_Caregive_Inform_HH` == "0"] <- 0
GF$`HH[6]/Parent_Caregive_Inform_HH`[GF$`HH[6]/Parent_Caregive_Inform_HH` == "Y"] <- 1
GF$`HH[6]/Parent_Caregive_Inform_HH`[GF$`HH[6]/Parent_Caregive_Inform_HH` == "Yes"] <- 1
GF$`HH[6]/Parent_Caregive_Inform_HH`[GF$`HH[6]/Parent_Caregive_Inform_HH` == "1"] <- 1
GF$`HH[6]/Parent_Caregive_Inform_HH`[GF$`HH[6]/Parent_Caregive_Inform_HH` == "N"] <- 0
GF$`HH[6]/Parent_Caregive_Inform_HH`[GF$`HH[6]/Parent_Caregive_Inform_HH` == "No"] <- 0
GF$`HH[6]/Parent_Caregive_Inform_HH`[GF$`HH[6]/Parent_Caregive_Inform_HH` == "0"] <- 0
GF$`HH[7]/Parent_Caregive_Inform_HH`[GF$`HH[7]/Parent_Caregive_Inform_HH` == "Y"] <- 1
GF$`HH[7]/Parent_Caregive_Inform_HH`[GF$`HH[7]/Parent_Caregive_Inform_HH` == "Yes"] <- 1
GF$`HH[7]/Parent_Caregive_Inform_HH`[GF$`HH[7]/Parent_Caregive_Inform_HH` == "1"] <- 1
GF$`HH[7]/Parent_Caregive_Inform_HH`[GF$`HH[7]/Parent_Caregive_Inform_HH` == "N"] <- 0
GF$`HH[7]/Parent_Caregive_Inform_HH`[GF$`HH[7]/Parent_Caregive_Inform_HH` == "No"] <- 0
GF$`HH[7]/Parent_Caregive_Inform_HH`[GF$`HH[7]/Parent_Caregive_Inform_HH` == "0"] <- 0
GF$`HH[8]/Parent_Caregive_Inform_HH`[GF$`HH[8]/Parent_Caregive_Inform_HH` == "Y"] <- 1
GF$`HH[8]/Parent_Caregive_Inform_HH`[GF$`HH[8]/Parent_Caregive_Inform_HH` == "Yes"] <- 1
GF$`HH[8]/Parent_Caregive_Inform_HH`[GF$`HH[8]/Parent_Caregive_Inform_HH` == "1"] <- 1
GF$`HH[8]/Parent_Caregive_Inform_HH`[GF$`HH[8]/Parent_Caregive_Inform_HH` == "N"] <- 0
GF$`HH[8]/Parent_Caregive_Inform_HH`[GF$`HH[8]/Parent_Caregive_Inform_HH` == "No"] <- 0
GF$`HH[8]/Parent_Caregive_Inform_HH`[GF$`HH[8]/Parent_Caregive_Inform_HH` == "0"] <- 0
GF$`HH[9]/Parent_Caregive_Inform_HH`[GF$`HH[9]/Parent_Caregive_Inform_HH` == "Y"] <- 1
GF$`HH[9]/Parent_Caregive_Inform_HH`[GF$`HH[9]/Parent_Caregive_Inform_HH` == "Yes"] <- 1
GF$`HH[9]/Parent_Caregive_Inform_HH`[GF$`HH[9]/Parent_Caregive_Inform_HH` == "1"] <- 1
GF$`HH[9]/Parent_Caregive_Inform_HH`[GF$`HH[9]/Parent_Caregive_Inform_HH` == "N"] <- 0
GF$`HH[9]/Parent_Caregive_Inform_HH`[GF$`HH[9]/Parent_Caregive_Inform_HH` == "No"] <- 0
GF$`HH[9]/Parent_Caregive_Inform_HH`[GF$`HH[9]/Parent_Caregive_Inform_HH` == "0"] <- 0
GF$`HH[10]/Parent_Caregive_Inform_HH`[GF$`HH[10]/Parent_Caregive_Inform_HH` == "Y"] <- 1
GF$`HH[10]/Parent_Caregive_Inform_HH`[GF$`HH[10]/Parent_Caregive_Inform_HH` == "Yes"] <- 1
GF$`HH[10]/Parent_Caregive_Inform_HH`[GF$`HH[10]/Parent_Caregive_Inform_HH` == "1"] <- 1
GF$`HH[10]/Parent_Caregive_Inform_HH`[GF$`HH[10]/Parent_Caregive_Inform_HH` == "N"] <- 0
GF$`HH[10]/Parent_Caregive_Inform_HH`[GF$`HH[10]/Parent_Caregive_Inform_HH` == "No"] <- 0
GF$`HH[10]/Parent_Caregive_Inform_HH`[GF$`HH[10]/Parent_Caregive_Inform_HH` == "0"] <- 0
#mutate_for_summarise
GG <- GF |>
  # replace(is.na("."), "0") |> 
  mutate(across((starts_with("HH[")),
                as.numeric))
GH <- GG |>
  mutate(
    u5_present = rowSums(across(
      c("HH[1]/Total_U5_Present_HH":"HH[10]/Total_U5_Present_HH")
    )),
    u5_FM1 = rowSums(across(
      c("HH[1]/U5_Vac_FM_HH":"HH[10]/U5_Vac_FM_HH")
    )),
    u5_FM = ifelse(u5_FM1>u5_present, u5_present, u5_FM1),
    missed_child = (u5_present - u5_FM),
    r_non_FM_Absent = rowSums(across(
      c(
        "HH[1]/group1/Tot_child_Absent_HH":"HH[10]/group1/Tot_child_Absent_HH"
      )
    )),
    r_non_FM_NC = rowSums(across(
      c(
        "HH[1]/group1/Tot_child_NC_HH":"HH[10]/group1/Tot_child_NC_HH"
      )
    )),
    r_non_FM_hh_notvisited = rowSums(across(
      c(
        "HH[1]/group1/Tot_child_NotVisited_HH":"HH[10]/group1/Tot_child_NotVisited_HH"
      )
    )),
    r_non_FM_hh_notrevisited = rowSums(across(
      c(
        "HH[1]/group1/Tot_child_NotRevisited":"HH[10]/group1/Tot_child_NotRevisited"
      )
    )),
    r_non_FM_sleep = rowSums(across(
      c(
        "HH[1]/group1/Tot_child_Asleep_HH":"HH[10]/group1/Tot_child_Asleep_HH"
      )
    )),
    r_non_FM_vaccinatedRoutine = rowSums(across(
      c(
        "HH[1]/group1/Tot_child_VaccinatedRoutine":"HH[10]/group1/Tot_child_VaccinatedRoutine"
      )
    )),
    r_non_FM_other = rowSums(across(
      c(
        "HH[1]/group1/Tot_child_Others_HH":"HH[10]/group1/Tot_child_Others_HH"
      )
    )),
    # care_Giver_Informed_SIA = sum(`HH[1]/Parent_Caregive_Inform_HH`, `HH[2]/Parent_Caregive_Inform_HH`, `HH[3]/Parent_Caregive_Inform_HH`, `HH[4]/Parent_Caregive_Inform_HH`, `HH[5]/Parent_Caregive_Inform_HH`, `HH[6]/Parent_Caregive_Inform_HH`, `HH[7]/Parent_Caregive_Inform_HH`, `HH[8]/Parent_Caregive_Inform_HH`, `HH[9]/Parent_Caregive_Inform_HH`, `HH[10]/Parent_Caregive_Inform_HH`, na.rm = TRUE )
    care_Giver_Informed_SIA = rowSums(across(
      c(
        "HH[1]/Parent_Caregive_Inform_HH":"HH[10]/Parent_Caregive_Inform_HH"
      )
    ))
  )
  
GJ<- GH |>
  mutate(HH_count = as.numeric(HH_count)) |> 
  mutate(District = case_when(
    Country	=="COTE D'IVOIRE"&	Region	=="MORONOU" &	District	=="MBATTO" ~	"MBATTO",
    Country	=="COTE D'IVOIRE"&	Region	=="ABIDJAN1" &	District	=="ABOBO_EST" ~	"ABOBO EST",
    Country	=="COTE D'IVOIRE"&	Region	=="ABIDJAN1" &	District	=="ABOBO_OUEST" ~	"ABOBO OUEST",
    Country	=="COTE D'IVOIRE"&	Region	=="ABIDJAN1" &	District	=="YOPOUGON_EST" ~	"YOPOUGON_EST",
    Country	=="COTE D'IVOIRE"&	Region	=="ABIDJAN1" &	District	=="YOPOUGON_OUEST_SONGON" ~	"YOPOUGON_OUEST_SONGON",
    Country	=="COTE D'IVOIRE"&	Region	=="ABIDJAN2" &	District	=="ADJAME_PLATEAU_ATTECOUBE" ~	"ADJAME_PLATEAU_ATTECOUBE",
    Country	=="COTE D'IVOIRE"&	Region	=="ABIDJAN2" &	District	=="COCODY_BINGERVILLE" ~	"COCODY_BINGERVILLE",
    Country	=="COTE D'IVOIRE"&	Region	=="ABIDJAN2" &	District	=="PORT_BOUET_VRIDI" ~	"PORT_BOUET_VRIDI",
    Country	=="COTE D'IVOIRE"&	Region	=="ABIDJAN2" &	District	=="TREICHVILLE_MARCORY" ~	"TREICHVILLE_MARCORY",
    Country	=="COTE D'IVOIRE"&	Region	=="GBEKE" &	District	=="BOUAKE_NORD_EST" ~	"BOUAKE_NORD_EST",
    Country	=="COTE D'IVOIRE"&	Region	=="GBEKE" &	District	=="BOUAKE_NORD_OUEST" ~	"BOUAKE_NORD_OUEST",
    Country	=="COTE D'IVOIRE"&	Region	=="GBEKE" &	District	=="BOUAKE_SUD" ~	"BOUAKE_SUD",
    Country	=="COTE D'IVOIRE"&	Region	=="GONTOUGO" &	District	=="KOUN_FAO" ~	"KOUN_FAO",
    Country	=="COTE D'IVOIRE"&	Region	=="GRANDS_PONTS" &	District	=="GRAND_LAHOU" ~	"GRAND_LAHOU",
    Country	=="COTE D'IVOIRE"&	Region	=="IFFOU" &	District	=="M'BAHIAKRO" ~	"MBAHIAKRO",
    Country	=="COTE D'IVOIRE"&	Region	=="LAME" &	District	=="YAKASSE_ATTOBROU" ~	"YAKASSE_ATTOBROU",
    Country	=="COTE D'IVOIRE"&	Region	=="PORO" &	District	=="MBENGUE" ~	"MBENGUE",
    Country	=="COTE D'IVOIRE"&	Region	=="SUD_COMOE" &	District	=="GRAND_BASSAM" ~	"GRAND_BASSAM",
    Country	=="COTE D'IVOIRE"&	Region	=="TONKPI" &	District	=="ZOUAN_HOUNIEN" ~	"ZOUAN_HOUNIEN",
    Country	=="COTE D'IVOIRE"&	Region	=="SAN_PEDRO1" &	District	=="SAN_PEDRO" ~	"SAN_PEDRO",
    Country	=="COTE D'IVOIRE"&	Region	=="TONKPI" &	District	=="ZOUAN-HOUNIEN" ~	"ZOUAN_HOUNIEN",
    Country	=="COTE D'IVOIRE"&	Region	=="GRANDS PONTS" &	District	=="GRAND-LAHOU" ~	"GRAND_LAHOU",
    Country	=="COTE D'IVOIRE"&	Region	=="SAN PEDRO" &	District	=="SAN PEDRO" ~	"SAN_PEDRO",
    Country	=="COTE D'IVOIRE"&	Region	=="GONTOUGO" &	District	=="KOUN-FAO" ~	"KOUN_FAO",
    Country	=="COTE D'IVOIRE"&	Region	=="IFFOU" &	District	=="MBAHIAKRO" ~	"MBAHIAKRO",
    Country	=="COTE D'IVOIRE"&	Region	=="ABIDJAN 1" &	District	=="YOPOUGON-EST" ~	"YOPOUGON_EST",
    Country	=="COTE D'IVOIRE"&	Region	=="ABIDJAN 1" &	District	=="YOPOUGON-OUEST SONGON" ~	"YOPOUGON_OUEST_SONGON",
    Country	=="COTE D'IVOIRE"&	Region	=="ABIDJAN 2" &	District	=="ADJAME-PLATEAU-ATTECOUBE" ~	"ADJAME_PLATEAU_ATTECOUBE",
    Country	=="COTE D'IVOIRE"&	Region	=="ABIDJAN 2" &	District	=="COCODY BINGERVILLE" ~	"COCODY_BINGERVILLE",
    Country	=="COTE D'IVOIRE"&	Region	=="ABIDJAN 2" &	District	=="PORT-BOUET-VRIDI" ~	"PORT_BOUET_VRIDI",
    Country	=="COTE D'IVOIRE"&	Region	=="ABIDJAN 2" &	District	=="TREICHVILLE-MARCORY" ~	"TREICHVILLE_MARCORY",
    Country	=="COTE D'IVOIRE"&	Region	=="GBEKE" &	District	=="BOUAKE NORD-EST" ~	"BOUAKE_NORD_EST",
    Country	=="COTE D'IVOIRE"&	Region	=="GBEKE" &	District	=="BOUAKE NORD-OUEST" ~	"BOUAKE_NORD_OUEST",
    Country	=="COTE D'IVOIRE"&	Region	=="GBEKE" &	District	=="BOUAKE-SUD" ~	"BOUAKE_SUD",
    Country	=="COTE D'IVOIRE"&	Region	=="LA ME" &	District	=="YAKASSE-ATTOBROU" ~	"YAKASSE_ATTOBROU",
    Country	=="COTE D'IVOIRE"&	Region	=="MORONOU" &	District	=="M'BATTO" ~	"MBATTO",
    Country	=="COTE D'IVOIRE"&	Region	=="PORO" &	District	=="M'BENGUE" ~	"MBENGUE",
    Country	=="COTE D'IVOIRE"&	Region	=="SUD-COMOE" &	District	=="GRAND-BASSAM" ~	"GRAND_BASSAM",
    Country	=="COTE D'IVOIRE"&	Region	=="ABIDJAN 2" &	District	=="COCODY-BINGERVILLE" ~	"COCODY_BINGERVILLE",
    Country	=="COTE D'IVOIRE"&	Region	=="GBEKE" &	District	=="BOUAKE SUD" ~	"BOUAKE_SUD",
    Country	=="COTE D'IVOIRE"&	Region	=="SAN PEDRO" &	District	=="SAN-PEDRO" ~	"SAN_PEDRO",
    Country	=="CHAD"&	Region	=="BATHA" &	District	=="OUM_HADJER" ~	"OUM_HADJER",
    Country	=="CHAD"&	Region	=="KANEM" &	District	=="RIG_RIG" ~	"RIG_RIG",
    Country	=="CHAD"&	Region	=="OUADDAI" &	District	=="AMDAM" ~	"AMDAM",
    Country	=="CHAD"&	Region	=="SALAMAT" &	District	=="AM_TIMAN" ~	"AM_TIMAN",
    Country	=="CHAD"&	Region	=="TANDJILE" &	District	=="BAKTCHORO" ~	"BAKTCHORO",
    Country	=="CHAD"&	Region	=="MAYO_KEBBI_EST" &	District	=="PONT_CAROL" ~	"PONT_CAROL",
    Country	=="CHAD"&	Region	=="SALAMAT" &	District	=="HARAZE_MANGUEIGNE" ~	"HARAZE_MANGUEIGNE",
    Country	=="CHAD"&	Region	=="NDJAMENA" &	District	=="NDJAMENA_SUD" ~	"N'DJAMENA-SUD",
    Country	=="CHAD"&	Region	=="NDJAMENA" &	District	=="NDJAMENA_NORD" ~	"N'DJAMENA-NORD",
    Country	=="CHAD"&	Region	=="NDJAMENA" &	District	=="NDJAMENA_CENTRE" ~	"N'DJAMENA-CENTRE",
    Country	=="CHAD"&	Region	=="NDJAMENA" &	District	=="NDJAMENA_EST" ~	"N'DJAMENA-EST",
    Country	=="CHAD"&	Region	=="NDJAMENA" &	District	=="NDJAMENA_9AR" ~	"NDJAMENA_9AR",
    Country	=="CHAD"&	Region	=="CHARI_BAGUIRMI" &	District	=="BA_ILLI" ~	"BA_ILLI",
    Country	=="CHAD"&	Region	=="MAYO KEBBI EST" &	District	=="PONT CAROL" ~	"PONT_CAROL",
    Country	=="CHAD"&	Region	=="N'DJAMENA" &	District	=="N'DJAMENA-NORD" ~	"N'DJAMENA-NORD",
    Country	=="CHAD"&	Region	=="CHARI BAGUIRMI" &	District	=="BA-ILLI" ~	"BA_ILLI",
    Country	=="CHAD"&	Region	=="BATHA" &	District	=="OUM HADJER" ~	"OUM_HADJER",
    Country	=="CHAD"&	Region	=="KANEM" &	District	=="RIG-RIG" ~	"RIG_RIG",
    Country	=="CHAD"&	Region	=="N'DJAMENA" &	District	=="9e ARRONDISSEMENT" ~	"NDJAMENA_9AR",
    Country	=="CHAD"&	Region	=="N'DJAMENA" &	District	=="N'DJAMENA-CENTRE" ~	"N'DJAMENA-CENTRE",
    Country	=="CHAD"&	Region	=="N'DJAMENA" &	District	=="N'DJAMENA-EST" ~	"N'DJAMENA-EST",
    Country	=="CHAD"&	Region	=="N'DJAMENA" &	District	=="N'DJAMENA-SUD" ~	"N'DJAMENA-SUD",
    Country	=="CHAD"&	Region	=="OUADDAI" &	District	=="AM DAM" ~	"AMDAM",
    Country	=="CHAD"&	Region	=="SALAMAT" &	District	=="AM TIMAN" ~	"AM_TIMAN",
    Country	=="CHAD"&	Region	=="SALAMAT" &	District	=="HARAZE MANGUEIGNE" ~	"HARAZE_MANGUEIGNE",
    Country	=="CHAD"&	Region	=="TANDJILE" &	District	=="BACKTCHORO" ~	"BAKTCHORO",
    Country	=="CHAD"&	Region	=="NDJAMENA" &	District	=="NDJAMENA-NORD" ~	"N'DJAMENA-NORD",
    Country	=="CHAD"&	Region	=="NDJAMENA" &	District	=="NDJAMENA-SUD" ~	"N'DJAMENA-SUD",
    Country	=="CHAD"&	Region	=="NDJAMENA" &	District	=="NDJAMENA-CENTRE" ~	"N'DJAMENA-CENTRE",
    Country	=="CHAD"&	Region	=="NDJAMENA" &	District	=="NDJAMENA-EST" ~	"N'DJAMENA-EST",
    Country	=="CHAD"&	Region	=="SALAMAT" &	District	=="AM-TIMAN" ~	"AM_TIMAN",
    Country	=="BENIN"&	Region	=="LITTORAL" &	District	=="COTONOU 1" ~	"COTONOU 1",
    Country	=="BENIN"&	Region	=="LITTORAL" &	District	=="COTONOU 2" ~	"COTONOU 2",
    Country	=="BENIN"&	Region	=="LITTORAL" &	District	=="COTONOU 3" ~	"COTONOU 3",
    Country	=="BENIN"&	Region	=="LITTORAL" &	District	=="COTONOU 4" ~	"COTONOU 4",
    Country	=="BENIN"&	Region	=="LITTORAL" &	District	=="COTONOU 5" ~	"COTONOU 5",
    Country	=="BENIN"&	Region	=="LITTORAL" &	District	=="COTONOU 6" ~	"COTONOU 6",
    Country	=="BENIN"&	Region	=="OUEME" &	District	=="SEME-PODJI" ~	"SEME-PODJI",
    Country	=="BENIN"&	Region	=="Littoral" &	District	=="Cotonou I" ~	"COTONOU 1",
    Country	=="BENIN"&	Region	=="Littoral" &	District	=="Cotonou II" ~	"COTONOU 2",
    Country	=="BENIN"&	Region	=="Littoral" &	District	=="Cotonou III" ~	"COTONOU 3",
    Country	=="BENIN"&	Region	=="Littoral" &	District	=="Cotonou IV" ~	"COTONOU 4",
    Country	=="BENIN"&	Region	=="Littoral" &	District	=="Cotonou V" ~	"COTONOU 5",
    Country	=="BENIN"&	Region	=="Littoral" &	District	=="Cotonou VI" ~	"COTONOU 6",
    Country	=="BENIN"&	Region	=="Oueme" &	District	=="Seme-Kpodji" ~	"SEME-PODJI",
    TRUE ~ District)) |> 
  select(Country, Region, District, Response, roundNumber, date_monitored, Number_of_HH_visited = HH_count, u5_present, u5_FM, missed_child, r_non_FM_Absent, r_non_FM_NC, r_non_FM_hh_notvisited, r_non_FM_hh_notrevisited, r_non_FM_sleep, r_non_FM_vaccinatedRoutine, r_non_FM_other,care_Giver_Informed_SIA) |>
  group_by(Country, Region, District, Response, roundNumber) |>
  mutate(date_monitored = as_date(date_monitored)) |> 
  arrange(date_monitored) |> 
  mutate(date.diff = c(1, diff(date_monitored))) |> 
  mutate(period = cumsum(date.diff != 1)) |> 
  ungroup() |> 
  group_by(Country, Region, District, Response, roundNumber) |> 
  summarise(start_date = min(date_monitored),
            endate_date = max(date_monitored),
            Number_of_HH_visited = sum(Number_of_HH_visited),
            u5_present = sum(u5_present),
            u5_FM = sum(u5_FM),
            missed_child = sum(missed_child),
            r_non_FM_Absent = sum(r_non_FM_Absent),
            r_non_FM_NC = sum(r_non_FM_NC),
            r_non_FM_hh_notvisited = sum(r_non_FM_hh_notvisited),
            r_non_FM_hh_notrevisited = sum(r_non_FM_hh_notrevisited),
            r_non_FM_sleep = sum(r_non_FM_sleep),
            r_non_FM_vaccinatedRoutine = sum(r_non_FM_vaccinatedRoutine),
            r_non_FM_other = sum(r_non_FM_other),
            care_Giver_Informed_SIA = sum(care_Giver_Informed_SIA)) |>
  ungroup() |>
  mutate(cv = round(u5_FM/u5_present, 2),
         year = year(start_date),
         percent_care_Giver_Informed_SIA = (care_Giver_Informed_SIA/Number_of_HH_visited),
         percent_care_Giver_Informed_SIA = round(percent_care_Giver_Informed_SIA, 2),
         percent_care_Giver_Informed_SIA = percent_care_Giver_Informed_SIA*100) |>
  filter(start_date > 1-10-2019)
GJ<-GJ |> 
  mutate(Response = case_when(
    Country	=="BENIN"&	Response	=="KETOU" ~	"KETOU",
    Country	=="CIV"&	Response	=="ABENGOUROU" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="ABOBO_EST" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="ABOBO_OUEST" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="ABOISSO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="ADIAKE" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="ADJAME_PLATEAU_ATTECOUBE" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="ADZOPE" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="AGBOVILLE" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="AGNIBILEKROU" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="AKOUPE" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="ALEPE" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="ANYAMA" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="ARRAH" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="BANGOLO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="BEOUMI" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="BETTIE" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="BIANKOUMA" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="BLOLEQUIN" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="BOCANDA" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="BONDOUKOU" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="BONGOUANOU" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="BOTRO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="BOUAFLE" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="BOUAKE_NORD_EST" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="BOUAKE_NORD_OUEST" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="BOUAKE_SUD" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="BOUNA" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="BOUNDIALI" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="BUYO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="COCODY_BINGERVILLE" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="DABAKALA" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="DABOU" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="DALOA" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="DANANE" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="DAOUKRO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="DIANRA" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="DIDIEVI" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="DIKODOUGOU" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="DIMBOKRO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="DIVO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="DOROPO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="DUEKOUE" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="FRESCO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="GAGNOA1" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="GAGNOA2" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="GRAND_BASSAM" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="GRAND_LAHOU" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="GUEYO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="GUIGLO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="GUITRY" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="ISSIA" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="JACQUEVILLE" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="KANI" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="KANIASSO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="KATIOLA" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="KONG" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="KORHOGO_1" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="KORHOGO2" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="KORO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="KOUASSI_KOUASSIKRO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="KOUMASSI" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="KOUN_FAO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="KOUNAHIRI" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="KOUTO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="LAKOTA" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="MADINANI" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="MAN" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="MANKONO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="M'BAHIAKRO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="MBATTO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="MBENGUE" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="NIAKARAMADOUGOU" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="ODIENNE" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="OUANGOLODOUGOU" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="OUANINOU" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="OUME" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="PORT_BOUET_VRIDI" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="PORT_BOUET_VRIDI" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="PRIKRO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="SAKASSOU" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="SAN_PEDRO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="SANDEGUE" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="SASSANDRA" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="SEGUELA" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="SIKENSI" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="SINEMATIALI" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="SINFRA" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="SOUBRE" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="TABOU" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="TAI" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="TANDA" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="TEHINI" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="TENGRELA" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="TIAPOUM" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="TIASSALE" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="TIEBISSOU" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="TOUBA" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="TOULEPLEU" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="TOUMODI" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="TRANSUA" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="TREICHVILLE_MARCORY" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="VAVOUA" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="YAKASSE_ATTOBROU" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="YAMOUSSOUKRO" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="ZOUAN_HOUNIEN" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="ZOUKOUGBEU" ~	"CIV-113DS-09-2020",
    Country	=="CIV"&	Response	=="ZUENOULA" ~	"CIV-113DS-09-2020",
    Country	=="COG"&	Response	=="Congo" ~	"Congo",
    Country	=="GUI"&	Response	=="Conakry" ~	"Conakry",
    Country	=="KEN"&	Response	=="KEN95DS022021" ~	"KEN95DS022021",
    Country	=="MAL"&	Response	=="Arfounda" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Arifounda" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="BAMAKO" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Banamaba Nara" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Banamba" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Banamba et" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="BANAMBA et  NARA" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="BANAMBA et NAARA" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Banamba et nana" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Banamba et Nara" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Banamba Nara" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Banamba Narra" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="BANAMBA-NARA" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Bananba nara" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Banmba" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Banna" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="BEMA" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Bnamba" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Boye" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Cheickh Hamallah Camara" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Dedji" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Déméké" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Diandjoumé" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Dianguirdé" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="DIEMA" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Dimina" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Diongui" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="DIOUMARA" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Dire" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Diré" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="District de Nara" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Gadiba n'bombéyabé" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Gourel haïré" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Guedebiné" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Guinba nianga" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Guinba yel" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Hamadi-mourou" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Kahé" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Korkodio" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Koumarega" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Lewa ardo sadio" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Lewa dekolé" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Madihawaya" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="MENAKA" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Nara" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Nara Banamba" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Nara Banamba." ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Nara Bananba" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Nara et Banamba" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Nioro" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Nomo" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Riposte Banamba et Nara" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Riposte de Banamba et de Nara" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Riposte de Banamba et Nara" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Senewaly" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Seoundé" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Td" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Tintiba" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="TORODO" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Toumani" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Tourourou" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Yeligara" ~	"MLI-12DS-01-2021",
    Country	=="MAL"&	Response	=="Yéréré" ~	"MLI-12DS-01-2022",
    Country	=="MAL"&	Response	=="Youri" ~	"MLI-12DS-01-2023",
    Country	=="NIE"&	Response	=="Kwara response" ~	"Kwara response",
    Country	=="NIE"&	Response	=="Mop_Up" ~	"Mop_Up",
    Country	=="NIE"&	Response	=="n/a" ~	"n/a",
    Country	=="NIE"&	Response	=="NID" ~	"NID",
    Country	=="NIE"&	Response	=="OBR" ~	"OBR",
    Country	=="NIE"&	Response	=="Revaccination" ~	"Revaccination",
    Country	=="NIE"&	Response	=="SNID" ~	"SNID",
    Country	=="RCA"&	Response	=="Batafango" ~	"Batafango",
    Country	=="RDC"&	Response	=="MATETE" ~	"MATETE",
    Country	=="RDC"&	Response	=="MATETE" ~	"MATETE",
    Country == "SSUD" & Response == "SSD-2024-02-01_bOPV"  ~ "SSD-2024-02-01_nOPV",
    TRUE ~Response))
GK<-GJ |> 
  mutate(Vaccine.type = case_when(
    Country	=="BENIN"&	Response	=="KETOU" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="CIV"&	Response	=="CIV-113DS-09-2020" ~	"mOPV",
    Country	=="COG"&	Response	=="Congo" ~	"nOPV",
    Country	=="GUI"&	Response	=="Conakry" ~	"mOPV",
    Country	=="KEN"&	Response	=="KEN95DS022021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2021" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2022" ~	"mOPV",
    Country	=="MAL"&	Response	=="MLI-12DS-01-2023" ~	"mOPV",
    Country	=="NIE"&	Response	=="Kwara response" ~	"other",
    Country	=="NIE"&	Response	=="Mop_Up" ~	"other",
    Country	=="NIE"&	Response	=="n/a" ~	"n/a",
    Country	=="NIE"&	Response	=="NID" ~	"other",
    Country	=="NIE"&	Response	=="OBR" ~	"other",
    Country	=="NIE"&	Response	=="Revaccination" ~	"other",
    Country	=="NIE"&	Response	=="SNID" ~	"other",
    Country	=="RCA"&	Response	=="Batafango" ~	"mOPV",
    Country	=="RDC"&	Response	=="MATETE" ~	"mOPV",
    Country	=="RDC"&	Response	=="MATETE" ~	"mOPV",
    str_detect(Response, pattern = "Ouagadogou") ~ "mOPV",
    str_detect(Response, pattern = "BITTOU") ~ "mOPV",
    str_detect(Response, pattern = "Ouagadogou") ~ "mOPV",
    str_detect(Response, pattern = "nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "bOPV") ~ "bOPV",
    str_detect(Response, pattern = "WPV1") ~ "bOPV",
    str_detect(Response, pattern = "VPOn") ~ "nOPV2",
    str_detect(Response, pattern = "TSHUAPA") ~ "nOPV2",
    str_detect(Response, pattern = "VPOb") ~ "bOPV",
    str_detect(Response, pattern = "Tanganyika") ~ "nOPV2",
    str_detect(Response, pattern = "Bangui 1") ~ "mOPV",
    str_detect(Response, pattern = "nVPO") ~ "nOPV",
    str_detect(Response, pattern = "GOTHEY") ~ "mOPV",
    str_detect(Response, pattern = "OPV") ~ "bOPV",
    str_detect(Response, pattern = "Liberia") ~ "nOPV2",
    str_detect(Response, pattern = "Mauritania") ~ "nOPV2",
    str_detect(Response, pattern = "YOPOUGON") ~ "mOPV",
    str_detect(Response, pattern = "Golfe") ~ "mOPV",
    str_detect(Response, pattern = "Kankan") ~ "nOPV2",
    str_detect(Response, pattern = "KOUIBLY") ~ "nOPV2",
    str_detect(Response, pattern = "Sierra Leone") ~ "nOPV2",
    str_detect(Response, pattern = "SEN") ~ "nOPV2",
    str_detect(Response, pattern = "CEN") ~ "nOPV2",
    str_detect(Response, pattern = "MAL") ~ "nOPV2",
    str_detect(Response, pattern = "BEN-26DS-08-2020") ~ "nOPV2",
    str_detect(Response, pattern = "BEN-39DS-01-2021") ~ "nOPV2",
    str_detect(Response, pattern = "BEN-39DS-01-2021") ~ "nOPV2",
    str_detect(Response, pattern = "BEN-xxDS-02-2020") ~ "mOPV2",
    str_detect(Response, pattern = "BERTOUA") ~ "nOPV2",
    str_detect(Response, pattern = "EBOLOWA") ~ "nOPV2",
    str_detect(Response, pattern = "EXNORD") ~ "nOPV2",
    str_detect(Response, pattern = "ExtNord2023") ~ "nOPV2",
    str_detect(Response, pattern = "NID_LID_preventive") ~ "bOPV",
    str_detect(Response, pattern = "ExtNord2023") ~ "nOPV2",
    str_detect(Response, pattern = "ADDIS ABABA") ~ "nOPV2",
    str_detect(Response, pattern = "Mekelle") ~ "nOPV2",
    str_detect(Response, pattern = "AMANSIE SOUTH") ~ "nOPV2",
    str_detect(Response, pattern = "Bangui 1") ~ "nOPV2",
    str_detect(Response, pattern = "CAF-2020-002") ~ "nOPV2",
    str_detect(Response, pattern = "CENBLOCK") ~ "nOPV2",
    str_detect(Response, pattern = "CENTRALBLK") ~ "nOPV2",
    str_detect(Response, pattern = "CHA-17DS-02-2020") ~ "nOPV2",
    str_detect(Response, pattern = "DONOMANGA") ~ "nOPV2",
    str_detect(Response, pattern = "GNBnOPV") ~ "nOPV2",
    str_detect(Response, pattern = "GOLFE") ~ "nOPV2",
    str_detect(Response, pattern = "GOTHEYE") ~ "nOPV2",
    str_detect(Response, pattern = "KEN-13DS-02-2021") ~ "nOPV2",
    str_detect(Response, pattern = "Liberia") ~ "nOPV2",
    str_detect(Response, pattern = "MopUp2022") ~ "nOPV2",
    str_detect(Response, pattern = "SSD-79DS-09-2020") ~ "nOPV2",
    str_detect(Response, pattern = "WPV1MLW") ~ "bOPV",
    str_detect(Response, pattern = "WPV1Response") ~ "bOPV",
    str_detect(Response, pattern = "Liberia") ~ "nOPV2",
    str_detect(Response, pattern = "ALG-2023-09-01_nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "ALG-2024-01-01_nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "nOPV2022") ~ "nOPV2",
    str_detect(Response, pattern = "BEN-2023-09-01_nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "BFA-2023-05-01_nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "BFA-2023-09-01_nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "BFA-2024-02-01_nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "BITTOU-mOPV2") ~ "mOPV",
    str_detect(Response, pattern = "Ouagadogou-mOPV2") ~ "mOPV",
    str_detect(Response, pattern = "BOT-2023-02-01_nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "CAM-2023-05-01_nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "CAM-2023-08-01_nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "CAM-2024-02-01_nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "MDG-2023-03-01_bOPV") ~ "mOPV",
    str_detect(Response, pattern = "MENAKA-mOPV2") ~ "nOPV2",
    str_detect(Response, pattern = "MLI-12DS-01-2021-mOPV2") ~ "mOPV",
    str_detect(Response, pattern = "nOPV2022") ~ "nOPV2",
    str_detect(Response, pattern = "nOPV2023") ~ "nOPV2",
    str_detect(Response, pattern = "nVPO") ~ "nOPV2",
    str_detect(Response, pattern = "nVPO_Maradi") ~ "nOPV2",
    str_detect(Response, pattern = "nVPO_Zinder") ~ "nOPV2",
    str_detect(Response, pattern = "nVPO2") ~ "nOPV2",
    str_detect(Response, pattern = "OPVb May2021") ~ "nOPV2",
    str_detect(Response, pattern = "OPVb2021") ~ "nOPV2",
    str_detect(Response, pattern = "OPVb2022") ~ "nOPV2",
    str_detect(Response, pattern = "RSSmOPV10C2021") ~ "nOPV2",
    str_detect(Response, pattern = "SEN_VPOn") ~ "nOPV2",
    str_detect(Response, pattern = "UGAnOPV") ~ "nOPV2",
    str_detect(Response, pattern = "VPOb") ~ "nOPV2",
    str_detect(Response, pattern = "nOPV2022") ~ "nOPV2",
    str_detect(Response, pattern = "nVPO2") ~ "nOPV2",
    str_detect(Response, pattern = "VPOb13ProV") ~ "nOPV2",
    str_detect(Response, pattern = "Liberia") ~ "nOPV2"))

GL<-GK |>
  mutate(M1 = rowSums(across(c(r_non_FM_Absent, r_non_FM_NC, r_non_FM_hh_notvisited, r_non_FM_hh_notrevisited, r_non_FM_sleep, r_non_FM_vaccinatedRoutine))),
         M2 = rowSums(across(c(r_non_FM_other,M1))),
         Other_reasons = ifelse((M2 = missed_child), r_non_FM_other, missed_child - M1),
         Country = case_when(
           Country == "DRC" ~ "RDC",
           Country == "Camerooun" ~ "CAE",
           Country =="Ethiopia" ~ "ETH",
           Country == "BURKINA_FASO" ~ "BFA",
           Country == "BENIN" ~ "BEN",
           Country == "CAMEROON" ~ "CAE",
           Country == "CHAD" ~ "CHD",
           TRUE ~ Country),
         roundNumber =  case_when(
           roundNumber =="RND2" ~ "Rnd2",
           TRUE ~ roundNumber))

G4<-GL |> 
  mutate(
    start_date = as_date(start_date),
    start_date = case_when(
      Response == "DRC-7DS-02-2022" & roundNumber =="Rnd1"~ as_date("2022-05-28"),
      Response == "DRC-7DS-02-2022" & roundNumber =="Rnd2"~ as_date("2022-07-26"),
      Response == "DRC-39DS-01-2021" & roundNumber =="Rnd1"~ as_date("2021-06-23"),
      Response == "DRC-23DS-12-2020" & roundNumber =="Rnd1"~ as_date("2020-10-12"),
      Response == "DRC-23DS-12-2020" & roundNumber =="Rnd2"~ as_date("2021-03-25"),
      Response == "DRC-39DS-01-2021" & roundNumber =="Rnd1"~ as_date("2021-06-23"),
      Response == "DRC-2023-03-01_bOPV" & roundNumber =="Rnd1"~ as_date("2023-03-20"),
      Response == "DRC-2023-05-01_nOPV" & roundNumber =="Rnd1"~ as_date("2023-06-05"),
      Response == "DRC-2023-09-KIN_nOPV" & roundNumber =="Rnd1"~ as_date("2023-09-26"),
      Response == "DRC-2023-11-02_nOPV" & roundNumber =="Rnd1"~ as_date("2023-11-20"),
      Response == "DRC-NID-01-2024-bOPV" & roundNumber =="Rnd1"~ as_date("2024-02-06"),
      Response == "DRC-NID-03-2024-B_nOPV" & roundNumber =="Rnd1"~ as_date("2024-04-02"),
      Response == "DRC-NID-04-2024-bOPV" & roundNumber =="Rnd1"~ as_date("2024-06-17"),
      Response == "DRC-NID-07-2023-bOPV" & roundNumber =="Rnd1"~ as_date("2023-07-31"),
      Response == "DRC-NID-07-2023-bOPV" & roundNumber =="Rnd2"~ as_date("2023-09-26"),
      Response == "DRC-sNID-08-2024-B_nOPV" & roundNumber =="Rnd1"~ as_date("2024-08-12"),
      Response == "Tanganyika" & roundNumber =="Rnd1"~ as_date("2023-01-30"),
      Response == "BEN-2023-09-01_nOPV" & start_date ==as_date("2024-07-02")~ as_date("2024-06-13"),
      Response == "BEN-2023-09-01_nOPV" & start_date ==as_date("2024-08-02")~ as_date("2024-06-13"),
      Response == "ALG-2024-01-01_nOPV" & start_date ==as_date("2024-02-28")~ as_date("2024-02-23"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-02-29")~ as_date("2024-02-23"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-03-01")~ as_date("2024-02-23"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-03-02")~ as_date("2024-02-23"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-03-03")~ as_date("2024-02-23"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-03-04")~ as_date("22024-02-23"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-04-04")~ as_date("2024-02-23"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-07-02")~ as_date("2024-02-23"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-08-02")~ as_date("2024-02-23"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-09-02")~ as_date("2024-02-23"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-10-02")~ as_date("2024-02-23"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-11-02")~ as_date("2024-02-23"),
      Response == "BEN-2023-09-01_nOPV" & roundNumber =="Rnd1"~ as_date("2024-02-06"),
      Response == "BEN-2023-09-01_nOPV" & roundNumber =="Rnd2"~ as_date("2024-06-13"),
      Response == "SSD-2024-02-01_nOPV2" & roundNumber =="Rnd1"~ as_date("2024-03-13"),
      Response == "SSD-2024-02-01_nOPV2" & roundNumber =="Rnd2"~ as_date("2024-04-19"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-02-02")~ as_date("2024-02-06"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-05-02")~ as_date("2024-02-06"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-06-02")~ as_date("2024-02-06"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-07-02")~ as_date("2024-02-06"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-10-02")~ as_date("2024-02-06"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-08-02")~ as_date("2024-02-06"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-01-04")~ as_date("2024-04-02"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-02-04")~ as_date("2024-04-02"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-03-04")~ as_date("2024-04-02"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-07-02")~ as_date("2024-04-02"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-10-02")~ as_date("2024-04-02"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-08-02")~ as_date("2024-04-02"),
      Response == "CAM-2024-02-01_nOPV" & roundNumber =="Rnd1"~ as_date("2024-03-05"),
      Response == "DRC-NID-03-2024-B_nOPV" ~ as_date("2024-04-02"),
      Response == "LBR-2024-05-01_nOPV" & roundNumber =="Rnd2"~ as_date("2024-06-12"),
      TRUE ~ start_date),
    endate_date = case_when(
      Response == "DRC-7DS-02-2022" & roundNumber =="Rnd1"~ as_date("2022-05-28"),
      Response == "DRC-7DS-02-2022" & roundNumber =="Rnd2"~ as_date("2022-07-26"),
      Response == "DRC-39DS-01-2021" & roundNumber =="Rnd1"~ as_date("2021-06-23"),
      Response == "DRC-23DS-12-2020" & roundNumber =="Rnd1"~ as_date("2020-10-12"),
      Response == "DRC-23DS-12-2020" & roundNumber =="Rnd2"~ as_date("2021-03-25"),
      Response == "DRC-39DS-01-2021" & roundNumber =="Rnd1"~ as_date("2021-06-23"),
      Response == "DRC-2023-03-01_bOPV" & roundNumber =="Rnd1"~ as_date("2023-03-20"),
      Response == "DRC-2023-05-01_nOPV" & roundNumber =="Rnd1"~ as_date("2023-06-05"),
      Response == "DRC-2023-09-KIN_nOPV" & roundNumber =="Rnd1"~ as_date("2023-09-26"),
      Response == "DRC-2023-11-02_nOPV" & roundNumber =="Rnd1"~ as_date("2023-11-20"),
      Response == "DRC-NID-01-2024-bOPV" & roundNumber =="Rnd1"~ as_date("2024-02-06"),
      Response == "DRC-NID-03-2024-B_nOPV" & roundNumber =="Rnd1"~ as_date("2024-04-02"),
      Response == "DRC-NID-04-2024-bOPV" & roundNumber =="Rnd1"~ as_date("2024-06-17"),
      Response == "DRC-NID-07-2023-bOPV" & roundNumber =="Rnd1"~ as_date("2023-07-31"),
      Response == "DRC-NID-07-2023-bOPV" & roundNumber =="Rnd2"~ as_date("2023-09-26"),
      Response == "DRC-sNID-08-2024-B_nOPV" & roundNumber =="Rnd1"~ as_date("2024-08-12"),
      Response == "Tanganyika" & roundNumber =="Rnd1"~ as_date("2023-01-30"),
      Response == "BEN-2023-09-01_nOPV" & start_date ==as_date("2024-07-02")~ as_date("2024-06-13"),
      Response == "BEN-2023-09-01_nOPV" & start_date ==as_date("2024-08-02")~ as_date("2024-06-13"),
      Response == "ALG-2024-01-01_nOPV" & start_date ==as_date("2024-02-28")~ as_date("2024-02-23"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-02-29")~ as_date("2024-02-23"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-03-01")~ as_date("2024-02-23"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-03-02")~ as_date("2024-02-23"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-03-03")~ as_date("2024-02-23"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-03-04")~ as_date("22024-02-23"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-04-04")~ as_date("2024-02-23"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-07-02")~ as_date("2024-02-23"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-08-02")~ as_date("2024-02-23"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-09-02")~ as_date("2024-02-23"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-10-02")~ as_date("2024-02-23"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-11-02")~ as_date("2024-02-23"),
      Response == "BEN-2023-09-01_nOPV" & roundNumber =="Rnd1"~ as_date("2024-02-06"),
      Response == "BEN-2023-09-01_nOPV" & roundNumber =="Rnd2"~ as_date("2024-06-13"),
      Response == "SSD-2024-02-01_nOPV2" & roundNumber =="Rnd1"~ as_date("2024-03-13"),
      Response == "SSD-2024-02-01_nOPV2" & roundNumber =="Rnd2"~ as_date("2024-04-19"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-02-02")~ as_date("2024-02-06"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-05-02")~ as_date("2024-02-06"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-06-02")~ as_date("2024-02-06"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-07-02")~ as_date("2024-02-06"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-10-02")~ as_date("2024-02-06"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==as_date("2024-08-02")~ as_date("2024-02-06"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-01-04")~ as_date("2024-04-02"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-02-04")~ as_date("2024-04-02"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-03-04")~ as_date("2024-04-02"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-07-02")~ as_date("2024-04-02"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-10-02")~ as_date("2024-04-02"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==as_date("2024-08-02")~ as_date("2024-04-02"),
      Response == "CAM-2024-02-01_nOPV" & roundNumber =="Rnd1"~ as_date("2024-03-05"),
      Response == "DRC-NID-03-2024-B_nOPV" ~ as_date("2024-04-02"),
      Response == "LBR-2024-05-01_nOPV" & roundNumber =="Rnd2"~ as_date("2024-06-12"),
      TRUE ~ endate_date))

# write_csv(C,"C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/AFRO_LQAS_data.csv")

G5<-G4 |> 
  mutate(
    start_date = as_date(start_date),
    start_date = case_when(
      Response == "ZAM-2024-06-01_nOPV" & year(start_date) ==2024~ as_date("2024-07-29"),
      Response == "NIG-2023-11-01_nOPV" & year(start_date) ==2024~ as_date("2024-01-09"),
      Response == "ALG-2024-01-01_nOPV" & year(start_date) ==2024~ as_date("2024-02-23"),
      Response == "ALG-2023-09-01_nOPV" & year(start_date) ==2024~ as_date("2024-02-01"),
      Response == "Mop_Up" & start_date ==2024-06-05~ as_date("2024-04-28"),
      Response == "Revaccination" & start_date ==2024-05-05~ as_date("2024-04-28"),
      Response == "Revaccination" & start_date ==2024-05-04~ as_date("2024-04-28"), 
      Response == "LBR-2024-05-01_nOPV" & start_date ==2024-11-06~ as_date("2024-06-11"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-11-04~ as_date("2024-04-11"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-11-06~ as_date("2024-06-11"),
      Response == "ALG-2023-09-01_nOPV" & start_date ==2024-11-02~ as_date("2024-02-11"),
      Response == "LBR-2024-05-01_nOPV" & start_date ==2024-10-06~ as_date("2024-06-10"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-10-04~ as_date("2024-04-10"),
      Response == "SSD-2024-02-01_nOPV2" & start_date ==2024-10-03~ as_date("2024-03-10"),
      Response == "ALG-2023-09-01_nOPV" & start_date ==2024-10-02~ as_date("2024-02-10"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==2024-10-02~ as_date("2024-02-10"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-09-04~ as_date("2024-04-09"),
      Response == "SSD-2024-02-01_nOPV2" & start_date ==2024-09-03~ as_date("2024-03-09"),
      Response == "ALG-2023-09-01_nOPV" & start_date ==2024-09-02~ as_date("2024-02-09"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-08-04~ as_date("2024-04-08"),
      Response == "SSD-2024-02-01_nOPV2" & start_date ==2024-08-03~ as_date("2024-03-08"),
      Response == "ALG-2023-09-01_nOPV" & start_date ==2024-08-02~ as_date("2024-02-08"),
      Response == "BEN-2023-09-01_nOPV" & start_date ==2024-08-02~ as_date("2024-02-08"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-07-04~ as_date("2024-02-07"),
      Response == "SSD-2024-02-01_nOPV2" & start_date ==2024-07-03~ as_date("2024-02-07"),
      Response == "ALG-2023-09-01_nOPV" & start_date ==2024-07-02~ as_date("2024-02-07"),
      Response == "BEN-2023-09-01_nOPV" & start_date ==2024-07-02~ as_date("2024-02-07"),
      Response == "OPVb May2021" & Country == "MAL" ~ as_date("2021-05-10"),
      TRUE ~ start_date),
    end_date = case_when(
      Response == "NIG-2023-11-01_nOPV" & year(start_date) ==2024~ as_date("2024-01-09"),
      Response == "ALG-2024-01-01_nOPV" & year(start_date) ==2024~ as_date("2024-02-23"),
      Response == "ALG-2023-09-01_nOPV" & year(start_date) ==2024~ as_date("2024-02-01"),
      Response == "LBR-2024-05-01_nOPV" & start_date ==2024-11-06~ as_date("2024-06-11"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-11-04~ as_date("2024-04-11"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-11-06~ as_date("2024-06-11"),
      Response == "ALG-2023-09-01_nOPV" & start_date ==2024-11-02~ as_date("2024-02-11"),
      Response == "LBR-2024-05-01_nOPV" & start_date ==2024-10-06~ as_date("2024-06-10"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-10-04~ as_date("2024-04-10"),
      Response == "SSD-2024-02-01_nOPV2" & start_date ==2024-10-03~ as_date("2024-03-10"),
      Response == "ALG-2023-09-01_nOPV" & start_date ==2024-10-02~ as_date("2024-02-10"),
      Response == "DRC-NID-01-2024-bOPV" & start_date ==2024-10-02~ as_date("2024-02-10"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-09-04~ as_date("2024-04-09"),
      Response == "SSD-2024-02-01_nOPV2" & start_date ==2024-09-03~ as_date("2024-03-09"),
      Response == "ALG-2023-09-01_nOPV" & start_date ==2024-09-02~ as_date("2024-02-09"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-08-04~ as_date("2024-04-08"),
      Response == "SSD-2024-02-01_nOPV2" & start_date ==2024-08-03~ as_date("2024-03-08"),
      Response == "ALG-2023-09-01_nOPV" & start_date ==2024-08-02~ as_date("2024-02-08"),
      Response == "BEN-2023-09-01_nOPV" & start_date ==2024-08-02~ as_date("2024-02-08"),
      Response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-07-04~ as_date("2024-02-07"),
      Response == "SSD-2024-02-01_nOPV2" & start_date ==2024-07-03~ as_date("2024-02-07"),
      Response == "ALG-2023-09-01_nOPV" & start_date ==2024-07-02~ as_date("2024-02-07"),
      Response == "ZAM-2024-06-01_nOPV" & year(start_date) ==2024~ as_date("2024-07-29"),
      Response == "BEN-2023-09-01_nOPV" & start_date ==2024-07-02~ as_date("2024-02-07"),
      TRUE ~ endate_date)) |>  
  filter(District != "NA")


F5<-G5 |> 
  mutate(
    start_date = as_date(start_date),
    start_date = case_when(
      Response == "ANGOLA-SNID-2024-07-nOPV2" ~ as_date("2024-06-28"),
      TRUE ~ start_date)) |> 
  select(Country, Region, District, Response, Vaccine.type, roundNumber, start_date, endate_date, year, Number_of_HH_visited, u5_present, u5_FM, missed_child, cv, r_non_FM_Absent, r_non_FM_NC, r_non_FM_hh_notvisited, r_non_FM_hh_notrevisited, r_non_FM_sleep, r_non_FM_vaccinatedRoutine, Other_reasons, care_Giver_Informed_SIA, percent_care_Giver_Informed_SIA) |> 
  arrange(start_date)
  
  
  
      
write_csv(F5,"C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/IM_level/COG_IM1.csv")   
  
  


