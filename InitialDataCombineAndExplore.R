# Intial Data Visualization

# Imports
library("tidyverse")
library(dplyr)
library(ggplot2)
#rm(list = ls())

# Read in 
active <- read.csv("./Data/activeltcoutbreak.csv", stringsAsFactors = FALSE)
colnames(active) <- c("Date", "PHU_Num", "PHU", "LTC_Home", "LTCH_Num", "City", "Beds", "Total_LTC_Resident_Cases", "Total_LTC_Resident_Deaths", "Total_LTC_HCW_Cases")
#Get most recent
LTC_Home <- n_distinct(active$LTC_Home) #514 LTC homes, need to get the most recent
mostRecent <- active %>% 
  group_by(LTC_Home) %>%
  slice(which.max(as.Date(Date, '%Y-%m-%d')))
mostRecent$Total_LTC_Resident_Cases[mostRecent$Total_LTC_Resident_Cases == "<5"] <- 4
mostRecent$Total_LTC_Resident_Deaths[mostRecent$Total_LTC_Resident_Deaths == "<5"] <- 4
mostRecent$Total_LTC_HCW_Cases[mostRecent$Total_LTC_HCW_Cases == "<5"] <- 4

# Add back in webscraped data:
accredit <- read.csv("./Data/AccreditationById.csv", stringsAsFactors = FALSE)
merged <- merge(x = mostRecent, y = accredit, by = "LTCH_Num", all = TRUE)
nonCompliance <- read.csv("./Data/nonCompliance.csv", stringsAsFactors = FALSE)
merged2 <- merge(x = merged, y = nonCompliance, by = "LTCH_Num", all.x = TRUE)

#write.csv(merged2,"./Data/CombinedLTC.csv", row.names = FALSE)


#Deprecated
#cities <- unique(merged2$City) #514 LTC homes, need to get the most recent
# Add data on population density:
#medianIncome <- read.csv("./Data/canadacities.csv", stringsAsFactors = FALSE)
#medianIncome <- medianIncome %>% filter(province_id == "ON")
#common <- medianIncome %>% filter(city_ascii %in% cities ) #95 matched no filtering 

#medianIncome <- read.csv("./Data/medianHouseholdIncome2015.CSV", stringsAsFactors = FALSE)
#medianIncome <- medianIncome %>% select(Geographic.name, Geographic.code..Province.or.territory, Median.household.total.income..2015.constant.dollars...2015)
#colnames(medianIncome) <- c("City", "ProvinceCode", "MedianHHIncome2015")
#medianIncome <- medianIncome %>% filter(ProvinceCode == 35)
#medianIncome <- medianIncome %>% select(City, MedianHHIncome2015)

