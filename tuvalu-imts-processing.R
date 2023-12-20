#Load required libraries
library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)
library(crosstable)
library(gmodels)

#Dynamic directory path mapping
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

# Load the import excel file into the R working environment
import <- read_excel("data/imports_Q3_2023.xlsx")
export <- read_excel("data/Exports_Q3_2023.xlsx")

# Create month dataframe 
month <- data.frame(
  Month = c(1,2,3,4,5,6,7,8,9,10,11,12),
  monthName = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
  Quarter = c(1,1,1,2,2,2,3,3,3,4,4,4),
  quarterName = c("Quarter 1", "Quarter 1", "Quarter 1", "Quarter 2", "Quarter 2", "Quarter 2", "Quarter 3", "Quarter 3", "Quarter 3", "Quarter 4", "Quarter 4", "Quarter 4")
)

# Create Quarter dataframe
quarter <- data.frame(
  Quarter = c(1,2,3,4),
  quarterName = c("Quarter 1", "Quarter 2", "Quarter 3", "Quarter 4")
)

# Create .STAT empty dataframe

One_BOT <- function(statFrame){
  statFrame <- data.frame()
  
  #### Annual Import Summary - 1_BOT ####
  
  annualImport <- import %>%
    filter(Year > 2000) %>%
    group_by(Year) %>%
    summarise(OBS_VALUE = sum(CIF))
  
  colnames(annualImport)[colnames(annualImport) == "Year"] <- "TIME_PERIOD"
  annualImport$FREQ  = "A"
  annualImport$GEO_PICT = "TV"
  annualImport$INDICATOR = "AMT"
  annualImport$TRADE_FLOW = "M"
  annualImport$COMMODITY = "_T"
  annualImport$COUNTERPART = "_T"
  annualImport$TRANSPORT = "_T"
  annualImport$CURRENCY = "DOM"
  annualImport$UNIT_MEASURE ="AUD"
  annualImport$UNIT_MULT = 3
  annualImport$OBS_STATUS = ""
  annualImport$DATA_SOURCE = ""
  annualImport$OBS_COMMENT = ""
  
  
  export$date <- as.Date(export$`SAD Date`)
  export$Year <- as.integer(format(export$date, "%Y"))
  export$Month <- as.integer(format(export$date, "%m"))
  
  annualExport <- export %>%
    filter(Year > 2000) %>%
    group_by(Year) %>%
    summarise(OBS_VALUE = sum(CIF))
  
  colnames(annualExport)[colnames(annualExport) == "Year"] <- "TIME_PERIOD"
  
  annualExport$FREQ  = "A"
  annualExport$GEO_PICT = "TV"
  annualExport$INDICATOR = "AMT"
  annualExport$TRADE_FLOW = "X"
  annualExport$COMMODITY = "_T"
  annualExport$COUNTERPART = "_T"
  annualExport$TRANSPORT = "_T"
  annualExport$CURRENCY = "DOM"
  annualExport$UNIT_MEASURE ="AUD"
  annualExport$UNIT_MULT = 3
  annualExport$OBS_STATUS = ""
  annualExport$DATA_SOURCE = ""
  annualExport$OBS_COMMENT = ""
  
  annual_imts <- rbind(annualImport, annualExport)
  
  statFrame <- rbind(statFrame, annual_imts)
  
  return(statFrame)
  
  
} 

statFrame <- data.frame()

#### Annual Import Summary - 1_BOT ####

annualImport <- import %>%
  filter(Year > 2000) %>%
  group_by(Year) %>%
  summarise(OBS_VALUE = sum(CIF))

colnames(annualImport)[colnames(annualImport) == "Year"] <- "TIME_PERIOD"
annualImport$FREQ  = "A"
annualImport$GEO_PICT = "TV"
annualImport$INDICATOR = "AMT"
annualImport$TRADE_FLOW = "M"
annualImport$COMMODITY = "_T"
annualImport$COUNTERPART = "_T"
annualImport$TRANSPORT = "_T"
annualImport$CURRENCY = "DOM"
annualImport$UNIT_MEASURE ="AUD"
annualImport$UNIT_MULT = 3
annualImport$OBS_STATUS = ""
annualImport$DATA_SOURCE = ""
annualImport$OBS_COMMENT = ""


export$date <- as.Date(export$`SAD Date`)
export$Year <- as.integer(format(export$date, "%Y"))
export$Month <- as.integer(format(export$date, "%m"))

annualExport <- export %>%
  filter(Year > 2000) %>%
  group_by(Year) %>%
  summarise(OBS_VALUE = sum(CIF))

colnames(annualExport)[colnames(annualExport) == "Year"] <- "TIME_PERIOD"

annualExport$FREQ  = "A"
annualExport$GEO_PICT = "TV"
annualExport$INDICATOR = "AMT"
annualExport$TRADE_FLOW = "X"
annualExport$COMMODITY = "_T"
annualExport$COUNTERPART = "_T"
annualExport$TRANSPORT = "_T"
annualExport$CURRENCY = "DOM"
annualExport$UNIT_MEASURE ="AUD"
annualExport$UNIT_MULT = 3
annualExport$OBS_STATUS = ""
annualExport$DATA_SOURCE = ""
annualExport$OBS_COMMENT = ""

annual_imts <- rbind(annualImport, annualExport)

statFrame <- rbind(statFrame, annual_imts)


#### Import by Classifications 2_M #####

# Annual Data preparation

importClass <- read.csv("other/importClassification.csv")
width <- 2
importClass$HS2 <- sprintf(paste0('%0', width, 'd'), importClass$HS2)

importHS <- import %>%
  select(Year, Month, HS2, CIF)

importHS_merge_class <- merge(importHS, importClass, by = "HS2")

importHS_merge_class_summary <- importHS_merge_class %>%
  group_by(Year, Month, hsGroup, Description) %>%
  summarise(total = sum(CIF))

# Annual Import by Commodity

annualImportCommodity <- importHS_merge_class_summary %>%
  group_by(Year, hsGroup) %>%
  summarise(OBS_VALUE = sum(total))

colnames(annualImportCommodity)[colnames(annualImportCommodity) == "Year"] <- "TIME_PERIOD"
colnames(annualImportCommodity)[colnames(annualImportCommodity) == "hsGroup"] <- "COMMODITY"

annualImportCommodity$FREQ  = "A"
annualImportCommodity$GEO_PICT = "TV"
annualImportCommodity$INDICATOR = "AMT"
annualImportCommodity$TRADE_FLOW = "M"
annualImportCommodity$COUNTERPART = "_T"
annualImportCommodity$TRANSPORT = "_T"
annualImportCommodity$CURRENCY = "DOM"
annualImportCommodity$UNIT_MEASURE ="AUD"
annualImportCommodity$UNIT_MULT = 3
annualImportCommodity$OBS_STATUS = ""
annualImportCommodity$DATA_SOURCE = ""
annualImportCommodity$OBS_COMMENT = ""

statFrame <- rbind(statFrame, annualImportCommodity)


# Monthly data preparation

import$YrMth <- paste(import$Year, import$Month, sep = "-")

importYM <- import %>%
  select(YrMth, HS2, CIF)

importHS_merge_class_YM <- merge(importYM, importClass, by = "HS2")

monthImportCommodity <- importHS_merge_class_YM %>%
  group_by(YrMth, hsGroup) %>%
  summarise(OBS_VALUE = sum(CIF))

colnames(monthImportCommodity)[colnames(monthImportCommodity) == "YrMth"] <- "TIME_PERIOD"
colnames(monthImportCommodity)[colnames(monthImportCommodity) == "hsGroup"] <- "COMMODITY"

monthImportCommodity$FREQ  = "M"
monthImportCommodity$GEO_PICT = "TV"
monthImportCommodity$INDICATOR = "AMT"
monthImportCommodity$TRADE_FLOW = "M"
monthImportCommodity$COUNTERPART = "_T"
monthImportCommodity$TRANSPORT = "_T"
monthImportCommodity$CURRENCY = "DOM"
monthImportCommodity$UNIT_MEASURE ="AUD"
monthImportCommodity$UNIT_MULT = 3
monthImportCommodity$OBS_STATUS = ""
monthImportCommodity$DATA_SOURCE = ""
monthImportCommodity$OBS_COMMENT = ""


statFrame <- rbind(statFrame, monthImportCommodity)

