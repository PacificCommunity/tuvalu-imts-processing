#*********************************************************************************#
#************************ Table 05 - Trade by Transport Mode *********************#
#********************************************************************************#

table5 <- function(statFrame) {
  
  statFrame <- data.frame()
  # Import Transport mode Annual
  importTransportAnnual <- import %>%
    filter(Year > 2000) %>%
    group_by(Office, Year) %>%
    summarise(OBS_VALUE = sum(CIF))
  
  colnames(importTransportAnnual)[colnames(importTransportAnnual) == "Year"] <- "TIME_PERIOD"
  colnames(importTransportAnnual)[colnames(importTransportAnnual) == "Office"] <- "TRANSPORT"
  
  importTransportAnnual$FREQ <- "A"
  importTransportAnnual$TRADE_FLOW <- "M"
  
  #Export Transport mode Annual
  exportTransportAnnual <- export %>%
    filter(Year > 2000) %>%
    group_by(Office, Year) %>%
    summarise(OBS_VALUE = sum(CIF))
  
  colnames(exportTransportAnnual)[colnames(exportTransportAnnual) == "Year"] <- "TIME_PERIOD"
  colnames(exportTransportAnnual)[colnames(exportTransportAnnual) == "Office"] <- "TRANSPORT"
  
  exportTransportAnnual$FREQ <- "A"
  exportTransportAnnual$TRADE_FLOW <- "X"
  
  transportAnnual <- rbind(importTransportAnnual, exportTransportAnnual)
  transportAnnual$TIME_PERIOD <- as.character(transportAnnual$TIME_PERIOD)
  
  
  # Import Transport mode Monthly
  importTransportMonthly <- import %>%
    filter(Year > 2000) %>%
    group_by(Office, yearMonth) %>%
    summarise(OBS_VALUE = sum(CIF))
  
  colnames(importTransportMonthly)[colnames(importTransportMonthly) == "yearMonth"] <- "TIME_PERIOD"
  colnames(importTransportMonthly)[colnames(importTransportMonthly) == "Office"] <- "TRANSPORT"
  
  importTransportMonthly$FREQ <- "M"
  importTransportMonthly$TRADE_FLOW <- "M"
  
  
  #Export Transport mode Monthly
  exportTransportMonthly <- export %>%
    filter(Year > 2000) %>%
    group_by(Office, yearMonth) %>%
    summarise(OBS_VALUE = sum(CIF))
  
  colnames(exportTransportMonthly)[colnames(exportTransportMonthly) == "yearMonth"] <- "TIME_PERIOD"
  colnames(exportTransportMonthly)[colnames(exportTransportMonthly) == "Office"] <- "TRANSPORT"
  
  exportTransportMonthly$FREQ <- "M"
  exportTransportMonthly$TRADE_FLOW <- "X"
  
  transportMonthly <- rbind(importTransportMonthly, exportTransportMonthly)
  
  statFrame <- rbind(transportAnnual, transportMonthly)
  
  statFrame$GEO_PICT <- "TV"
  statFrame$INDICATOR <- "AMT"
  statFrame$COMMODITY <- "_T"
  statFrame$CURRENCY <- "DOM"
  statFrame$UNIT_MEASURE <- "NZD"
  statFrame$UNIT_MULT <- "3"
  statFrame$OBS_STATUS <- ""
  statFrame$DATA_SOURCE <- ""
  statFrame$OBS_COMMENT <- ""
  statFrame$COUNTERPART <- "_T"
  
  return(statFrame)
  
  
}
