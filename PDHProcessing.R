#Load required libraries
library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)

#### Declaration of Global Variables ####


#Dynamic directory path mapping
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

# Load the import excel file into the R working environment
import <- read_excel("data/Imports_Q3_2023.xlsx")
export <- read_excel("data/Exports_Q3_2023.xlsx")
country <- read.csv("other/country.csv")
hsClass <- read.csv("other/importClassification.csv")
mode <- read_excel("other/classifications.xlsx", sheet = "mod")
procedure <- read_excel("other/classifications.xlsx", sheet = "procedure")



#Reformatting HS2 column into having a width of 2 digit
width <- 2
hsClass$HS2 <- sprintf(paste0('%0', width, 'd'), hsClass$HS2)
import$HS2 <- as.numeric(import$HS2)
import$HS2 <- sprintf(paste0('%0', width, 'd'), import$HS2)
colnames(export)[colnames(export) == "Chapter"] <- "HS2"
export$HS2 <- sprintf(paste0('%0', width, 'd'), export$HS2)

#merge imports and exports with hs classes
import_class <- merge(import, hsClass, by = "HS2")
export_class <- merge(export, hsClass, by = "HS2")

# Define TIME_PERIOD for later use
import$yearMonth <- paste(import$Year, import$Month, sep = "-")

# Reformat Export date to get proper date and define Year and Month of export
export$date <- as.Date(export$`SAD Date`)
export$Year <- as.integer(format(export$date, "%Y"))
export$Month <- as.integer(format(export$date, "%m"))

export$yearMonth <- paste(export$Year, export$Month, sep = "-")

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

#Sourcing all the functions that are generating the tables
source("functions/table1.R")
source("functions/table2.R")
source("functions/table3.R")
source("functions/table4.R")
source("functions/table5.R")


table1 <- table1(statFrame)
table2 <- table2(statFrame)
table3 <- table3(statFrame)
table4 <- table4(statFrame)
table5 <- table5(statFrame)


#Merge all the extracted tables
final_data <- rbind(table1, table2, table3, table4, table5)

#Reordering the columns according to the .STAT column arrangement.
columnOrder <- c("FREQ", "TIME_PERIOD", "GEO_PICT", "INDICATOR", "TRADE_FLOW", "COMMODITY", "COUNTERPART", "TRANSPORT", "CURRENCY", "OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "OBS_STATUS", "DATA_SOURCE", "OBS_COMMENT")
final_data <- final_data[, columnOrder]

# Write final data to csv file
write.csv(final_data, "output/tivalue_imts_14_tables.csv", row.names = FALSE)



