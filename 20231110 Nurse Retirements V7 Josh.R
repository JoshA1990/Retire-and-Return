## Nursing Retire and Return Data Manipulation ##

## This code reads in the ESR outputs, manipulates them and carries out the calculations required for the analysis.
## It outputs 2 CSV files which can be fed into Excel data models in their corresponding workbook.
## Updates may sometimes be required in the section below any comments marked "USER INPUT" - there are 9 of these in total.

# Load required packages (the code will install them if necessary)
if (!require(plyr)){
  install.packages("plyr")
  library(plyr)
}

if (!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if (!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require(lubridate)){
  install.packages("lubridate")
  library(lubridate)
}

if (!require(tidyr)){
  install.packages("tidyr")
  library(tidyr)
}

if (!require(stringr)){
  install.packages("stringr")
  library(stringr)
}


# Part 1 - Compile retirement data for Nurses

# Set working directory to be folder containing ESR nursing retirement outputs (from the Workforce Movement Subject Area)
# USER INPUT 1 - Check that this is pointing to the right folder and change it if not
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Retirement")

# Read in the required CSV files, adding underscores in column names

# Read in the first one (2013)
# USER INPUT 2 - Change the file name if required to ensure you are reading in the correct file.
nurse_ret_data <- read.csv("20220712_Nurse_Ret_2013.csv", na.strings = c("", " ", "NA"), check.names = FALSE, fileEncoding = "UTF-8-BOM")
colnames(nurse_ret_data)[1] <- "Unique_NHS_ID"
colnames(nurse_ret_data) <- str_replace_all(colnames(nurse_ret_data), " ", "_")

# USER INPUT 3 - There is a chance date formats may be inconsistent so may need to change
nurse_ret_data$Date_Of_Birth <- as.Date(nurse_ret_data$Date_Of_Birth, format = "%Y-%m-%d")
nurse_ret_data$Date_Of_Leaving_Org <- as.Date(nurse_ret_data$Date_Of_Leaving_Org, format = "%Y-%m-%d")

# Read in the remaining nurse retirement files
nurse_ret_import <- function(nurse_ret_name){
  
  # Read in the extract
  nurse_ret_import_data <- read.csv(paste0(nurse_ret_name,".csv"), na.strings = c("", " ", "NA"), check.names = FALSE, fileEncoding = "UTF-8-BOM")
  
  # Add underscores to variable names, then improve names of first variable
  colnames(nurse_ret_import_data)[1] <- "Unique_NHS_ID"
  colnames(nurse_ret_import_data) <- str_replace_all(colnames(nurse_ret_import_data), " ", "_")
  
  # USER INPUT 4 - There is a chance date formats may be inconsistent so may need to change
  nurse_ret_import_data$Date_Of_Birth <- as.Date(nurse_ret_import_data$Date_Of_Birth, format = "%Y-%m-%d")
  nurse_ret_import_data$Date_Of_Leaving_Org <- as.Date(nurse_ret_import_data$Date_Of_Leaving_Org, format = "%Y-%m-%d")
  
  nurse_ret_data <<- rbind(nurse_ret_data, nurse_ret_import_data)
}

# USER INPUT 5 - Change the file name if required to ensure you are reading in the correct file, and add a new one if needed.
nurse_ret_import("20220712_Nurse_Ret_2014")
nurse_ret_import("20220712_Nurse_Ret_2015")
nurse_ret_import("20220712_Nurse_Ret_2016")
nurse_ret_import("20220712_Nurse_Ret_2017")
nurse_ret_import("20220712_Nurse_Ret_2018")
nurse_ret_import("20220712_Nurse_Ret_2019")
nurse_ret_import("20220712_Nurse_Ret_2020")
nurse_ret_import("20220712_Nurse_Ret_2021")
nurse_ret_import("20230627_Nurse_Ret_2022")
nurse_ret_import("20231115_Nurse_Ret_2023")


# The import from ESR may contain multiple retirements for the same nurse, we are only interested in the first recorded retirement
# Create dataset containing list of first retirement dates for each nurse (i.e. one row per nurse)
# Note that a small number of these may also have a retirement date prior to the start of the extract that won't have been captured
nurse_ret_dates <- nurse_ret_data %>% group_by(Unique_NHS_ID) %>% summarise(First_Retirement_Date = min(Date_Of_Leaving_Org, na.rm = TRUE), .groups = 'drop') %>% as.data.frame(.)

# Join on the details from the imported dataset, then select the role information with the highest FTE (attributing all FTE to this role)
nurse_first_ret <- left_join(nurse_ret_dates, nurse_ret_data, by = c("Unique_NHS_ID" = "Unique_NHS_ID", "First_Retirement_Date" = "Date_Of_Leaving_Org"))
nurse_first_ret <- arrange(nurse_first_ret, Unique_NHS_ID, desc(Leaver_Wte))
df_dups <- nurse_first_ret["Unique_NHS_ID"]
nurse_first_ret_noFTE <- nurse_first_ret[!duplicated(df_dups),] %>% select(., -Leaver_Wte)
rm(df_dups, nurse_ret_data, nurse_ret_dates)

nurse_first_ret_FTE <- nurse_first_ret %>% group_by(Unique_NHS_ID, First_Retirement_Date) %>% summarise(Leaver_FTE = sum(Leaver_Wte, na.rm = TRUE), .groups = 'drop') %>% as.data.frame(.)
nurse_first_ret_FTE <- left_join(nurse_first_ret_noFTE, nurse_first_ret_FTE, by = c("Unique_NHS_ID", "First_Retirement_Date"))
rm(nurse_first_ret_noFTE, nurse_first_ret)

# Create dataset containing list of dates of birth for each nurse (takes earliest if duplicates, which are very rare anyway)
nurse_dobs <- nurse_first_ret_FTE %>% group_by(Unique_NHS_ID) %>% summarise(Date_Of_Birth = min(Date_Of_Birth, na.rm = TRUE), .groups = 'drop') %>% as.data.frame(.)

# Bring together to give a unique retirement date and date of birth for each nurse, and calculate age at first retirement date
nurse_retirement <- left_join(nurse_dobs, nurse_first_ret_FTE, by = c("Unique_NHS_ID", "Date_Of_Birth"))
nurse_retirement$First_Retirement_Age <- trunc((nurse_retirement$Date_Of_Birth %--%nurse_retirement$First_Retirement_Date) / years(1))
rm(nurse_first_ret_FTE, nurse_dobs)

# Finally, convert the retirement date to a month (basically sets all retirement dates to the 1st of the month in which they occur)
nurse_retirement$First_Retirement_Month <- floor_date(nurse_retirement$First_Retirement_Date, "month")
nurse_retirement <- select(nurse_retirement, -First_Retirement_Date)


# You should now have a nice cleaned data frame containing information for each nurse that retired in the period of interest (one row per nurse)
# Now need to find out the monthly FTE figures to identify returners, so it is time for:

# Part 2 - Compile FTE from all ESR records (from Payroll Subject Area - joins to retirements data as each file is imported to improve run time)

# Set working directory to be folder containing ESR all staff FTE outputs (i.e. the quarterly extracts showing monthly FTE for all staff in ESR with FTE>0)
# USER INPUT 6 - Check that this is pointing to the right folder and change it if not
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Retirement/All Staff FTE/")

# This function is used to import and join on FTEs from ESR (a repetitive process)
esr_data <- function(esr_name){
  
  # Read in the extract
  esr_import <- read.csv(paste0(esr_name,".csv"), na.strings = c("", " ", "NA"), check.names = FALSE, fileEncoding = "UTF-8-BOM")
  
  # Add underscores to variable names, then improve names of first 2 variables
  colnames(esr_import) <- str_replace_all(colnames(esr_import), " ", "_")
  colnames(esr_import)[1] <- "Tm_Month"
  colnames(esr_import)[2] <- "Unique_NHS_ID"
  
  # Convert Tm_Month into a date format (reads in as character) and remove unwanted blank records
  # USER INPUT 7 - There is a chance date formats may be inconsistent so may need to change (will become NAs if wrong)
  esr_import$Tm_Month <- as.Date(esr_import$Tm_Month, format = "%Y-%m-%d")
  esr_import <- filter(esr_import, is.na(Unique_NHS_ID) == FALSE & is.na(Tm_Month) == FALSE)
  
  # Remove the occupation codes and contract types and re-sum the FTE so one row per month per NHS ID
  esr_import <- esr_import %>% group_by(Tm_Month, Unique_NHS_ID) %>% summarise(FTE = sum(FTE, na.rm = TRUE), .groups = 'drop') %>% as.data.frame(.)
  
  esr_import <- spread(esr_import, Tm_Month, FTE, fill = 0)
  
  
  nurse_retirement <<- left_join(nurse_retirement, esr_import, by = "Unique_NHS_ID")

}

# USER INPUT 8 - Add a row at the bottom for each new extract to be imported
esr_data("2012Q1")
esr_data("2012Q2")
esr_data("2012Q3")
esr_data("2012Q4")

esr_data("2013Q1")
esr_data("2013Q2")
esr_data("2013Q3")
esr_data("2013Q4")

esr_data("2014Q1")
esr_data("2014Q2")
esr_data("2014Q3")
esr_data("2014Q4")

esr_data("2015Q1")
esr_data("2015Q2")
esr_data("2015Q3")
esr_data("2015Q4")

esr_data("2016Q1")
esr_data("2016Q2")
esr_data("2016Q3")
esr_data("2016Q4")

esr_data("2017Q1")
esr_data("2017Q2")
esr_data("2017Q3")
esr_data("2017Q4")

esr_data("2018Q1")
esr_data("2018Q2")
esr_data("2018Q3")
esr_data("2018Q4")

esr_data("2019Q1")
esr_data("2019Q2")
esr_data("2019Q3")
esr_data("2019Q4")

esr_data("2020Q1")
esr_data("2020Q2")
esr_data("2020Q3")
esr_data("2020Q4")

esr_data("2021Q1")
esr_data("2021Q2")
esr_data("2021Q3")
esr_data("2021Q4")

esr_data("2022Q1")
esr_data("2022Q2")
esr_data("2022Q3")
esr_data("2022Q4")

esr_data("2023Q1")
esr_data("2023Q2")
esr_data("2023Q3")

# Identify which columns contain the FTE values and replace NAs with 0s in these columns
recode_start <- which(names(nurse_retirement)=="First_Retirement_Month") + 1  # first FTE column
recode_end <- ncol(nurse_retirement)                                          # last FTE column
nurse_retirement[, c(recode_start:recode_end)][is.na(nurse_retirement[, c(recode_start:recode_end)])] <- 0

# Now need to transform FTE columns to be months pre/post retirement
# Create values for first and last ESR months, plus first retirement month
first_esr_month <- min(as.Date(names(nurse_retirement)[c(recode_start:recode_end)]))
last_esr_month <- max(as.Date(names(nurse_retirement)[c(recode_start:recode_end)]))
first_retirement <- min(nurse_retirement$First_Retirement_Month)

# Calculate number of months pre retirement data is available for
pre_ret_mth_ct <- interval(first_esr_month, first_retirement) %/% months(1)

# Define start of month dates for the months prior to retirement (populates created variables with dates)
preret_dates <- function(n) {
  varname <- paste0("Date_R-", n, "m")
  nurse_retirement[[varname]] <- with(nurse_retirement, as.character(as.Date(First_Retirement_Month) %m-% months(n)))
  nurse_retirement
}  

for (i in pre_ret_mth_ct:1) {
  nurse_retirement <- preret_dates(n=i)
}

# Redefine the start of the retirement month so it sits nicely in the series
nurse_retirement$Date_R <- as.Date(nurse_retirement$First_Retirement_Month) %>% as.character()

# Define start of month dates for the months after retirement (populates created variables with dates)
postret_dates <- function(n) {
  varname <- paste0("Date_R+", n, "m")
  nurse_retirement[[varname]] <- with(nurse_retirement, as.character(as.Date(First_Retirement_Month) %m+% months(n)))
  nurse_retirement
} 

# Calculate number of months post retirement data is available for
ret_mth_ct <- interval(first_retirement, last_esr_month) %/% months(1)

for (i in 1:ret_mth_ct) {
  nurse_retirement <- postret_dates(n=i)
}

# Now need to assign the corresponding FTE for the dates identified in the pre and post retirement periods
# Create FTE time series for the months prior to retirement
preret_fte <- function(n) {
  varname <- paste0("FTE_R-", n, "m")
  matchvar <- paste0("Date_R-", n, "m")
  nurse_retirement[[varname]] <- as.numeric(nurse_retirement[cbind(seq_len(nrow(nurse_retirement)), match(nurse_retirement[[matchvar]], names(nurse_retirement)))])
  nurse_retirement
}  

for (i in pre_ret_mth_ct:1) {
  nurse_retirement <- preret_fte(n=i)
}

# FTE in month of retirement - sits nicely in the time series
nurse_retirement$FTE_R <- as.numeric(nurse_retirement[cbind(seq_len(nrow(nurse_retirement)), match(nurse_retirement$Date_R, names(nurse_retirement)))])

# Create FTE time series for the months after retirement
postret_fte <- function(n) {
  varname <- paste0("FTE_R+", n, "m")
  matchvar <- paste0("Date_R+", n, "m")
  nurse_retirement[[varname]] <- as.numeric(nurse_retirement[cbind(seq_len(nrow(nurse_retirement)), match(nurse_retirement[[matchvar]], names(nurse_retirement)))])
  nurse_retirement
}

for (i in 1:ret_mth_ct) {
  nurse_retirement <- postret_fte(n=i)
}

# Only select range of columns need (ID, leaver WTE, retirement month, FTE time series)
exc_start <- which(names(nurse_retirement)=="First_Retirement_Month") + 1
exc_end <- which(names(nurse_retirement)==paste0("FTE_R-", pre_ret_mth_ct, "m")) - 1

nurse_ret_fte_series_trunc <- nurse_retirement[, -c(exc_start:exc_end)]

# Turn into a long dataset and add additional calculated variables
nurse_ret_long <- pivot_longer(nurse_ret_fte_series_trunc, starts_with("FTE_R"), names_to = "Attribute", values_to = "FTE")
nurse_ret_long$Months_After_Retirement <- as.integer(ifelse(nurse_ret_long$Attribute == "FTE_R", "0", substr(nurse_ret_long$Attribute, 6, nchar(nurse_ret_long$Attribute)-1)))
nurse_ret_long$Years_After_Retirement <- round(nurse_ret_long$Months_After_Retirement / 12, digits = 6)
nurse_ret_long$First_Retirement_Year <- year(nurse_ret_long$First_Retirement_Month)
Last_Full_Retirement_Year <- ifelse(month(max(nurse_ret_long$First_Retirement_Month)) == 12, year(max(nurse_ret_long$First_Retirement_Month)), year(max(nurse_ret_long$First_Retirement_Month)) - 1)
nurse_ret_long$Complete_Ret_Yr_Data <- ifelse(Last_Full_Retirement_Year < nurse_ret_long$First_Retirement_Year, "No", "Yes")
nurse_ret_long$Has_FTE_Value <- ifelse(is.na(nurse_ret_long$FTE), "No", "Yes")
nurse_ret_long$FTE_Above0 <- ifelse(nurse_ret_long$Has_FTE_Value == "Yes" & nurse_ret_long$FTE > 0, "Yes", "No")

# Calculate if they return after retirement (total post-retirement FTE > 0)
nurse_rr <- filter(nurse_ret_long, Months_After_Retirement > 0)
nurse_rr <- nurse_rr %>% group_by(Unique_NHS_ID, First_Retirement_Year, Complete_Ret_Yr_Data) %>% summarise(Post_Ret_FTE_Total = sum(FTE, na.rm = TRUE), .groups = 'drop') %>% as.data.frame(.)
nurse_rr$Returns_After_Retirement <- ifelse(nurse_rr$Post_Ret_FTE_Total > 0, "Yes", "No")
nurse_rr$Post_Ret_FTE_Years <- ifelse(nurse_rr$Returns_After_Retirement == "Yes", round(nurse_rr$Post_Ret_FTE_Total / 12, digits = 6), NA)
nurse_rr2 <- select(nurse_rr, Unique_NHS_ID, Returns_After_Retirement)

# Join back on to long dataset
nurse_ret_long <- left_join(nurse_ret_long, nurse_rr2, by="Unique_NHS_ID")

# Create a version with just the details of retired nurses (no FTE time series)
nurse_ret_summary <- unique(nurse_ret_fte_series_trunc[, c(1:exc_start-1)])
nurse_ret_summary <- left_join(nurse_ret_summary, nurse_rr, by="Unique_NHS_ID")

# Calculate if they return within a year of retirement (total FTE in year after retirement > 0)
nurse_rr_1yr <- filter(nurse_ret_long, Months_After_Retirement > 0 & Months_After_Retirement <= 12)
nurse_rr_1yr <- nurse_rr_1yr %>% group_by(Unique_NHS_ID) %>% summarise(Post_Ret_First_Year_FTE = sum(FTE, na.rm = TRUE), .groups = 'drop') %>% as.data.frame(.)
nurse_rr_1yr$Returns_Within_Year <- ifelse(nurse_rr_1yr$Post_Ret_First_Year_FTE > 0, "Yes", "No")

# Calculate information about the months seen post-retirement
nurse_post_ret_mths <- filter(nurse_ret_long, Months_After_Retirement > 0, FTE > 0)
nurse_post_ret_mths <- nurse_post_ret_mths %>% group_by(Unique_NHS_ID) %>% 
  summarise(Post_Ret_Mths_Seen = n_distinct(Months_After_Retirement, na.rm = TRUE), 
            First_Post_Ret_Mth_Seen = min(Months_After_Retirement, na.rm = TRUE), 
            Last_Post_Ret_Mth_Seen = max(Months_After_Retirement, na.rm = TRUE), .groups = 'drop') %>% as.data.frame(.)
nurse_post_ret_mths$Post_Ret_Work_Mths <- nurse_post_ret_mths$Last_Post_Ret_Mth_Seen - nurse_post_ret_mths$First_Post_Ret_Mth_Seen + 1
nurse_post_ret_mths$Post_Ret_Work_Yrs <- round(nurse_post_ret_mths$Post_Ret_Work_Mths / 12, digits = 6)

# Calculate FTE in year prior to retirement (if less than 12 months of pre-retirement data then takes average for period available)
nurse_preret_FTE <- filter(nurse_ret_long, Months_After_Retirement < 0, Months_After_Retirement >= -12)
nurse_preret_FTE <- nurse_preret_FTE %>% group_by(Unique_NHS_ID) %>% summarise(Pre_Ret_Year_FTE = sum(FTE, na.rm = TRUE), .groups = 'drop') %>% as.data.frame(.)
nurse_preret_FTE$Pre_Ret_Year_FTE_Average <- round(nurse_preret_FTE$Pre_Ret_Year_FTE / min(12, pre_ret_mth_ct), digits = 6)

# Calculate FTEs in return period
nurse_postret_FTE <- left_join(nurse_ret_long, nurse_post_ret_mths, by="Unique_NHS_ID")

nurse_postret_FTE_firstyear <- filter(nurse_postret_FTE, Months_After_Retirement >= First_Post_Ret_Mth_Seen, Months_After_Retirement < First_Post_Ret_Mth_Seen + 12)
nurse_postret_FTE_firstyear <- nurse_postret_FTE_firstyear %>% group_by(Unique_NHS_ID, Last_Post_Ret_Mth_Seen, First_Post_Ret_Mth_Seen) %>% summarise(First_Year_Back_FTE_Total = sum(FTE, na.rm = TRUE), .groups = 'drop') %>% as.data.frame(.)
nurse_postret_FTE_firstyear$Mths_For_FTE_Average <- nurse_postret_FTE_firstyear$Last_Post_Ret_Mth_Seen - nurse_postret_FTE_firstyear$First_Post_Ret_Mth_Seen + 1
nurse_postret_FTE_firstyear$Mths_For_FTE_Average <- ifelse(nurse_postret_FTE_firstyear$Mths_For_FTE_Average > 12, 12, nurse_postret_FTE_firstyear$Mths_For_FTE_Average)
nurse_postret_FTE_firstyear$First_Year_Back_FTE_Average <- round(nurse_postret_FTE_firstyear$First_Year_Back_FTE_Total / nurse_postret_FTE_firstyear$Mths_For_FTE_Average, digits = 6)
nurse_postret_FTE_firstyear <- select(nurse_postret_FTE_firstyear, Unique_NHS_ID, First_Year_Back_FTE_Total, First_Year_Back_FTE_Average)

nurse_postret_FTE_overall <- filter(nurse_postret_FTE, Months_After_Retirement >= First_Post_Ret_Mth_Seen, Months_After_Retirement <= Last_Post_Ret_Mth_Seen)
nurse_postret_FTE_overall <- nurse_postret_FTE_overall %>% group_by(Unique_NHS_ID, Last_Post_Ret_Mth_Seen, First_Post_Ret_Mth_Seen) %>% summarise(Return_Period_FTE_Total = sum(FTE, na.rm = TRUE), .groups = 'drop') %>% as.data.frame(.)
nurse_postret_FTE_overall$Mths_For_FTE_Average <- nurse_postret_FTE_overall$Last_Post_Ret_Mth_Seen - nurse_postret_FTE_overall$First_Post_Ret_Mth_Seen + 1
nurse_postret_FTE_overall$Return_Period_FTE_Average <- round(nurse_postret_FTE_overall$Return_Period_FTE_Total / nurse_postret_FTE_overall$Mths_For_FTE_Average, digits = 6)
nurse_postret_FTE_overall <- select(nurse_postret_FTE_overall, Unique_NHS_ID, Return_Period_FTE_Average)

# Merge extra columns onto summary data frame
nurse_ret_summary <- left_join(nurse_ret_summary, nurse_rr_1yr, by="Unique_NHS_ID")
nurse_ret_summary <- left_join(nurse_ret_summary, nurse_post_ret_mths, by="Unique_NHS_ID")
nurse_ret_summary <- left_join(nurse_ret_summary, nurse_preret_FTE, by="Unique_NHS_ID")
nurse_ret_summary <- left_join(nurse_ret_summary, nurse_postret_FTE_firstyear, by="Unique_NHS_ID")
nurse_ret_summary <- left_join(nurse_ret_summary, nurse_postret_FTE_overall, by="Unique_NHS_ID")

# Determine if there are at least 5 retirements in each complete retirement year
nurse_ret_compyr <- filter(nurse_ret_summary, Complete_Ret_Yr_Data == "Yes")
nurse_ret_5 <- nurse_ret_compyr %>% group_by(First_Retirement_Year, Pos_Occ_Desc) %>% summarise(Ret_Ct_Lvl_Yr = n_distinct(Unique_NHS_ID, na.rm = TRUE), .groups = 'drop') %>% as.data.frame(.)
nurse_ret_5 <- nurse_ret_5 %>% group_by(Pos_Occ_Desc) %>% summarise(Min_Ret_Lvl = min(Ret_Ct_Lvl_Yr, na.rm = TRUE), Yr_ct = n_distinct(First_Retirement_Year, na.rm = TRUE), .groups = 'drop') %>% as.data.frame(.)
Max_Yr_Ct <- max(nurse_ret_5$Yr_ct)
nurse_ret_5$Sufficient_Retirements <- ifelse(nurse_ret_5$Min_Ret_Lvl >= 5 & nurse_ret_5$Yr_ct == Max_Yr_Ct, "Yes", "No")
nurse_ret_5 <- select(nurse_ret_5, Pos_Occ_Desc, Sufficient_Retirements)

# Determine if there are at least 5 returners in each complete retirement year
nurse_rr_5 <- filter(nurse_ret_compyr, Returns_After_Retirement == "Yes")
nurse_rr_5 <- nurse_rr_5 %>% group_by(First_Retirement_Year, Pos_Occ_Desc) %>% summarise(RR_Ct_Lvl_Yr = n_distinct(Unique_NHS_ID, na.rm = TRUE), .groups = 'drop') %>% as.data.frame(.)
nurse_rr_5 <- nurse_rr_5 %>% group_by(Pos_Occ_Desc) %>% summarise(Min_RR_Lvl = min(RR_Ct_Lvl_Yr, na.rm = TRUE), Yr_ct = n_distinct(First_Retirement_Year, na.rm = TRUE), .groups = 'drop') %>% as.data.frame(.)
nurse_rr_5$Sufficient_Retirees_Returned <- ifelse(nurse_rr_5$Min_RR_Lvl >= 5 & nurse_rr_5$Yr_ct == Max_Yr_Ct, "Yes", "No")
nurse_rr_5 <- select(nurse_rr_5, Pos_Occ_Desc, Sufficient_Retirees_Returned)

# Determine if there are at least 5 returners within a year in each complete retirement year
nurse_rr1y_5 <- filter(nurse_ret_compyr, Returns_Within_Year == "Yes")
nurse_rr1y_5 <- nurse_rr1y_5 %>% group_by(First_Retirement_Year, Pos_Occ_Desc) %>% summarise(RR1y_Ct_Lvl_Yr = n_distinct(Unique_NHS_ID, na.rm = TRUE), .groups = 'drop') %>% as.data.frame(.)
nurse_rr1y_5 <- nurse_rr1y_5 %>% group_by(Pos_Occ_Desc) %>% summarise(Min_RR1y_Lvl = min(RR1y_Ct_Lvl_Yr, na.rm = TRUE), Yr_ct = n_distinct(First_Retirement_Year, na.rm = TRUE), .groups = 'drop') %>% as.data.frame(.)
nurse_rr1y_5$Sufficient_Ret_Ret_Within_Year <- ifelse(nurse_rr1y_5$Min_RR1y_Lvl >= 5 & nurse_rr1y_5$Yr_ct == Max_Yr_Ct, "Yes", "No")
nurse_rr1y_5 <- select(nurse_rr1y_5, Pos_Occ_Desc, Sufficient_Ret_Ret_Within_Year)

# Merge extra columns onto summary data frame
nurse_ret_summary <- left_join(nurse_ret_summary, nurse_ret_5, by="Pos_Occ_Desc")
nurse_ret_summary <- left_join(nurse_ret_summary, nurse_rr_5, by="Pos_Occ_Desc")
nurse_ret_summary <- left_join(nurse_ret_summary, nurse_rr1y_5, by="Pos_Occ_Desc")

# Calculate calendar and FTE year groups for returners
nurse_ret_summary$Cal_Yr_Ret_Grp <- ifelse(nurse_ret_summary$Returns_After_Retirement == "Yes", 
                                           ifelse(nurse_ret_summary$Post_Ret_Work_Yrs > 7, "y > 7", 
                                                  paste(ceiling(nurse_ret_summary$Post_Ret_Work_Yrs) - 1, ceiling(nurse_ret_summary$Post_Ret_Work_Yrs), 
                                                        sep = " < y <= ")), NA)

nurse_ret_summary$FTE_Yr_Ret_Grp <- ifelse(nurse_ret_summary$Returns_After_Retirement == "Yes", 
                                           ifelse(nurse_ret_summary$Post_Ret_FTE_Years > 7, "y > 7", 
                                                  paste(ceiling(nurse_ret_summary$Post_Ret_FTE_Years) - 1, ceiling(nurse_ret_summary$Post_Ret_FTE_Years), 
                                                        sep = " < y <= ")), NA)

# Write into CSV files
# USER INPUT 9 - Change names and file paths if needed
write.csv(nurse_ret_long, "C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/Retirement/Retire and Return (Analysts Only)/Improved (DAX) Method/CSV Outputs/20231110_nurse_ret_long_jan13_sep23.csv", row.names = FALSE)
write.csv(nurse_ret_summary, "C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/Retirement/Retire and Return (Analysts Only)/Improved (DAX) Method/CSV Outputs/20231110_nurse_ret_summary_jan13_sep23.csv", row.names = FALSE)
write.csv(nurse_ret_long, "C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/Retire and Return/20231110_nurse_ret_long_jan13_sep23.csv", row.names = FALSE)
write.csv(nurse_ret_summary, "C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/Retire and Return/20231110_nurse_ret_summary_jan13_sep23.csv", row.names = FALSE)
