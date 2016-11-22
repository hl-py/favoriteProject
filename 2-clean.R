# ----------------------------------------------------------------------------
# PROJECT
# Name: *
# Professor: *
# Author: Heather Low
# ----------------------------------------------------------------------------
# CODE
# Name: 2- 
# Date:
# Purpose: Subset data and make employee shift_info list.
# Input: "Co_checks_pts.RData"
# Output: subset_lists, Day_ShiftCt.RData
# ----------------------------------------------------------------------------
rm(list = ls())
# Set-Up
source("./proj-helpers.R")

# --------------------------------------------------------------------- 
# Wrangle Checks
# --------------------------------------------------------------------- 

# Data
proj_data <- c("Co_checks_pts.RData")  
load_proj_data(proj_data)

# --------------------------------------------------------------------- 
################################# Checks ##############################

## "Bad" checks
# Voided
Co_checks_NTotal <- Co_checks_pts[Co_checks_pts$Total < 0,] 
voided_check <- Co_checks_NTotal$Check_ID
# Misrung
misring_check <- Co_checks_NTotal$Check_ID - 1
Co_checks_VTotal <- Co_checks_pts[Co_checks_pts$Check_ID %in% misring_check,] 
Co_checks_VM <- rbind(Co_checks_NTotal, Co_checks_VTotal) 
# Comped
Co_checks_0Total  <- Co_checks_pts[Co_checks_pts$Total == 0,] 
compED_check <- Co_checks_0Total$Check_ID
# Pre-Comp
compOrig_check <- Co_checks_0Total$Check_ID - 1 
Co_checks_compOrig <- Co_checks_pts[Co_checks_pts$Check_ID %in% compOrig_check,] 
Co_checks_CompOrig <- rbind(Co_checks_0Total, Co_checks_compOrig) 
# "Bad" Checks
bad_checks <- c(misring_check, voided_check, compOrig_check, compED_check) 
Co_checks_pts <- subset(Co_checks_pts, !(Co_checks_pts$Check_ID %in% bad_checks))

## "Suspect" checks
# Low Total 
Co_checks_1Total <- Co_checks_pts[Co_checks_pts$Total < 1,] 
Co_checks_2Total <- Co_checks_pts[Co_checks_pts$Total < 2 ,] 
# High Total 
Co_checks_1000Total <- Co_checks_pts[Co_checks_pts$Total > 1000,] 
# Suspect" Checks 
suspect_checks <- c(Co_checks_2Total$Check_ID, Co_checks_1000Total$Check_ID) 
# Remove
Co_checks_ptsCut_Checks <- subset(Co_checks_pts, !(Co_checks_pts$Check_ID %in% suspect_checks))

# Add Yr_Wk
Co_checks_ptsCut_Checks <- mutate(Co_checks_ptsCut_Checks, Yr_Wk = paste(Year, "_", Week, sep = ""))

#.................................................. 
# Add cols with revenue, gratuity, and points info per Date_Shift
# x
x <- Co_checks_ptsCut_Checks

# Revenue
y <- select(Co_checks_ptsCut_Checks, Employee_ID, Date_Shift, Total) 
y <- data.table(y)
setkey(y, Date_Shift)
y <- y[, sum(Total), by = list(Date_Shift, Employee_ID)]
names(y)[3] <- "Date_ShiftTotal_Sales" 

# z
# Merge
Co_checks_ptsCut_Checks <- merge(x, y, by = c("Date_Shift", "Employee_ID")) 


# x
x <- Co_checks_ptsCut_Checks

# Gratuity
y <- select(Co_checks_ptsCut_Checks, Employee_ID, Date_Shift, Gratuity) 
y <- data.table(y)
setkey(y, Date_Shift)
y <- y[, sum(Gratuity), by = list(Date_Shift, Employee_ID)]
names(y)[3] <- "Date_ShiftTotal_Gratuity" 

# z
# Merge
Co_checks_ptsCut_Checks <- merge(x, y, by = c("Date_Shift", "Employee_ID"))


# x
x <- Co_checks_ptsCut_Checks

# Points (NormalizedUserTotalPoints)
y <- select(Co_checks_ptsCut_Checks, Employee_ID, Date_Shift, NormalizedUserTotalPoints) 
y <- data.table(y)
setkey(y, Date_Shift)
y <- y[, sum(NormalizedUserTotalPoints), by = list(Date_Shift, Employee_ID)]
names(y)[3] <- "Date_ShiftTotal_Points" 

# z
# Merge
Co_checks_ptsCut_Checks <- merge(x, y, by = c("Date_Shift", "Employee_ID"))


# Employee Shift Counts
# --------------------------------------------------------------------- 
data <- Co_checks_ptsCut_Checks
data <- unique(select(data, Employee_ID, Date_Shift, Day_Shift))

# Date_Shift
Date_ShiftCt <- data %>% count(Employee_ID)
colnames(Date_ShiftCt)[2] <- 'Total_Date_ShiftCt'

# Day_Shift 
Day_ShiftCt <- data %>% group_by(Employee_ID) %>% count(as.factor(Day_Shift))
colnames(Day_ShiftCt)[2] <- 'Day_Shift'
colnames(Day_ShiftCt)[3] <- 'Day_ShiftCt'
# --------------------------------------------------------------------- 


# --------------------------------------------------------------------- 
Co_HShiftCt <- Date_ShiftCt$Employee_ID[Date_ShiftCt$Total_Date_ShiftCt > 1000] 
Co_LShiftCt <- Date_ShiftCt$Employee_ID[Date_ShiftCt$Total_Date_ShiftCt < 10]  

# "Suspect" Employees
suspect_emps <- c(Co_HShiftCt, Co_LShiftCt) 
# Remove
Co_checks_ptsCut_ChecksEmps <- subset(Co_checks_ptsCut_Checks, !(Co_checks_ptsCut_Checks$Employee_ID %in% suspect_emps)) 

# info on remaining emps in ...Cut_ChecksEmps
length(unique(Co_checks_ptsCut_ChecksEmps$Employee_ID[Co_checks_ptsCut_ChecksEmps$Unit_ID == "34"])) 
length(unique(Co_checks_ptsCut_ChecksEmps$Employee_ID[Co_checks_ptsCut_ChecksEmps$Unit_ID == "35"]))
length(unique(Co_checks_ptsCut_ChecksEmps$Employee_ID[Co_checks_ptsCut_ChecksEmps$Unit_ID == "36"])) 


# How many have points?
pt_emps <- subset(Co_checks_ptsCut_Checks, Co_checks_ptsCut_Checks$NormalizedUserTotalPoints >= 0)
# pt_emps <- select(pt_emps, Employee_ID, Unit_ID, Date_ShiftTotal_Points)
pt_emps <- unique(select(pt_emps, Employee_ID, Unit_ID)) 

length(unique(pt_emps$Employee_ID[pt_emps$Unit_ID == "34"]))
length(unique(pt_emps$Employee_ID[pt_emps$Unit_ID == "35"])) 
length(unique(pt_emps$Employee_ID[pt_emps$Unit_ID == "36"])) 

Co_checks_ptsCut_Checks <- mutate(Co_checks_ptsCut_Checks, HasPts = Employee_ID %in% pt_emps$Employee_ID) 
# --------------------------------------------------------------------- 


# --------------------------------------------------------------------- 
# Save lists of bad and suspect checks and employees to one .RData file
# save_list <- c("suspect_emps", "suspect_checks", "pt_emps", "allEmps_34", "allEmps_35","allEmps_36", "allEmps_Co", "Co_HShiftCt", "Co_LShiftCt")
save_list <- c("suspect_emps", "suspect_checks", "pt_emps", "Co_HShiftCt", "Co_LShiftCt")
# save_files <- c(paste(parent, "/data/processed/lists/", save_list, ".RData", sep = ""))
save_files <- c(paste(processed_data_dir, "subset_lists", ".RData", sep = ""))
save(list = save_list, file = save_files)

# Save data without bad/suspect employees and checks
# save_proj_data("Co_checks_pts_ShiftCut")
save(Co_checks_ptsCut_Checks, file = make_processed_data_paths("Co_checks_ptsCut_Checks.RData"))
save(Date_ShiftCt, file = make_processed_data_paths("Date_ShiftCt.RData"))
save(Day_ShiftCt, file = make_processed_data_paths("Day_ShiftCt.RData"))
# save(Co_checks_ptsCut_ChecksEmps, file = make_processed_data_paths("Co_checks_ptsCut_ChecksEmps.RData"))
# --------------------------------------------------------------------- 


