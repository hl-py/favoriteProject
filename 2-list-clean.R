# ----------------------------------------------------------------------------
# PROJECT
# Name: *
# Professor: *
# Author: Heather Low
# ----------------------------------------------------------------------------
# CODE
# Name: 2-list.R
# Date:
# Purpose: Employee Shift Info Packet
# Input: "Co_checks_ptsCut_Checks.RData", "subset_lists.RData", "Date_ShiftCt.RData", "Day_ShiftCt.RData"
# Output: EmpStats_Shifts, EmpStats, EmpStats_CoreSample, EmpStats
# ----------------------------------------------------------------------------


# Employee Shift Info Packet #######################
# --------------------------------------------------------------------- 
# Set-up
rm(list = ls())
source("./proj-helpers.R")
proj_data <- c("Co_checks_ptsCut_Checks.RData", "subset_lists.RData", "Date_ShiftCt.RData", "Day_ShiftCt.RData")  
load_proj_data(proj_data)
# --------------------------------------------------------------------- 


# --------------------------------------------------------------------- 
# Lists ####
length(unique(Co_checks_ptsCut_Checks$Employee_ID)) # 252
list_emp_numbers <- unique(select(Co_checks_ptsCut_Checks, Employee_ID, Unit_ID)) 
list_emp_names <- unique(select(Co_checks_ptsCut_Checks, Employee_ID, FirstName, LastName))
list_emp_names <- na.omit(list_emp_names)
emp_names_numbers <- merge(list_emp_numbers, list_emp_names, by = "Employee_ID", all.x = TRUE) 
list_Day_Shift <- unique(Co_checks_ptsCut_Checks$Day_Shift)
list_Day_Shift <- na.omit(list_Day_Shift)
# --------------------------------------------------------------------- 


# Checks ####
# --------------------------------------------------------------------- 

## Contents:
# Co_checks_ptsCut_Checks
# PrePts_CutChecks
# WithPts_CutChecks

pts_start <- ymd("20140102", tz = "EST")
# Split
PrePts_CutChecks <- subset(Co_checks_ptsCut_Checks, Shift_Date < pts_start)
WithPts_CutChecks <- subset(Co_checks_ptsCut_Checks, Shift_Date >= pts_start)
# Shift_Dates
list_PrePts_Shift_Dates <- as.list(unique(select(PrePts_CutChecks, Shift_Date)))
list_WithPts_Shift_Dates <- as.list(unique(select(WithPts_CutChecks, Shift_Date)))


## Pre
data <- PrePts_CutChecks
data <- unique(select(data, Employee_ID, Date_Shift, Day_Shift))
# Date
PrePts_Date_ShiftCt <- data %>% count(Employee_ID)
colnames(PrePts_Date_ShiftCt)[2] <- 'PrePts_Total_Date_ShiftCt'
# Day 
PrePts_Day_ShiftCt <- data %>% group_by(Employee_ID) %>% count(as.factor(Day_Shift))
colnames(PrePts_Day_ShiftCt)[2] <- 'Day_Shift'
colnames(PrePts_Day_ShiftCt)[3] <- 'PrePts_Day_ShiftCt'


## Post 
data <- WithPts_CutChecks
data <- unique(select(data, Employee_ID, Date_Shift, Day_Shift))
# Date
WithPts_Date_ShiftCt <- data %>% count(Employee_ID)
colnames(WithPts_Date_ShiftCt)[2] <- 'WithPts_Total_Date_ShiftCt'
# Day
WithPts_Day_ShiftCt <- data %>% group_by(Employee_ID) %>% count(as.factor(Day_Shift))
colnames(WithPts_Day_ShiftCt)[2] <- 'Day_Shift'
colnames(WithPts_Day_ShiftCt)[3] <- 'WithPts_Day_ShiftCt'
# --------------------------------------------------------------------- 


# Count ####
# --------------------------------------------------------------------- 
## Contents:
# Day_ShiftCt 
# PrePts_Day_ShiftCt 
# WithPts_Day_ShiftCt

## Day_ShiftCt ####
# Count
df_m <- melt(Day_ShiftCt, id.vars = c("Employee_ID", "Day_Shift"))
df_c <- dcast(df_m, Employee_ID ~ Day_Shift, var.values = Total_Day_ShiftCt) 
Day_ShiftCt <- df_c
Day_ShiftCt[is.na(Day_ShiftCt)] <- 0
Day_ShiftCt$TotalShiftCount <- rowSums(Day_ShiftCt[,c("Sun_Dinner", "Sun_Lunch",  "Mon_Lunch",  "Mon_Dinner", "Tue_Lunch",  "Tue_Dinner", "Wed_Lunch", "Wed_Dinner", "Thu_Lunch", "Thu_Dinner", "Fri_Dinner", "Fri_Lunch",  "Sat_Lunch",  "Sat_Dinner")], na.rm = FALSE) 
# Suspect
Day_ShiftCt <- mutate(Day_ShiftCt, Suspect = Employee_ID %in% suspect_emps)
# First/Last Shift
df <- Co_checks_ptsCut_Checks
df <- select(df, Shift_Date, Employee_ID, Unit_ID, OpenDate) 
df <- as.data.table(df)
setkey(df, Employee_ID) 
first_shift <- aggregate(Shift_Date ~ Employee_ID, df, FUN=head, 1)
names(first_shift)[2] <- "FirstShift"
last_shift <- aggregate(Shift_Date ~ Employee_ID, df, FUN=tail, 1)
names(last_shift)[2] <- "LastShift"
firstlast_shift <- merge(first_shift, last_shift, by = "Employee_ID")
Day_ShiftCt <- merge(Day_ShiftCt, firstlast_shift, by = "Employee_ID", all.x = TRUE)
Day_ShiftCt <- merge(Day_ShiftCt, emp_names_numbers, by = "Employee_ID", all.x = TRUE)
# HasPts
Day_ShiftCt <- mutate(Day_ShiftCt, HasPts = Employee_ID %in% pt_emps$Employee_ID)
# Tenure
Day_ShiftCt <- mutate(Day_ShiftCt, Tenure = 1 + (interval(FirstShift, LastShift) / ddays(1)))
# Workload
Day_ShiftCt <- mutate(Day_ShiftCt, Workload = round(TotalShiftCount / Tenure, digits = 2))
# Revenue
x <- Day_ShiftCt
y <- unique(select(Co_checks_ptsCut_Checks, Employee_ID, Date_Shift, Date_ShiftTotal_Sales))
y <- data.table(y)
setkey(y, Date_Shift)
y <- y[, sum(Date_ShiftTotal_Sales), by = list(Employee_ID)]
names(y)[2] <- "TenureTotalSales" 
Day_ShiftCt <- merge(x, y, by = c("Employee_ID")) # 
Day_ShiftCt  <- mutate(Day_ShiftCt, AvgShiftRevenue = TenureTotalSales / TotalShiftCount)
# --------------------------------------------------------------------- 


# --------------------------------------------------------------------- 
### PrePts_Day_ShiftCt ####
# Count
df_m <- melt(PrePts_Day_ShiftCt, id.vars = c("Employee_ID", "Day_Shift"))
df_c <- dcast(df_m, Employee_ID ~ Day_Shift, var.values = Total_PrePts_Day_ShiftCt) 
PrePts_Day_ShiftCt <- df_c
PrePts_Day_ShiftCt[is.na(PrePts_Day_ShiftCt)] <- 0
PrePts_Day_ShiftCt$PrePts_TotalShiftCount <- rowSums(PrePts_Day_ShiftCt[,c("Sun_Dinner", "Sun_Lunch",  "Mon_Lunch",  "Mon_Dinner", "Tue_Lunch",  "Tue_Dinner", "Wed_Lunch", "Wed_Dinner", "Thu_Lunch", "Thu_Dinner", "Fri_Dinner", "Fri_Lunch",  "Sat_Lunch",  "Sat_Dinner")], na.rm = FALSE) 
# Suspect
PrePts_Day_ShiftCt <- mutate(PrePts_Day_ShiftCt, Suspect = Employee_ID %in% suspect_emps)
# First/Last Shift
df <- PrePts_CutChecks
df <- select(df, Shift_Date, Employee_ID, Unit_ID, OpenDate) 
df <- as.data.table(df)
setkey(df, Employee_ID) 
first_shift <- aggregate(Shift_Date ~ Employee_ID, df, FUN=head, 1)
names(first_shift)[2] <- "FirstShift"
last_shift <- aggregate(Shift_Date ~ Employee_ID, df, FUN=tail, 1)
names(last_shift)[2] <- "LastShift"
firstlast_shift <- merge(first_shift, last_shift, by = "Employee_ID", all.x = TRUE)
PrePts_Day_ShiftCt <- merge(PrePts_Day_ShiftCt, firstlast_shift, by = "Employee_ID", all.x = TRUE)
PrePts_Day_ShiftCt <- merge(PrePts_Day_ShiftCt, emp_names_numbers, by = "Employee_ID", all.x = TRUE)
# HasPts
PrePts_Day_ShiftCt <- mutate(PrePts_Day_ShiftCt, HasPts = Employee_ID %in% pt_emps$Employee_ID)
# Tenure
PrePts_Day_ShiftCt <- mutate(PrePts_Day_ShiftCt, PrePts_Tenure = 1 + (interval(FirstShift, LastShift) / ddays(1)))
# Workload
PrePts_Day_ShiftCt <- mutate(PrePts_Day_ShiftCt, PrePts_Workload = round(PrePts_TotalShiftCount / PrePts_Tenure, digits = 2))
# Revenue
x <- PrePts_Day_ShiftCt
y <- unique(select(PrePts_CutChecks, Employee_ID, Date_Shift, Date_ShiftTotal_Sales))
y <- data.table(y)
setkey(y, Date_Shift)
y <- y[, sum(Date_ShiftTotal_Sales), by = list(Employee_ID)]
names(y)[2] <- "PrePts_TenureTotalSales" 
PrePts_Day_ShiftCt <- merge(x, y, by = c("Employee_ID"))
PrePts_Day_ShiftCt  <- mutate(PrePts_Day_ShiftCt, PrePts_AvgShiftRevenue = PrePts_TenureTotalSales / PrePts_TotalShiftCount)
# --------------------------------------------------------------------- 

# --------------------------------------------------------------------- 
### WithPts_Day_ShiftCt ####
# Count
df_m <- melt(WithPts_Day_ShiftCt, id.vars = c("Employee_ID", "Day_Shift"))
df_c <- dcast(df_m, Employee_ID ~ Day_Shift, var.values = Total_WithPts_Day_ShiftCt) 
WithPts_Day_ShiftCt <- df_c
WithPts_Day_ShiftCt[is.na(WithPts_Day_ShiftCt)] <- 0
WithPts_Day_ShiftCt$WithPts_TotalShiftCount <- rowSums(WithPts_Day_ShiftCt[,c("Sun_Dinner", "Sun_Lunch",  "Mon_Lunch",  "Mon_Dinner", "Tue_Lunch",  "Tue_Dinner", "Wed_Lunch", "Wed_Dinner", "Thu_Lunch", "Thu_Dinner", "Fri_Dinner", "Fri_Lunch",  "Sat_Lunch",  "Sat_Dinner")], na.rm = FALSE) 
# Suspect
WithPts_Day_ShiftCt <- mutate(WithPts_Day_ShiftCt, Suspect = Employee_ID %in% suspect_emps)
# First/Last Shift
df <- WithPts_CutChecks
df <- select(df, Shift_Date, Employee_ID, Unit_ID, OpenDate) 
df <- as.data.table(df)
setkey(df, Employee_ID) 
first_shift <- aggregate(Shift_Date ~ Employee_ID, df, FUN=head, 1)
names(first_shift)[2] <- "FirstShift"
last_shift <- aggregate(Shift_Date ~ Employee_ID, df, FUN=tail, 1)
names(last_shift)[2] <- "LastShift"
firstlast_shift <- merge(first_shift, last_shift, by = "Employee_ID")
WithPts_Day_ShiftCt <- merge(WithPts_Day_ShiftCt, firstlast_shift, by = "Employee_ID", all.x = TRUE)
WithPts_Day_ShiftCt <- merge(WithPts_Day_ShiftCt, emp_names_numbers, by = "Employee_ID", all.x = TRUE)
# HasPts
WithPts_Day_ShiftCt <- mutate(WithPts_Day_ShiftCt, HasPts = Employee_ID %in% pt_emps$Employee_ID)
# Tenure
WithPts_Day_ShiftCt <- mutate(WithPts_Day_ShiftCt, WithPts_Tenure = 1 + (interval(FirstShift, LastShift) / ddays(1)))
# Workload
WithPts_Day_ShiftCt <- mutate(WithPts_Day_ShiftCt, WithPts_Workload = round(WithPts_TotalShiftCount / WithPts_Tenure, digits = 2))
# Revenue
x <- WithPts_Day_ShiftCt
y <- unique(select(WithPts_CutChecks, Employee_ID, Date_Shift, Date_ShiftTotal_Sales))
y <- data.table(y)
setkey(y, Date_Shift)
y <- y[, sum(Date_ShiftTotal_Sales), by = list(Employee_ID)]
names(y)[2] <- "WithPts_TenureTotalSales" 
WithPts_Day_ShiftCt <- merge(x, y, by = c("Employee_ID"))  
WithPts_Day_ShiftCt  <- mutate(WithPts_Day_ShiftCt, WithPts_AvgShiftRevenue = WithPts_TenureTotalSales / WithPts_TotalShiftCount)

# Points
x <- WithPts_Day_ShiftCt
WithPts_CutChecks$NormalizedUserTotalPoints[is.na(WithPts_CutChecks$NormalizedUserTotalPoints)] <- 0
y <- unique(select(WithPts_CutChecks, Employee_ID, Date_Shift, NormalizedUserTotalPoints))

y <- data.table(y)
setkey(y, Date_Shift)
# y$Employee_ID <- as.factor(y$Employee_ID)
y <- y[, sum(NormalizedUserTotalPoints), by = list(Employee_ID)]
names(y)[2] <- "TenureTotalPoints" 
WithPts_Day_ShiftCt <- merge(x, y, by = c("Employee_ID")) 
WithPts_Day_ShiftCt  <- mutate(WithPts_Day_ShiftCt, PtsPerDollar = TenureTotalPoints /  WithPts_TenureTotalSales)
# --------------------------------------------------------------------- 


# --------------------------------------------------------------------- 
# EmpStats ####

# PrePts
x <- Day_ShiftCt
y <- select(PrePts_Day_ShiftCt, Employee_ID, PrePts_TotalShiftCount, PrePts_Tenure, PrePts_Workload, PrePts_TenureTotalSales, PrePts_AvgShiftRevenue)
y <- data.table(y)
EmpStats <- merge(x, y, by = c("Employee_ID"), all = TRUE) 

# Add WithPts
x <- EmpStats
y <- select(WithPts_Day_ShiftCt, Employee_ID, WithPts_TotalShiftCount, WithPts_Tenure, WithPts_Workload, WithPts_TenureTotalSales, WithPts_AvgShiftRevenue, TenureTotalPoints, PtsPerDollar)
y <- data.table(y)
EmpStats <- merge(x, y, by = c("Employee_ID"), all = TRUE) 

### Order columns
EmpStats <- select(EmpStats, Employee_ID, FirstName, LastName, Unit_ID, HasPts, Suspect, FirstShift, LastShift, TotalShiftCount, PrePts_TotalShiftCount, WithPts_TotalShiftCount, Tenure, PrePts_Tenure, WithPts_Tenure, Workload, PrePts_Workload, WithPts_Workload, PrePts_TenureTotalSales, WithPts_TenureTotalSales, PrePts_AvgShiftRevenue, WithPts_AvgShiftRevenue, TenureTotalPoints, PtsPerDollar)

# --------------------------------------------------------------------- 
# Select Core Sample
typeof(EmpStats$PrePts_TotalShiftCount)
typeof(EmpStats$WithPts_TotalShiftCount)

EmpStats_CoreSample <- subset(EmpStats, (PrePts_TotalShiftCount >= 40 & WithPts_TotalShiftCount >= 40)) # 48
# EmpStats_CoreSample <- mutate(EmpStats_CoreSample, ShiftRev_Drop = PrePts_AvgShiftRevenue > WithPts_AvgShiftRevenue + 20)
# EmpStats_CoreSample <- mutate(EmpStats_CoreSample, Workload_Drop = PrePts_Workload > WithPts_Workload + .10)

# List of Core Sample Emps
list_emp_CoreSample <- EmpStats_CoreSample$Employee_ID

# Remove suspect emps and registers
list_suspect_CoreSample <- c(*) # registers, workload near 1 
list_suspect_CoreSample <- append(list_suspect_CoreSample, c(*), after = length(list_suspect_CoreSample)) 

list_emp_CoreSample <- setdiff(list_emp_CoreSample, list_suspect_CoreSample) 
# --------------------------------------------------------------------- 




# --------------------------------------------------------------------- 
# Make EmpStats Pretty
# EmpStats[is.na(EmpStats)] <- 0
# Caution: prettyNum turns into characters
EmpStats_CoreSample[16:23] <- round(EmpStats_CoreSample[16:23], digits = 2)
# EmpStats_CoreSample[16:19] <- EmpStats_CoreSample[16:19] %>% mutate_each(funs(prettyNum(., big.mark=",")))
# EmpStats_CoreSample[9:13] <- EmpStats_CoreSample[9:13] %>% mutate_each(funs(prettyNum(., big.mark=",")))

EmpStats[16:19] <- round(EmpStats[16:19], digits = 2)
# EmpStats[16:19] <- EmpStats[16:19] %>% mutate_each(funs(prettyNum(., big.mark=",")))
# EmpStats[9:13] <- EmpStats[9:13] %>% mutate_each(funs(prettyNum(., big.mark=",")))
# --------------------------------------------------------------------- 

# --------------------------------------------------------------------- 
### Order columns
# Order by week
EmpStats_Shifts <- select(Day_ShiftCt, Employee_ID, FirstName, LastName, Unit_ID, HasPts, FirstShift, LastShift, Suspect, Tenure, Workload, Mon_Lunch, Mon_Dinner, Tue_Lunch, Tue_Dinner, Wed_Lunch, Wed_Dinner, Thu_Lunch, Thu_Dinner, Fri_Lunch, Fri_Dinner, Sat_Lunch, Sat_Dinner, Sun_Lunch, Sun_Dinner, TotalShiftCount, TenureTotalSales)

# Order by shift
employee_Info_byShift <- select(Day_ShiftCt, Employee_ID, FirstName, LastName, Unit_ID, HasPts, FirstShift, LastShift, Suspect, Tenure, Workload, Mon_Lunch, Tue_Lunch, Wed_Lunch, Thu_Lunch, Fri_Lunch, Sat_Lunch, Sun_Lunch, Mon_Dinner, Tue_Dinner, Wed_Dinner, Thu_Dinner, Fri_Dinner, Sat_Dinner, Sun_Dinner, TotalShiftCount, TenureTotalSales)
# ---------------------------------------------------------------------


# --------------------------------------------------------------------- 
save(EmpStats_Shifts, file = make_processed_data_paths("EmpStats_Shifts.RData"))
write.csv(EmpStats_Shifts, file = paste(data_dir, "/processed/EmpStats_Shifts.csv", sep = ""))

save(EmpStats, file = make_processed_data_paths("EmpStats.RData"))
write.csv(EmpStats, file = paste(data_dir, "/processed/EmpStats.csv", sep = ""))

save(EmpStats_CoreSample, file = make_processed_data_paths("EmpStats_CoreSample.RData"))
write.csv(EmpStats_CoreSample, file = paste(data_dir, "/processed/EmpStats_CoreSample.csv", sep = ""))

save(EmpStats, list_emp_CoreSample, file = make_processed_data_paths("list_emp_CoreSample.RData"))



