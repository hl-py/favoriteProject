# ----------------------------------------------------------------------------
# PROJECT
# Name: *
# Professor: *
# Author: Heather Low
# ----------------------------------------------------------------------------
# CODE
# Name: 1-
# Date: *
# Purpose: Wrangle checks and points.
# Input: "all_employeesInChecks.RData", "Co_checks.RData", "all_employeesInChecks.RData", "Co_checks.RData"
# Output: Co_checks_dt, Co_pts, Co_checks_pts
# ----------------------------------------------------------------------------

# Set-Up
source("./proj-helpers.R")

# --------------------------------------------------------------------- 
################################# Checks ##############################

# Data
proj_data <- c("all_employeesInChecks.RData", "Co_checks.RData")  
load_proj_data(proj_data)

# - unique, subset, merge employees and checks
all_employeesInChecks_1 <- unique(all_employeesInChecks)
rm("all_employeesInChecks")
Co_checks_1 <- unique(Co_checks)
Co_checks_1 <- subset(Co_checks_1, select = c("ID", "OpenDate", "PrintDate", "Unit_ID", "Total", "Gratuity", "PartySize", "SeatsServed"))

rm("Co_checks")
Co_checks_1a <- merge(Co_checks_1, all_employeesInChecks_1, by.x = c("ID"), by.y = c("Check_ID"))
rm(list = c("all_employeesInChecks_1", "Co_checks_1"))
sapply(Co_checks_1a,typeof) ##

############################### Date & Time
names(Co_checks_1a)[1] <- "Check_ID"
checks_dt <- Co_checks_1a
rm(Co_checks_1a)

############################### By Calander Date 
# Change time zone (from assumed GMT/UTC to EST)
checks_dt$OpenDate <- as.POSIXct(checks_dt$OpenDate) - dhours(5)
checks_dt$CloseDate <- as.POSIXct(checks_dt$PrintDate) - dhours(5)
# CheckOpenTime 
checks_dt <- mutate(checks_dt, CheckOpenTime = format(as.POSIXct(checks_dt$OpenDate), format = "%H:%M:%S")) 
# CheckCloseTime
checks_dt <- mutate(checks_dt, CheckCloseTime = format(as.POSIXct(checks_dt$PrintDate), format = "%H:%M:%S")) 
# Year
checks_dt$Year <-format(as.Date(checks_dt$OpenDate), format ="%Y", tz = "EST")
# Week
checks_dt$Week <-format(as.Date(checks_dt$OpenDate), format ="%V", tz = "EST")
# WeekDay 
checks_dt <- mutate(checks_dt, WeekDay = format(as.POSIXct(checks_dt$OpenDate), format = "%a")) 
# WeekDay_Numeric
checks_dt$WeekDay_Numeric <- 0
checks_dt$WeekDay_Numeric[checks_dt$WeekDay == "Sun"] <- 0
checks_dt$WeekDay_Numeric[checks_dt$WeekDay == "Mon"] <- 1
checks_dt$WeekDay_Numeric[checks_dt$WeekDay == "Tue"] <- 2
checks_dt$WeekDay_Numeric[checks_dt$WeekDay == "Wed"] <- 3
checks_dt$WeekDay_Numeric[checks_dt$WeekDay == "Thu"] <- 4
checks_dt$WeekDay_Numeric[checks_dt$WeekDay == "Fri"] <- 5
checks_dt$WeekDay_Numeric[checks_dt$WeekDay == "Sat"] <- 6

############################### By Shift Date 
# ShiftDay_Numeric
checks_dt$ShiftDay_Numeric <- 0
next_day <- "06:00:00"
checks_dt$ShiftDay_Numeric <- ifelse(checks_dt$CheckOpenTime < next_day, checks_dt$WeekDay_Numeric - 1, checks_dt$WeekDay_Numeric)
# - 1am Sat labled 0 for Sun, -1 should be 6 for Sat
checks_dt$ShiftDay_Numeric[checks_dt$ShiftDay_Numeric == -1] <- 6 

# ShiftDay
checks_dt$ShiftDay <- 0
checks_dt$ShiftDay[checks_dt$ShiftDay_Numeric == 0 ] <- "Sun"
checks_dt$ShiftDay[checks_dt$ShiftDay_Numeric == 1 ] <- "Mon"
checks_dt$ShiftDay[checks_dt$ShiftDay_Numeric == 2 ] <- "Tue"
checks_dt$ShiftDay[checks_dt$ShiftDay_Numeric == 3 ] <- "Wed"
checks_dt$ShiftDay[checks_dt$ShiftDay_Numeric == 4 ] <- "Thu"
checks_dt$ShiftDay[checks_dt$ShiftDay_Numeric == 5 ] <- "Fri"
checks_dt$ShiftDay[checks_dt$ShiftDay_Numeric == 6 ] <- "Sat"

# ShiftDate
names(checks_dt)[1] <- "Check_ID"
checks_dt$ShiftDate <- checks_dt$OpenDate
afterMidnightCheckID_list <- checks_dt$Check_ID[checks_dt$ShiftDay_Numeric < checks_dt$WeekDay_Numeric]
checks_dt_AfterMidnight <- subset(checks_dt, checks_dt$Check_ID %in% afterMidnightCheckID_list)
checks_dt_AfterMidnight$ShiftDate <- checks_dt_AfterMidnight$ShiftDate - ddays(1) 
checks_dt$ShiftDate[match(checks_dt_AfterMidnight$Check_ID, checks_dt$Check_ID)] <- as.POSIXct(with_tz(strptime(checks_dt_AfterMidnight$ShiftDate, "%Y-%m-%d", tz = "EST"), tz = "EST")) 
checks_dt <- mutate(checks_dt, ShiftDate = format(as.POSIXct(checks_dt$ShiftDate), format = "%Y-%m-%d"))
rm(afterMidnightCheckID_list, checks_dt_AfterMidnight, next_day) 

############################### In Times  
Co_inTimes <- checks_dt
Co_inTimes <- select(Co_inTimes, ShiftDate, OpenDate, Employee_ID) 
Co_inTimes <- as.data.table(Co_inTimes)
setkey(Co_inTimes, ShiftDate) 
Co_inTimes <- Co_inTimes[,head(OpenDate, n = 1), by = list(ShiftDate, Employee_ID)] 
names(Co_inTimes)[3] <- "InTime"

# Round to half-hour  
typeof(Co_inTimes$InTime) ##
class(Co_inTimes$InTime) ##
Co_inTimes$InTime[1]
Co_inTimes$InTime <- align.time(Co_inTimes$InTime, 30*60) 
Co_inTimes$InTime <- format(Co_inTimes$InTime, format ="%H:%M", tz = "EST") # makes character

t <- Co_inTimes[Co_inTimes$InTime < "09:00",] 
t <- Co_inTimes[Co_inTimes$InTime > "18:30",] 
t <- Co_inTimes[Co_inTimes$InTime > "20:30",] 
# all these odd in-times less than *%

############################# Define Shift with In Times
# Use in-time to define shift
Co_inTimes$Shift <- 0

# 1pm cut off for counting to lunch in-time
Co_inTimes$Shift <- ifelse(Co_inTimes$InTime < "13:00", "Lunch", "Dinner") 

# Add in-times to checks table
checks_dt <- merge(checks_dt, Co_inTimes, by = c("ShiftDate", "Employee_ID"))

# Label Double
checks_dt$Double <- 0
checks_dt$Double <- ifelse(checks_dt$InTime < "13:00" & checks_dt$CheckOpenTime > "17:00", TRUE, FALSE) 
t <- checks_dt[checks_dt$Double == TRUE,] 

# info
test <- as.data.table(checks_dt)
Co_freq_Dbl <- test[, .N ,by = checks_dt$Double] 
checks_dt$Shift[checks_dt$CheckOpenTime > "17:00" & checks_dt$Double == TRUE] <- "Dinner"
t <- checks_dt[checks_dt$Employee_ID == "7592" & checks_dt$Double == TRUE & checks_dt$ShiftDate == "2010-10-03",]
t <- checks_dt[checks_dt$Employee_ID == "7592" & checks_dt$ShiftDate == "2010-10-03",] # dosen't label lunch double, only dinner as a second shift, but shift labels correct
rm(test, Co_freq_Dbl, t)


# Label Date_Shift 
checks_dt <- mutate(checks_dt, Date_Shift = paste(checks_dt$ShiftDate, checks_dt$Shift, sep  = "_"))
# Label Day_Shift 
checks_dt <- mutate(checks_dt, Day_Shift = paste(checks_dt$ShiftDay, checks_dt$Shift, sep  = "_"))

# Tidy columns
colnames(checks_dt)
head(checks_dt)
# t <- checks_dt[c(3, 1, 14, 15, 4, 5, 12, 13, 23, 19, 21, 20, 22, 24, 2, 6:10)]
checks_dt <- checks_dt[c(3, 1, 14, 15, 4, 5, 12, 13, 23, 19, 21, 20, 22, 24, 2, 6:10)]
Co_checks_dt <- checks_dt
rm(checks_dt)

# --------------------------------------------------------------------- 
# Save checks with new date-time formats
save(Co_checks_dt, file = make_processed_data_paths("Co_checks_dt.RData"))
# --------------------------------------------------------------------- 


# --------------------------------------------------------------------- 
################################# Points ##############################

# Data
proj_data <- c("all_employees.RData", "theProjectData.RData")   
load_proj_data(proj_data)

upsellSlices$NormalizedUserTotalPoints <- 0
upsellSlices$NormalizedSkillAveragePoints <- 0
upsellSlices$NormalizedSkillTotalPoints <- 0
all_upsellSlices <- rbind(upsellSlices, upsellSlices2)
rm(upsellSlices, upsellSlices2)

# Remove duplicates by UserContext_ID
upsellSlicesID_list <- unique(all_upsellSlices$ID) 
all_upsellSlices <- subset(all_upsellSlices, all_upsellSlices$ID %in% upsellSlicesID_list) 
# UpsellSlices-
locations <- c("34", "35", "36", "37")
Co_all_upsellSlices <- unique(subset(all_upsellSlices, all_upsellSlices$UnitID %in% locations))
names(Co_all_upsellSlices)[2] <- "UserContext_ID"
# Employees- Just Co Employees
Co_all_employees <- subset(all_employees, all_employees$Unit_ID %in% locations)
Co_all_employees$Unit_ID <- as.character(Co_all_employees$Unit_ID)
# Merge UpsellSlices and Employees to add Employee_ID 
names(Co_all_upsellSlices)[1] <- "UpsellSlicesID"
Co_all_empUpsellSlices2 <- merge(Co_all_upsellSlices, Co_all_employees, by = "UserContext_ID") # 
names(Co_all_empUpsellSlices2)[20] <- "Employee_ID" 
# Employees- Remove rows with empty FirstName, look like duplicates, remove.
Co_all_empUpsellSlices2 <- subset(Co_all_empUpsellSlices2, Co_all_empUpsellSlices2$FirstName > 0) 

# Dates
Co_all_empUpsellSlices2$ShiftDate <- as.POSIXct(Co_all_empUpsellSlices2$StartTime)
Co_all_empUpsellSlices2$ShiftDate <- format(Co_all_empUpsellSlices2$ShiftDate, "%Y-%m-%d")
Co_all_empUpsellSlices2$ShiftDate[1] ## "2014-06-08 EDT"

Co_points <- Co_all_empUpsellSlices2
colnames(Co_points)
Co_points <- select(Co_points, UpsellSlicesID, StartTime, ShiftDate, Employee_ID, UserTotalPoints, NormalizedUserTotalPoints, Unit_ID, FirstName, LastName) 

Co_points <- unique(Co_points)
Co_points <- subset(Co_points, NormalizedUserTotalPoints != 0)
Co_points <- Co_points[order(Co_points$ShiftDate, Co_points$Employee_ID),]

# Adjust for time zone
Co_points$StartTime <- as.POSIXct(Co_points$StartTime) - dhours(5)
# StartTime_Hr 
Co_points <- mutate(Co_points, StartTime_Hr = format(as.POSIXct(Co_points$StartTime), format = "%H:%M")) 
# WeekDay 
Co_points <- mutate(Co_points, WeekDay = format(as.POSIXct(Co_points$StartTime), format = "%a")) 
# WeekDay_Numeric
Co_points$WeekDay_Numeric <- 0
Co_points$WeekDay_Numeric[Co_points$WeekDay == "Sun"] <- 0
Co_points$WeekDay_Numeric[Co_points$WeekDay == "Mon"] <- 1
Co_points$WeekDay_Numeric[Co_points$WeekDay == "Tue"] <- 2
Co_points$WeekDay_Numeric[Co_points$WeekDay == "Wed"] <- 3
Co_points$WeekDay_Numeric[Co_points$WeekDay == "Thu"] <- 4
Co_points$WeekDay_Numeric[Co_points$WeekDay == "Fri"] <- 5
Co_points$WeekDay_Numeric[Co_points$WeekDay == "Sat"] <- 6

############################### By Shift Date 
# ShiftDay_Numeric
Co_points$ShiftDay_Numeric <- 0
next_day <- "06:00:00"
Co_points$ShiftDay_Numeric <- ifelse(Co_points$StartTime_Hr < next_day, Co_points$WeekDay_Numeric - 1, Co_points$WeekDay_Numeric)
Co_points$ShiftDay_Numeric[Co_points$ShiftDay_Numeric == -1] <- 6 # 1am Sat labled 0 for Sun, -1 should be 6 for Sat
head(Co_points)
# ShiftDay
Co_points$ShiftDay <- 0
Co_points$ShiftDay[Co_points$ShiftDay_Numeric == 0 ] <- "Sun"
Co_points$ShiftDay[Co_points$ShiftDay_Numeric == 1 ] <- "Mon"
Co_points$ShiftDay[Co_points$ShiftDay_Numeric == 2 ] <- "Tue"
Co_points$ShiftDay[Co_points$ShiftDay_Numeric == 3 ] <- "Wed"
Co_points$ShiftDay[Co_points$ShiftDay_Numeric == 4 ] <- "Thu"
Co_points$ShiftDay[Co_points$ShiftDay_Numeric == 5 ] <- "Fri"
Co_points$ShiftDay[Co_points$ShiftDay_Numeric == 6 ] <- "Sat"

# ShiftDate
Co_points$Date_Shift <- Co_points$StartTime
afterMidnightUpsellSlicesID_list <- Co_points$UpsellSlicesID[Co_points$ShiftDay_Numeric < Co_points$WeekDay_Numeric]
Co_points_AfterMidnight <- subset(Co_points, Co_points$UpsellSlicesID %in% afterMidnightUpsellSlicesID_list)
Co_points_AfterMidnight$Date_Shift <- as.POSIXct(Co_points_AfterMidnight$Date_Shift) - ddays(1) 
Co_points$Date_Shift[match(Co_points_AfterMidnight$UpsellSlicesID, Co_points$UpsellSlicesID)] <- as.POSIXct(Co_points_AfterMidnight$Date_Shift, "%Y-%m-%d", tz = "EST") #
rm(afterMidnightUpsellSlicesID_list, Co_points_AfterMidnight, next_day) 

Co_points <- merge.data.frame(Co_points, Co_inTimes, by = c("ShiftDate", "Employee_ID"))

############################# Label Date_Shift 
Co_points$Date_Shift <- strptime(Co_points$Date_Shift, format = "%Y-%m-%d")
Co_points <- mutate(Co_points, Date_Shift = paste(Co_points$Date_Shift, Co_points$Shift, sep  = "_"))
Co_points_dt <- select(Co_points, UpsellSlicesID, ShiftDate, WeekDay, Shift, Employee_ID, FirstName, LastName, Date_Shift, StartTime, UserTotalPoints, NormalizedUserTotalPoints)

# --------------------------------------------------------------------- 
# Save points with new date-time formats
# save_proj_data("Co_pts_dt")
save(Co_points_dt, file = make_processed_data_paths("Co_points_dt.RData"))
# --------------------------------------------------------------------- 

# info
length(unique(Co_points_dt$Employee_ID)) 
length(unique(Co_checks_dt$Employee_ID)) 

# --------------------------------------------------------------------- 
#################### Mergre Checks and Points #########################

# Merge 
x <- Co_checks_dt
y <- Co_points_dt
Co_checks_pts <- full_join(x, y, by = c("Employee_ID", "Date_Shift"))
colnames(Co_checks_pts)

# Edit names
names(Co_checks_pts)[2] <- "Shift_Date"
names(Co_checks_pts)[11] <- "Shift"
Co_checks_pts[22] <- NULL
colnames(Co_checks_pts)
Co_checks_pts[23] <- NULL
colnames(Co_checks_pts)
Co_checks_pts <- unique(Co_checks_pts) # same

# Remove any duplicate check ids that might be left over from merging
check_ids <- Co_checks_pts$Check_ID
check_ids <- unique(Co_checks_pts$Check_ID)
head(check_ids)
Co_checks_pts <-Co_checks_pts[!duplicated(Co_checks_pts$Check_ID),]

# info
length(unique(Co_checks_pts$Employee_ID)) 

# --------------------------------------------------------------------- 
# Save merged checks and points tables
save(Co_checks_pts, file = make_processed_data_paths("Co_checks_pts.RData"))
# --------------------------------------------------------------------- 
