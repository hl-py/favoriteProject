# ----------------------------------------------------------------------------
# PROJECT
# Name: *
# Professor: *
# Author: Heather Low
# ----------------------------------------------------------------------------
# CODE
# Name: 3-
# Date:
# Purpose: Linear regression <<< in progress.
# Input: "Co_checks_ptsCut_Checks.RData", "subset_lists.RData", "EmpStats_CoreSample.RData", "list_emp_CoreSample.RData"
# Output: EmpStats_CoreSample_Shifts_lm, EmpCoreStats 
# ----------------------------------------------------------------------------
rm(list = ls())
# Set-Up
source("./proj-helpers.R")


####################### Linear Regression ####################################
# Data
# proj_data <- c("Co_checks_ptsCut_Checks.RData", "subset_lists.RData")  
proj_data <- c("Co_checks_ptsCut_Checks.RData", "subset_lists.RData", "EmpStats_CoreSample.RData", "list_emp_CoreSample.RData")  
load_proj_data(proj_data)
# --------------------------------------------------------------------- 

# consolidated varibles - By Date_Shift 
# Get unique characteristics for each Date_Shift worked by an employee
Co_checks_ptsCut_Checks_orig <- Co_checks_ptsCut_Checks
colnames(Co_checks_ptsCut_Checks)
# names(Co_checks_ptsCut_Checks)[21] <- "Shift"
Co_checks_ptsCut_Checks <- mutate(Co_checks_ptsCut_Checks, Yr_Wk = paste(Year, "_", Week, sep = ""))

x <- select(Co_checks_ptsCut_Checks, Check_ID, Unit_ID, Date_Shift, Yr_Wk, Day_Shift, InTime)
colnames(df)

# ------- Get each employees total sales for each Date_Shift worked
y <- select(Co_checks_ptsCut_Checks, Employee_ID, Date_Shift, Total) 
y <- data.table(y)
setkey(y, Date_Shift)
y <- y[, sum(Total), by = list(Date_Shift, Employee_ID)]
names(y)[3] <- "TotalSales" 

# Merge employee's total sales with characteristics to get a summery of each Data_Shift
z <- merge.data.frame(x, y, by = intersect(names(x), names(y)), all.x = TRUE) # too long, too many
z <- full_join(x, y) 

# 



### Prep for regression

# Make lists for betas for this set
beta_names <- c("Employee_ID", "Unit_ID", "Yr_Wk", "Day_Shift", "InTime")
beta_ct <- length(beta_names)


# --------------------------------------------------------------------- 
# --------------------------- Model - By Date_Shift 
# ------- Y
df <- z
Y = df$TotalSales

# ------- Betas
a = df$ID
b = df$Unit_ID
c = df$Yr_Wk # "2010_43"
d = df$Day_Shift # "Mon_Lunch"
e = df$InTime
# ------- 

# ------- Model - lm() 
# ------- without interaction
model <- lm(Y ~ a + b + c + d + e)
model <- lm(Y ~ factor(a) + factor(b) + factor(c) + factor(d) + factor(e))

# # ------- with interaction  <<< in progress
# # simple case
# model <- lm(Y ~ (a * b))
# model <- lm(Y ~ (a * b) * c))
#                  
# # characters automatically converted to factors?
# model <- lm(Y ~ (a + b + c + d + e)^2)
# model <- lm(Y ~ (a + b + c + d + e)*(a + b + c + d + e))
# # explicitly convert to factors
# model <- lm(Y ~ (factor(a) + factor(b) + factor(c) + factor(d) + factor(e))^2)
# model <- lm(Y ~ (factor(a) + factor(b) + factor(c) + factor(d) + factor(e))*(factor(a) + factor(b) + factor(c) + factor(d) + factor(e)))

# ------- Explore
plot(model)
termplot(model)
summary(model)

# # ------- Predict <<< in progress
# new.df <- data.frame(WeekDay = "Fri") #
# predict(model, new.df)


####################### Linear Regression 2 ####################################


# Data
# --------------------------------------------------------------------- 
proj_data <- c("Co_checks_ptsCut_Checks.RData", "subset_lists.RData", "EmpStats_CoreSample.RData", "list_emp_CoreSample.RData")  
load_proj_data(proj_data)
EmpStats_CoreSample_Checks <- subset(Co_checks_ptsCut_Checks, Employee_ID %in% list_emp_CoreSample) 
# --------------------------------------------------------------------- 


# --------------------------------------------------------------------- 
# Get each employees total sales for each Date_Shift worked
x <- select(EmpStats_CoreSample_Checks, Check_ID, Employee_ID, Unit_ID, Date_Shift, Yr_Wk, Day_Shift, InTime)
y <- select(EmpStats_CoreSample_Checks, Employee_ID, Date_Shift, Total) 
y <- data.table(y)
setkey(y, Date_Shift)
y <- y[, sum(Total), by = list(Date_Shift, Employee_ID)]
names(y)[3] <- "Date_ShiftTotalSales" 
class(y)
y <- as.data.frame(y)
y$Employee_ID <- as.factor(y$Employee_ID)
z <- merge.data.frame(x, y, by = c("Employee_ID", "Date_Shift"))  
# --------------------------------------------------------------------- 


# Betas for regression
# --------------------------------------------------------------------- 
beta_names <- c("Employee_ID", "Unit_ID", "Yr_Wk", "Day_Shift", "InTime")
beta_ct <- length(beta_names)


# Model - By Date_Shift 
# --------------------------------------------------------------------- 
# Y
df <- z
Y = df$Date_ShiftTotalSales

# Betas
a = factor(df$Employee_ID)
b = factor(df$Unit_ID)
c = factor(df$Yr_Wk)
d = factor(df$Day_Shift)
e = factor(df$InTime)
# --------------------------------------------------------------------- 


# Model:
# lm() - without interaction
model <- lm(Y ~ factor(a) + factor(b) + factor(c) + factor(d) + factor(e), data = df) 

# Model:
# lm() - with interaction  <<< in progress
# simple case
model <- lm(Y ~ (a * b, data = df) 
            
model <- lm(Y ~ (factor(a) * factor(b)), data = df) 

model <- lm(Y ~ (a * b * c), data = df) 

model <- lm(Y ~ (factor(a) * factor(b) * factor(c)), data = df) # crashes 

model <- lm(Y ~ (a + b)^2, data = df) 
            
model <- lm(Y ~ (factor(a) + factor(b))^2, data = df) 
            
model <- lm(Y ~ (a + b + c)^2, data = df) 

model <- lm(Y ~ (factor(a) + factor(b) + factor(c))^2, data = df) 

model <- lm(Y ~ (a + b + c + d)^2, data = df) 

model <- lm(Y ~ (a + b + c + d + e)^2, data = df) # crashes

model <- lm(Y ~ (factor(a) + factor(b) + factor(c) + factor(d) + factor(e))^2)

model <- lm(Y ~ (a + b + c + d + e)*(a + b + c + d + e))

model <- lm(Y ~ (factor(a) + factor(b) + factor(c) + factor(d) + factor(e))*(factor(a) + factor(b) + factor(c) + factor(d) + factor(e)))

# ------- Explore
            plot(model)
            termplot(model)
            summary(model)
            
# Residuals
            summary(residuals(model))
            plot(model$residuals)
            model.resid <- resid(model)
# plot(df$Date_ShiftTotalSales, model.resid, + ylab="Residuals", xlab="Date_ShiftTotalSales", + main="CoreSample Sales")
            plot(df$Date_ShiftTotalSales, model.resid) # looks like some "positive trend" heterosedacasticity?

             
            
            
# Predict
            t.p <- as.data.frame(predict(model))
            new <- as.character(df$Date_ShiftTotalSales)
            t.p <- as.data.frame(predict(model), new, se.fit = TRUE)
            # t <- cbind(z, t.p) # Warning: In data.row.names(row.names, rowsi, i) : some row.names duplicated: 
            
            
            
            
            
# Leave One Out - Predict <<< in progress
            # a)
            n = length(unique(df$Employee_ID))
            predicted.v <- rep(NA, n)
            real.v      <- rep(NA, n)
            
            for (i in 1:n){
              masked.id <- sample (1:nrow(xmat), 1)
              ymat1     <- ymat 
              real.v[i] <- ymat[masked.id,]
              ymat1[masked.id,] <- NA
              mydata            <- data.frame(ymat1, xmat)
              fit               <- lm(ymat1 ~ ., data=mydata)
              predicted.v[i]    <- fit$fitted.values[masked.id]
            }
            # b)
            # model <- lm(Y ~ a.Employee_ID + b.Unit_ID + c.Yr_Wk + d.Day_Shift + e.InTime, data = df) # ok?
            install.packages("cvTools")
            library(cvTools)
            
            glm.fit <- glm(Y ~ a.Employee_ID + b.Unit_ID + c.Yr_Wk + d.Day_Shift + e.InTime, data = df, family=gaussian(link="log"))
            summary(glm.fit)
            
            # calculate mean error of prediction (leave-one-out cross-validation)
            cv.res <- cv.glm(sub, glm.fit)
            cv.res$delta
            
            '''an integer giving the number of groups into which the data should be split (the
            default is five). Keep in mind that this should be chosen such that all groups
            are of approximately equal size. Setting K equal to n yields leave-one-out crossvalidation.'''
            
            cvFit(object, data = NULL, x = NULL,
                  y, cost = rmspe, K = 5, R = 1,
                  foldType = c("random", "consecutive", "interleaved"),
                  folds = NULL, names = NULL, predictArgs = list(),
                  costArgs = list(), envir = parent.frame(), seed = NULL,
                  ...)
            
            n = length(unique(df$Employee_ID))
            cv.fit_model <- cvFit(model, data = df, y, K = n) #
            cv.fit_model <- cvFit(model, data = df, y=df$Employee_ID, K = n) #
            
            
            
            cv.fit_model <- cvTool(Y ~ a.Employee_ID + b.Unit_ID + c.Yr_Wk + d.Day_Shift + e.InTime, data = df, K = n) #
            # <<< in progress
            
            
            
            
            
            # Add residuals
            t.r <- as.data.frame(residuals(model))
            CoreSample_lm <- cbind(df, t.r)
            names(CoreSample_lm)[9] <- c("Rev_Residuals")
            
            # Sum over employee
            x <- EmpStats
            y <- select(CoreSample_lm, Employee_ID, Rev_Residuals) 
            y <- data.table(y)
            setkey(y, Employee_ID)
            
            
            
            
            # <<< testing
            typeof(y$Rev_Residuals)
            class(y$Rev_Residuals)
            sum(y$Rev_Residuals[1] + y$Rev_Residuals[2])
            typeof(y$Employee_ID)
            class(y$Employee_ID)
            y$Employee_ID <- as.factor(y$Employee_ID)
            y %>% group_by(Employee_ID) %>% summarize(median=median(Rev_Residuals), sum=sum(Rev_Residuals))
            
            # Sum over employee
            yy <- y[, sum(Rev_Residuals), by = list(Employee_ID)] #
            yy <- y[, median(Rev_Residuals), by = list(Employee_ID)] # 
            y <- y[, median(Rev_Residuals), by = list(Employee_ID)] # 
            # >>> testing
            
            
            # Sum over employee
            y$Employee_ID <- as.factor(y$Employee_ID)
            y <- y[, sum(Rev_Residuals), by = list(Employee_ID)] #
            names(y)[2] <- "Rev_SumResiduals" 
            class(y)
            y <- as.data.frame(y)
            y$Employee_ID <- as.factor(y$Employee_ID)
            z <- merge.data.frame(x, y, by = c("Employee_ID")) 
            EmpStats_CoreSample_lm <- z
            
            
            
################ Model Points ##############
# Get each employees total points for each Date_Shift worked
            x <- unique(select(EmpStats_CoreSample_Checks, Employee_ID, Unit_ID, Yr_Wk, Day_Shift, Date_Shift, InTime)) # 18,442 
            y <- unique(select(EmpStats_CoreSample_Checks, Employee_ID, Date_Shift, NormalizedUserTotalPoints)) # 
            y <- data.table(y)
            setkey(y, Date_Shift)
            y <- y[, sum(NormalizedUserTotalPoints), by = list(Date_Shift, Employee_ID)] # 
            names(y)[3] <- "Date_ShiftTotalPoints" 
            class(y)
            y <- as.data.frame(y)
            y$Employee_ID <- as.factor(y$Employee_ID)
            z <- merge.data.frame(x, y, by = c("Employee_ID", "Date_Shift")) # 
            z[is.na(z)] <- 0
            df <- z
            colnames(df)
            
            # Predict pts
            Y = df$Date_ShiftTotalPoints
            
            # Betas
            a.Employee_ID = factor(df$Employee_ID)
            b.Unit_ID = factor(df$Unit_ID)
            c.Yr_Wk = factor(df$Yr_Wk)
            d.Day_Shift = factor(df$Day_Shift)
            e.InTime = factor(df$InTime)
            
            model <- lm(Y ~ b.Unit_ID + c.Yr_Wk + d.Day_Shift + e.InTime, data = df) # ok?
            
            
            # Predict
            t.p <- as.data.frame(predict(model))
            new <- as.character(df$Date_ShiftTotalPoints)
            class(new)
            typeof(new)
            t.p <- as.data.frame(predict(model), new, se.fit = TRUE)
            
            # # Add predicted
            # t <- cbind(z, t.p) # Warning: In data.row.names(row.names, rowsi, i) : some row.names duplicated: 
            
            # Add residuals
            t.r <- as.data.frame(residuals(model))
            CoreSample_lm <- cbind(CoreSample_lm, t.r)
            names(CoreSample_lm)[10] <- c("Pts_Residuals")
            
            # Sum over employee
            x <- EmpStats_CoreSample_lm
            y <- select(CoreSample_lm, Employee_ID, Pts_Residuals) 
            y <- data.table(y)
            setkey(y, Employee_ID)
            y <- y[, sum(Pts_Residuals), by = list(Employee_ID)] #
            # y <- y[, sum(Residuals)]
            
            names(y)[2] <- "Pts_SumResiduals" 
            class(y)
            y <- as.data.frame(y)
            y$Employee_ID <- as.factor(y$Employee_ID)
            z <- merge.data.frame(x, y, by = c("Employee_ID")) 
            EmpStats_CoreSample_lm <- z
            View(EmpStats_CoreSample_lm)
            
            
            
################ Categories #############
# Overall
            median_rev_residual <- median(EmpStats_CoreSample_lm$Rev_SumResiduals)
            median_pts_residual <- median(EmpStats_CoreSample_lm$Pts_SumResiduals)
            EmpStats_CoreSample_lm <- mutate(EmpStats_CoreSample_lm, Rev_TopEmp = Rev_SumResiduals > median_rev_residual)
            EmpStats_CoreSample_lm <- mutate(EmpStats_CoreSample_lm, Pts_TopEmp = Pts_SumResiduals > median_rev_residual)
            
# All together - rev
            EmpStats_CoreSample_lm$Rev_TopEmp <- ifelse(EmpStats_CoreSample_lm$Unit_ID == 34 & EmpStats_CoreSample_lm$Rev_SumResiduals > median(EmpStats_CoreSample_lm$Rev_SumResiduals[EmpStats_CoreSample_lm$Unit_ID == 34]), "Top 34", ifelse(EmpStats_CoreSample_lm$Unit_ID == 35 & EmpStats_CoreSample_lm$Rev_SumResiduals > median(EmpStats_CoreSample_lm$Rev_SumResiduals[EmpStats_CoreSample_lm$Unit_ID == 35]), "Top 35", ifelse(EmpStats_CoreSample_lm$Unit_ID == 36 & EmpStats_CoreSample_lm$Rev_SumResiduals > median(EmpStats_CoreSample_lm$Rev_SumResiduals[EmpStats_CoreSample_lm$Unit_ID == 36]), "Top 36", 0)))
            
# All together - pts
            EmpStats_CoreSample_lm$Pts_TopEmp <- ifelse(EmpStats_CoreSample_lm$Unit_ID == 34 & EmpStats_CoreSample_lm$Pts_SumResiduals > median(EmpStats_CoreSample_lm$Pts_SumResiduals[EmpStats_CoreSample_lm$Unit_ID == 34]), "Top 34", ifelse(EmpStats_CoreSample_lm$Unit_ID == 35 & EmpStats_CoreSample_lm$Pts_SumResiduals > median(EmpStats_CoreSample_lm$Pts_SumResiduals[EmpStats_CoreSample_lm$Unit_ID == 35]), "Top 35", ifelse(EmpStats_CoreSample_lm$Unit_ID == 36 & EmpStats_CoreSample_lm$Pts_SumResiduals > median(EmpStats_CoreSample_lm$Pts_SumResiduals[EmpStats_CoreSample_lm$Unit_ID == 36]), "Top 36", 0)))
            
            
# Labels
            EmpStats_CoreSample_lm <- mutate(EmpStats_CoreSample_lm, TopPts_TopRev = (Rev_TopEmp > 0 & Pts_TopEmp > 0))
            players <- EmpStats_CoreSample_lm$Employee_ID[EmpStats_CoreSample_lm$TopPts_TopRev == TRUE] 
            
            EmpStats_CoreSample_lm <- mutate(EmpStats_CoreSample_lm, TopPts_BtmRev = (Rev_TopEmp == 0 & Pts_TopEmp > 0))
            gamers <- EmpStats_CoreSample_lm$Employee_ID[EmpStats_CoreSample_lm$TopPts_BtmRev == TRUE] 
            
            EmpStats_CoreSample_lm <- mutate(EmpStats_CoreSample_lm, BtmPts_TopRev = (Rev_TopEmp > 0 & Pts_TopEmp == 0))
            overlooked <- EmpStats_CoreSample_lm$Employee_ID[EmpStats_CoreSample_lm$BtmPts_TopRev == TRUE] 
            
            EmpStats_CoreSample_lm <- mutate(EmpStats_CoreSample_lm, BtmPts_BtmRev = (Rev_TopEmp == 0 & Pts_TopEmp == 0))
            bench <- EmpStats_CoreSample_lm$Employee_ID[EmpStats_CoreSample_lm$BtmPts_BtmRev == TRUE] 
            
            
# Deals
            EmpStats_CoreSample_lm <- mutate(EmpStats_CoreSample_lm, BestDeal = (WithPts_AvgShiftRevenue > PrePts_AvgShiftRevenue & WithPts_Workload < PrePts_Workload))
            Best_Deal <- EmpStats_CoreSample_lm$Employee_ID[EmpStats_CoreSample_lm$BestDeal == TRUE] 
            
            EmpStats_CoreSample_lm <- mutate(EmpStats_CoreSample_lm, WorstDeal = (WithPts_AvgShiftRevenue < PrePts_AvgShiftRevenue & WithPts_Workload > PrePts_Workload))
            Worst_Deal <- EmpStats_CoreSample_lm$Employee_ID[EmpStats_CoreSample_lm$WorstDeal == TRUE] 
            
            EmpStats_CoreSample_lm <- mutate(EmpStats_CoreSample_lm, ShiftAvgDrop = (WithPts_AvgShiftRevenue < PrePts_AvgShiftRevenue))
            ShiftAvgDrop <- EmpStats_CoreSample_lm$Employee_ID[EmpStats_CoreSample_lm$ShiftAvgDrop == TRUE] 
            
            
# Label employee
            EmpStats_CoreSample_lm <- mutate(EmpStats_CoreSample_lm, Label = ifelse(Employee_ID %in% players, "A-Players", ifelse(Employee_ID %in% gamers, "C-Gamers", ifelse(Employee_ID %in% overlooked, "B-Earners", ifelse(Employee_ID %in% bench, "D-Bench", "unlabeled"))))) 
            EmpStats_CoreSample_lm <- mutate(EmpStats_CoreSample_lm, Deal = ifelse(Employee_ID %in% Best_Deal, "Best Deal", ifelse(Employee_ID %in% Worst_Deal, "Worst Deal", "Ok")))
            
# Order
            EmpStats_CoreSample_lm <- EmpStats_CoreSample_lm[with(EmpStats_CoreSample_lm, order(Unit_ID)), ]
            EmpStats_CoreSample_lm <- EmpStats_CoreSample_lm[with(EmpStats_CoreSample_lm, order(Label)), ]
            EmpStats_CoreSample_lm <- EmpStats_CoreSample_lm[with(EmpStats_CoreSample_lm, order(Unit_ID, Label)), ]
            EmpStats_CoreSample_lm <- EmpStats_CoreSample_lm[with(EmpStats_CoreSample_lm, order(Unit_ID, Label, Deal)), ]
            
# Merge with shift info
            EmpStats_CoreSample_Shifts_lm <- merge(EmpStats_Shifts, EmpStats_CoreSample_lm)
            
# Order
            EmpStats_CoreSample_Shifts_lm <- EmpStats_CoreSample_Shifts_lm[with(EmpStats_CoreSample_Shifts_lm, order(Unit_ID)), ]
            EmpStats_CoreSample_Shifts_lm <- EmpStats_CoreSample_Shifts_lm[with(EmpStats_CoreSample_Shifts_lm, order(Label)), ]
            EmpStats_CoreSample_Shifts_lm <- EmpStats_CoreSample_Shifts_lm[with(EmpStats_CoreSample_Shifts_lm, order(Unit_ID, Label)), ]
            EmpStats_CoreSample_Shifts_lm <- EmpStats_CoreSample_Shifts_lm[with(EmpStats_CoreSample_Shifts_lm, order(Unit_ID, Label, Deal)), ]
            
# Format Numbers
            EmpStats_CoreSample_Shifts_lm[33:37] <- round(EmpStats_CoreSample_Shifts_lm[33:37], digits = 2)
            EmpStats_CoreSample_Shifts_lm[38] <- round(EmpStats_CoreSample_Shifts_lm[38], digits = 3)
            EmpStats_CoreSample_Shifts_lm[40] <- round(EmpStats_CoreSample_Shifts_lm[40], digits = 2)
# CAUTION: Makes character:
            EmpStats_CoreSample_Shifts_lm[33:40] <- EmpStats_CoreSample_Shifts_lm[33:40]%>% mutate_each(funs(prettyNum(., big.mark=",")))
            
            
# Save
            View(EmpStats_CoreSample_Shifts_lm)
            save(EmpStats_CoreSample_Shifts_lm, file = make_processed_data_paths("EmpStats_CoreSample_Shifts_lm.RData"))
            write.csv(EmpStats_CoreSample_Shifts_lm, file = paste(data_dir, "/processed/EmpStats_CoreSample_Shifts_lm.csv", sep = ""))
            
            
# Select and order from EmpStats_CoreSample_Shifts_lm
            colnames(EmpStats_CoreSample_Shifts_lm)
            # EmpCoreStats <- select(EmpStats_CoreSample_Shifts_lm, Employee_ID, Unit_ID, FirstName, LastName, FirstShift, PrePts_Tenure, WithPts_Tenure, PrePts_Workload, WithPts_Workload, PreShift_AvgShiftRevenue, WithPts_AvgShiftRevenue, Rev_SumResiduals, Pts_SumResiduals)
            
            # EmpCoreStats <- select(EmpStats_CoreSample_Shifts_lm, Employee_ID, Unit_ID, FirstName, LastName, Workload, TotalShiftCount, Rev_SumResiduals, Pts_SumResiduals, PrePts_AvgShiftRevenue, WithPts_AvgShiftRevenue, ShiftAvgDrop, Label, Deal)
            
            # EmpCoreStats <- select(EmpStats_CoreSample_Shifts_lm, Employee_ID, Unit_ID, FirstName, LastName, Workload, TotalShiftCount, ShiftAvgDrop, Label, Deal)
            
            EmpCoreStats <- select(EmpStats_CoreSample_Shifts_lm, Employee_ID, Unit_ID, FirstName, LastName, Label, Deal)
            EmpCoreStats <- EmpCoreStats[with(EmpCoreStats, order(Label, Deal)), ]
            save(EmpCoreStats, file = make_processed_data_paths("EmpCoreStats.RData"))
            write.csv(EmpCoreStats, file = paste(data_dir, "/processed/EmpCoreStats.csv", sep = ""))
            
            
            
        