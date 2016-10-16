## This time, we just keep the missing values as is (??) 
## Later, we can do a full Multivariate imputation if required...

## Oct 12 2016 - High time - Karthik Srinivasan
## See previous versions

### Re-integration all four cohorts ## 5 minutes HRV data first 
## Why: Template to easily integrate data in the future 
# Four steps: RE-int, Lag, HLM, GAMM

## First try and modify code_dump_int.R, then here

## All source files and folders in one folder
## Clean all codes and make it kind of uniform HERE
## Repeat for 5minMW in other folder

## Verify each dataset  

## Also left_join only ??

library(readxl)
library(reshape)
library(memisc)
library(DataCombine)
library(MASS)
library(psych)
library(dplyr)
library(lattice)
library(gtools)
library(Hmisc)
library(xlsx)
library(lubridate)
library(randomForest)


setwd("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_1_integration\IEQ_c1\\Wearnodes_CO2_Pr_Temp_RH_Sound_minutely")
ieq_part_1 <- list()
temp = list.files(pattern="*.csv")

### Read the files into list of dataframes ##

system.time(
  for (i in 1:length(temp)) 
  {
    all_content = readLines(temp[i])
    skip_second = all_content[-c(1,2)]
    ieq_part_1[[i]] <- read.csv(textConnection(skip_second), header = FALSE)
  }
)


device_ids <- read.xlsx("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_1_integration\\Finished_p_C1.xlsx",sheetName = "Sensor_ID_lookup")

### label column

for (i in 1: nrow(device_ids))
{
  ieq_part_1[[i]]$device_ID <- rep(device_ids[i,2],nrow(ieq_part_1[[i]]))
  ieq_part_1[[i]]$WB2_ID <- rep(device_ids[i,1],nrow(ieq_part_1[[i]]))
}


ieq_data <- do.call("rbind", ieq_part_1)

### Mention the column names carefully:

colnames(ieq_data) <- c("Timestamp","Pressure","Relative_humidity","Temperature","CO2","Sound","device_ID","WB2_ID")
ieq_data$Timestamp <- as.POSIXct(ieq_data$Timestamp,tz="EST")
## Check Timestamp & later manually verify with participant's 'IN' time
ieq_data_sorted <- ieq_data[order(ieq_data$WB2_ID,ieq_data$Timestamp),]

## Now add P_ID to it
ieq_data_sorted$WB2_ID <- as.factor(ieq_data_sorted$WB2_ID)
ieq_data_sorted$Date <- as.Date(ieq_data_sorted$Timestamp)

P_ID_lookup <- read.xlsx("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_1_integration\\Finished_p_C1.xlsx",sheetName = "Sensor_PID_map")

P_ID_data <- data.frame(matrix(NA,nrow=nrow(P_ID_lookup),ncol=0))
P_ID_data$Date <- P_ID_lookup$Date
P_ID_data$WB2_ID <- as.factor(P_ID_lookup$WB2_ID)
P_ID_data$P_ID <- as.factor(P_ID_lookup$ID)
## Assign P_ID based on lookup (Participant on a particular day)
#ieq_pid <- merge(ieq_data_sorted,P_ID_data,by = c("WB2_ID","Date"))
ieq_pid <- inner_join(ieq_data_sorted,P_ID_data,by = c("WB2_ID"="WB2_ID","Date"="Date"))

#### HRV dataset #########################################################
##################

## Read both step and MW and compare:

P_ids <- read.xlsx("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_1_integration\\Finished_p_c1.xlsx",sheetName = "P_ID_HRV",header=T)

append_files <- function(p_ids)
{
  phy_part_1 <- list()
  temp = list.files(pattern="*.xls")
  
  for (i in 1:length(temp)) 
  {
    phy_part_1[[i]] <- read_excel(temp[i], sheet=2,col_names=TRUE)
    colnames(phy_part_1[[i]])[1] <- c("Timestamp")
  }
  
  for (i in 1: length(p_ids))
  {
    phy_part_1[[i]]$P_ID <- rep(p_ids[i],nrow(phy_part_1[[i]]))
  }
  
  phy_all <- do.call("rbind",phy_part_1)
  
  return (phy_all)
}

setwd("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_1_integration\\hrv_c1_5")
hrv_file_list <- list.files(pattern="*.xls")

system.time(
  phy_5min <- append_files(na.omit(P_ids$fivemin))
)

setwd("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_1_integration\\hrv_c1")
hrv_file_list2 <- list.files(pattern="*.xls")

system.time(
  phy_5minmw <- append_files(na.omit(P_ids$fiveminmw))
)

force_time <- function(data_in)
{  
data_in$Timestamp <- force_tz(data_in$Timestamp, tzone = "EST")   # Force it to EST as it defaulted to GMT in read_excel
data_in$P_ID <- as.factor(data_in$P_ID)
return(data_in)
}

phy_5min <- force_time(phy_5min)
phy_5minmw <- force_time(phy_5minmw)

## HRV_padding (important for 5 minute FLAT data)

phy_1 <-as.data.frame(phy_5min)
phy_1$P_ID<-as.factor(phy_1$P_ID)
phy_part <- split(phy_1,phy_1$P_ID)

phy_part_min <- list()

pad_5 <- function(data_in)
{ 
  data_in <- data_in[order(data_in$Timestamp),] 
  for(j in 1:ncol(data_in))
  {
    if(!colnames(data_in)[j] %in% c("Timestamp","flag"))
    {
      for (i in 1:nrow(data_in))
      {
        if (!is.na(data_in$flag[i]))
        {
          for (r in 1:4)
          {
            data_in[i+r,j] <- data_in[i,j]  ##Fills the next 5 minutes
          }
        }
      }
    }
  }
  return(data_in)    
}

to_min <- function(data_in)
{
  data_in$Timestamp <- as.POSIXct(round(data_in$Timestamp , "mins"),tz="EST")
  data_in$flag <- 1
  data_o <- TimeExpand(data_in,TimeVar='Timestamp',by='min')
  data_out <- pad_5(data_o)
  data_out$P_ID<-as.factor(data_out$P_ID)
  return(data_out)
}

for(i in 1:length(phy_part))
{
  phy_part_min[[i]] <- to_min(phy_part[[i]])
}

phy_1mmm <- do.call("rbind",phy_part_min)

phy_min <- phy_1mmm

##### SMARTER THING TO DO IS MERGE HRV_5_MW and HRV_5 as a left join
phy_min_names <- paste(names(phy_min),"_5",sep="")
names(phy_min) <- phy_min_names

phy_5minmw$Timestamp <- as.POSIXct(round(phy_5minmw$Timestamp , "mins"))

phy_both <- full_join(phy_5minmw, phy_min, by=c("Timestamp"="Timestamp_5","P_ID"="P_ID_5" ))
phy_both$P_ID <- as.factor(phy_both$P_ID)

#write.csv(phy_both,"C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\workspace\\Oct_2016\\C1_hrv_allday.csv",na="",row.names=F)

ieq_dat <- ieq_pid[order(ieq_pid$P_ID,ieq_pid$Timestamp),]
ieq_dat$Timestamp <- as.POSIXct(round(ieq_dat$Timestamp , "mins"))  ## Ignore the warning

## HRV-IEQ integration 
## Note: Timestamps for both datasets are good!  
## Repeat all codes: For 5 min MW and flat 

phy_ieq_mw <- left_join(phy_both,ieq_dat,by = c("Timestamp"="Timestamp","P_ID"="P_ID"))
phy_ieq_mw$P_ID <- as.factor(phy_ieq_mw$P_ID)
phy_ieq_mw <- phy_ieq_mw[order(phy_ieq_mw$P_ID,phy_ieq_mw$Timestamp),]

## Time filter

part_time <- read.xlsx("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_1_integration\\Finished_p_C1.xlsx",sheetName = "Sensor_PID_map",stringsAsFactors=T)
part_df <- data.frame(Date = part_time$Date, P_ID = part_time$ID,WB2_ID = part_time$WB2_ID,
                      Time_in = part_time$Time_in,Time_out = part_time$Time_out,
                      Tin1 = part_time$T_in1,Tout1 = part_time$T_out1,Tin2 = part_time$T_in2,Tout2 = part_time$T_out2) 

part_df$P_ID <- as.factor(part_df$P_ID)
part_df$WB2_ID <- as.factor(part_df$WB2_ID)

part_wears <- left_join(phy_ieq_mw,part_df,by=c("P_ID"="P_ID","Date"="Date","WB2_ID"="WB2_ID"))  ## Include WB2_ID in join key too!
part_wears$P_ID <-as.factor(part_wears$P_ID)
part_wears <- part_wears[order(part_wears$P_ID,part_wears$Timestamp),]

part_wears$Time <- as.POSIXct(strptime(format(part_wears$Timestamp,"%H:%M"), "%H:%M"),tz="EST")

ret_time <- function(timestamp_in) { return(as.POSIXct(strptime(format(timestamp_in,"%H:%M"), "%H:%M"),tz="EST"))}

# part_wears$Time_in <- ret_time(part_wears$Time_in)
part_wears <- as.data.frame(part_wears)

for(col in which(names(part_wears)=="Time_in"):which(names(part_wears)=="Tout2"))
{
  part_wears[,col] <- ret_time(part_wears[,col])
}

subset1 <- subset(part_wears,part_wears$Time_in < part_wears$Time & part_wears$Time_out > part_wears$Time)
subset2 <- subset1[-which((subset1$Time > subset1$Tin1) & (subset1$Tout1 > subset1$Time)),]
subset3 <- subset2[-which((subset2$Time > subset2$Tin2) & (subset2$Tout2 > subset2$Time)),] 

subset3$SDNN <- subset3[,c("SDNN [milisec.]")]
subset3$RMSSD <- subset3[,c("RMSSD [milisec.]")]
subset3$nHF <- subset3[,c("HF [%]")]
subset3$nLF <- subset3[,c("LF [%]")]

sapply(subset3, function(y) sum(length(which(is.na(y)))))

subset3$n_LF <- subset3$nLF/(subset3$nHF+subset3$nLF)*100
subset3$n_HF <- subset3$nHF/(subset3$nHF+subset3$nLF)*100

subset3$SDNN_5 <- subset3[,c("SDNN [milisec.]_5")]
subset3$RMSSD_5 <- subset3[,c("RMSSD [milisec.]_5")]
subset3$nHF_5 <- subset3[,c("HF [%]_5")]
subset3$nLF_5 <- subset3[,c("LF [%]_5")]
subset3$n_LF_5 <- subset3$nLF_5/(subset3$nHF_5+subset3$nLF_5)*100
subset3$n_HF_5 <- subset3$nHF_5/(subset3$nHF_5+subset3$nLF_5)*100

## Add survey outputs
#ESM_data <- read.xlsx("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_1_integration\\Intake_survey_C1.xlsx",sheetName = "ESM_survey")
ESM_data <- read_excel("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_1_integration\\Intake_survey_C1.xlsx",sheet = "ESM_survey",col_names=T)
ESM_data$P_ID <- as.factor(ESM_data$P_ID)

## For C1, this is a bit different
#ESM_data$Tint <- strftime(ESM_data$Form_finish_time, format="%H:%M",tz="EST")
#ESM_data$Tp <- paste(ESM_data$Form_start_date,ESM_data$Tint)
#ESM_data$Timestamp <- as.POSIXct(strptime(ESM_data$Tp,"%Y-%m-%d %H:%M"),tz="EST")
ESM_data$Timestamp <- as.POSIXct(strptime(ESM_data$Form_finish_date,"%Y-%m-%d %H:%M"),tz="EST")

ESM_data_ordered <- ESM_data[order(ESM_data$P_ID,ESM_data$Timestamp),]
ESM_data_ord <- ESM_data_ordered[!ESM_data_ordered$Form == "Missing",]
d_esm_lp <- left_join(subset3,ESM_data_ord,by = c("P_ID"="P_ID","Timestamp"="Timestamp")) 
d_esm_lp$P_ID <- as.factor(d_esm_lp$P_ID)

d_esm_lp2 <- d_esm_lp[!is.na(d_esm_lp$RMSSD),]
sapply(d_esm_lp2, function(y) sum(length(which(is.na(y)))))
#write.csv(d_esm_lp2,"for_padding.csv")

### Pad 15 mins before and after!!
data_padded <- d_esm_lp2
data_padded$flag <- NA
summary(data_padded$alert)
system.time(
  for (i in 1:(nrow(d_esm_lp2)-15)) ## We use 15 minutes ahead data to impute
  {
    print(i)
    if (!is.na(d_esm_lp2$alert[i]))
    {
      for (t in (ncol(data_padded) - ncol(ESM_data) +1):(ncol(data_padded)-1))   ### These are the columns with psy variables that 'need' padding
      { 
        for (r in 1:15)
        {
          if (i > r)       ### Logic begins only after 30 minutes
          {
            if(data_padded$P_ID[i-r] != data_padded$P_ID[i] |  data_padded$Date[i-r] != data_padded$Date[i]  )
            { 
              #data_padded$flag[i-r] <- "Not imputed" 
              next 
            } 
            else
            { 
              if (is.na(d_esm_lp2$alert[i-r]))
              {
                data_padded[i-r,t] <- d_esm_lp2[i,t] 
                # data_padded$flag[i-r] <- "OK"   ## Pads the previous 15 minutes
              }
            }
            if(data_padded$P_ID[i+r] != data_padded$P_ID[i] |  data_padded$Date[i+r] != data_padded$Date[i]  )
            { 
              #  data_padded$flag[i+r] <- "Not imputed"
              next
            } 
            else
            { 
              if (is.na(d_esm_lp2$alert[i+r]))
              {
                data_padded[i+r,t] <- d_esm_lp2[i,t]
                data_padded$flag[i+r] <- "OK"   ## Pads the next 15 minutes
              }
            }  
          }
        }
      }
    }
  }
)

## Notes: For some reason, the actual value is not copied (+15 min and -15 values are present, but ESM at 0 min disappears). TO fix this later!

data_c1 <- data_padded
data_c1$Time <- ret_time(data_c1$Timestamp)
data_c1$ToD <- cut(data_c1$Time,breaks=c(ret_time(as.POSIXct("00:01",format="%H:%M")),ret_time(as.POSIXct("8:00",format="%H:%M")),
                                        ret_time(as.POSIXct("12:00",format="%H:%M")),
                                        ret_time(as.POSIXct("15:00",format="%H:%M")),
                                        ret_time(as.POSIXct("18:00",format="%H:%M")),
                                        ret_time(as.POSIXct("20:00",format="%H:%M")),
                                        ret_time(as.POSIXct("23:59",format="%H:%M"))))
levels(data_c1$ToD ) <- c("Early_Morning","Morning","Afternoon","Evening","Late_Evening","Night")

data_c1$DoW <- weekdays(data_c1$Timestamp)

## Add demographic information 
Part_demo <- read.xlsx("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_1_integration\\Intake_survey_C1.xlsx",sheetName = "P_demo_clean")
Part_demo$P_ID <- as.factor(Part_demo$P_ID)
both_demo <- left_join(data_c1,Part_demo,by = c("P_ID"="P_ID"))
both_demo$P_ID <- as.factor(both_demo$P_ID)

## For demographic, make some variables as binary (health conditions)
b_data <- both_demo
for (col in which(names(b_data)=="HighBP"):which(names(b_data)=="Sleep.problems") )
{
  b_data[,col] <- as.numeric(as.character(b_data[,col]))
  for(row in 1:nrow(b_data))
  {  b_data[row,col] <- ifelse(b_data[row,col]==1,"Yes","No")
  }
  b_data[,col] <- as.factor(b_data[,col])
}

for (col in which(names(b_data)=="HighBP"):which(names(b_data)=="Sleep.problems") )
{
  b_data[,col] <- as.character(b_data[,col])
  b_data[is.na(b_data[,col]),col] <- "No"
  b_data[,col] <- as.factor(b_data[,col])
}

levels(b_data$Average_alcohol_intake)
b_data$Average_alcohol_intake[b_data$Average_alcohol_intake=="maybe 1"] <- "1"
b_data$Average_alcohol_intake <- as.numeric(as.character(b_data$Average_alcohol_intake))
b_data$Average_alcohol_intake[b_data$Average_alcohol_intake > 40] <- NA

sapply(b_data, function(y) sum(length(which(is.na(y)))))

## Expensive multivariate imputation only for IEQ wearnodes 
#d_data <- b_data[,c(which(names(b_data)=="CO2"):which(names(b_data)=="Absolute_humidity"),which(names(b_data)=="RMSSD"))]
# d_data <- b_data[,c("CO2","Sound","Temperature","Relative_humidity","Pressure","RMSSD")]
# 
# ## Cheap solution 
# #e_data <- list()
# #e_data[[1]] <- d_data[1:20000,]
# #e_data[[2]] <- d_data[1:nrow(d_data),]
# #e_data[[2]] <- d_data[20001:40000,]
# #e_data[[3]] <- d_data[40001:nrow(d_data),]
# #e_data[[4]] <- d_data[60001:nrow(d_data),]
# 
 
# for(list in 1:length(e_data))
# {
#   print(memory.size())
#   e_data[[list]] <- rfImpute(e_data[[list]],e_data[[list]][,c("RMSSD")],ntree=50,iter=3)
#   print(memory.size())
#   gc()
# }
# g_data <- do.call("rbind", e_data)

impute_all <- function(data_in)
{
  for(j in 1:ncol(data_in))
  {
    if(is.factor(data_in[,j]))
    {
      for(k in 1:nrow(data_in))
      {
        if(is.na(data_in[k,j]) | data_in[k,j] == "")
        {
          data_in[k,j] <- data_in[which.max(data_in[,j]),j]
        }
      }
      data_in[,j] <- as.character(data_in[,j])  
      data_in[,j] <- factor(data_in[,j])  
    }
    else
    {
      data_in[,j]  <- as.numeric(round(impute(data_in[,j],mean),6))
    }  
    
  }
  return(data_in)
}

# Have to do this again, once I get Big 5 data
#f_data <- b_data[,c(which(names(b_data)=="Age"):which(names(b_data)=="Openness"))]
# f_data <- b_data[,c(which(names(b_data)=="Age"):which(names(b_data)=="BMI"))]

## Here, for C1, we impute using impute_all only, even for IEQ due to too many missing values

#b_data$DoW <- weekdays(b_data$Timestamp)

#f_data <- b_data[,c(which(names(b_data)=="Gender"):which(names(b_data)=="Openness"),which(names(b_data)=="Pressure"):which(names(b_data)=="Sound"))]

#system.time( f_data <- impute_all(f_data) )

## Again get all the data in one place:

#h_data <- b_data[,-c(which(names(b_data)=="Gender"):which(names(b_data)=="Openness"),which(names(b_data)=="Pressure"):which(names(b_data)=="Sound"))]

#all_data_back <- cbind(h_data,f_data)
all_data_back <- b_data

sapply(all_data_back, function(y) sum(length(which(is.na(y)))))

write.csv(all_data_back,"C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\workspace\\Oct_2016\\Imp_data_C1_Oct12_unimputed.csv",row.names=F,na="")
#save.image("C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\workspace\\Oct_2016\\workspaces\\Cohort1_Oct10.RData")

########
#######
###### Cohort II
#######
########
rm(list=ls())
gc()

setwd("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_2_integration\\Wearnodes_C2")
ieq_part_1 <- list()
temp = list.files(pattern="*.csv")

### Read the files into list of dataframes ##

system.time(
  for (i in 1:length(temp)) 
  {
    all_content = readLines(temp[i])
    skip_second = all_content[-c(1,2)]
    ieq_part_1[[i]] <- read.csv(textConnection(skip_second), header = FALSE)
  }
)


device_ids <- read.xlsx("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_2_integration\\Finished_p_C2.xlsx",sheetName = "Sensor_ID_lookup")

### label column

for (i in 1: nrow(device_ids))
{
  ieq_part_1[[i]]$device_ID <- rep(device_ids[i,2],nrow(ieq_part_1[[i]]))
  ieq_part_1[[i]]$WB2_ID <- rep(device_ids[i,1],nrow(ieq_part_1[[i]]))
}


ieq_data <- do.call("rbind", ieq_part_1)

### Mention the column names carefully:

colnames(ieq_data) <- c("Timestamp","CO2","Temperature","Pressure","Relative_humidity","Sound","Light","device_ID","WB2_ID")
ieq_data$Timestamp <- as.POSIXct(ieq_data$Timestamp,tz="EST")
ieq_data_sorted <- ieq_data[order(ieq_data$WB2_ID,ieq_data$Timestamp),]

## Now add P_ID to it
ieq_data_sorted$WB2_ID <- as.factor(ieq_data_sorted$WB2_ID)
ieq_data_sorted$Date <- as.Date(ieq_data_sorted$Timestamp)

P_ID_lookup <- read.xlsx("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_2_integration\\Finished_p_C2.xlsx",sheetName = "Sensor_PID_map")

P_ID_data <- data.frame(matrix(NA,nrow=nrow(P_ID_lookup),ncol=0))
P_ID_data$Date <- P_ID_lookup$Date
P_ID_data$WB2_ID <- as.factor(P_ID_lookup$WB2_ID)
P_ID_data$P_ID <- as.factor(P_ID_lookup$ID)
ieq_pid <- inner_join(ieq_data_sorted,P_ID_data,by = c("WB2_ID"="WB2_ID","Date"="Date"))
ieq_pid$P_ID <- as.factor(ieq_pid$P_ID)
#### HRV dataset #########################################################
##################

## Read both step and MW and compare:

P_ids <- read.xlsx("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_2_integration\\Finished_p_c2.xlsx",sheetName = "P_ID_HRV",header=T)

append_files <- function(p_ids)
{
  phy_part_1 <- list()
  temp = list.files(pattern="*.xls")
  
  for (i in 1:length(temp)) 
  {
    phy_part_1[[i]] <- read_excel(temp[i], sheet=2,col_names=TRUE)
    colnames(phy_part_1[[i]])[1] <- c("Timestamp")
  }
  
  for (i in 1: length(p_ids))
  {
    phy_part_1[[i]]$P_ID <- rep(p_ids[i],nrow(phy_part_1[[i]]))
  }
  
  phy_all <- do.call("rbind",phy_part_1)
  
  return (phy_all)
}

setwd("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_2_integration\\hrv_c2_5")
hrv_file_list <- list.files(pattern="*.xls")

system.time(
  phy_5min <- append_files(na.omit(P_ids$fivemin))
)

setwd("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_2_integration\\hrv_c2")
hrv_file_list2 <- list.files(pattern="*.xls")

system.time(
  phy_5minmw <- append_files(na.omit(P_ids$fiveminmw))
)

force_time <- function(data_in)
{  
  data_in$Timestamp <- force_tz(data_in$Timestamp, tzone = "EST")   # Force it to EST as it defaulted to GMT in read_excel
  data_in$P_ID <- as.factor(data_in$P_ID)
  return(data_in)
}

phy_5min <- force_time(phy_5min)
phy_5minmw <- force_time(phy_5minmw)

## HRV_padding (important for 5 minute FLAT data)

phy_1 <-as.data.frame(phy_5min)
phy_1$P_ID<-as.factor(phy_1$P_ID)
phy_part <- split(phy_1,phy_1$P_ID)

phy_part_min <- list()

pad_5 <- function(data_in)
{ 
  data_in <- data_in[order(data_in$Timestamp),] 
  for(j in 1:ncol(data_in))
  {
    if(!colnames(data_in)[j] %in% c("Timestamp","flag"))
    {
      for (i in 1:nrow(data_in))
      {
        if (!is.na(data_in$flag[i]))
        {
          for (r in 1:4)
          {
            data_in[i+r,j] <- data_in[i,j]  ##Fills the next 5 minutes
          }
        }
      }
    }
  }
  return(data_in)    
}

to_min <- function(data_in)
{
  data_in$Timestamp <- as.POSIXct(round(data_in$Timestamp , "mins"),tz="EST")
  data_in$flag <- 1
  data_o <- TimeExpand(data_in,TimeVar='Timestamp',by='min')
  data_out <- pad_5(data_o)
  data_out$P_ID<-as.factor(data_out$P_ID)
  return(data_out)
}

for(i in 1:length(phy_part))
{
  phy_part_min[[i]] <- to_min(phy_part[[i]])
}  ## Ignore warnings (one for each participant)

phy_1mmm <- do.call("rbind",phy_part_min)
phy_min <- phy_1mmm

##### SMARTER THING TO DO IS MERGE HRV_5_MW and HRV_5 as a left join
phy_min_names <- paste(names(phy_min),"_5",sep="")
names(phy_min) <- phy_min_names

phy_5minmw$Timestamp <- as.POSIXct(round(phy_5minmw$Timestamp , "mins"))

phy_both <- full_join(phy_5minmw, phy_min, by=c("Timestamp"="Timestamp_5","P_ID"="P_ID_5" ))
phy_both$P_ID <- as.factor(phy_both$P_ID)

#write.csv(phy_both,"C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\workspace\\Oct_2016\\C2_hrv_allday.csv",na="",row.names=F)

ieq_dat <- ieq_pid[order(ieq_pid$P_ID,ieq_pid$Timestamp),]
ieq_dat$Timestamp <- as.POSIXct(round(ieq_dat$Timestamp , "mins"))  ## Ignore the warning

phy_ieq_mw <- left_join(phy_both,ieq_dat,by = c("Timestamp"="Timestamp","P_ID"="P_ID"))
phy_ieq_mw$P_ID <- as.factor(phy_ieq_mw$P_ID)
phy_ieq_mw <- phy_ieq_mw[order(phy_ieq_mw$P_ID,phy_ieq_mw$Timestamp),]

## Time filter
part_time <- read.xlsx("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_2_integration\\Finished_p_C2.xlsx",sheetName = "Sensor_PID_map")
part_df <- data.frame(Date = part_time$Date, P_ID = part_time$ID,WB2_ID = part_time$WB2_ID,
                      Time_in = part_time$Time_in,Time_out = part_time$Time_out,
                      Tin1 = part_time$T_in1,Tout1 = part_time$T_out1,Tin2 = part_time$T_in2,Tout2 = part_time$T_out2) 

part_df$P_ID <- as.factor(part_df$P_ID)
part_df$WB2_ID <- as.factor(part_df$WB2_ID)

part_wears <- left_join(phy_ieq_mw,part_df,by=c("P_ID"="P_ID","Date"="Date","WB2_ID"="WB2_ID"))  ## Include WB2_ID in join key too!
part_wears$P_ID <-as.factor(part_wears$P_ID)
part_wears <- part_wears[order(part_wears$P_ID,part_wears$Timestamp),]

part_wears$Time <- as.POSIXct(strptime(format(part_wears$Timestamp,"%H:%M"), "%H:%M"))

ret_time <- function(timestamp_in) { return(as.POSIXct(strptime(format(timestamp_in,"%H:%M"), "%H:%M")))}

# part_wears$Time_in <- ret_time(part_wears$Time_in)
part_wears <- as.data.frame(part_wears)

for(col in which(names(part_wears)=="Time_in"):which(names(part_wears)=="Tout2"))
{
  part_wears[,col] <- ret_time(part_wears[,col])
}

subset1 <- subset(part_wears,part_wears$Time_in < part_wears$Time & part_wears$Time_out > part_wears$Time)
subset2 <- subset1[-which((subset1$Time > subset1$Tin1) & (subset1$Tout1 > subset1$Time)),]
subset3 <- subset2[-which((subset2$Time > subset2$Tin2) & (subset2$Tout2 > subset2$Time)),] 

subset3$SDNN <- subset3[,c("SDNN [milisec.]")]
subset3$RMSSD <- subset3[,c("RMSSD [milisec.]")]
subset3$nHF <- subset3[,c("HF [%]")]
subset3$nLF <- subset3[,c("LF [%]")]

sapply(subset3, function(y) sum(length(which(is.na(y)))))

subset3$n_LF <- subset3$nLF/(subset3$nHF+subset3$nLF)*100
subset3$n_HF <- subset3$nHF/(subset3$nHF+subset3$nLF)*100

subset3$SDNN_5 <- subset3[,c("SDNN [milisec.]_5")]
subset3$RMSSD_5 <- subset3[,c("RMSSD [milisec.]_5")]
subset3$nHF_5 <- subset3[,c("HF [%]_5")]
subset3$nLF_5 <- subset3[,c("LF [%]_5")]
subset3$n_LF_5 <- subset3$nLF_5/(subset3$nHF_5+subset3$nLF_5)*100
subset3$n_HF_5 <- subset3$nHF_5/(subset3$nHF_5+subset3$nLF_5)*100

## Add survey outputs
ESM_data <- read_excel("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_2_integration\\Intake_survey_C2.xlsx",sheet = "ESM_survey",col_names=T)
ESM_data$P_ID <- as.factor(ESM_data$P_ID)
ESM_data$fTime <- force_tz(ESM_data$Form_finish_time, tzone = "EST")
ESM_data$Tint <- strftime(ESM_data$fTime, format="%H:%M",tz="EST")
ESM_data$Tp <- paste(ESM_data$Form_start_date,ESM_data$Tint)
ESM_data$Timestamp <- as.POSIXct(force_tz(strptime(ESM_data$Tp,"%Y-%m-%d %H:%M"),tzone="EST"))
ESM_data_ordered <- ESM_data[order(ESM_data$P_ID,ESM_data$Timestamp),]
ESM_data_ord <- ESM_data_ordered[!ESM_data_ordered$Form == "Missing",]
d_esm_lp <- merge(subset3,ESM_data_ord,by = c("P_ID","Timestamp"),all.x=T)
d_esm_lp <- left_join(subset3,ESM_data_ord,by = c("P_ID"="P_ID","Timestamp"="Timestamp")) 
d_esm_lp$P_ID <- as.factor(d_esm_lp$P_ID)

d_esm_lp2 <- d_esm_lp[!is.na(d_esm_lp$RMSSD),]
sapply(d_esm_lp2, function(y) sum(length(which(is.na(y)))))
#write.csv(d_esm_lp2,"for_padding.csv")

### Pad 15 mins before and after!!
data_padded <- d_esm_lp2
data_padded$flag <- NA
summary(data_padded$alert)
system.time(
  for (i in 1:(nrow(d_esm_lp2)-15)) ## We use 15 minutes ahead data to impute
  {
    print(i)
    if (!is.na(d_esm_lp2$alert[i]))
    {
      for (t in (ncol(data_padded) - ncol(ESM_data) +3):(ncol(data_padded)-4))   ### These are the columns with psy variables that 'need' padding
      { 
        for (r in 1:15)
        {
          if (i > r)       ### Logic begins only after 30 minutes
          {
            if(data_padded$P_ID[i-r] != data_padded$P_ID[i] |  data_padded$Date[i-r] != data_padded$Date[i]  )
            { 
              #data_padded$flag[i-r] <- "Not imputed" 
              next 
            } 
            else
            { 
              if (is.na(d_esm_lp2$alert[i-r]))
              {
                data_padded[i-r,t] <- d_esm_lp2[i,t] 
                # data_padded$flag[i-r] <- "OK"   ## Pads the previous 15 minutes
              }
            }
            if(data_padded$P_ID[i+r] != data_padded$P_ID[i] |  data_padded$Date[i+r] != data_padded$Date[i]  )
            { 
              #  data_padded$flag[i+r] <- "Not imputed"
              next
            } 
            else
            { 
              if (is.na(d_esm_lp2$alert[i+r]))
              {
                data_padded[i+r,t] <- d_esm_lp2[i,t]
                data_padded$flag[i+r] <- "OK"   ## Pads the next 15 minutes
              }
            }  
          }
        }
      }
    }
  }
)

## Notes: For some reason, the actual value is not copied (+15 min and -15 values are present, but ESM at 0 min disappears). TO fix this later!

data_c2 <- data_padded
data_c2$Time <- ret_time(data_c2$Timestamp)
data_c2$ToD <- cut(data_c2$Time,breaks=c(ret_time(as.POSIXct("00:01",format="%H:%M")),ret_time(as.POSIXct("8:00",format="%H:%M")),
                                         ret_time(as.POSIXct("12:00",format="%H:%M")),
                                         ret_time(as.POSIXct("15:00",format="%H:%M")),
                                         ret_time(as.POSIXct("18:00",format="%H:%M")),
                                         ret_time(as.POSIXct("20:00",format="%H:%M")),
                                         ret_time(as.POSIXct("23:59",format="%H:%M"))))
levels(data_c2$ToD ) <- c("Early_Morning","Morning","Afternoon","Evening","Late_Evening","Night")

data_c2$DoW <- weekdays(data_c2$Timestamp)

## Add demographic information 
Part_demo <- read.xlsx("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_2_integration\\Intake_survey_C2.xlsx",sheetName = "P_demo_clean")
Part_demo$P_ID <- as.factor(Part_demo$P_ID)
both_demo <- left_join(data_c2,Part_demo,by = c("P_ID"="P_ID"))
both_demo$P_ID <- as.factor(both_demo$P_ID)

## For demographic, make some variables as binary (health conditions)
b_data <- both_demo
for (col in which(names(b_data)=="HighBP"):which(names(b_data)=="Sleep.problems") )
{
  b_data[,col] <- as.numeric(as.character(b_data[,col]))
  for(row in 1:nrow(b_data))
  {  b_data[row,col] <- ifelse(b_data[row,col]==1,"Yes","No")
  }
  b_data[,col] <- as.factor(b_data[,col])
}

for (col in which(names(b_data)=="HighBP"):which(names(b_data)=="Sleep.problems") )
{
  b_data[,col] <- as.character(b_data[,col])
  b_data[is.na(b_data[,col]),col] <- "No"
  b_data[,col] <- as.factor(b_data[,col])
}

levels(b_data$Average_alcohol_intake)
b_data$Average_alcohol_intake[b_data$Average_alcohol_intake=="maybe 1"] <- "1"
b_data$Average_alcohol_intake <- as.numeric(as.character(b_data$Average_alcohol_intake))
b_data$Average_alcohol_intake[b_data$Average_alcohol_intake > 40] <- NA

sapply(b_data, function(y) sum(length(which(is.na(y)))))

# Expensive multivariate imputation only for IEQ wearnodes
#d_data <- b_data[,c(which(names(b_data)=="CO2"):which(names(b_data)=="Absolute_humidity"),which(names(b_data)=="RMSSD"))]
#d_data <- b_data[,c("CO2","Sound","Temperature","Relative_humidity","Pressure","RMSSD","Light")]
## Expensive multivariate imputation only for IEQ wearnodes 
# ## Cheap solution 
# #e_data <- list()
# #e_data[[1]] <- d_data[1:20000,]
# #e_data[[2]] <- d_data[1:nrow(d_data),]
# #e_data[[2]] <- d_data[20001:40000,]
# #e_data[[3]] <- d_data[40001:nrow(d_data),]
# #e_data[[4]] <- d_data[60001:nrow(d_data),]
# 

# for(list in 1:length(e_data))
# {
#   print(memory.size())
#   e_data[[list]] <- rfImpute(e_data[[list]],e_data[[list]][,c("RMSSD")],ntree=50,iter=3)
#   print(memory.size())
#   gc()
# }
# g_data <- do.call("rbind", e_data)


## Here too, we use mean imputation alone!

impute_all <- function(data_in)
{
  for(j in 1:ncol(data_in))
  {
    if(is.factor(data_in[,j]))
    {
      for(k in 1:nrow(data_in))
      {
        if(is.na(data_in[k,j]) | data_in[k,j] == "")
        {
          data_in[k,j] <- data_in[which.max(data_in[,j]),j]
        }
      }
      data_in[,j] <- as.character(data_in[,j])  
      data_in[,j] <- factor(data_in[,j])  
    }
    else
    {
      data_in[,j]  <- as.numeric(round(impute(data_in[,j],mean),6))
    }  
    
  }
  return(data_in)
}

f_data <- b_data[,c(which(names(b_data)=="Gender"):which(names(b_data)=="Openness"),which(names(b_data)=="CO2"):which(names(b_data)=="Light"))]
system.time( f_data <- impute_all(f_data) )
h_data <- b_data[,-c(which(names(b_data)=="Gender"):which(names(b_data)=="Openness"),which(names(b_data)=="CO2"):which(names(b_data)=="Light"))]

# all_data_back <- cbind(h_data,f_data)
all_data_back <- b_data
sapply(all_data_back, function(y) sum(length(which(is.na(y)))))

write.csv(all_data_back,"C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\workspace\\Oct_2016\\Imp_data_C2_Oct12_unimputed.csv",row.names=F,na="")
#save.image("C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\workspace\\Oct_2016\\workspaces\\Cohort2_Oct10.RData")






########
#######
###### Cohort III
#######
########
gc()
rm(list=ls())

setwd("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_3_integration\\IEQ_c3")
##### NOTE: THIS IS IN GMT +0:00 time
ieq_part_1 <- list()
temp <- list.files(pattern="*.csv")

### Read the files into list of dataframes ##

system.time(
  for (i in 1:length(temp)) 
  {
    all_content = readLines(temp[i])
    skip_second = all_content[-c(1,2)]
    ieq_part_1[[i]] <- read.csv(textConnection(skip_second), header = FALSE)
  }
)


device_ids <- read.xlsx("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_3_integration\\Finished_p_C3.xlsx",sheetName = "Sensor_ID_lookup")

### label column

for (i in 1: nrow(device_ids))
{
  ieq_part_1[[i]]$device_ID <- rep(device_ids[i,2],nrow(ieq_part_1[[i]]))
  ieq_part_1[[i]]$WB2_ID <- rep(device_ids[i,1],nrow(ieq_part_1[[i]]))
}


ieq_data <- do.call("rbind", ieq_part_1)

### Mention the column names carefully:
colnames(ieq_data) <- c("Timestamp","CO2","Temperature","Pressure","Relative_humidity","Sound","Light","Absolute_humidity","device_ID","WB2_ID")
ieq_data$Timestamp2 <- as.POSIXct(ieq_data$Timestamp,tz="GMT")  ## Have to convert it to "CST6CDT"
ieq_data_sorted <- ieq_data[order(ieq_data$WB2_ID,ieq_data$Timestamp2),]

## Now add P_ID to it
ieq_data_sorted$WB2_ID <- as.factor(ieq_data_sorted$WB2_ID)

#### IMPORTANT HERE!! ## RUN ONLY ONCE
ieq_data_sorted$Timestamp <- as.POSIXct(format(as.POSIXct(ieq_data_sorted$Timestamp2,tz="GMT"),tz="CST6CDT"),tz="CST6CDT")

ieq_data_sorted$Date <- as.Date(ieq_data_sorted$Timestamp)
P_ID_lookup <- read.xlsx("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_3_integration\\Finished_p_C3.xlsx",sheetName = "Sensor_PID_map")

P_ID_data <- data.frame(matrix(NA,nrow=nrow(P_ID_lookup),ncol=0))
P_ID_data$Date <- P_ID_lookup$Date
P_ID_data$WB2_ID <- as.factor(P_ID_lookup$WB2_ID)
P_ID_data$P_ID <- as.factor(P_ID_lookup$ID)
ieq_pid <- inner_join(ieq_data_sorted,P_ID_data,by = c("WB2_ID"="WB2_ID","Date"="Date"))
ieq_pid$P_ID <- as.factor(ieq_pid$P_ID)
#### HRV dataset #########################################################
##################

## Read both step and MW and compare:

P_ids <- read.xlsx("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_3_integration\\Finished_p_c3.xlsx",sheetName = "P_ID_HRV",header=T)

append_files <- function(p_ids)
{
  phy_part_1 <- list()
  temp = list.files(pattern="*.xls")
  
  for (i in 1:length(temp)) 
  {
    phy_part_1[[i]] <- read_excel(temp[i], sheet=2,col_names=TRUE)
    colnames(phy_part_1[[i]])[1] <- c("Timestamp")
  }
  
  for (i in 1: length(p_ids))
  {
    phy_part_1[[i]]$P_ID <- rep(p_ids[i],nrow(phy_part_1[[i]]))
  }
  
  phy_all <- do.call("rbind",phy_part_1)
  
  return (phy_all)
}

setwd("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_3_integration\\hrv_c3_5")
hrv_file_list <- list.files(pattern="*.xls")

system.time(
  phy_5min <- append_files(na.omit(P_ids$fivemin))
)

setwd("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_3_integration\\hrv_c3")
hrv_file_list2 <- list.files(pattern="*.xls")

system.time(
  phy_5minmw <- append_files(na.omit(P_ids$fiveminmw))
)

force_time <- function(data_in)
{  
  data_in$Timestamp <- force_tz(data_in$Timestamp, tzone = "CST6CDT")   # Force it to CST6CDT as it defaulted to GMT in read_excel
  data_in$P_ID <- as.factor(data_in$P_ID)
  return(data_in)
}

phy_5min <- force_time(phy_5min)
phy_5minmw <- force_time(phy_5minmw)

## HRV_padding (important for 5 minute FLAT data)

phy_1 <-as.data.frame(phy_5min)
phy_1$P_ID<-as.factor(phy_1$P_ID)
phy_part <- split(phy_1,phy_1$P_ID)

phy_part_min <- list()

pad_5 <- function(data_in)
{ 
  data_in <- data_in[order(data_in$Timestamp),] 
  for(j in 1:ncol(data_in))
  {
    if(!colnames(data_in)[j] %in% c("Timestamp","flag"))
    {
      for (i in 1:nrow(data_in))
      {
        if (!is.na(data_in$flag[i]))
        {
          for (r in 1:4)
          {
            data_in[i+r,j] <- data_in[i,j]  ##Fills the next 5 minutes
          }
        }
      }
    }
  }
  return(data_in)    
}

to_min <- function(data_in)
{
  data_in$Timestamp <- as.POSIXct(round(data_in$Timestamp , "mins"),tz="CST6CDT")
  data_in$flag <- 1
  data_o <- TimeExpand(data_in,TimeVar='Timestamp',by='min')
  data_out <- pad_5(data_o)
  data_out$P_ID<-as.factor(data_out$P_ID)
  return(data_out)
}

for(i in 1:length(phy_part))
{
  phy_part_min[[i]] <- to_min(phy_part[[i]])
}  ## Ignore warnings (one for each participant)

phy_1mmm <- do.call("rbind",phy_part_min)
phy_min <- phy_1mmm

##### SMARTER THING TO DO IS MERGE HRV_5_MW and HRV_5 as a left join
phy_min_names <- paste(names(phy_min),"_5",sep="")
names(phy_min) <- phy_min_names

phy_5minmw$Timestamp <- as.POSIXct(round(phy_5minmw$Timestamp , "mins"))

phy_both <- full_join(phy_5minmw, phy_min, by=c("Timestamp"="Timestamp_5","P_ID"="P_ID_5" ))
phy_both$P_ID <- as.factor(phy_both$P_ID)

#write.csv(phy_both,"C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\workspace\\Oct_2016\\C3_hrv_allday.csv",na="",row.names=F)

ieq_dat <- ieq_pid[order(ieq_pid$P_ID,ieq_pid$Timestamp),]
ieq_dat$Timestamp <- as.POSIXct(round(ieq_dat$Timestamp , "mins"))  ## Ignore the warning

phy_ieq_mw <- left_join(phy_both,ieq_dat,by = c("Timestamp"="Timestamp","P_ID"="P_ID"))
phy_ieq_mw$P_ID <- as.factor(phy_ieq_mw$P_ID)
phy_ieq_mw <- phy_ieq_mw[order(phy_ieq_mw$P_ID,phy_ieq_mw$Timestamp),]

## Time filter
part_time <- read.xlsx("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_3_integration\\Finished_p_C3.xlsx",sheetName = "Sensor_PID_map")
part_df <- data.frame(Date = part_time$Date, P_ID = part_time$ID,WB2_ID = part_time$WB2_ID,
                      Time_in = part_time$Time_in,Time_out = part_time$Time_out,
                      Tin1 = part_time$T_in1,Tout1 = part_time$T_out1,Tin2 = part_time$T_in2,Tout2 = part_time$T_out2) 

part_df$P_ID <- as.factor(part_df$P_ID)
part_df$WB2_ID <- as.factor(part_df$WB2_ID)

part_wears <- left_join(phy_ieq_mw,part_df,by=c("P_ID"="P_ID","Date"="Date","WB2_ID"="WB2_ID"))  ## Include WB2_ID in join key too!
part_wears$P_ID <-as.factor(part_wears$P_ID)
part_wears <- part_wears[order(part_wears$P_ID,part_wears$Timestamp),]

part_wears$Time <- as.POSIXct(strptime(format(part_wears$Timestamp,"%H:%M"), "%H:%M"))

ret_time <- function(timestamp_in) { return(as.POSIXct(strptime(format(timestamp_in,"%H:%M"), "%H:%M")))}

# part_wears$Time_in <- ret_time(part_wears$Time_in)
part_wears <- as.data.frame(part_wears)

for(col in which(names(part_wears)=="Time_in"):which(names(part_wears)=="Tout2"))
{
  part_wears[,col] <- ret_time(part_wears[,col])
}

subset1 <- subset(part_wears,part_wears$Time_in < part_wears$Time & part_wears$Time_out > part_wears$Time)
subset2 <- subset1[-which((subset1$Time > subset1$Tin1) & (subset1$Tout1 > subset1$Time)),]
subset3 <- subset2[-which((subset2$Time > subset2$Tin2) & (subset2$Tout2 > subset2$Time)),] 

subset3$SDNN <- subset3[,c("SDNN [milisec.]")]
subset3$RMSSD <- subset3[,c("RMSSD [milisec.]")]
subset3$nHF <- subset3[,c("HF [%]")]
subset3$nLF <- subset3[,c("LF [%]")]

sapply(subset3, function(y) sum(length(which(is.na(y)))))

subset3$n_LF <- subset3$nLF/(subset3$nHF+subset3$nLF)*100
subset3$n_HF <- subset3$nHF/(subset3$nHF+subset3$nLF)*100

subset3$SDNN_5 <- subset3[,c("SDNN [milisec.]_5")]
subset3$RMSSD_5 <- subset3[,c("RMSSD [milisec.]_5")]
subset3$nHF_5 <- subset3[,c("HF [%]_5")]
subset3$nLF_5 <- subset3[,c("LF [%]_5")]
subset3$n_LF_5 <- subset3$nLF_5/(subset3$nHF_5+subset3$nLF_5)*100
subset3$n_HF_5 <- subset3$nHF_5/(subset3$nHF_5+subset3$nLF_5)*100

## Add survey outputs
ESM_data <- read_excel("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_3_integration\\Intake_survey_C3.xlsx",sheet = "ESM_survey",col_names=T)
ESM_data$P_ID <- as.factor(ESM_data$P_ID)
ESM_data$fTime <- force_tz(ESM_data$Form_finish_time, tzone = "CST6CDT")
ESM_data$Tint <- strftime(ESM_data$fTime, format="%H:%M",tz="CST6CDT")
ESM_data$Tp <- paste(ESM_data$Form_start_date,ESM_data$Tint)
ESM_data$Timestamp <- as.POSIXct(force_tz(strptime(ESM_data$Tp,"%Y-%m-%d %H:%M"),tzone="CST6CDT"))
ESM_data_ordered <- ESM_data[order(ESM_data$P_ID,ESM_data$Timestamp),]
ESM_data_ord <- ESM_data_ordered[!ESM_data_ordered$Form == "Missing",]
d_esm_lp <- merge(subset3,ESM_data_ord,by = c("P_ID","Timestamp"),all.x=T)
d_esm_lp <- left_join(subset3,ESM_data_ord,by = c("P_ID"="P_ID","Timestamp"="Timestamp")) 
d_esm_lp$P_ID <- as.factor(d_esm_lp$P_ID)

d_esm_lp2 <- d_esm_lp[!is.na(d_esm_lp$RMSSD),]
sapply(d_esm_lp2, function(y) sum(length(which(is.na(y)))))
#write.csv(d_esm_lp2,"for_padding.csv")

### Pad 15 mins before and after!!
data_padded <- d_esm_lp2
data_padded$flag <- NA
summary(data_padded$alert)
system.time(
  for (i in 1:(nrow(d_esm_lp2)-15)) ## We use 15 minutes ahead data to impute
  {
    print(i)
    if (!is.na(d_esm_lp2$alert[i]))
    {
      for (t in (ncol(data_padded) - ncol(ESM_data) +3):(ncol(data_padded)-4))   ### These are the columns with psy variables that 'need' padding
      { 
        for (r in 1:15)
        {
          if (i > r)       ### Logic begins only after 30 minutes
          {
            if(data_padded$P_ID[i-r] != data_padded$P_ID[i] |  data_padded$Date[i-r] != data_padded$Date[i]  )
            { 
              #data_padded$flag[i-r] <- "Not imputed" 
              next 
            } 
            else
            { 
              if (is.na(d_esm_lp2$alert[i-r]))
              {
                data_padded[i-r,t] <- d_esm_lp2[i,t] 
                # data_padded$flag[i-r] <- "OK"   ## Pads the previous 15 minutes
              }
            }
            if(data_padded$P_ID[i+r] != data_padded$P_ID[i] |  data_padded$Date[i+r] != data_padded$Date[i]  )
            { 
              #  data_padded$flag[i+r] <- "Not imputed"
              next
            } 
            else
            { 
              if (is.na(d_esm_lp2$alert[i+r]))
              {
                data_padded[i+r,t] <- d_esm_lp2[i,t]
                data_padded$flag[i+r] <- "OK"   ## Pads the next 15 minutes
              }
            }  
          }
        }
      }
    }
  }
)

## Notes: For some reason, the actual value is not copied (+15 min and -15 values are present, but ESM at 0 min disappears). TO fix this later!

data_c3 <- data_padded
data_c3$Time <- ret_time(data_c3$Timestamp)
data_c3$ToD <- cut(data_c3$Time,breaks=c(ret_time(as.POSIXct("00:01",format="%H:%M")),
                                         ret_time(as.POSIXct("8:00",format="%H:%M")),
                                         ret_time(as.POSIXct("12:00",format="%H:%M")),
                                         ret_time(as.POSIXct("15:00",format="%H:%M")),
                                         ret_time(as.POSIXct("18:00",format="%H:%M")),
                                         ret_time(as.POSIXct("20:00",format="%H:%M")),
                                         ret_time(as.POSIXct("23:59",format="%H:%M"))))
levels(data_c3$ToD ) <- c("Early_Morning","Morning","Afternoon","Evening","Late_Evening","Night")

data_c3$DoW <- weekdays(data_c3$Timestamp)

## Add demographic information 
Part_demo <- read.xlsx("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_3_integration\\Intake_survey_C3.xlsx",sheetName = "P_demo_clean")
Part_demo$P_ID <- as.factor(Part_demo$P_ID)
both_demo <- left_join(data_c3,Part_demo,by = c("P_ID"="P_ID"))
both_demo$P_ID <- as.factor(both_demo$P_ID)

## For demographic, make some variables as binary (health conditions)
b_data <- both_demo
for (col in which(names(b_data)=="HighBP"):which(names(b_data)=="Sleep.problems") )
{
  b_data[,col] <- as.numeric(as.character(b_data[,col]))
  for(row in 1:nrow(b_data))
  {  b_data[row,col] <- ifelse(b_data[row,col]==1,"Yes","No")
  }
  b_data[,col] <- as.factor(b_data[,col])
}

for (col in which(names(b_data)=="HighBP"):which(names(b_data)=="Sleep.problems") )
{
  b_data[,col] <- as.character(b_data[,col])
  b_data[is.na(b_data[,col]),col] <- "No"
  b_data[,col] <- as.factor(b_data[,col])
}

levels(b_data$Average_alcohol_intake)
b_data$Average_alcohol_intake[b_data$Average_alcohol_intake=="maybe 1"] <- "1"
b_data$Average_alcohol_intake <- as.numeric(as.character(b_data$Average_alcohol_intake))
b_data$Average_alcohol_intake[b_data$Average_alcohol_intake > 40] <- NA

sapply(b_data, function(y) sum(length(which(is.na(y)))))

# Expensive multivariate imputation only for IEQ wearnodes
#d_data <- b_data[,c(which(names(b_data)=="CO2"):which(names(b_data)=="Absolute_humidity"),which(names(b_data)=="RMSSD"))]
#d_data <- b_data[,c("CO2","Sound","Temperature","Relative_humidity","Pressure","RMSSD","Light")]
## Expensive multivariate imputation only for IEQ wearnodes 
# ## Cheap solution 
# #e_data <- list()
# #e_data[[1]] <- d_data[1:20000,]
# #e_data[[2]] <- d_data[1:nrow(d_data),]
# #e_data[[2]] <- d_data[20001:40000,]
# #e_data[[3]] <- d_data[40001:nrow(d_data),]
# #e_data[[4]] <- d_data[60001:nrow(d_data),]
# 

# for(list in 1:length(e_data))
# {
#   print(memory.size())
#   e_data[[list]] <- rfImpute(e_data[[list]],e_data[[list]][,c("RMSSD")],ntree=50,iter=3)
#   print(memory.size())
#   gc()
# }
# g_data <- do.call("rbind", e_data)


## Here too, we use mean imputation alone!

impute_all <- function(data_in)
{
  for(j in 1:ncol(data_in))
  {
    if(is.factor(data_in[,j]))
    {
      for(k in 1:nrow(data_in))
      {
        if(is.na(data_in[k,j]) | data_in[k,j] == "")
        {
          data_in[k,j] <- data_in[which.max(data_in[,j]),j]
        }
      }
      data_in[,j] <- as.character(data_in[,j])  
      data_in[,j] <- factor(data_in[,j])  
    }
    else
    {
      data_in[,j]  <- as.numeric(round(impute(data_in[,j],mean),6))
    }  
    
  }
  return(data_in)
}

f_data <- b_data[,c(which(names(b_data)=="Gender"):which(names(b_data)=="Openness"),which(names(b_data)=="CO2"):which(names(b_data)=="Absolute_humidity"))]
system.time( f_data <- impute_all(f_data) )
h_data <- b_data[,-c(which(names(b_data)=="Gender"):which(names(b_data)=="Openness"),which(names(b_data)=="CO2"):which(names(b_data)=="Absolute_humidity"))]

#all_data_back <- cbind(h_data,f_data)
all_data_back <- b_data
sapply(all_data_back, function(y) sum(length(which(is.na(y)))))

write.csv(all_data_back,"C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\workspace\\Oct_2016\\Imp_data_C3_Oct12_unimputed.csv",row.names=F,na="")
#save.image("C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\workspace\\Oct_2016\\workspaces\\Cohort3_Oct10.RData")



















########
#######
###### Cohort IV
#######
########
gc()
rm(list=ls())

setwd("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_4_integration\\IEQ_c4")
##### NOTE: THIS IS IN GMT +0:00 time
ieq_part_1 <- list()
temp <- list.files(pattern="*.csv")

### Read the files into list of dataframes ##

system.time(
  for (i in 1:length(temp)) 
  {
    all_content = readLines(temp[i])
    skip_second = all_content[-c(1,2)]
    ieq_part_1[[i]] <- read.csv(textConnection(skip_second), header = FALSE)
  }
)


device_ids <- read.xlsx("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_4_integration\\Finished_p_C4.xlsx",sheetName = "Sensor_ID_lookup")

### label column

for (i in 1: nrow(device_ids))
{
  ieq_part_1[[i]]$device_ID <- rep(device_ids[i,2],nrow(ieq_part_1[[i]]))
  ieq_part_1[[i]]$WB2_ID <- rep(device_ids[i,1],nrow(ieq_part_1[[i]]))
}


ieq_data <- do.call("rbind", ieq_part_1)

### Mention the column names carefully:
colnames(ieq_data) <- c("Timestamp","CO2","Temperature","Pressure","Relative_humidity","Sound","Light","Absolute_humidity","device_ID","WB2_ID")

ieq_data$Timestamp <- as.POSIXct(ieq_data$Timestamp,tz="EST")
## Check Timestamp & later manually verify with participant's 'IN' time
ieq_data_sorted <- ieq_data[order(ieq_data$WB2_ID,ieq_data$Timestamp),]

## Now add P_ID to it
ieq_data_sorted$WB2_ID <- as.factor(ieq_data_sorted$WB2_ID)
ieq_data_sorted$Date <- as.Date(ieq_data_sorted$Timestamp)

P_ID_lookup <- read.xlsx("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_4_integration\\Finished_p_C4.xlsx",sheetName = "Sensor_PID_map")

P_ID_data <- data.frame(matrix(NA,nrow=nrow(P_ID_lookup),ncol=0))
P_ID_data$Date <- P_ID_lookup$Date
P_ID_data$WB2_ID <- as.factor(P_ID_lookup$WB2_ID)
P_ID_data$P_ID <- as.factor(P_ID_lookup$ID)
ieq_pid <- inner_join(ieq_data_sorted,P_ID_data,by = c("WB2_ID"="WB2_ID","Date"="Date"))
ieq_pid$P_ID <- as.factor(ieq_pid$P_ID)
#### HRV dataset #########################################################
##################

## Read both step and MW and compare:

P_ids <- read.xlsx("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_4_integration\\Finished_p_C4.xlsx",sheetName = "P_ID_HRV",header=T)

append_files <- function(p_ids)
{
  phy_part_1 <- list()
  temp = list.files(pattern="*.xls")
  
  for (i in 1:length(temp)) 
  {
    phy_part_1[[i]] <- read_excel(temp[i], sheet=2,col_names=TRUE)
    colnames(phy_part_1[[i]])[1] <- c("Timestamp")
  }
  
  for (i in 1: length(p_ids))
  {
    phy_part_1[[i]]$P_ID <- rep(p_ids[i],nrow(phy_part_1[[i]]))
  }
  
  phy_all <- do.call("rbind",phy_part_1)
  
  return (phy_all)
}

setwd("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_4_integration\\hrv_c4_5")
hrv_file_list <- list.files(pattern="*.xls")

system.time(
  phy_5min <- append_files(P_ids$fivemin)
)

setwd("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_4_integration\\hrv_c4")
hrv_file_list2 <- list.files(pattern="*.xls")

system.time(
  phy_5minmw <- append_files(na.omit(P_ids$fiveminmw))
)

force_time <- function(data_in)
{  
  data_in$Timestamp <- force_tz(data_in$Timestamp, tzone = "EST")   # Force it to EST as it defaulted to GMT in read_excel
  data_in$P_ID <- as.factor(data_in$P_ID)
  return(data_in)
}

phy_5min <- force_time(phy_5min)
phy_5minmw <- force_time(phy_5minmw)

## HRV_padding (important for 5 minute FLAT data)

phy_1 <-as.data.frame(phy_5min)
phy_1$P_ID<-as.factor(phy_1$P_ID)
phy_part <- split(phy_1,phy_1$P_ID)

phy_part_min <- list()

pad_5 <- function(data_in)
{ 
  data_in <- data_in[order(data_in$Timestamp),] 
  for(j in 1:ncol(data_in))
  {
    if(!colnames(data_in)[j] %in% c("Timestamp","flag"))
    {
      for (i in 1:nrow(data_in))
      {
        if (!is.na(data_in$flag[i]))
        {
          for (r in 1:4)
          {
            data_in[i+r,j] <- data_in[i,j]  ##Fills the next 5 minutes
          }
        }
      }
    }
  }
  return(data_in)    
}

to_min <- function(data_in)
{
  data_in$Timestamp <- as.POSIXct(round(data_in$Timestamp , "mins"),tz="EST")
  data_in$flag <- 1
  data_o <- TimeExpand(data_in,TimeVar='Timestamp',by='min')
  data_out <- pad_5(data_o)
  data_out$P_ID<-as.factor(data_out$P_ID)
  return(data_out)
}

for(i in 1:length(phy_part))
{
  phy_part_min[[i]] <- to_min(phy_part[[i]])
}  ## Ignore warnings (one for each participant)

phy_1mmm <- do.call("rbind",phy_part_min)
phy_min <- phy_1mmm

##### SMARTER THING TO DO IS MERGE HRV_5_MW and HRV_5 as a left join
phy_min_names <- paste(names(phy_min),"_5",sep="")
names(phy_min) <- phy_min_names

phy_5minmw$Timestamp <- as.POSIXct(round(phy_5minmw$Timestamp , "mins"),tz="EST")
phy_5minmw$P_ID <- as.factor(phy_5minmw$P_ID)

phy_both <- full_join(phy_5minmw, phy_min, by=c("Timestamp"="Timestamp_5","P_ID"="P_ID_5" ))
phy_both$P_ID <- as.factor(phy_both$P_ID)

#write.csv(phy_both,"C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\workspace\\Oct_2016\\C4_hrv_allday.csv",na="",row.names=F)

ieq_dat <- ieq_pid[order(ieq_pid$P_ID,ieq_pid$Timestamp),]
ieq_dat$Timestamp <- as.POSIXct(round(ieq_dat$Timestamp , "mins"),tz="EST")  ## Ignore the warning

phy_ieq_mw <- left_join(phy_both,ieq_dat,by = c("Timestamp"="Timestamp","P_ID"="P_ID"))
phy_ieq_mw$P_ID <- as.factor(phy_ieq_mw$P_ID)
phy_ieq_mw <- phy_ieq_mw[order(phy_ieq_mw$P_ID,phy_ieq_mw$Timestamp),]

## Time filter
part_time <- read.xlsx("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_4_integration\\Finished_p_C4.xlsx",sheetName = "Sensor_PID_map")
part_df <- data.frame(Date = part_time$Date, P_ID = part_time$ID,WB2_ID = part_time$WB2_ID,
                      Time_in = part_time$Time_in,Time_out = part_time$Time_out,
                      Tin1 = part_time$T_in1,Tout1 = part_time$T_out1,Tin2 = part_time$T_in2,Tout2 = part_time$T_out2) 

part_df$P_ID <- as.factor(part_df$P_ID)
part_df$WB2_ID <- as.factor(part_df$WB2_ID)

part_wears <- left_join(phy_ieq_mw,part_df,by=c("P_ID"="P_ID","Date"="Date","WB2_ID"="WB2_ID"))  ## Include WB2_ID in join key too!
part_wears$P_ID <-as.factor(part_wears$P_ID)
part_wears <- part_wears[order(part_wears$P_ID,part_wears$Timestamp),]

part_wears$Time <- as.POSIXct(strptime(format(part_wears$Timestamp,"%H:%M"), "%H:%M"),tz="EST")

ret_time <- function(timestamp_in) { return(as.POSIXct(strptime(format(timestamp_in,"%H:%M"), "%H:%M"),tz="EST"))}

# part_wears$Time_in <- ret_time(part_wears$Time_in)
part_wears <- as.data.frame(part_wears)

for(col in which(names(part_wears)=="Time_in"):which(names(part_wears)=="Tout2"))
{
  part_wears[,col] <- ret_time(part_wears[,col])
}

subset1 <- subset(part_wears,part_wears$Time_in < part_wears$Time & part_wears$Time_out > part_wears$Time)
subset2 <- subset1[-which((subset1$Time > subset1$Tin1) & (subset1$Tout1 > subset1$Time)),]
subset3 <- subset2[-which((subset2$Time > subset2$Tin2) & (subset2$Tout2 > subset2$Time)),] 

subset3$SDNN <- subset3[,c("SDNN [milisec.]")]
subset3$RMSSD <- subset3[,c("RMSSD [milisec.]")]
subset3$nHF <- subset3[,c("HF [%]")]
subset3$nLF <- subset3[,c("LF [%]")]

sapply(subset3, function(y) sum(length(which(is.na(y)))))

subset3$n_LF <- subset3$nLF/(subset3$nHF+subset3$nLF)*100
subset3$n_HF <- subset3$nHF/(subset3$nHF+subset3$nLF)*100

subset3$SDNN_5 <- subset3[,c("SDNN [milisec.]_5")]
subset3$RMSSD_5 <- subset3[,c("RMSSD [milisec.]_5")]
subset3$nHF_5 <- subset3[,c("HF [%]_5")]
subset3$nLF_5 <- subset3[,c("LF [%]_5")]
subset3$n_LF_5 <- subset3$nLF_5/(subset3$nHF_5+subset3$nLF_5)*100
subset3$n_HF_5 <- subset3$nHF_5/(subset3$nHF_5+subset3$nLF_5)*100

## Add survey outputs
ESM_data <- read_excel("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_4_integration\\Intake_survey_C4.xlsx",sheet = "ESM_survey",col_names=T)
ESM_data$P_ID <- as.factor(ESM_data$P_ID)
ESM_data$fTime <- force_tz(ESM_data$Form_finish_time, tzone = "EST")
ESM_data$Tint <- strftime(ESM_data$fTime, format="%H:%M",tz="EST")
ESM_data$Tp <- paste(ESM_data$Form_start_date,ESM_data$Tint)
ESM_data$Timestamp <- as.POSIXct(force_tz(strptime(ESM_data$Tp,"%Y-%m-%d %H:%M"),tzone="EST"))
ESM_data_ordered <- ESM_data[order(ESM_data$P_ID,ESM_data$Timestamp),]
ESM_data_ord <- ESM_data_ordered[!ESM_data_ordered$Form == "Missing",]
d_esm_lp <- merge(subset3,ESM_data_ord,by = c("P_ID","Timestamp"),all.x=T)
d_esm_lp <- left_join(subset3,ESM_data_ord,by = c("P_ID"="P_ID","Timestamp"="Timestamp")) 
d_esm_lp$P_ID <- as.factor(d_esm_lp$P_ID)

d_esm_lp2 <- d_esm_lp[!is.na(d_esm_lp$RMSSD),]
sapply(d_esm_lp2, function(y) sum(length(which(is.na(y)))))
#write.csv(d_esm_lp2,"for_padding.csv")

### Pad 15 mins before and after!!
data_padded <- d_esm_lp2
data_padded$flag <- NA
summary(data_padded$alert)
system.time(
  for (i in 1:(nrow(d_esm_lp2)-15)) ## We use 15 minutes ahead data to impute
  {
    print(i)
    if (!is.na(d_esm_lp2$alert[i]))
    {
      for (t in (ncol(data_padded) - ncol(ESM_data) +3):(ncol(data_padded)-4))   ### These are the columns with psy variables that 'need' padding
      { 
        for (r in 1:15)
        {
          if (i > r)       ### Logic begins only after 30 minutes
          {
            if(data_padded$P_ID[i-r] != data_padded$P_ID[i] |  data_padded$Date[i-r] != data_padded$Date[i]  )
            { 
              #data_padded$flag[i-r] <- "Not imputed" 
              next 
            } 
            else
            { 
              if (is.na(d_esm_lp2$alert[i-r]))
              {
                data_padded[i-r,t] <- d_esm_lp2[i,t] 
                # data_padded$flag[i-r] <- "OK"   ## Pads the previous 15 minutes
              }
            }
            if(data_padded$P_ID[i+r] != data_padded$P_ID[i] |  data_padded$Date[i+r] != data_padded$Date[i]  )
            { 
              #  data_padded$flag[i+r] <- "Not imputed"
              next
            } 
            else
            { 
              if (is.na(d_esm_lp2$alert[i+r]))
              {
                data_padded[i+r,t] <- d_esm_lp2[i,t]
                data_padded$flag[i+r] <- "OK"   ## Pads the next 15 minutes
              }
            }  
          }
        }
      }
    }
  }
)

## Notes: For some reason, the actual value is not copied (+15 min and -15 values are present, but ESM at 0 min disappears). TO fix this later!

data_c4 <- data_padded
data_c4$Time <- ret_time(data_c4$Timestamp)
data_c4$ToD <- cut(data_c4$Time,breaks=c(ret_time(as.POSIXct("00:01",format="%H:%M")),
                                         ret_time(as.POSIXct("8:00",format="%H:%M")),
                                         ret_time(as.POSIXct("12:00",format="%H:%M")),
                                         ret_time(as.POSIXct("15:00",format="%H:%M")),
                                         ret_time(as.POSIXct("18:00",format="%H:%M")),
                                         ret_time(as.POSIXct("20:00",format="%H:%M")),
                                         ret_time(as.POSIXct("23:59",format="%H:%M"))))
levels(data_c4$ToD ) <- c("Early_Morning","Morning","Afternoon","Evening","Late_Evening","Night")

data_c4$DoW <- weekdays(data_c4$Timestamp)

## Add demographic information 
Part_demo <- read.xlsx("C:\\Users\\karthik\\Google Drive\\GSA DATA\\Data integration\\cohort_4_integration\\Intake_survey_C4.xlsx",sheetName = "P_demo_clean")
Part_demo$P_ID <- as.factor(Part_demo$P_ID)
both_demo <- left_join(data_c4,Part_demo,by = c("P_ID"="P_ID"))
both_demo$P_ID <- as.factor(both_demo$P_ID)

## For demographic, make some variables as binary (health conditions)
b_data <- both_demo
for (col in which(names(b_data)=="HighBP"):which(names(b_data)=="Sleep.problems") )
{
  b_data[,col] <- as.numeric(as.character(b_data[,col]))
  for(row in 1:nrow(b_data))
  {  b_data[row,col] <- ifelse(b_data[row,col]==1,"Yes","No")
  }
  b_data[,col] <- as.factor(b_data[,col])
}

for (col in which(names(b_data)=="HighBP"):which(names(b_data)=="Sleep.problems") )
{
  b_data[,col] <- as.character(b_data[,col])
  b_data[is.na(b_data[,col]),col] <- "No"
  b_data[,col] <- as.factor(b_data[,col])
}

levels(b_data$Average_alcohol_intake)
b_data$Average_alcohol_intake[b_data$Average_alcohol_intake=="maybe 1"] <- "1"
b_data$Average_alcohol_intake <- as.numeric(as.character(b_data$Average_alcohol_intake))
b_data$Average_alcohol_intake[b_data$Average_alcohol_intake > 40] <- NA

sapply(b_data, function(y) sum(length(which(is.na(y)))))

# Expensive multivariate imputation only for IEQ wearnodes
#d_data <- b_data[,c(which(names(b_data)=="CO2"):which(names(b_data)=="Absolute_humidity"),which(names(b_data)=="RMSSD"))]
#d_data <- b_data[,c("CO2","Sound","Temperature","Relative_humidity","Pressure","RMSSD","Light")]
## Expensive multivariate imputation only for IEQ wearnodes 
# ## Cheap solution 
# #e_data <- list()
# #e_data[[1]] <- d_data[1:20000,]
# #e_data[[2]] <- d_data[1:nrow(d_data),]
# #e_data[[2]] <- d_data[20001:40000,]
# #e_data[[3]] <- d_data[40001:nrow(d_data),]
# #e_data[[4]] <- d_data[60001:nrow(d_data),]
# 

# for(list in 1:length(e_data))
# {
#   print(memory.size())
#   e_data[[list]] <- rfImpute(e_data[[list]],e_data[[list]][,c("RMSSD")],ntree=50,iter=3)
#   print(memory.size())
#   gc()
# }
# g_data <- do.call("rbind", e_data)


## Here too, we use mean imputation alone!

impute_all <- function(data_in)
{
  for(j in 1:ncol(data_in))
  {
    if(is.factor(data_in[,j]))
    {
      for(k in 1:nrow(data_in))
      {
        if(is.na(data_in[k,j]) | data_in[k,j] == "")
        {
          data_in[k,j] <- data_in[which.max(data_in[,j]),j]
        }
      }
      data_in[,j] <- as.character(data_in[,j])  
      data_in[,j] <- factor(data_in[,j])  
    }
    else
    {
      data_in[,j]  <- as.numeric(round(impute(data_in[,j],mean),6))
    }  
    
  }
  return(data_in)
}

f_data <- b_data[,c(which(names(b_data)=="Gender"):which(names(b_data)=="Openness"),which(names(b_data)=="CO2"):which(names(b_data)=="Absolute_humidity"))]
system.time( f_data <- impute_all(f_data) )
h_data <- b_data[,-c(which(names(b_data)=="Gender"):which(names(b_data)=="Openness"),which(names(b_data)=="CO2"):which(names(b_data)=="Absolute_humidity"))]

# all_data_back <- cbind(h_data,f_data)
all_data_back <- b_data
sapply(all_data_back, function(y) sum(length(which(is.na(y)))))

write.csv(all_data_back,"C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\workspace\\Oct_2016\\Imp_data_C4_Oct12_unimputed.csv",row.names=F,na="")
#save.image("C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\workspace\\Oct_2016\\workspaces\\Cohort4_Oct12_.RData")








