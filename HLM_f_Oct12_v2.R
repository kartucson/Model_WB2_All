### SInce the coefficients are too small, we will proble further... 

## HLM model ##
## Karthik S - Oct 10,2016 ##
## All in one HLM code

load("All_HLM_models_saved_12Oct.RData")

library(randomForest)
library(caret)
library(Hmisc)
library(data.table)
library(earth)
library(lme4)
library(nlme)
library(rpart)
library(gtools)
library(dplyr)
library(psychometric)
library(car)
library(nortest)
library(lattice)
library(lmtest)

data_c1 <- read.csv("C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\workspace\\Oct_2016\\datasets\\Imp_data_C1_Oct10.csv")
data_c2 <- read.csv("C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\workspace\\Oct_2016\\datasets\\Imp_data_C2_Oct10.csv")
data_c3 <- read.csv("C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\workspace\\Oct_2016\\datasets\\Imp_data_C3_Oct10.csv")
data_c4 <- read.csv("C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\workspace\\Oct_2016\\datasets\\Imp_data_C4_Oct10.csv")

## Manually cleaned:
# data_c1 <- read.csv("Imp_data_C1_Oct10_pruned.csv")
# data_c2 <- read.csv("Imp_data_C2_Oct10_pruned.csv")
# data_c3 <- read.csv("Imp_data_C3_Oct10_pruned.csv")
# data_c4 <- read.csv("Imp_data_C4_Oct10_pruned.csv")

data_c1$cohort <- "cohort1"
data_c2$cohort <- "cohort2"
data_c3$cohort <- "cohort3"
data_c4$cohort <- "cohort4"

data_a <- smartbind(data_c1,data_c2,data_c3,data_c4)

sapply(data_a, function(y) sum(length(which(is.na(y)))))

data_a$P_ID <- as.factor(data_a$P_ID)
data_a$cohort <- as.factor(data_a$cohort)
data_a$lRMSSD <- log(data_a$RMSSD)
data_a$lSDNN <- log(data_a$SDNN)

length(levels(data_a$P_ID ))

data_a$place[data_a$cohort =="cohort3"] <- "FtWorth"
data_a$place[data_a$cohort =="cohort4"] <- "NIH"
data_a$place[data_a$cohort %in% c("cohort1","cohort2")] <- "1800F"

loc <- read.csv("WB2_C1C2C3C4_worklocations.csv")
loc$P_ID <- as.factor(loc$P_ID)

data_a <- left_join(data_a,loc,by=c("P_ID"="P_ID"))
data_a$P_ID <- as.factor(data_a$P_ID)

data_a$place[data_a$wing_floor %in% c("ROB")] <- "ROB"
data_a$place <- as.factor(data_a$place)

#write.csv(data_a,"Complete_dataset_11Oct.csv",na="",row.names=F)

## Cute code for centering ##
# data_c <- sapply(data_a,function(x) ifelse(is.numeric(x),scale(x,center=T,scale=F),x) )
## Doesnt work...

data_a <- read.csv("Complete_dataset_11Oct.csv")
data_c <- data_a
data_s <- data_a

for(col in 1:ncol(data_a))
{
  x <- data_a[,col]
  if(is.numeric(data_a[,col]))
  {
    data_c[,col] <-  scale(x,center=T,scale=F) 
  }
  else {
    data_c[,col]  <- x
  }
}

for(col in 1:ncol(data_a))
{
  x <- data_a[,col]
  if(is.numeric(data_a[,col]))
  {
    data_s[,col] <-  scale(x,center=T,scale=T) 
  }
  else {
    data_s[,col]  <- x
  }
}

names(data_c) <- paste(names(data_a),"_c",sep="")
names(data_s) <- paste(names(data_a),"_s",sep="")

data_pl <- cbind(data_a,data_c,data_s)
data_t <- data_pl
data_pl <- data_pl[data_pl$RMSSD < 110,]
#data_pl <- data_pl[data_pl$RMSSD < 200,]
## Mention the relevels here:
# data_pl$place <- relevel(data_pl$place,ref= "")
data_pl$ToD <- relevel(data_pl$ToD,ref= "Morning")
data_pl$DoW <- relevel(data_pl$DoW,ref= "Monday")
data_pl$Gender <- relevel(data_pl$Gender,ref= "Male")

mod_uncond_RMSSD <- lme(fixed = RMSSD ~ 1, random = list(P_ID=~1),data=data_pl,control=lmeControl(opt="optim"))  
mod_uncond_n_HF <- lme(fixed = n_HF ~ 1, random = list(P_ID=~1),data=data_pl,control=lmeControl(opt="optim"))  

ICC1.lme(RMSSD, P_ID, data = data_pl)
ICC1.lme(SDNN, P_ID, data = data_pl)
ICC1.lme(n_HF, P_ID, data = data_pl[!is.na(data_pl$n_HF),]) ## DOESNT CONVERGE IN ML
ICC1.lme(LF.HF, P_ID, data = data_pl)

fit1 <- lme(n_HF ~ 1 , data = data_pl, random = ~ 1 | P_ID,control=lmeControl(opt="optim"))
I.var <- as.numeric(VarCorr(fit1)[1,1])
resid.var <- as.numeric(VarCorr(fit1)[2,1])
ICC <- I.var/(I.var+resid.var) 
ICC

## Full model, only fixed effects random intercept
mod_cond_RMSSD <- lme(fixed = RMSSD ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c , random = list(P_ID=~1),data=data_pl,control=lmeControl(opt="optim"),method="ML")  
mod_cond_SDNN <- lme(fixed = SDNN ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c , random = list(P_ID=~1),data=data_pl,control=lmeControl(opt="optim"),method="ML")  
mod_cond_n_HF <- lme(fixed = n_HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c , random = list(P_ID=~1),data=data_pl,control=lmeControl(opt="optim"),method="ML")  
mod_cond_LF.HF <- lme(fixed = LF.HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c , random = list(P_ID=~1),data=data_pl,control=lmeControl(opt="optim"),method="ML")  

save.image("Plain_model_11Oct.RData")

## Remove insig. for each outcome 
mod_cond_RMSSD_par <- lme(fixed = RMSSD ~ 1+ ToD + DoW + Age + BMI + Temperature_c + Relative_humidity_c + Pressure_c , random = list(P_ID=~1),data=data_pl,control=lmeControl(opt="optim"),method="ML")  
mod_cond_SDNN_par <- lme(fixed = SDNN ~ 1+ ToD + DoW + Age + BMI + Sound_c + Temperature_c + Relative_humidity_c + Pressure_c , random = list(P_ID=~1),data=data_pl,control=lmeControl(opt="optim"),method="ML")  
mod_cond_n_HF_par <- lme(fixed = n_HF ~ 1+ ToD + DoW + place +  Gender + Sound_c +  Relative_humidity_c + Pressure_c + CO2_c , random = list(P_ID=~1),data=data_pl,control=lmeControl(opt="optim"),method="ML")  
mod_cond_LF.HF_par <- lme(fixed = LF.HF ~ 1+ ToD + DoW + place + Gender + Sound_c + Temperature_c + Relative_humidity_c + Pressure_c , random = list(P_ID=~1),data=data_pl,control=lmeControl(opt="optim"),method="ML")  

anova(mod_cond_RMSSD_par,mod_cond_RMSSD) 
anova(mod_cond_SDNN_par,mod_cond_SDNN)
anova(mod_cond_n_HF_par,mod_cond_n_HF)
anova(mod_cond_LF.HF_par,mod_cond_LF.HF)

## No need to remove the insig. predictors (no reduction in AIC)

# ## ADding fixed effects with curves, with standardization (NN)
# mod_cond_RMSSD_curve_s <- lme(fixed = RMSSD ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=~1),data=data_pl,control=lmeControl(opt="optim"),method="ML")  
# mod_cond_SDNN_curve_s <- lme(fixed = SDNN ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=~1),data=data_pl,control=lmeControl(opt="optim"),method="ML")  
# mod_cond_n_HF_curve_s <- lme(fixed = n_HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=~1),data=data_pl,control=lmeControl(opt="optim"),method="ML")  
# mod_cond_LF.HF_curve_s <- lme(fixed = LF.HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=~1),data=data_pl,control=lmeControl(opt="optim"),method="ML")  


## With random slopes (Only diagonal)
mod_cond_RMSSD_pd <- lme(fixed = RMSSD ~ 1+ ToD + DoW + Age + BMI + Temperature_c + Relative_humidity_c + Pressure_c , random = list(P_ID=pdDiag(~ Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c)),data=data_pl,control=lmeControl(opt="optim"),na.action = na.exclude,method="ML")  
mod_cond_SDNN_pd <- lme(fixed = SDNN ~ 1+ ToD + DoW +  Age + Gender + BMI + Sound_c + Temperature_c + Relative_humidity_c + Pressure_c , random = list(P_ID=pdDiag(~ Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c)),data=data_pl,control=lmeControl(opt="optim"),na.action = na.exclude,method="ML")  
mod_cond_n_HF_pd <- lme(fixed = n_HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c , random = list(P_ID=pdDiag(~ Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c)),data=data_pl,control=lmeControl(opt="optim"),na.action = na.exclude,method="ML")  
mod_cond_LF.HF_pd <- lme(fixed = LF.HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c , random = list(P_ID=pdDiag(~ Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c)),data=data_pl,control=lmeControl(opt="optim"),na.action = na.exclude,method="ML")  

anova(mod_cond_RMSSD_pd,mod_cond_RMSSD) 
anova(mod_cond_SDNN_pd,mod_cond_SDNN)
anova(mod_cond_n_HF_pd,mod_cond_n_HF)
anova(mod_cond_LF.HF_pd,mod_cond_LF.HF)

# With light
data_l <- data_pl[!is.na(data_pl$Light),]
mod_cond_RMSSD_light <- lme(fixed = RMSSD ~ 1+ ToD + DoW + place + Age + Gender + BMI + Light_c + Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c , random = list(P_ID=~1),data=data_l,control=lmeControl(opt="optim"))  
mod_cond_SDNN_light <- lme(fixed = SDNN ~ 1+ ToD + DoW + place + Age + Gender + BMI + Light_c +Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c , random = list(P_ID=~1),data=data_l,control=lmeControl(opt="optim"))  
mod_cond_n_HF_light <- lme(fixed = n_HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Light_c +Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c , random = list(P_ID=~1),data=data_l,control=lmeControl(opt="optim"))  
mod_cond_LF.HF_light <- lme(fixed = LF.HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Light_c +Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c , random = list(P_ID=~1),data=data_l,control=lmeControl(opt="optim"))  

## Lets see, just for fun!! - Light is still insig (YEYY)
mod_cond_RMSSD_pd_l <- lme(fixed = RMSSD ~ 1+ ToD + DoW + Age + BMI + Temperature_c + Relative_humidity_c + Pressure_c + Light_c, random = list(P_ID=pdDiag(~ Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c)),data=data_l,control=lmeControl(opt="optim"),na.action = na.exclude,method="ML")  
mod_cond_SDNN_pd_l <- lme(fixed = SDNN ~ 1+ ToD + DoW +  Age + Gender + BMI + Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + Light_c, random = list(P_ID=pdDiag(~ Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c+ Light_c)),data=data_l,control=lmeControl(opt="optim"),na.action = na.exclude,method="ML")  
mod_cond_n_HF_pd_l <- lme(fixed = n_HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c + Light_c, random = list(P_ID=pdDiag(~ Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c+ Light_c)),data=data_l,control=lmeControl(opt="optim"),na.action = na.exclude,method="ML")  
mod_cond_LF.HF_pd_l <- lme(fixed = LF.HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c + Light_c, random = list(P_ID=pdDiag(~ Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c+ Light_c)),data=data_l,control=lmeControl(opt="optim"),na.action = na.exclude,method="ML")  


## Curvilinear - Only intercept

# mod_cond_RMSSD_temp <- lme(fixed = RMSSD ~ 1+ ToD + DoW + I(Temperature_c) +  I(Temperature_c^2)  , random = list(P_ID=~1),data=data_pl,control=lmeControl(opt="optim"),method="ML")  

mod_cond_RMSSD_curve <- lme(fixed = RMSSD ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_c + Temperature_c + Relative_humidity_c + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_c + CO2_c , random = list(P_ID=~1),data=data_pl,control=lmeControl(opt="optim"),method="ML")  
mod_cond_SDNN_curve <- lme(fixed = SDNN ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_c + Temperature_c + Relative_humidity_c + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_c + CO2_c , random = list(P_ID=~1),data=data_pl,control=lmeControl(opt="optim"),method="ML")  
mod_cond_n_HF_curve <- lme(fixed = n_HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_c + Temperature_c + Relative_humidity_c + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_c + CO2_c , random = list(P_ID=~1),data=data_pl,control=lmeControl(opt="optim"),method="ML")  
mod_cond_LF.HF_curve <- lme(fixed = LF.HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_c + Temperature_c + Relative_humidity_c + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_c + CO2_c , random = list(P_ID=~1),data=data_pl,control=lmeControl(opt="optim"),method="ML")  

anova(mod_cond_RMSSD_curve,mod_cond_RMSSD) 
anova(mod_cond_SDNN_curve,mod_cond_SDNN)
anova(mod_cond_n_HF_curve,mod_cond_n_HF)
anova(mod_cond_LF.HF_curve,mod_cond_LF.HF)

## Curvilinear with random slopes
mod_cond_RMSSD_curve_pd <- lme(fixed = RMSSD ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_c + Temperature_c + Relative_humidity_c + I(Sound_c^2) + I(Temperature_c^2) + I(Relative_humidity_c^2) + I(CO2_c^2) + Pressure_c + CO2_c , random = list(P_ID=pdDiag(~ Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c+ I(Sound_c^2) + I(Temperature_c^2) + I(Relative_humidity_c^2) + I(CO2_c^2))),data=data_pl,control=lmeControl(opt="optim"),method="ML")  
mod_cond_SDNN_curve_pd <- lme(fixed = SDNN ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_c + Temperature_c + Relative_humidity_c + I(Sound_c^2) + I(Temperature_c^2) + I(Relative_humidity_c^2) + I(CO2_c^2) + Pressure_c + CO2_c , random = list(P_ID=pdDiag(~ Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c+ I(Sound_c^2) + I(Temperature_c^2) + I(Relative_humidity_c^2) + I(CO2_c^2))),data=data_pl,control=lmeControl(opt="optim"),method="ML")  
mod_cond_n_HF_curve_pd <- lme(fixed = n_HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_c + Temperature_c + Relative_humidity_c + I(Sound_c^2) + I(Temperature_c^2) + I(Relative_humidity_c^2) + I(CO2_c^2) + Pressure_c + CO2_c , random = list(P_ID=pdDiag(~ Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c+ I(Sound_c^2) + I(Temperature_c^2) + I(Relative_humidity_c^2) + I(CO2_c^2))),data=data_pl,control=lmeControl(opt="optim"),method="ML")  
mod_cond_LF.HF_curve_pd <- lme(fixed = LF.HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_c + Temperature_c + Relative_humidity_c + I(Sound_c^2) + I(Temperature_c^2) + I(Relative_humidity_c^2) + I(CO2_c^2) + Pressure_c + CO2_c , random = list(P_ID=pdDiag(~ Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c+ I(Sound_c^2) + I(Temperature_c^2) + I(Relative_humidity_c^2) + I(CO2_c^2))),data=data_pl,control=lmeControl(opt="optim"),method="ML")  

mod_cond_lRMSSD_curve_pd <- lme(fixed = lRMSSD ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_c + Temperature_c + Relative_humidity_c + I(Sound_c^2) + I(Temperature_c^2) + I(Relative_humidity_c^2) + I(CO2_c^2) + Pressure_c + CO2_c , random = list(P_ID=pdDiag(~ Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c+ I(Sound_c^2) + I(Temperature_c^2) + I(Relative_humidity_c^2) + I(CO2_c^2))),data=data_pl,control=lmeControl(opt="optim"),method="ML")  
mod_cond_lSDNN_curve_pd <- lme(fixed = lSDNN ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_c + Temperature_c + Relative_humidity_c + I(Sound_c^2) + I(Temperature_c^2) + I(Relative_humidity_c^2) + I(CO2_c^2) + Pressure_c + CO2_c , random = list(P_ID=pdDiag(~ Sound_c + Temperature_c + Relative_humidity_c + Pressure_c + CO2_c+ I(Sound_c^2) + I(Temperature_c^2) + I(Relative_humidity_c^2) + I(CO2_c^2))),data=data_pl,control=lmeControl(opt="optim"),method="ML")  


anova(mod_cond_RMSSD_curve_pd,mod_cond_RMSSD_pd) 
anova(mod_cond_SDNN_curve_pd,mod_cond_SDNN_pd)
anova(mod_cond_n_HF_curve_pd,mod_cond_n_HF_pd)
anova(mod_cond_LF.HF_curve_pd,mod_cond_LF.HF_pd)

anova(mod_cond_RMSSD_curve_pd,mod_cond_RMSSD_curve) 
anova(mod_cond_SDNN_curve_pd,mod_cond_SDNN_curve)
anova(mod_cond_n_HF_curve_pd,mod_cond_n_HF_curve)
anova(mod_cond_LF.HF_curve_pd,mod_cond_LF.HF_curve)

## Now to do the same with standardized coefficients (to see relative effects)
mod_cond_RMSSD_curve_pd_s <- lme(fixed = RMSSD ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_pl,control=lmeControl(opt="optim"),method="ML")  
mod_cond_SDNN_curve_pd_s <- lme(fixed = SDNN ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_pl,control=lmeControl(opt="optim"),method="ML")  
mod_cond_n_HF_curve_pd_s <- lme(fixed = n_HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_pl,control=lmeControl(opt="optim"),method="ML")  
mod_cond_LF.HF_curve_pd_s <- lme(fixed = LF.HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_pl,control=lmeControl(opt="optim"),method="ML")  

mod_cond_lRMSSD_curve_pd_s <- lme(fixed = lRMSSD ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_pl,control=lmeControl(opt="optim"),method="ML")  
mod_cond_lSDNN_curve_pd_s <- lme(fixed = lSDNN ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_pl,control=lmeControl(opt="optim"),method="ML")  

anova(mod_cond_RMSSD_curve_pd_s,mod_cond_RMSSD_curve_pd) 
anova(mod_cond_SDNN_curve_pd_s,mod_cond_SDNN_curve_pd)
anova(mod_cond_n_HF_curve_pd_s,mod_cond_n_HF_curve_pd)
anova(mod_cond_LF.HF_curve_pd_s,mod_cond_LF.HF_curve_pd)

## Just curious to add ToD and DoW as random effects into model
mod_cond_RMSSD_curve_pd_s_t <- lme(fixed = RMSSD ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + ToD + DoW )),data=data_pl,control=lmeControl(opt="optim"),method="ML")  
anova(mod_cond_RMSSD_curve_pd_s,mod_cond_RMSSD_curve_pd_s_t) 

# HighBP Heart.disease,Musculoskeletal.,Depression,Anxiety,Sleep.problems,Pain_neck_knee,Ethnicity
## ADding fixed effects of health conditions to check for accounting for between person variability
mod_cond_RMSSD_curve_pd_hc_s <- lme(fixed = RMSSD ~ 1+ 
                                      HighBP +Heart.disease+Musculoskeletal.+
                                      Depression+Anxiety+Sleep.problems+Pain_neck_knee+
                                      Ethnicity+
ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_pl,control=lmeControl(opt="optim"),method="ML",na.action = na.exclude)  
mod_cond_SDNN_curve_pd_hc_s <- lme(fixed = SDNN ~ 1 +
                                     HighBP +Heart.disease+Musculoskeletal.+
                                     Depression+Anxiety+Sleep.problems+Pain_neck_knee+
                                     Ethnicity+
                                     ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_pl,control=lmeControl(opt="optim"),method="ML",na.action = na.exclude)  
mod_cond_n_HF_curve_pd_hc_s <- lme(fixed = n_HF ~ 1+ 
                                     HighBP +Heart.disease+Musculoskeletal.+
                                     Depression+Anxiety+Sleep.problems+Pain_neck_knee+
                                     Ethnicity+
                                     ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_pl,control=lmeControl(opt="optim"),method="ML",na.action = na.exclude)  
mod_cond_LF.HF_curve_pd_hc_s <- lme(fixed = LF.HF ~ 1+ 
                                      HighBP +Heart.disease+Musculoskeletal.+
                                      Depression+Anxiety+Sleep.problems+Pain_neck_knee+
                                      Ethnicity+
                                      ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_pl,control=lmeControl(opt="optim"),method="ML",na.action = na.exclude)  

anova(mod_cond_RMSSD_curve_pd_hc_s,mod_cond_RMSSD_curve_pd_s) 
anova(mod_cond_SDNN_curve_pd_hc_s,mod_cond_SDNN_curve_pd_s)
anova(mod_cond_n_HF_curve_pd_hc_s,mod_cond_n_HF_curve_pd_s)
anova(mod_cond_LF.HF_curve_pd_hc_s,mod_cond_LF.HF_curve_pd_s)

## ADding health conditions doesnt improve model fit!!

## Interactions: Age and BMI ##
#tem <- sapply(data_a, function(y) sum(length(which(is.na(y)))))
#write.csv(tem,"Missing_complete_dataset.csv",na="")

data_5 <- data_pl[!is.na(data_pl$RMSSD_5),]
### What about HRV_5
mod_cond_RMSSD_5_curve_pd_s <- lme(fixed = RMSSD_5 ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_5,control=lmeControl(opt="optim"),method="ML",na.action = na.exclude)  
mod_cond_SDNN_5_curve_pd_s <- lme(fixed = SDNN_5 ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_5,control=lmeControl(opt="optim"),method="ML",na.action = na.exclude)  
mod_cond_n_HF_5_curve_pd_s <- lme(fixed = n_HF_5 ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_5,control=lmeControl(opt="optim"),method="ML",na.action = na.exclude)  
mod_cond_LF.HF_5_curve_pd_s <- lme(fixed = LF.HF_5 ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_5,control=lmeControl(opt="optim"),method="ML",na.action = na.exclude)  


## Final data is therefore: mod_cond_RMSSD_curve_pd ##

#### Check model assumptions:
## Also normality tests ##
## VVImp:

#plot(mod_cond_RMSSD_curve_pd, resid(., type="p")~P_ID, id=0.05)
plot(mod_cond_RMSSD_curve_pd, P_ID~resid(., type="p"), abline=0, id=0.05,ylab=c("Participants"))
plot(mod_cond_SDNN_curve_pd, P_ID~resid(., type="p"), abline=0, id=0.05,ylab=c("Participants"))
plot(mod_cond_n_HF_curve_pd, P_ID~resid(., type="p"), abline=0, id=0.05,ylab=c("Participants"))
plot(mod_cond_LF.HF_curve_pd, P_ID~resid(., type="p"), abline=0, id=0.05,ylab=c("Participants"))
plot(mod_cond_lRMSSD_curve_pd, P_ID~resid(., type="p"), abline=0, id=0.05,ylab=c("Participants"))
plot(mod_cond_lSDNN_curve_pd, P_ID~resid(., type="p"), abline=0, id=0.05,ylab=c("Participants"))

plot(mod_cond_RMSSD_curve_pd)
qqnorm(mod_cond_RMSSD_curve_pd, ~ranef(., level=2))

# Level-2
qqnorm(fit6, ~ranef(.), id=0.05)

## Each of the estimated coefficients need to be randomly distributed...

ran.int <- ranef(mod_cond_RMSSD_curve_pd)[1] # create object with the random intercept estimates
ran.int <- unlist(ran.int) 
names(ran.int) <- NULL 
P_ID <- levels(data_pl$P_ID) # create an ID variable
plot(ran.int~P_ID,ylab="Random intercepts",xlab="Participant IDs")
## 
P_ID[which.max(ran.int)]

plot(unlist(ranef(mod_cond_SDNN_curve_pd)[1])~P_ID,ylab="Random intercepts",xlab="Participant IDs")
P_ID[which.max(unlist(ranef(mod_cond_SDNN_curve_pd)[1]))]
P_ID[order(-unlist(ranef(mod_cond_SDNN_curve_pd)[1]))]

plot(unlist(ranef(mod_cond_n_HF_curve_pd)[1])~P_ID,ylab="Random intercepts",xlab="Participant IDs")
P_ID[which.max(unlist(ranef(mod_cond_n_HF_curve_pd)[1]))]
P_ID[order(-unlist(ranef(mod_cond_n_HF_curve_pd)[1]))]

plot(unlist(ranef(mod_cond_LF.HF_curve_pd)[1])~P_ID,ylab="Random intercepts",xlab="Participant IDs")
P_ID[which.max(unlist(ranef(mod_cond_LF.HF_curve_pd)[1]))]
P_ID[order(-unlist(ranef(mod_cond_LF.HF_curve_pd)[1]))]

## Random slopes
plot(unlist(ranef(mod_cond_RMSSD_curve_pd)[2])~P_ID,ylab="Random slopes",xlab="Participant IDs")
P_ID[which.max(unlist(ranef(mod_cond_RMSSD_curve_pd)[2]))]
P_ID[order(-unlist(ranef(mod_cond_RMSSD_curve_pd)[2]))]

plot(unlist(ranef(mod_cond_SDNN_curve_pd)[2])~P_ID,ylab="Random slopes",xlab="Participant IDs")
P_ID[which.max(unlist(ranef(mod_cond_SDNN_curve_pd)[2]))]
P_ID[order(-unlist(ranef(mod_cond_SDNN_curve_pd)[2]))]

plot(unlist(ranef(mod_cond_n_HF_curve_pd)[2])~P_ID,ylab="Random slopes",xlab="Participant IDs")
P_ID[which.max(unlist(ranef(mod_cond_n_HF_curve_pd)[2]))]
P_ID[order(-unlist(ranef(mod_cond_n_HF_curve_pd)[2]))]

plot(unlist(ranef(mod_cond_LF.HF_curve_pd)[2])~P_ID,ylab="Random slopes",xlab="Participant IDs")
P_ID[which.max(unlist(ranef(mod_cond_LF.HF_curve_pd)[2]))]
P_ID[order(-unlist(ranef(mod_cond_LF.HF_curve_pd)[2]))]

nor_metric<-function(x){                                                      ############ #NN 
  print("AD test")
  print("CVM test")
  print("Lillie test")
  #print("DW test")
  #print("BP NCV test")
  print(ad.test(x$residuals/(summary(x)$sigma))$p.value)
  print(cvm.test(x$residuals/(summary(x)$sigma))$p.value)
  print(lillie.test(x$residuals/(summary(x)$sigma))$p.value)
  #print(as.numeric(bptest(x)$p.value))
  #print(as.numeric(dwtest(x)$p.value))
  #print(as.numeric(ncvTest(x)$p.value))
}

## ncvTest(fit)  ## Brusch-Pagan test

## <-ad.test(coxed$residuals/(summary(coxed)$sigma))$p.value 

#names(mod_cond_RMSSD_curve_pd)

nor_metric(mod_cond_RMSSD_curve_pd)
nor_metric(mod_cond_SDNN_curve_pd)
nor_metric(mod_cond_n_HF_curve_pd)
nor_metric(mod_cond_LF.HF_curve_pd)

 plot(mod_cond_RMSSD_curve_pd)
 hist(mod_cond_RMSSD_curve_pd$residuals,breaks=1000)
 hist(mod_cond_SDNN_curve_pd$residuals,breaks=1000)
 hist(mod_cond_n_HF_curve_pd$residuals,breaks=1000)
 hist(mod_cond_LF.HF_curve_pd$residuals,breaks=1000)
qqnorm(mod_cond_RMSSD_curve_pd$residuals) 

hist(data_a$LF.HF,breaks=1000)
hist(data_a$n_HF,breaks=1000)
hist(data_a$SDNN,breaks=1000)
hist(data_a$RMSSD,breaks=1000)


####
####

### Same: ML as REML 
mod_cond_RMSSD_curve_pd_s_allr <- lme(fixed = RMSSD ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_pl,method="REML")  
mod_cond_SDNN_curve_pd_s_allr <- lme(fixed = SDNN ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_pl,method="REML")  
mod_cond_n_HF_curve_pd_s_allr <- lme(fixed = n_HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_pl,method="REML")  
mod_cond_LF.HF_curve_pd_s_allr <- lme(fixed = LF.HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_pl,method="REML")  

system.time(
### Try all covariance here... (same as SPSS last time)
mod_cond_RMSSD_curve_pd_s_lastM <- lme(fixed = RMSSD ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2)  + Pressure_s + CO2_s , random = list(P_ID=~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2 )),control=lmeControl(opt="optim"),data=data_pl,method="REML")  
)
mod_cond_SDNN_curve_pd_s_lastM <- lme(fixed = SDNN ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) +  Pressure_s + CO2_s , random = list(P_ID=~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2 )),control=lmeControl(opt="optim"),data=data_pl,method="REML")  
mod_cond_n_HF_curve_pd_s_lastM <- lme(fixed = n_HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) +  Pressure_s + CO2_s , random = list(P_ID=~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2 )),control=lmeControl(opt="optim"),data=data_pl,method="REML")  
mod_cond_LF.HF_curve_pd_s_lastM <- lme(fixed = LF.HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) +  Pressure_s + CO2_s , random = list(P_ID=~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2 )),control=lmeControl(opt="optim"),data=data_pl,method="REML")  

###### Bring some stability to bring back example: - Parsimony! - DOESNT CHANGE THINGS MUCH!!!
mod_cond_RMSSD_curve_pd_s_par <- lme(fixed = RMSSD ~ 1+ ToD + DoW + Age + Gender + BMI + I(Temperature_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_pl,method="ML")  
mod_cond_SDNN_curve_pd_s_par <- lme(fixed = SDNN ~ 1  + ToD + DoW  + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2)  + CO2_s , random = list(P_ID=pdDiag(~ Pressure_s +  Sound_s + Temperature_s + Relative_humidity_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_pl,method="ML")  
mod_cond_n_HF_curve_pd_s_par <- lme(fixed = n_HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_pl,method="REML")  
mod_cond_LF.HF_curve_pd_s_par <- lme(fixed = LF.HF ~ 1+ ToD + DoW + place + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + I(CO2_s^2))),data=data_pl,method="REML")  

## Only for C1 & C2, then C3 & C4, C1, C2, C3, C4: SDNN

summary(data_pl$cohort)

datc1c2 <- data_pl[data_pl$cohort %in% c("cohort1","cohort2"),] 
datc3c4 <- data_pl[data_pl$cohort %in% c("cohort3","cohort4"),]
datc1 <- data_pl[data_pl$cohort %in% c("cohort1"),]
datc2 <- data_pl[data_pl$cohort %in% c("cohort2"),]
datc3 <- data_pl[data_pl$cohort %in% c("cohort3"),]
datc4 <- data_pl[data_pl$cohort %in% c("cohort4"),]

t <- as.POSIXct(data_a$Timestamp)

mod_cond_SDNN_curve_pd_s_c1c2 <- lme(fixed = SDNN ~ 1+ ToD + DoW  + Age + Gender + BMI  + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) )),data=datc1c2,method="ML")  
mod_cond_SDNN_curve_pd_s_c3c4 <- lme(fixed = SDNN ~ 1+ ToD + DoW+ Age + Gender + BMI   + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2))),data=datc3c4,method="ML")  
mod_cond_SDNN_curve_pd_s_c1 <- lme(fixed = SDNN ~ 1+ ToD + DoW + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) )),data=datc1,method="REML")  
mod_cond_SDNN_curve_pd_s_c2 <- lme(fixed = SDNN ~ 1+ ToD + DoW  + Age + Gender + BMI + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) )),data=datc2,method="REML")  
mod_cond_SDNN_curve_pd_s_c3 <- lme(fixed = SDNN ~ 1+ ToD + DoW  + Age + Gender + BMI  + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2)+ Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) )),data=datc3,method="ML")  
mod_cond_SDNN_curve_pd_s_c4 <- lme(fixed = SDNN ~ 1+ ToD + DoW  + Age + Gender + BMI  + Sound_s + Temperature_s + Relative_humidity_s + I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) + Pressure_s + CO2_s , random = list(P_ID=pdDiag(~ Sound_s + Temperature_s + Relative_humidity_s + Pressure_s + CO2_s+ I(Sound_s^2) + I(Temperature_s^2) + I(Relative_humidity_s^2) )),data=datc4,method="ML")  

