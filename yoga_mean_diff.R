library(haven)
sms1 <- read_spss("S:/shared/PPALab/SMS PA 2/Data/SMSPA2_surveydata_RAW.sav")


library(dplyr)
library(car)
library(psych)

# Reverse STAI_1,4,5 scores
sms1[, c("STAI_1", "STAI_4", "STAI_5")] <-
  5-sms1[, c("STAI_1", "STAI_4", "STAI_5")]

# Reverse SBS_1,2,3,6,7 scores
sms1[, c("SBS_1", "SBS_2", "SBS_3", "SBS_6", "SBS_7")] <-
  8-sms1[, c("SBS_1", "SBS_2", "SBS_3", "SBS_6", "SBS_7")]

# Reverse al MAAS scores
sms1[, c(73:87)] <- 7-sms1[, c(73:87)]



#new column for variable means
sms1$SMindM_MEAN <- rowMeans(select(sms1, SMindM_1, SMindM_3, SMindM_4, SMindM_5))
sms1$SMindB_MEAN <- rowMeans(select(sms1, SMindB_3, SMindB_4, SMindB_5, SMindB_6))
sms1$AcceptM_MEAN <- rowMeans(select(sms1, AcceptM_3, AcceptM_4, AcceptM_6))
sms1$AcceptB_MEAN <- rowMeans(select(sms1, AcceptB_3, AcceptB_4, AcceptB_5, AcceptB_6))

sms1$SBS_MEAN <- rowMeans(select(sms1, SBS_1:SBS_7))
sms1$SIM_MEAN <- rowMeans(select(sms1, SIM_1:SIM_4))
sms1$SBA_MEAN <- rowMeans(select(sms1, SBA_1:SBA_9))
sms1$STAI_MEAN <- rowMeans(select(sms1, STAI_1:STAI_6)) 
sms1$BREQ_MEAN <- rowMeans(select(sms1, BREQ_1:BREQ_4))
sms1$MAAS_MEAN <- rowMeans(select(sms1, MAAS_1:MAAS_15))






smsyoga <- filter(sms1, CLASS == "PEACT 118")
smsnotyoga <- filter(sms1, CLASS != "PEACT 118")


# T-Test of variable means between yoga and non yoga groups
# Test of normality





