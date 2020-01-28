library(haven)
sms_sample1 <- read_spss('S:/shared/PPALab/SMS PA 2/Data/SMSPA2_surveydata_jan2020sample1_11.sav')


# Age descriptive stats
summary(sms_sample1$AGE)
sd(sms_sample1$AGE, na.rm = TRUE) #standard deviation


# Percent female 
sum(sms_sample1$GENDER == 2, na.rm = TRUE) / nrow(sms_sample1) * 100
sum(sms_sample1$GENDER == 1, na.rm = TRUE) / nrow(sms_sample1) * 100 #male
sum(sms_sample1$GENDER == 5, na.rm = TRUE) / nrow(sms_sample1) * 100 #prefer not to ans
sum(is.na(sms_sample1$GENDER)) / nrow(sms_sample1) * 100 #na

# Percent African American
sum(sms_sample1$RACE == 1, na.rm = T) / nrow(sms_sample1) * 100
sum(sms_sample1$RACE == 2, na.rm = T) / nrow(sms_sample1) * 100 #asian/pacificislander
sum(sms_sample1$RACE == 3, na.rm = T) / nrow(sms_sample1) * 100 #white
sum(sms_sample1$RACE == 4, na.rm = T) / nrow(sms_sample1) * 100 #latino
sum(is.na(sms_sample1$RACE)) / nrow(sms_sample1) * 100 #na

# Duration of activity
sms_sample1$LENGTH_MIN[sms_sample1$LENGTH_MIN == 999] <- NA
summary(sms_sample1$LENGTH_MIN)
sd(sms_sample1$LENGTH_MIN, na.rm = T)

# Percent activity type
sum(sms_sample1$CLASS == "PEACT 118") / nrow(sms_sample1) * 100 
sum(sms_sample1$CLASS == "PEACT 112") / nrow(sms_sample1) * 100 
sum(sms_sample1$CLASS == "UREC") / nrow(sms_sample1) * 100 

library(dplyr)
sms_sample1 <- select(sms_sample1, SMindM_1:AcceptB_13)




# Count NA's, replace '88's with NA, recount NA's
sum(is.na(sms_sample1))
is.na(sms_sample1) <- sms_sample1 == 88
is.na(sms_sample1) <- sms_sample1 > 4
sum(is.na(sms_sample1))

library(BaylorEdPsych)
library(mvnmle)
LittleMCAR(sms_sample1)



library(lavaan)

model <-
'SMindM =~ SMindM_1 + SMindM_3 + SMindM_4 + SMindM_5
SMindB =~  SMindB_3 + SMindB_4 + SMindB_5 + SMindB_6
AcceptM =~ AcceptM_3 + AcceptM_4 + AcceptM_6
AcceptB =~ AcceptB_3 + AcceptB_4 + AcceptB_5 + AcceptB_6'

# Default = unit loading identification
# Unit variance identification -> 'std.lv = T'
fit <- cfa(model, data = sms_sample1, estimator = "WLSMVS")

summary(fit, fit.measures = TRUE, rsq = TRUE, standardized = TRUE)





