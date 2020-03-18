library(haven)
sms_sample1 <- read_spss('S:/shared/PPALab/SMS PA 2/Data/SMSPA2_EM394.sav')
sms_sample1 <- filter(sms_sample1, Sample == 1)

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

# Count rows with missing values
row.has.na <- apply(sms_sample1, 1, function(x){any(is.na(x))})
sum(row.has.na)

# Percent missing values by variable
percentmiss <- function(x){
  sum(is.na(x)) / length(x) * 100} 

apply(sms_sample1, 2, percentmiss)

# Number of subjects with missing values by percent
table(apply(sms_sample1, 1, percentmiss))



library(lavaan)

model <-
'SMindM =~ SMindM_2 + SMindM_3 + SMindM_6
SMindB =~  SMindB_2 + SMindB_3 + SMindB_4 + SMindB_6
AcceptM =~ AcceptM_3 + AcceptM_4 + AcceptM_6
AcceptB =~ AcceptB_6 + AcceptB_7 + AcceptB_8'

# Default = unit loading identification
# Unit variance identification -> 'std.lv = T'
fit <- cfa(model, data = sms_sample1, estimator = "WLSMVS")

summary(fit, fit.measures = TRUE, rsq = TRUE, standardized = TRUE)



bf.model2 <-
  'gen.factor =~ SMindM_1 + SMindM_3 + SMindM_4 + SMindM_5 + 
SMindB_3 + SMindB_4 + SMindB_5 + SMindB_6 + 
AcceptM_3 + AcceptM_4 + AcceptM_6 +
AcceptB_3 + AcceptB_4 + AcceptB_5 + AcceptB_6
body.factor =~ SMindB_3 + SMindB_4 + SMindB_5 + SMindB_6 + 
AcceptB_3 + AcceptB_4 + AcceptB_5 + AcceptB_6
mind.factor =~ SMindM_1 + SMindM_3 + SMindM_4 + SMindM_5 + 
AcceptM_3 + AcceptM_4 + AcceptM_6'


bf.fit2 <- cfa(bf.model2, data = efadat, orthogonal = T)

summary(bf.fit2, fit.measures = T, rsq = T, standardized = T)


bf.model4 <-
  'gen.factor =~ SMindM_1 + SMindM_3 + SMindM_4 + SMindM_5 + 
SMindB_3 + SMindB_4 + SMindB_5 + SMindB_6 + 
AcceptM_3 + AcceptM_4 + AcceptM_6 +
AcceptB_3 + AcceptB_4 + AcceptB_5 + AcceptB_6
SMindM =~ SMindM_1 + SMindM_3 + SMindM_4 + SMindM_5
SMindB =~  SMindB_3 + SMindB_4 + SMindB_5 + SMindB_6
AcceptM =~ AcceptM_3 + AcceptM_4 + AcceptM_6
AcceptB =~ AcceptB_3 + AcceptB_4 + AcceptB_5 + AcceptB_6'

bf.fit4 <- cfa(bf.model4, data = efadat, estimator = "WLSMVS")

summary(bf.fit4, fit.measures = T, rsq = T, standardized = T)







