library(haven)
sms_sample1 <- read_spss('S:/shared/PPALab/SMS PA 2/Data/SMSPA2_surveydata_jan2020sample1_11.sav')

library(dplyr)
sms_sample1 <- select(sms_sample1, SMindM_1:AcceptB_13)


# Count NA's, replace '88's with NA, recount NA's
sum(is.na(sms_sample1))
is.na(sms_sample1) <- sms_sample1 == 88
is.na(sms_sample1) <- sms_sample1 > 4
sum(is.na(sms_sample1))



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

