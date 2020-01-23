library(haven)
sms_sample1 <- read_spss('/users/christopherhuong/downloads/SMSPA2_sample1.sav')

library(dplyr)
sms_sample1 <- select(sms_sample1, SMindM_1:AcceptB_13)

library(lavaan)

model <-
'SMindM =~ SMindM_1 + SMindM_2 + SMindM_3 + SMindM_4 + SMindM_5
SMindB =~ SMindB_2 + SMindB_3 + SMindB_4 + SMindB_5 + SMindB_6
AcceptM =~ AcceptM_1 + AcceptM_2 + AcceptM_3 + AcceptM_4 + AcceptM_6
AcceptB =~ AcceptB_6 + AcceptB_7 + AcceptB_11 + AcceptB_12 + AcceptB_13'

# Default = unit loading identification
# Unit variance identification -> 'std.lv = T'
fit <- cfa(model, data = sms_sample1, estimator = "WLSMVS")

summary(fit, fit.measures = TRUE, rsq = TRUE)

