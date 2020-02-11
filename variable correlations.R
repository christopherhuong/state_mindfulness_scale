library(haven)
sms1 <- read_spss("S:/shared/PPALab/SMS PA 2/Data/SMSPA2_surveydata_RAW.sav")

library(dplyr)


sum(is.na(sms1))
is.na(sms1) <- sms1 == 88

#percent female 
sum(sms1$GENDER==2, na.rm = T) / nrow(sms1) * 100
#percent caucasian
sum(sms1$RACE==3, na.rm = T) / nrow(sms1) * 100
# Percent activity type
sum(sms1$CLASS == "PEACT 118") / nrow(sms1) * 100 
sum(sms1$CLASS == "PEACT 112") / nrow(sms1) * 100 
sum(sms1$CLASS == "UREC") / nrow(sms1) * 100 
#duration of activity
sms1$LENGTH_MIN[sms1$LENGTH_MIN == 999] <- NA
summary(sms1$LENGTH_MIN)
sd(sms_sample0$LENGTH_MIN, na.rm = T)

library(car)

# Reverse STAI_1,4,5 scores
table(sms1$STAI_1) #Check score frequencies

sms1[, c("STAI_1", "STAI_4", "STAI_5")] <-
  5-sms1[, c("STAI_1", "STAI_4", "STAI_5")]


# Reverse SBS_1,2,3,6,7 scores
table(sms1$SBS_1)

sms1[, c("SBS_1", "SBS_2", "SBS_3", "SBS_6", "SBS_7")] <-
  8-sms1[, c("SBS_1", "SBS_2", "SBS_3", "SBS_6", "SBS_7")]


# Reverse al MAAS scores
table(sms1$MAAS_2)

sms1[, c(73:87)] <-
  7-sms1[, c(73:87)]



#new column for 4 factors
sms1$SMindM_MEAN <- rowMeans(select(sms1, SMindM_1, SMindM_3, SMindM_4, SMindM_5))
sms1$SMindB_MEAN <- rowMeans(select(sms1, SMindB_3, SMindB_4, SMindB_5, SMindB_6))
sms1$AcceptM_MEAN <- rowMeans(select(sms1, AcceptM_3, AcceptM_4, AcceptM_6))
sms1$AcceptB_MEAN <- rowMeans(select(sms1, AcceptB_3, AcceptB_4, AcceptB_5, AcceptB_6))



sms1$SMindM_ORIGINAL <- rowMeans(select(sms1, SMindM_1, SMindM_2, SMindM_3, SMindM_4, SMindM_5, SMindM_6))
sms1$SMindB_ORIGINAL <- rowMeans(select(sms1, SMindB_1, SMindB_2, SMindB_3, SMindB_4, SMindB_5, SMindB_6))

#new column for SBS mean
sms1$SBS_MEAN <- rowMeans(select(sms1, SBS_1:SBS_7))

#new column for SIM mean
sms1$SIM_MEAN <- rowMeans(select(sms1, SIM_1:SIM_4))

#new column for SBA mean
sms1$SBA_MEAN <- rowMeans(select(sms1, SBA_1:SBA_9))

#new column for STAI mean
sms1$STAI_MEAN <- rowMeans(select(sms1, STAI_1:STAI_6)) 

#new column for BREQ 3 intrinsic motivation
sms1$BREQ_MEAN <- rowMeans(select(sms1, BREQ_1:BREQ_4))

#new column for mean of MAAS
sms1$MAAS_MEAN <- rowMeans(select(sms1, MAAS_1:MAAS_15))


#New data frame with variables to correlate
VAR_COR <- select(sms1, Rem_Affect, For_Affect, 
                  SMindM_MEAN, SMindB_MEAN, AcceptM_MEAN, AcceptB_MEAN,
                  SBS_MEAN, SIM_MEAN, SBA_MEAN,
                  STAI_MEAN, BREQ_MEAN, MAAS_MEAN, SMindM_ORIGINAL, SMindB_ORIGINAL)




library(psych)
cor_matrix <- cor(VAR_COR, use = "pairwise.complete.obs") #casewise deletion
cor_matrix


# Correlogram w/significance using corrplot
library(corrplot)
correlation.pvalues <- cor.mtest(cor_matrix)

corrplot(cor_matrix, method = "number",col="black", type = "upper", order = "original",
         p.mat = correlation.pvalues[["p"]], sig.level = .05)

corrplot(cor_matrix, method = "number",col="black", tl.col="black", type = "upper")



# Cronbachs alpha, standardized w CI
library(ltm)

SBS_DF <- select(sms1, SBS_1:SBS_7)



cronbach.alpha(VAR_COR, standardized = T, CI = T, na.rm = T)

## items within each variable



library(ggplot2)

ggplot(sms, aes(x = sms1$BREQ_MEAN, y = sms1$STAI_MEAN)) + 
        geom_point()
       
cor.test(sms1$STAI_MEAN, sms1$BREQ_MEAN,
         method = "pearson")



# Mindfulness = average within each 4 factor
# SBS = average of 7 scores w reverse coding (high score = more body surveilence)
# SIM
# SBA = average
# STAI - find article
# BREQ 3 - intrinsic motivation averaged 4 items
# MAAS - reverse all, average (high score = more mindful)
# remembered affect  Rem_Affect
# forecasted affect  For_Affect

# internal consistency and reliability with each variable
alpha(select(sms1, SMindM_1:SMindM_6))
alpha(select(sms1, SMindB_1:SMindB_6))
alpha(select(sms1, AcceptM_1:AcceptM_7))
alpha(select(sms1, AcceptB_1:AcceptB_13))

alpha(select(sms1, SMindM_1:AcceptB_13))
