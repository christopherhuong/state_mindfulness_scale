library(haven)
sms1 <- read_spss("S:/shared/PPALab/SMS PA 2/Data/SMSPA2_surveydata_RAW.sav")

library(dplyr)

STAI <- select(sms1, STAI_1:STAI_6)
sum(is.na(STAI))
is.na(sms1) <- sms1 == 88
is.na(sms1) <- sms1 > 4




library(car)

# Reverse STAI_2,3,6 scores
table(sms1$STAI_2) #Check score frequencies
sms1$STAI_2 = recode(sms1$STAI_2, "1=4; 2=3; 3=2; 4=1") 
sms1$STAI_3 = recode(sms1$STAI_3, "1=4; 2=3; 3=2; 4=1")
sms1$STAI_6 = recode(sms1$STAI_6, "1=4; 2=3; 3=2; 4=1")


# Reverse SBS_1,2,3,6,7 scores
table(sms1$SBS_1)
sms1$SBS_1 = recode(sms1$SBS_1, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1") 
sms1$SBS_2 = recode(sms1$SBS_2, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
sms1$SBS_3 = recode(sms1$SBS_3, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
sms1$SBS_6 = recode(sms1$SBS_6, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
sms1$SBS_7 = recode(sms1$SBS_7, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")


# Reverse al MAAS scores
table(sms1$MAAS_1)
sms1$MAAS_1 = recode(sms1$MAAS_1, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1")
sms1$MAAS_2 = recode(sms1$MAAS_2, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1")
sms1$MAAS_3 = recode(sms1$MAAS_3, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1")
sms1$MAAS_4 = recode(sms1$MAAS_4, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1")
sms1$MAAS_5 = recode(sms1$MAAS_5, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1")
sms1$MAAS_6 = recode(sms1$MAAS_6, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1")
sms1$MAAS_7 = recode(sms1$MAAS_7, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1")
sms1$MAAS_8 = recode(sms1$MAAS_8, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1")
sms1$MAAS_9 = recode(sms1$MAAS_9, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1")
sms1$MAAS_10 = recode(sms1$MAAS_10, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1")
sms1$MAAS_11 = recode(sms1$MAAS_11, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1")
sms1$MAAS_12 = recode(sms1$MAAS_12, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1")
sms1$MAAS_13 = recode(sms1$MAAS_13, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1")
sms1$MAAS_14 = recode(sms1$MAAS_14, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1")
sms1$MAAS_15 = recode(sms1$MAAS_15, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1")



#new column for 4 factors
sms1$SMindM_MEAN <- rowMeans(select(sms1, SMindM_1, SMindM_3, SMindM_4, SMindM_5))
sms1$SMindB_MEAN <- rowMeans(select(sms1, SMindB_3, SMindB_4, SMindB_5, SMindB_6))
sms1$AcceptM_MEAN <- rowMeans(select(sms1, AcceptM_3, AcceptM_4, AcceptM_6))
sms1$AcceptB_MEAN <- rowMeans(select(sms1, AcceptB_3, AcceptB_4, AcceptB_5, AcceptB_6))

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
                  STAI_MEAN, BREQ_MEAN, MAAS_MEAN)




library(psych)
cor_matrix <- cor(VAR_COR, use = "complete.obs") #casewise deletion
cor_matrix
# Correlogram w/significance using corrplot
library(corrplot)
correlation.pvalues <- cor.mtest(cor_matrix)

corrplot(cor_matrix, method = "number", type = "upper",
         p.mat = correlation.pvalues[["p"]], sig.level = .05)








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
# 


# 11x11 correlation matrix
