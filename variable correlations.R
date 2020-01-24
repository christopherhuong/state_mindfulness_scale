library(haven)
sms1 <- read_spss("/users/christopherhuong/downloads/SMSPA2.sav")

STAI <- select(sms1, STAI_1:STAI_6)
sum(is.na(STAI))
is.na(sms1) <- sms1 == 88
is.na(sms1) <- sms1 > 4




library(car)
table(sms$STAI_2) #Check score frequencies
sms1$STAI_2 = recode(sms1$STAI_2, "1=4; 2=3; 3=2; 4=1") #reverse score
sms1$STAI_3 = recode(sms1$STAI_3, "1=4; 2=3; 3=2; 4=1")
sms1$STAI_6 = recode(sms1$STAI_6, "1=4; 2=3; 3=2; 4=1")



library(dplyr)

sms1$STAI_MEAN <- rowMeans(select(sms, STAI_1:STAI_6)) #new column that gives mean of STAI_1:STAI_6

sms1$BREQ_MEAN <- rowMeans(select(sms, BREQ_1:BREQ_4))

library(ggplot2)

ggplot(sms, aes(x = sms1$BREQ_MEAN, y = sms1$STAI_MEAN)) + 
        geom_point()
       
cor.test(sms1$STAI_MEAN, sms1$BREQ_MEAN,
         method = "pearson")



# Mindfulness = average within each 4 factor
# SBS = average of 7 scores w reverse coding (high score = more body surveilence)
# SBA = average
# STAI - find article
# BREQ 3 - intrinsic motivation averaged 4 items
# MAAS - reverse all, average (high score = more mindful)
# remembered affect
# forecasted affect

# internal consistency and reliability with each variable
# 


# 11x11 correlation matrix
