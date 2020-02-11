#Packages: haven, dplyr, psych, corrplot

### Importing and Cleaning Data
# Klinke, S., & Wagner, C. (2008). Visualizing exploratory factor analysis models (No. 2008, 012). SFB 649 discussion paper.

# Import sav file as data frame
library(haven)
sms_sample0 <- read_spss('S:/shared/PPALab/SMS PA 2/Data/SMSPA2_surveydata_RAW.sav')

sms_sample0 <- filter(sms_sample0, Sample == 0)

library(psych)
library(dplyr)
library(GPArotation)
# Age descriptive stats
summary(sms_sample0$AGE)
sd(sms_sample0$AGE, na.rm = TRUE) #standard deviation


# Percent female 
sum(sms_sample0$GENDER == 2, na.rm = TRUE) / nrow(sms_sample0) * 100
sum(sms_sample0$GENDER == 1, na.rm = TRUE) / nrow(sms_sample0) * 100 #male
sum(sms_sample0$GENDER == 5, na.rm = TRUE) / nrow(sms_sample0) * 100 #prefer not to ans
sum(is.na(sms_sample$GENDER)) / nrow(sms_sample0) * 100 #na

# Percent African American
sum(sms_sample0$RACE == 1, na.rm = T) / nrow(sms_sample0) * 100
sum(sms_sample0$RACE == 2, na.rm = T) / nrow(sms_sample0) * 100 #asian/pacificislander
sum(sms_sample0$RACE == 3, na.rm = T) / nrow(sms_sample0) * 100 #white
sum(sms_sample0$RACE == 4, na.rm = T) / nrow(sms_sample0) * 100 #latino
sum(is.na(sms_sample0$RACE)) / nrow(sms_sample0) * 100 #na

# Duration of activity
sms_sample0$LENGTH_MIN[sms_sample0$LENGTH_MIN == 999] <- NA
summary(sms_sample0$LENGTH_MIN)
sd(sms_sample0$LENGTH_MIN, na.rm = T)

# Percent activity type
sum(sms_sample0$CLASS == "PEACT 118") / nrow(sms_sample0) * 100 
sum(sms_sample0$CLASS == "PEACT 112") / nrow(sms_sample0) * 100 
sum(sms_sample0$CLASS == "UREC") / nrow(sms_sample0) * 100 







# Create new data frame with columns from SMindM_1 to AcceptB_13 
sms_sample0 <- sms_sample0 %>% 
  select(SMindM_1:AcceptB_13)

# Count NA's, replace '88's with NA, recount NA's
sum(is.na(sms_sample0))
is.na(sms_sample0) <- sms_sample0 == 88
is.na(sms_sample0) <- sms_sample0 > 4
sum(is.na(sms_sample0))

library(BaylorEdPsych)
library(mvnmle)
LittleMCAR(sms_sample0)

# Name data frame 'efadat'
efadat <- sms_sample0


# Count rows with missing values
row.has.na <- apply(efadat, 1, function(x){any(is.na(x))})
sum(row.has.na)

# Percent missing values by variable
percentmiss <- function(x){
  sum(is.na(x)) / length(x) * 100} 

apply(efadat, 2, percentmiss)

# Number of subjects with missing values by percent
table(apply(efadat, 1, percentmiss))



# Correlation Matrix
correlation <- cor(efadat, use = "pairwise.complete.obs") #pairwise deletion

# Correlogram w/significance using corrplot
library(corrplot)
correlation.pvalues <- cor.mtest(correlation)

corrplot(correlation, type = "upper", order = "hclust",
         p.mat = correlation.pvalues[["p"]], sig.level = .01)

### Suitability of Data
# Dziuban, C. D., & Shirkey, E. C. (1974). When is a correlation matrix appropriate for factor analysis? Some decision rules. Psychological bulletin, 81(6), 358.

# Conduct Bartlett's test of sphericity 
# (test for difference between correlation matrix and identity matrix)
# and Kaiser-Meyer-Olkin test (measure of sampling adequacy)

# 0.00 to 0.49 unacceptable.
# 0.50 to 0.59 miserable.
# 0.60 to 0.69 mediocre.
# 0.70 to 0.79 middling.
# 0.80 to 0.89 meritorious.
# 0.90 to 1.00 marvelous

cortest.bartlett(correlation, n = nrow(efadat)) 

KMO(correlation)
    
### Normality w MVN package
#https://cran.r-project.org/web/packages/MVN/vignettes/MVN.pdf

library(MVN)
result <- mvn(efadat, mvnTest = "mardia")
result$multivariateNormality


### efadat <- efadat %>% mutate_if(is.numeric,as.factor)
    
### Estimating Number of Factors
# Parallel analysis
#Compare scree of factors w/ random data of same size, factor method = maximal likelihood, fa = factor analysis
# parallel <-
#   fa.parallel(efadat,
#               fm="ml", fa="fa",
#               n.iter = 50,
#               SMC = TRUE,
#               quant = .95)

parallel <- 
  fa.parallel(efadat, 
              fm="ml", fa="fa") 


# https://sakaluk.wordpress.com/2016/05/26/11-make-it-pretty-scree-plots-and-parallel-analysis-using-psych-and-ggplot2/

obs = data.frame(parallel$fa.values)
obs$type = c('Observed Data')
obs$num = c(row.names(obs))
obs$num = as.numeric(obs$num)
colnames(obs) = c('eigenvalue', 'type', 'num')

View(obs)
#Calculate quantiles for eigenvalues, but only store those from simulated CF model in percentile1
percentile = apply(parallel$values,2,function(x) quantile(x,.95))
min = as.numeric(nrow(obs))
min = (4*min) - (min-1)
max = as.numeric(nrow(obs))
max = 4*max
percentile1 = percentile[min:max]

# Create data frame called sim; with simulated eigenvalue data
sim = data.frame(percentile1)
sim$type = c('Simulated Data (95th %ile)')
sim$num = c(row.names(obs))
sim$num = as.numeric(sim$num)
colnames(sim) = c('eigenvalue', 'type', 'num')

#Merge the two data frames (obs and sim) together into data frame called eigendat
eigendat = rbind(obs,sim)
library(ggplot2)
apatheme=theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text=element_text(family='Arial'),
        legend.title=element_blank(),
        legend.position=c(.7,.8),
        axis.line.x = element_line(color='black'),
        axis.line.y = element_line(color='black'))

#Use data from eigendat. Map number of factors to x-axis, eigenvalue to y-axis, and give different data point shapes depending on whether eigenvalue is observed or simulated
p = ggplot(eigendat, aes(x=num, y=eigenvalue, shape=type)) +
  #Add lines connecting data points
  geom_line()+
  #Add the data points.
  geom_point(size=4)+
  #Label the y-axis 'Eigenvalue'
  scale_y_continuous(name='Eigenvalue')+
  #Label the x-axis 'Factor Number', and ensure that it ranges from 1-max # of factors, increasing by one with each 'tick' mark.
  scale_x_continuous(name='Factor Number', breaks=min(eigendat$num):max(eigendat$num))+
  #Manually specify the different shapes to use for actual and simulated data, in this case, white and black circles.
  scale_shape_manual(values=c(16,1)) +
  #Add vertical line indicating parallel analysis suggested max # of factors to retain
  geom_vline(xintercept = parallel$nfact, linetype = 'dashed')+
  #Apply our apa-formatting theme
  apatheme
# #Call the plot. Looks pretty!
# p
# # ggsave('parallel.png', width=6, height=6, unit='in', dpi=300)

#Return eigenvalues, and number of eigenvalues over 0.7
parallel$fa.values
sum(parallel$fa.values > .7) #new Kaiser criteria 



### Factor Loadings

# Simple structure with x factor model
#R = direct oblimin

nf = 4 #number of factors

fa(efadat, nf = 4,
   rotate = "promax", 
   fm = "ml")

# Remove AcceptM_5 (17) for conceptual reasons
# Remove AcceptM_7 (19) for loading > 0.3 on 2 factors
# Remove AcceptB_1 (20) for loading > 0.3 on 2 factors
# Remove AcceptB_2 (21) for conceptual reasons
initial<-efadat[ , -c(17, 19, 20, 21)] %>% 
  fa(nfactors = nf,
   rotate = "promax",
   fm = "ml")

# Remove AcceptB_3 (22) for loading > 0.3 on 2 factors
# Remove AcceptB_4 (23) for loading > 0.3 on 2 factors
# Remove AcceptB_5 (24) for loading > 0.3 on 2 factors
efadat[ , -c(17, 19, 20, 21:24)] %>% 
  fa(nfactors = nf,
     rotate = "promax",
     fm = "ml")

# Remove AcceptB_8, AcceptB_9, and AcceptB_10 (27,28,29) for being too similar
efadat[ , -c(17, 19, 20, 21:24, 27:29)] %>% 
  fa(nfactors = nf,
     rotate = "promax",
     fm = "ml")

# Remove SMindM_6 SMindB_1 to reduce items
efadat[ , -c(6, 7, 17, 19, 20, 21, 22:24, 27:29)] %>% 
  fa(nfactors = nf,
     rotate = "promax",
     fm = "ml")



finalmodel<- efadat[ , c(1, 3, 4, 5,                #factor1
             9, 10,  11, 12,               #factor2
            15, 16, 18,         #factor3
            22, 23, 24, 25       #factor4
            )] %>% 
  fa(nfactors = nf,
     rotate = "promax",
     fm = "ml")




# RMSR (root mean square of residual) < 0.05 
# TLI > 0.95 
# RMSEA (root mean square error of approx.) < 0.05 


# #CFI
finalmodel <- fa(efadat[ , -c()], nfactors = nf,
  rotate = "promax",
    fm = "ml")
1-((finalmodel$STATISTIC-finalmodel$dof) /
      (finalmodel$null.chisq-finalmodel$null.dof))

### reliability analysis
SMindM <- c(1,3,4,5)
SMindB <- c(9,10,11,12)
AcceptM <- c(15,16,18)
AcceptB <- c(22,23,24,25)

alpha(efadat[, SMindM])
alpha(efadat[, SMindB])
alpha(efadat[, AcceptM])
alpha(efadat[, AcceptB])

#exploratory bifactor

#update samples
#import to xl








    

