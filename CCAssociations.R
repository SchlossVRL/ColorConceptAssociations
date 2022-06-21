
####################################################################
## Analyses for paper: How are color-concept associations formed? ##
##             Melissa A. Schoenlein & Karen B. Schloss           ##
####################################################################


#Load in packages
library(psych)
library(dplyr)
library(lmSupport)
library(stringr)
library(ggplot2)
library(reshape2)
library(lme4)
library(psych)
library(car)
library(lmerTest)


#Set working directory 
setwd("C:/Users/melan/Dropbox/Research/Manuscripts/Submitted/OriginalAliens-Manuscript/DataAnalyses")
control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e6),check.nobs.vs.nRE = "ignore") 
gcontrol=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e3),check.nobs.vs.nRE = "ignore") 


# EXPERIMENT 1 ------------------------------------------------------------


# Exp1-Category learning --------------------------------------------------------

#Load in the data
dAll <- read.csv("Exp1-catLearning.csv")


#Model predicting correct from trial bin (every 40 trials) and two fit factors
m1Cat <- glmer(Correct ~TrialBin*ShapeNameC*ShapeColorC +(1+TrialBin|Subj),data=dAll,family=binomial, control = gcontrol) 
summary(m1Cat)


#Plot category learning data
dAll$MeanCorrect<- with(dAll, ave(Correct, TrialBin,Condition, FUN = function(x) mean(x, na.rm = TRUE)))
dAll$ShapeName = ifelse(dAll$Condition == 1 | dAll$Condition == 2, "fit","noFIt")
dAll$ShapeColor = ifelse(dAll$Condition == 1 | dAll$Condition == 3, "fit","noFit")


plotPoint = ggplot(data=dAll, aes(x = TrialBin, y = MeanCorrect)) +
  geom_point(aes(shape=ShapeName, size = 3,  color = ShapeColor)) +
  geom_line(aes(group = c(Condition)))+
  labs(x = 'Trial Bin', y = 'Mean Accuracy') +
  scale_color_hue()+
  theme_dark() + 
  ylim(.75,1)
plotPoint




# Exp1-Ratings ------------------------------------------------------------

#Load in the data
dAll = read.csv("Exp1-associations.csv")


#Filter to one row per participant
#Responses to the first questionnaire asking about strategy during category learning did not predict whether participants were noticers. 
dchi = dAll[!duplicated(dAll$Subj),]
table(dchi$UsedColorC,dchi$NoticedColorC)
testChi <- chisq.test(table(dchi$UsedColorC,dchi$NoticedColorC))
testChi




# Seen colors --------------------------------------------------------

#Filter to seen colors 
dSat = dAll[dAll$ColorSetStr == "Sat",]


#Correlations between co-occurrence frequency & associations 
#Warm colors
warmCorr = dSat[dSat$WarmCoolSet == 1,]
dwCorr <- warmCorr %>%
  group_by(ColorID,  Freq) %>%
  summarise(Ratings = mean(Ratings))
cor.test(dwCorr$Freq, dwCorr$Ratings)

#Cool colors
coolCorr = dSat[dSat$WarmCoolSet == -1,]
dcCorr <- coolCorr %>%
  group_by(ColorID,  Freq) %>%
  summarise(Ratings = mean(Ratings))
cor.test(dcCorr$Freq, dcCorr$Ratings)



# Model predicting associations for seen colors from co-occurrence frequency and two fit factors: shape-color & shape-name
m1 = lmer(Ratings ~ 1 + FreqC*ShapeNameC*ShapeColorC + (1 + FreqC|Subj), dSat)
summary(m1)
Anova(m1, type = 3, test = "F")




# Generalization to unseen colors --------------------------------------------------------

# Model predicting associations from co-occurrence frequency, exposure (seen vs. unseen), and delta E
m2 = lmer(Ratings ~ 1 +  deltaEC + FreqC + ExposureC  + (FreqC:deltaEC) + (FreqC:ExposureC) +  (1 + deltaEC + FreqC + ExposureC + (FreqC:deltaEC) + (FreqC:ExposureC)|Subj), dAll, control = control)
summary(m2)
Anova(m2, type = 3, test = "F")
anova(m2)





# Effects of noticing for seen colors --------------------------------------------------------


#Model predicting associations for seen colors from co-occurrence frequency and noticing (noticer vs. non-noticer)
m3 = lmer(Ratings ~ 1 + FreqC*NoticedColorC + (1 + FreqC|Subj), dSat)
summary(m3)
Anova(m3, type = 3, test = "F")


#Filter to noticers vs. non-noticers
noNoteSat<- dSat[dSat$NoticedColorC == -.5,]
noticedSat<- dSat[dSat$NoticedColorC == .5,]



#Noticers only: Model predicting associations for seen colors from co-occurrence frequency 
m3a = lmer(Ratings ~ 1 + FreqC + (1 + FreqC|Subj), noticedSat)
summary(m3a)
Anova(m3a, type = 3, test = "F")



#Non-noticers only: Model predicting associations for seen colors from co-occurrence frequency 
m3b = lmer(Ratings ~ 1 + FreqC + (1 + FreqC|Subj), noNoteSat)
summary(m3b)
Anova(m3b, type = 3, test = "F")



# Effects of noticing on generalization to unseen colors -----------------------------------------


# Model predicting associations from co-occurrence frequency, exposure (seen vs. unseen), delta E, and noticing
m4 = lmer(Ratings ~ 1 + NoticedColorC + deltaEC + FreqC + ExposureC + (FreqC:NoticedColorC) + (FreqC:deltaEC) + (FreqC:ExposureC) + (NoticedColorC:deltaEC)+ (NoticedColorC:ExposureC)+ (FreqC:NoticedColorC:deltaEC)+ (FreqC:NoticedColorC:ExposureC) + (1 + FreqC+deltaEC+ExposureC+ (FreqC:deltaEC) + (FreqC:ExposureC)|Subj), dAll, control = control)
summary(m4)
Anova(m4, type = 3, test = "F")
anova(m4)


#Filter to noticers vs. non-noticers
noticed<- dAll[dAll$NoticedColorC == .5,]
noNote<- dAll[dAll$NoticedColorC == -.5,]


#Noticers only: Model predicting associations from co-occurrence frequency, exposure, and delta E
m4a = lmer(Ratings ~ 1 +  deltaEC + FreqC + ExposureC + (FreqC:deltaEC) + (FreqC:ExposureC) + (1 + deltaEC + FreqC + ExposureC + (FreqC:deltaEC) + (FreqC:ExposureC)|Subj), noticed, control = control)
summary(m4a)
anova(m4a)
Anova(m4a, type = 3, test = "F")


#Non-noticers only: Model predicting associations from co-occurrence frequency, exposure, and delta E
m4b = lmer(Ratings ~ 1 +  deltaEC + FreqC + ExposureC + (FreqC:deltaEC) + (FreqC:ExposureC) + (1 + deltaEC + FreqC + ExposureC + (FreqC:deltaEC) + (FreqC:ExposureC)|Subj), noNote, control = control)
summary(m4b)
Anova(m4b, type = 3, test = "F")





# Exp1 - Ratings Plots -------------------------------------------------------

#Plot warm vs cool-biased separately

#Filter to Cool-biased species
dC = dAll[dAll$WarmCoolSet == -1,]
dCS = dC[dC$ExposureStr == "seen",] 

#Filter to Warm-biased species
dW = dAll[dAll$WarmCoolSet == 1,]
dWS = dW[dW$ExposureStr == "seen",] 


#SEEN COLORS ONLY

#Cool-biased species: Plot association by frequency for seen colors only
seenFreqC = aggregate(x= dCS$Ratings,             
                      by = list(dCS$Freq, dCS$ColorID, dCS$Freq),              
                      FUN = mean) 
seenFreqPlotC = ggplot(data=seenFreqC, aes(x = Group.1, y = x)) +
  geom_point(shape = 21,aes(size = Group.1,  fill = Group.2, stroke = 2)) + 
  scale_radius()+
  geom_smooth(method='lm', se = T)+
  labs(y = 'Rating', x = 'Freq ') +
  scale_color_hue()+
  theme_dark()+
  ylim(0,1)
seenFreqPlotC


#Warm-biased species: Plot association by frequency for seen colors only
seenFreqW = aggregate(x= dWS$Ratings,             
                      by = list(dWS$Freq, dWS$ColorID,  dWS$Freq),              
                      FUN = mean) 
seenFreqPlotW = ggplot(data=seenFreqW, aes(x = Group.1, y = x)) +
  geom_point(shape = 21,aes(size = Group.1, fill = Group.2, stroke = 2)) + 
  scale_radius()+
  geom_smooth(method='lm', se = T)+
  labs(y = 'Rating', x = 'Freq ') +
  scale_color_hue()+
  theme_dark()+
  ylim(0,1)
seenFreqPlotW



#GENERALIZATION TO UNSEEN COLORS

#cool-biased species: Plot associations by delta E, frequency, and exposure 
allDeltaC = aggregate(x= dC$Ratings,             
                      by = list(dC$deltaE, dC$Freq, dC$ColorID, dC$ColorSetStr, dC$Freq),              
                      FUN = mean) 
allDeltaPlotC = ggplot(data=allDeltaC, aes(x = Group.1, y = x, color = Group.3)) +
  geom_point(shape = 21,aes(size = Group.2, color = Group.4, fill = Group.3, stroke = 2)) + 
  scale_radius()+
  geom_smooth(method='lm', se = F, aes(size = Group.2))+
  labs(y = 'Rating', x = 'DeltaE ') +
  scale_color_hue()+
  theme_dark()+
  ylim(0,1)
allDeltaPlotC



#Warm-biased species: Plot associations by delta E, frequency, and exposure 
allDeltaW = aggregate(x= dW$Ratings,             
                      by = list(dW$deltaE, dW$Freq, dW$ColorID, dW$ColorSetStr, dW$Freq),              
                      FUN = mean) 
allDeltaPlotW = ggplot(data=allDeltaW, aes(x = Group.1, y = x, color = Group.3)) +
  geom_point(shape = 21,aes(size = Group.2, color = Group.4, fill = Group.3, stroke = 2)) + 
  scale_radius()+
  geom_smooth(method='lm', se = F, aes(size = Group.2))+
  labs(y = 'Rating', x = 'DeltaE ') +
  scale_color_hue()+
  theme_dark()+
  ylim(0,1)
allDeltaPlotW





# Plotting separate for noticers vs. non-noticers

#Cool-biased species: Plot association by frequency and noticing for seen colors only
seenFreqC = aggregate(x= dCS$Ratings,             
                      by = list( dCS$NoticedColorStr, dCS$Freq, dCS$ColorID, dCS$Freq),              
                      FUN = mean) 
seenFreqPlotC = ggplot(data=seenFreqC, aes(x = Group.2, y = x)) +
  geom_point(shape = 21,aes(size = Group.2,  fill = Group.3, stroke = 2)) + 
  scale_radius()+
  facet_wrap(~Group.1)+
  geom_smooth(method='lm', se = T)+
  labs(y = 'Rating', x = 'Freq ') +
  scale_color_hue()+
  theme_dark()+
  ylim(0,1)
seenFreqPlotC


#Warm-biased species: Plot association by frequency and noticing for seen colors only
seenFreqW = aggregate(x= dWS$Ratings,             
                      by = list( dWS$NoticedColorStr, dWS$Freq, dWS$ColorID,  dWS$Freq),              
                      FUN = mean) 
seenFreqPlotW = ggplot(data=seenFreqW, aes(x = Group.2, y = x)) +
  geom_point(shape = 21,aes(size = Group.2, fill = Group.3, stroke = 2)) + 
  scale_radius()+
  facet_wrap(~Group.1)+
  geom_smooth(method='lm', se = T)+
  labs(y = 'Rating', x = 'Freq ') +
  scale_color_hue()+
  theme_dark()+
  ylim(0,1)
seenFreqPlotW



#cool-biased species: Plot associations by delta E, frequency, exposure, and noticing 
allDeltaC = aggregate(x= dC$Ratings,             
                      by = list(dC$deltaE, dC$NoticedColorStr, dC$Freq, dC$ColorID, dC$ColorSetStr, dC$Freq),              
                      FUN = mean) 
allDeltaPlotC = ggplot(data=allDeltaC, aes(x = Group.1, y = x, color = Group.4)) +
  geom_point(shape = 21,aes(size = Group.3, color = Group.5, fill = Group.4, stroke = 2)) + 
  scale_radius()+
  facet_wrap(~Group.2)+
  geom_smooth(method='lm', se = F, aes(size = Group.3))+
  labs(y = 'Rating', x = 'DeltaE ') +
  scale_color_hue()+
  theme_dark()+
  ylim(0,1)
allDeltaPlotC



#Warm-biased species: Plot associations by delta E, frequency, exposure, and noticing 
allDeltaW = aggregate(x= dW$Ratings,             
                      by = list(dW$deltaE, dW$NoticedColorStr, dW$Freq, dW$ColorID, dW$ColorSetStr, dW$Freq),              
                      FUN = mean) 
allDeltaPlotW = ggplot(data=allDeltaW, aes(x = Group.1, y = x, color = Group.4)) +
  geom_point(shape = 21,aes(size = Group.3, color = Group.5, fill = Group.4, stroke = 2)) + 
  scale_radius()+
  facet_wrap(~Group.2)+
  geom_smooth(method='lm', se = F, aes(size = Group.3))+
  labs(y = 'Rating', x = 'DeltaE ') +
  scale_color_hue()+
  theme_dark()+
  ylim(0,1)
allDeltaPlotW




#Bar graphs

#Plot bar graphs Filter to just warm (red, orange, yellow) and cool (green, cyan, blue) hues
dchr <- dAll[dAll$Freq != 3,]
dchr$FreqInFreqC2 = varRecode(dchr$Freq, c(1,2,4,5), c(1,1,5,5)) #reorders for x axis
dchr$FreqInFreqStr = varRecode(dchr$FreqInFreqC2, c(1,5), c("InFreq", "Freq"))
dchr$NoticedColorStr =factor(dchr$NoticedColorStr, levels = c( "Noticers", "Non-noticers"))
dchr$ExposureStr <- factor(dchr$ExposureStr, levels = c( "seen", "Unseen"))# reorders for specific shapes to be used for plotting

d.summary <- dchr %>%
  group_by(FreqInFreqStr, NoticedColorStr, ExposureStr) %>%
  summarise(
    sd = sd(Ratings, na.rm = TRUE),
    Ratings = mean(Ratings),
    N    = length(Ratings/Subj),
    se   = sd / sqrt(N)
  )
d.summary

plotPoint = ggplot(data=d.summary, aes(x = ExposureStr, y = Ratings, fill = ExposureStr)) +
  geom_bar(stat='identity', position=position_dodge(.9), width=.5) +
  facet_grid(~NoticedColorStr+FreqInFreqStr)+
  scale_color_hue()+
  theme_dark()+
  geom_errorbar(aes(ymin = Ratings-se, ymax = Ratings+se),width = 0.2, position = position_dodge(.9)) + 
  labs(y = 'Rating', x = 'Frequency') +
  ylim(0,1)
plotPoint





# EXPERIMENT 2 ------------------------------------------------------------

# Exp2-Category learning --------------------------------------------------------

# Load in the data
dAll <- read.csv("Exp2-catLearning.csv")


# accuracy by trial 50
d50 = dAll[dAll$Trial <= 207,]
head(d50)
d50Sum = d50 %>% group_by(Subj) %>% 
  summarize(total50  = sum(Correct, na.rm = TRUE))
d50 = merge(d50,d50Sum)  
d50$acc = (d50$total50)/50
mean(d50$acc)


#Predicting a correct response from trial bin (every 30 trials) and typicality condition
m1cat <- glmer(Correct ~TrialBin*TypicalityC +(1+TrialBin|Subj),data=dAll,family=binomial, control = gcontrol) 
summary(m1cat)


#plot category learning
df.summary <- dAll %>%
  group_by(TrialBin, TypicalityStr) %>%
  summarise(
    sd = sd(Correct, na.rm = TRUE),
    Correct = mean(Correct),
    N    = length(Correct/Subj),
    se   = sd / sqrt(N)
  )
df.summary


plotPoint = ggplot(data=df.summary, aes(x = TrialBin, y = Correct, color = TypicalityStr)) +
  geom_point(aes(shape=TypicalityStr, size = 3,  color = TypicalityStr)) +
  geom_line(aes(group = c(TypicalityStr)))+
  labs(x = 'Trial Bin', y = 'Mean Accuracy') +
  scale_color_hue()+
  geom_errorbar(aes(ymin = Correct-se, ymax = Correct+se, color = TypicalityStr),width = 0.2) + 
  theme_dark() + 
  ylim(.75,1)
plotPoint




# Exp2-Ratings ------------------------------------------------------------

#Load in the data
dPS = read.csv("Exp2-associations.csv")


# Frequency hypothesis  -------------------------------------------------

#Filter to just seen colors
dPS_seen <- dPS[dPS$ExposureStr == "Seen",]


#Model predicting associations for seen colors from warmness, species, typicality condition, and noticing
m1 = lmer(Ratings ~ 1 + ColorWarmnessC*NameC*TypicalityC*NoticedColorC + (1 + ColorWarmnessC*NameC|Subj), dPS_seen, control = control)
summary(m1)
Anova(m1, type = 3, test = "F")


#Exploratory analyses - seperated by species
filk<- dPS_seen[dPS_seen$NameC == .5,]
slub <- dPS_seen[dPS_seen$NameC== -.5,]


#FILK: model predicting associations for seen colors with FILK 
m1filk = lmer(Ratings ~ 1 + ColorWarmnessC*TypicalityC*NoticedColorC + (1 + ColorWarmnessC|Subj), filk, control = control)
summary(m1filk)
Anova(m1filk, type = 3, test = "F")
 


#SLUB: model predicting associations for seen colors with SLUB
m1slub = lmer(Ratings ~ 1 + ColorWarmnessC*TypicalityC*NoticedColorC + (1 + ColorWarmnessC|Subj), slub, control = control)
summary(m1slub)
Anova(m1slub, type = 3, test = "F")
 




# Exposure hypothesis ---------------------------------------------------

#High frequency
#Filter to high frequency seen, corresponding unseen chromatic, and achromatic colors. 
dPSfreq = dPS[dPS$FreqInFreqC == .5 | dPS$FreqInFreqC == 0,] 


#Create contrast codes for each color set (seen, unseen chromatic, unseen achromatic)
dPSfreq$c1 = varRecode(dPSfreq$ExposureStr, c("Seen","UnseenChr","UnseenAch"), c(.5,-.5,0))      #Seen vs. unseen chromatic
dPSfreq$c2 = varRecode(dPSfreq$ExposureStr, c("Seen","UnseenChr","UnseenAch"), c(1/3,1/3,-2/3))  #Seen & unseen chromatic vs. unseen achromatic
dPSfreq$c1 = as.numeric(dPSfreq$c1)
dPSfreq$c2 = as.numeric(dPSfreq$c2)


# Model predicting associations for high frequency colors from noticing, typicality, and two contrast codes
m2 = lmer(Ratings ~ 1 + NoticedColorC*TypicalityC*(c1+c2) + (1+c1+c2|Subj), dPSfreq, control = control)
summary(m2)
Anova(m2, type = 3, test = "F")



  
#Low frequency
#Filter tolow frequency seen, corresponding unseen chromatic, and achromatic colors. 
dPSinfreq = dPS[dPS$FreqInFreqC == -.5 | dPS$FreqInFreqC == 0,] 

#Create contrast codes for each color set (seen, unseen chromatic, unseen achromatic)
dPSinfreq$c1 = varRecode(dPSinfreq$ExposureStr, c("Seen","UnseenChr","UnseenAch"), c(.5,-.5,0))      # seen vs unseenChr
dPSinfreq$c2 = varRecode(dPSinfreq$ExposureStr, c("Seen","UnseenChr","UnseenAch"), c(1/3,1/3,-2/3))  #Seen, UnseenCHr vs unseenAch
dPSinfreq$c1 = as.numeric(dPSinfreq$c1)
dPSinfreq$c2 = as.numeric(dPSinfreq$c2)


# Model predicting associations for low frequency colors from noticing, typicality, and two contrast codes
m3 = lmer(Ratings ~ 1 + NoticedColorC*TypicalityC*(c1+c2) + (1+c1+c2|Subj), dPSinfreq, control = control)
summary(m3)
anova(m3)
Anova(m3, type = 3, test = "F")




# Asymmetric generalization hypothesis ---------------------------------------------------------

#Filter to just chromatic colors (seen & unseen chromatic)
dchr = dPS[dPS$Freq != 0,]


#Model predicting associations from frequency, noticing, typicality, and exposure (Table S6)
m4 = lmer(Ratings ~ 1 + FreqInFreqC*NoticedColorC*TypicalityC*ExposureC + (1 + ExposureC*FreqInFreqC|Subj), dchr, control = control)
summary(m4)
Anova(m4, type = 3, test = "F")




# Exp2 - Ratings Plots -------------------------------------------------------------------


#Seen only - species x condition x noticing
dPS_seen$WarmCoolStr = varRecode(dPS_seen$ColorWarmnessC, c(.5,-.5), c("warm", "cool"))
dPS_seen$TypicalityStr = factor(dPS_seen$TypicalityStr, levels = c("Saw P", "Saw NP"))
dPS_seen$NoticedColorStr = factor(dPS_seen$NoticedColorStr, levels = c("Noticers", "Non-noticers"))

dmean <- dPS_seen %>%
  group_by( Subj, TypicalityStr,NoticedColorStr, WarmCoolStr, Name) %>%
  summarise(Ratings = mean(Ratings))
dmean

d.summary <- dmean %>%
  group_by( TypicalityStr, NoticedColorStr,WarmCoolStr, Name) %>%
  summarise(
    sd = sd(Ratings, na.rm = TRUE),
    Ratings = mean(Ratings),
    N    = length(Ratings/Subj),
    se   = sd / sqrt(N)
  )
d.summary


plotPoint = ggplot(data=d.summary, aes(x = TypicalityStr, y = Ratings,fill = WarmCoolStr)) +
  geom_bar(stat='identity', position=position_dodge(.9), width=.5) +
  facet_grid(~NoticedColorStr+Name)+
  scale_color_hue()+
  theme_dark()+
  geom_errorbar(aes(ymin = Ratings-se, ymax = Ratings+se),width = 0.2, position = position_dodge(.9)) + 
  labs(y = 'Rating', x = 'Frequency') +
  ylim(0,1)
plotPoint



# All colors - frequency x condition x noticing 
dchr <- dPS
dchr$FreqInFreqC2 = varRecode(dchr$FreqInFreqC, c(-.5,0,.5), c(2,3,1)) #reorders for x axis
dchr$NoticedColorStr =factor(dchr$NoticedColorStr, levels = c( "Noticers", "Non-noticers"))
dchr$TypicalityStr <- factor(dchr$TypicalityStr, levels = c( "Saw P", "Saw NP"))


dmean <- dchr %>%
  group_by( Subj, TypicalityStr,NoticedColorStr, FreqInFreqStr, ExposureStr) %>%
  summarise(Ratings = mean(Ratings))
dmean

d.summary <- dmean %>%
  group_by( TypicalityStr, NoticedColorStr,FreqInFreqStr,ExposureStr) %>%
  summarise(
    sd = sd(Ratings, na.rm = TRUE),
    Ratings = mean(Ratings),
    N    = length(Ratings/Subj),
    se   = sd / sqrt(N)
  )
d.summary


plotPoint = ggplot(data=d.summary, aes(x = TypicalityStr, y = Ratings,fill = ExposureStr)) +
  geom_bar(stat='identity', position=position_dodge(.9), width=.5) +
  facet_grid(~NoticedColorStr+FreqInFreqStr)+
  scale_color_hue()+
  theme_dark()+
  geom_errorbar(aes(ymin = Ratings-se, ymax = Ratings+se),width = 0.2, position = position_dodge(.9)) + 
  labs(y = 'Rating', x = 'Frequency') +
  ylim(0,1)
plotPoint
  



# SUPPLEMENTAL MATERIALS -------------------------------------------------

# GOODNESS OF FIT  ---------------------------------------------------------

# Color-word fit --------------------------------------------------------------

#load in the data
dCW <- read.csv("GoodFit-ColorWord.csv") 


#Filter to just saturated colors for model predicting ratings from color warmness and species name
dSat =(dCW[dCW$Sat.Light== "1",])
m1 = lmer(Ratings ~  WarmnessC*NameC + (1 + (WarmnessC*NameC)|Subj), dSat,  control= control)
summary(m1)
Anova(m1, type = 3, test = "F")


#Filter to just light colors for model predicting ratings from color warmness and species name
dLight <- (dCW[dCW$Sat.Light== "2",])
m2 <- lmer(Ratings ~ WarmnessC*NameC + (1+WarmnessC*NameC|Subj), dLight, control= control)
summary(m2)
Anova(m2, type = 3, test = "F")


#Filter to just muted colors for model predicting ratings from color warmness and species name
dMuted <- (dCW[dCW$Sat.Light== "3",])
m3 <- lmer(Ratings ~WarmnessC*NameC + (1+WarmnessC*NameC|Subj),dMuted, control = control)
summary(m3)
Anova(m3, type = 3, test = "F")


#Filter to just dark colors for model predicting ratings from color warmnessand species name
dDark <- (dCW[dCW$Sat.Light== "4",])
m4 <- lmer(Ratings ~ WarmnessC*NameC + (1+WarmnessC*NameC|Subj), dDark, control = control)
summary(m4)
Anova(m4, type = 3, test = "F")


#Plot association by warmness for saturated colors only
ColorWordSatC = aggregate(x = dSat$Ratings,             
                      by = list( dSat$Warmness, dSat$ColorID, dSat$NameStr),              
                      FUN = mean) 
ColorWordSatPlotC = ggplot(data=ColorWordSatC, aes(x = Group.1, y = x, fill = Group.3)) +
  geom_point(shape = 21, aes(size = Group.1, fill = Group.2, stroke = 2)) + 
  geom_smooth(method='lm', se = T)+
  labs(y = 'Rating', x = 'Warmness ') +
  scale_color_hue()+
  theme_dark()+
  ylim(0,1)
ColorWordSatPlotC



#Plot association by warmness for light colors only
ColorWordLightC = aggregate(x= dLight$Ratings,             
                      by = list( dLight$Warmness, dLight$ColorID,dLight$NameStr),              
                      FUN = mean) 
ColorWordLightPlotC = ggplot(data=ColorWordLightC, aes(x = Group.1, y = x, fill = Group.3)) +
  geom_point(shape = 21, aes(size = Group.1, fill = Group.2, stroke = 2)) + 
  geom_smooth(method='lm', se = T)+
  labs(y = 'Rating', x = 'Warmness ') +
  scale_color_hue()+
  theme_dark()+
  ylim(0,1)
ColorWordLightPlotC


#Plot association by warmness for muted colors only
ColorWordMutedC = aggregate(x= dMuted$Ratings,             
                      by = list( dMuted$Warmness, dMuted$ColorID,dMuted$NameStr),              
                      FUN = mean) 
ColorWordMutedPlotC = ggplot(data=ColorWordMutedC, aes(x = Group.1, y = x, fill = Group.3)) +
  geom_point(shape = 21, aes(size = Group.1, fill = Group.2, stroke = 2)) + 
  geom_smooth(method='lm', se = T)+
  labs(y = 'Rating', x = 'Warmness ') +
  scale_color_hue()+
  theme_dark()+
  ylim(0,1)
ColorWordMutedPlotC


#Plot association by warmness for dark colors only
ColorWordDarkC = aggregate(x= dDark$Ratings,             
                      by = list( dDark$Warmness, dDark$ColorID,dDark$NameStr),              
                      FUN = mean) 
ColorWordDarkPlotC = ggplot(data=ColorWordDarkC, aes(x = Group.1, y = x, fill = Group.3)) +
  geom_point(shape = 21, aes(size = Group.1, fill = Group.2, stroke = 2)) + 
  geom_smooth(method='lm', se = T)+
  labs(y = 'Rating', x = 'Warmness ') +
  scale_color_hue()+
  theme_dark()+
  ylim(0,1)
ColorWordDarkPlotC




# Shape-color fit  ------------------------------------------------------------

#Load in the data
dSC <- read.csv("GoodFit-ShapeColor.csv")

#Model predicting ratings from body shape (pointy vs. curvy) and color warmness
m1 <- lmer(Ratings ~ WarmnessC*ShapeC + (WarmnessC*ShapeC|Subj), data =dSC, control = control)
summary(m1)
Anova(m1, type = 3, test = "F")


#Separate colors into warm vs. cool for separate modesl
WarmC = dSC[dSC$ColorID == "R" | dSC$ColorID == "O" | dSC$ColorID == "Y",]
CoolC = dSC[dSC$ColorID == "G" | dSC$ColorID == "C" | dSC$Color == "B",]


#Model predicting ratings from body shape for warm colors
m2 <- lmer(Ratings ~ ShapeC + (ShapeC|Subj), data = WarmC, control = control)
summary(m2)
Anova(m2, type = 3, test = "F")

#Model predicting ratings from body shape for cool colors
m3 <- lmer(Ratings ~ ShapeC + (ShapeC|Subj), data = CoolC)
summary(m3)
Anova(m3, type = 3, test = "F")



#Plot association by warmness for each body type
ShapeColorC = aggregate(x= dSC$Ratings,             
                      by = list( dSC$Warmness, dSC$ColorID, dSC$ShapeStr),              
                      FUN = mean) 
ShapeColorPlotC = ggplot(data=ShapeColorC, aes(x = Group.1, y = x, fill = Group.3)) +
  geom_point(shape = 21,aes(size = Group.1, fill = Group.2)) + 
  scale_radius()+
  geom_smooth(method='lm', se = T)+
  labs(y = 'Rating', x = 'Warmness ') +
  scale_color_hue()+
  theme_dark()+
  ylim(0,1)
ShapeColorPlotC



# Shape-word fit --------------------------------------------------------------

#load in the data
dSW <- read.csv("GoodFit-ShapeWord.csv")

#model predicitng ratings from name (filk vs. slub) and body shape (pointy vs. curvy)
m1 <- lmer(Ratings ~ NameC*ShapeC + (NameC*ShapeC|Subj), data = dSW, control = control)
summary(m1)
Anova(m1, type = 3, test = "F")

#Figure made in Excel 


