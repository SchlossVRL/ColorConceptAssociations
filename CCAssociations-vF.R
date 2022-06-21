
################################################################################
## Analyses for paper: Color-concept association formation for novel concepts ##
##             Melissa A. Schoenlein & Karen B. Schloss                       ##
################################################################################


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
library(MuMIn)

control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e6),check.nobs.vs.nRE = "ignore") 
gcontrol=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e3),check.nobs.vs.nRE = "ignore") 


# EXPERIMENT 1 ------------------------------------------------------------


# Exp1-Category learning --------------------------------------------------------

#Load in the data
dExp1cat <- read.csv("Exp1-catLearning.csv")


# accuracy by trial 50
d50 = dExp1cat[dExp1cat$Trial <= 50,]
head(d50)
d50Sum = d50 %>% group_by(Subj) %>% 
  summarize(total50  = sum(Correct, na.rm = TRUE))
d50 = merge(d50,d50Sum)  
d50$acc = (d50$total50)/50
mean(d50$acc)

#accuracy by end
dAllTrials = dExp1cat %>% group_by(Subj) %>% 
  summarize(total240  = sum(Correct, na.rm = TRUE))
dAllTrials = merge(dExp1cat,dAllTrials)  
dAllTrials$acc = (dAllTrials$total240)/240
mean(dAllTrials$acc)




# Exp1-Ratings ------------------------------------------------------------

#Load in the data
dExp1rate = read.csv("Exp1-associations.csv")


# Effects of frequency for seen colors during category learning --------------------------------------------------------

#Filter to seen colors 
dSat = dExp1rate[dExp1rate$ColorSetStr == "Saturated",]


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


# Model predicting associations for seen colors from co-occurrence frequency
m1 = lmer(Ratings ~ 1 + FreqC+ (1 + FreqC|Subj), dSat)
summary(m1)
Anova(m1, type = 3, test = "F")
r.squaredLR(m1)




# Exposure, frequency generalization, and color distance generalization comparing seen and unseen colors --------------------------------------------------------


# Model predicting associations from co-occurrence frequency, exposure (seen vs. unseen), and delta E
m2 = lmer(Ratings ~ 1 +  deltaEC + FreqC + ExposureC  + (FreqC:deltaEC) + (FreqC:ExposureC) +  (1 + deltaEC + FreqC + ExposureC + (FreqC:deltaEC) + (FreqC:ExposureC)|Subj), dExp1rate, control = control)
summary(m2)
Anova(m2, type = 3, test = "F")
anova(m2)
r.squaredLR(m2)


#Filter to only unseen colors 
dExp1Unseen = dExp1rate[dExp1rate$ColorSetStr != "Saturated",]


# Model predicting associations from co-occurrence frequency and delta E for Unseen colors only
m2b = lmer(Ratings ~ 1 +  deltaEC + FreqC + (FreqC:deltaEC) +  (1 + deltaEC + FreqC + (FreqC:deltaEC) |Subj), dExp1Unseen, control = control)
summary(m2b)
Anova(m2b, type = 3, test = "F")
anova(m2b)




# Effects of noticing color patterns during alien category learning --------------------------------------------------------


#Responses to the first questionnaire asking about strategy during category learning did not predict whether participants were noticers. 
dchi = dExp1rate[!duplicated(dExp1rate$Subj),]
table(dchi$UsedColorC,dchi$NoticedColorC)
testChi <- chisq.test(table(dchi$UsedColorC,dchi$NoticedColorC))
testChi



# Model predicting associations from co-occurrence frequency, exposure (seen vs. unseen), delta E, and noticing (Table S1)
m3 = lmer(Ratings ~ 1 + NoticedColorC + deltaEC + FreqC + ExposureC + (FreqC:NoticedColorC) + (FreqC:deltaEC) + (FreqC:ExposureC) + (NoticedColorC:deltaEC)+ (NoticedColorC:ExposureC)+ (FreqC:NoticedColorC:deltaEC)+ (FreqC:NoticedColorC:ExposureC) + (1 + FreqC+deltaEC+ExposureC+ (FreqC:deltaEC) + (FreqC:ExposureC)|Subj), dExp1rate, control = control)
summary(m3)
Anova(m3, type = 3, test = "F")
anova(m3)
r.squaredLR(m3)



#Filter to noticers vs. non-noticers
noticed<- dExp1rate[dExp1rate$NoticedColorC == .5,]
noNote<- dExp1rate[dExp1rate$NoticedColorC == -.5,]


#Noticers only: Model predicting associations from co-occurrence frequency, exposure, and delta E (Table S2)
m3a = lmer(Ratings ~ 1 +  deltaEC + FreqC + ExposureC + (FreqC:deltaEC) + (FreqC:ExposureC) + (1 + deltaEC + FreqC + ExposureC + (FreqC:deltaEC) + (FreqC:ExposureC)|Subj), noticed, control = control)
summary(m3a)
Anova(m3a, type = 3, test = "F")


#Non-noticers only: Model predicting associations from co-occurrence frequency, exposure, and delta E (Table S2)
m3b = lmer(Ratings ~ 1 +  deltaEC + FreqC + ExposureC + (FreqC:deltaEC) + (FreqC:ExposureC) + (1 + deltaEC + FreqC + ExposureC + (FreqC:deltaEC) + (FreqC:ExposureC)|Subj), noNote, control = control)
summary(m3b)
Anova(m3b, type = 3, test = "F")





# Exp1 - Ratings Plots -------------------------------------------------------


#Associations for seen colors (Figure 4)
dSeen = dExp1rate[dExp1rate$ExposureStr == "seen",] 

# Seen only - Frequency for warm & cool-biased species
SeenFreq <- dSeen %>%
  group_by(ID, Freq, WarmCoolSet) %>%
  summarise(
    sd = sd(Ratings, na.rm = TRUE),
    Ratings = mean(Ratings),
    N    = length(Ratings/Subj),
    se   = sd / sqrt(N)
  )
SeenFreq

SeenFreqP = ggplot(data=SeenFreq, aes(x = Freq, y = Ratings)) +
  geom_smooth(method='lm', se = T)+
  geom_point(aes(size = Freq, color = factor(ID), fill = "black")) + 
  scale_radius()+
  facet_wrap(~WarmCoolSet)+
  labs(y = 'Rating', x = 'Freq ') +
  ylim(0,1)+
  scale_color_manual(values = c("1" = "#fd002e",
                                "2" = "#fd798f",
                                "3" = "#ca4e61",
                                "4" = "#b10205",
                                "5" = "#fd9704",
                                "6" = "#fdae76",
                                "7" = "#e08343",
                                "8" = "#8a5200",
                                "9" = "#fdf803",
                                "10" = "#fdeb83",
                                "11" = "#d9c230",
                                "12" = "#8a8a00",
                                "13" = "#8ff803",
                                "14" = "#c7fd80",
                                "15" = "#9ddc4b",
                                "16" = "#678a01",
                                "17" = "#07ee8a",
                                "18" = "#6debb6",
                                "19" = "#54d29c",
                                "20" = "#007859",
                                "21" = "#0edcfd",
                                "22" = "#69d9eb",
                                "23" = "#46aebd",
                                "24" = "#098397",
                                "25" = "#048afd",
                                "26" = "#9aa6fd",
                                "27" = "#7583de",
                                "28" = "#1042a6",
                                "29" = "#e303fd",
                                "30" = "#e47ffd",
                                "31" = "#c078c2",
                                "32" = "#8b0092"))
SeenFreqP



#Associations for seen and unseen colors (Figure 5A)
GenFreq <- dExp1rate %>%
  group_by(Freq, ExposureStr) %>%
  summarise(
    sd = sd(Ratings, na.rm = TRUE),
    Ratings = mean(Ratings),
    N    = length(Ratings/Subj),
    se   = sd / sqrt(N)
  )
GenFreq

GenFreqP = ggplot(data=GenFreq, aes(x = Freq, y = Ratings, color = ExposureStr)) +
  geom_smooth(method='lm', se = T)+
  geom_point(aes(size = Freq, color= ExposureStr)) + 
  scale_radius()+
  labs(y = 'Rating', x = 'Freq ') +
  ylim(0,1)+
  scale_color_manual(values = c("seen" = "black",
                                "Unseen" = "gray40"))
GenFreqP



#Associations for seen and unseen colors (Figure 5B)
#Filter to just warm (red, orange, yellow) and cool (green, cyan, blue) hues
dchr <- dExp1rate[dExp1rate$Freq != 3,]
dchr$FreqInFreqC2 = varRecode(dchr$Freq, c(1,2,4,5), c(1,1,5,5)) #reorders for x axis
dchr$FreqInFreqStr = varRecode(dchr$FreqInFreqC2, c(1,5), c("InFreq", "Freq"))
dchr$NoticedColorStr =factor(dchr$NoticedColorStr, levels = c( "Noticers", "Non-noticers"))
dchr$ExposureStr <- factor(dchr$ExposureStr, levels = c( "seen", "Unseen"))# reorders for specific shapes to be used for plotting

deltaFreq <- dchr %>%
  group_by(deltaE, FreqInFreqStr,Freq, ID) %>%
  summarise(
    sd = sd(Ratings, na.rm = TRUE),
    Ratings = mean(Ratings),
    N    = length(Ratings/Subj),
    se   = sd / sqrt(N)
  )
deltaFreq

#Separate seen vs. unseen
deltaFreqUnSeen = deltaFreq[deltaFreq$deltaE > 0, ]
deltaFreqSeen = deltaFreq[deltaFreq$deltaE == 0, ]

deltaFreqP = ggplot(data=deltaFreqUnSeen, aes(x = deltaE, y = Ratings, color = FreqInFreqStr)) +
  geom_point(aes(size = Freq, shape = FreqInFreqStr, color = factor(ID))) + 
  geom_point(data = deltaFreqSeen, aes(x = deltaE, y = Ratings, size = Freq, shape = FreqInFreqStr, color = factor(ID)))+
  scale_radius()+
  geom_smooth(method='lm', se = T)+
  labs(y = 'Rating', x = 'DeltaE ') +
  geom_vline(xintercept = 8)+
  ylim(0,1)+
  scale_color_manual(values = c("1" = "#fd002e",
                                "2" = "#fd798f",
                                "3" = "#ca4e61",
                                "4" = "#b10205",
                                "5" = "#fd9704",
                                "6" = "#fdae76",
                                "7" = "#e08343",
                                "8" = "#8a5200",
                                "9" = "#fdf803",
                                "10" = "#fdeb83",
                                "11" = "#d9c230",
                                "12" = "#8a8a00",
                                "13" = "#8ff803",
                                "14" = "#c7fd80",
                                "15" = "#9ddc4b",
                                "16" = "#678a01",
                                "17" = "#07ee8a",
                                "18" = "#6debb6",
                                "19" = "#54d29c",
                                "20" = "#007859",
                                "21" = "#0edcfd",
                                "22" = "#69d9eb",
                                "23" = "#46aebd",
                                "24" = "#098397",
                                "25" = "#048afd",
                                "26" = "#9aa6fd",
                                "27" = "#7583de",
                                "28" = "#1042a6",
                                "29" = "#e303fd",
                                "30" = "#e47ffd",
                                "31" = "#c078c2",
                                "32" = "#8b0092"))
deltaFreqP




# PLOTS SEPARATED BY NOTICING

#Associations for seen and unseen colors separated by noticing (Figure 6A)
GenFreqNote <- dExp1rate %>%
  group_by(NoticedColorStr, Freq, ExposureStr) %>%
  summarise(
    sd = sd(Ratings, na.rm = TRUE),
    Ratings = mean(Ratings),
    N    = length(Ratings/Subj),
    se   = sd / sqrt(N)
  )
GenFreqNote

GenFreqNoteP = ggplot(data=GenFreqNote, aes(x = Freq, y = Ratings, color = ExposureStr)) +
  geom_smooth(method='lm', se = T)+
  geom_point(aes(size = Freq, color= ExposureStr)) + 
  scale_radius()+
  facet_wrap(~NoticedColorStr)+
  labs(y = 'Rating', x = 'Freq ') +
  ylim(0,1)+
  scale_color_manual(values = c("seen" = "black",
                                "Unseen" = "gray40"))
GenFreqNoteP



#Associations for seen and unseen colors separated by noticing (Figure 6B)
deltaFreqNote <- dchr %>%
  group_by(deltaE, NoticedColorStr, FreqInFreqStr,Freq, ID) %>%
  summarise(
    sd = sd(Ratings, na.rm = TRUE),
    Ratings = mean(Ratings),
    N    = length(Ratings/Subj),
    se   = sd / sqrt(N)
  )
deltaFreqNote


#Separate seen vs. unseen
deltaFreqNoteUnSeen = deltaFreqNote[deltaFreqNote$deltaE > 0, ]
deltaFreqNoteSeen = deltaFreqNote[deltaFreqNote$deltaE == 0, ]

deltaFreqNoteP = ggplot(data=deltaFreqNoteUnSeen, aes(x = deltaE, y = Ratings, color = FreqInFreqStr)) +
  geom_point(aes(size = Freq, shape = FreqInFreqStr, color = factor(ID))) + 
  geom_point(data = deltaFreqNoteSeen, aes(x = deltaE, y = Ratings, size = Freq, shape = FreqInFreqStr, color = factor(ID)))+
  scale_radius()+
  facet_wrap(~NoticedColorStr)+
  geom_smooth(method='lm', se = T)+
  labs(y = 'Rating', x = 'DeltaE ') +
  geom_vline(xintercept = 8)+
  ylim(0,1)+
  scale_color_manual(values = c("1" = "#fd002e",
                                "2" = "#fd798f",
                                "3" = "#ca4e61",
                                "4" = "#b10205",
                                "5" = "#fd9704",
                                "6" = "#fdae76",
                                "7" = "#e08343",
                                "8" = "#8a5200",
                                "9" = "#fdf803",
                                "10" = "#fdeb83",
                                "11" = "#d9c230",
                                "12" = "#8a8a00",
                                "13" = "#8ff803",
                                "14" = "#c7fd80",
                                "15" = "#9ddc4b",
                                "16" = "#678a01",
                                "17" = "#07ee8a",
                                "18" = "#6debb6",
                                "19" = "#54d29c",
                                "20" = "#007859",
                                "21" = "#0edcfd",
                                "22" = "#69d9eb",
                                "23" = "#46aebd",
                                "24" = "#098397",
                                "25" = "#048afd",
                                "26" = "#9aa6fd",
                                "27" = "#7583de",
                                "28" = "#1042a6",
                                "29" = "#e303fd",
                                "30" = "#e47ffd",
                                "31" = "#c078c2",
                                "32" = "#8b0092"))
deltaFreqNoteP




# EXPERIMENT 2 ------------------------------------------------------------

# Exp2-Category learning --------------------------------------------------------

# Load in the data
dExp2cat <- read.csv("Exp2-catLearning.csv")


# accuracy by trial 50
d50 = dExp2cat[dExp2cat$Trial <= 207,]
head(d50)
d50Sum = d50 %>% group_by(Subj) %>% 
  summarize(total50  = sum(Correct, na.rm = TRUE))
d50 = merge(d50,d50Sum)  
d50$acc = (d50$total50)/50
mean(d50$acc)


#accuracy by end
dAllTrials = dExp2cat %>% group_by(Subj) %>% 
  summarize(total180  = sum(Correct, na.rm = TRUE))
dAllTrials = merge(dExp2cat,dAllTrials)  
dAllTrials$acc = (dAllTrials$total180)/180
mean(dAllTrials$acc)



# Exp2-Ratings ------------------------------------------------------------

#Load in the data
dPS = read.csv("Exp2-associations.csv")


# Frequency hypothesis  -------------------------------------------------

#Filter to just seen colors
dPS_seen <- dPS[dPS$ExposureStr == "Seen",]


#Preregistered Model predicting associations for seen colors from warmness, species (name), typicality condition, and noticing (Table S3)
m1 = lmer(Ratings ~ 1 + ColorWarmnessC*NameC*TypicalityC*NoticedColorC + (1 + ColorWarmnessC*NameC|Subj), dPS_seen, control = control)
summary(m1)
Anova(m1, type = 3, test = "F")
r.squaredLR(m1)


#Exploratory analyses - seperated by species
filk<- dPS_seen[dPS_seen$NameC == .5,]
slub <- dPS_seen[dPS_seen$NameC== -.5,]


#FILK: model predicting associations for seen colors with FILK (Table S4)
m1filk = lmer(Ratings ~ 1 + ColorWarmnessC*TypicalityC*NoticedColorC + (1 + ColorWarmnessC|Subj), filk, control = control)
summary(m1filk)
Anova(m1filk, type = 3, test = "F")
 

#SLUB: model predicting associations for seen colors with SLUB (Table S4)
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


#Preregistered Model predicting associations for high frequency colors from noticing, typicality, and two contrast codes (Table S5)
m2 = lmer(Ratings ~ 1 + NoticedColorC*TypicalityC*(c1+c2) + (1+c1+c2|Subj), dPSfreq, control = control)
summary(m2)
Anova(m2, type = 3, test = "F")
r.squaredLR(m2)


  
#Low frequency
#Filter tolow frequency seen, corresponding unseen chromatic, and achromatic colors. 
dPSinfreq = dPS[dPS$FreqInFreqC == -.5 | dPS$FreqInFreqC == 0,] 

#Create contrast codes for each color set (seen, unseen chromatic, unseen achromatic)
dPSinfreq$c1 = varRecode(dPSinfreq$ExposureStr, c("Seen","UnseenChr","UnseenAch"), c(.5,-.5,0))      # seen vs unseenChr
dPSinfreq$c2 = varRecode(dPSinfreq$ExposureStr, c("Seen","UnseenChr","UnseenAch"), c(1/3,1/3,-2/3))  #Seen, UnseenCHr vs unseenAch
dPSinfreq$c1 = as.numeric(dPSinfreq$c1)
dPSinfreq$c2 = as.numeric(dPSinfreq$c2)


#Preregistered Model predicting associations for low frequency colors from noticing, typicality, and two contrast codes (Table S5)
m3 = lmer(Ratings ~ 1 + NoticedColorC*TypicalityC*(c1+c2) + (1+c1+c2|Subj), dPSinfreq, control = control)
summary(m3)
Anova(m3, type = 3, test = "F")
r.squaredLR(m3)




# Asymmetric generalization hypothesis ---------------------------------------------------------


#Filter to just chromatic colors (seen & unseen chromatic)
dchr = dPS[dPS$Freq != 0,]


#Preregistered Model predicting associations from frequency, noticing, typicality, and exposure (Table S6)
m4 = lmer(Ratings ~ 1 + FreqInFreqC*NoticedColorC*TypicalityC*ExposureC + (1 + ExposureC*FreqInFreqC|Subj), dchr, control = control)
summary(m4)
Anova(m4, type = 3, test = "F")
r.squaredLR(m4)




# Exp2 - Ratings Plots -------------------------------------------------------------------


#Associations for seen colors (Figure 8A)
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
plotExp2 = ggplot(data=d.summary, aes(x = TypicalityStr, y = Ratings,fill = WarmCoolStr)) +
  geom_bar(stat='identity', position=position_dodge(.9), width=.5) +
  facet_grid(~NoticedColorStr+Name)+
  scale_color_hue()+
  theme_dark()+
  geom_errorbar(aes(ymin = Ratings-se, ymax = Ratings+se),width = 0.2, position = position_dodge(.9)) + 
  labs(y = 'Rating', x = 'Frequency') +
  ylim(0,1)
plotExp2



#Associations for seen and unseen colors (Figure 8B)
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
  



# SUPPLEMENTAL MATERIAL -------------------------------------------------


# Supplemental: Exp1 Full Set of Association ratings (Figure S1)  ----------------------------------------------------


#Load in the data
dExp1rate = read.csv("Exp1-associations.csv")  


#Filter to Cool-biased species
dC = dExp1rate[dExp1rate$WarmCoolSet == -1,]

#Filter to Warm-biased species
dW = dExp1rate[dExp1rate$WarmCoolSet == 1,]

#warm-biased species: Plot associations by delta E, frequency, exposure, and noticing 
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




# Supplemental: Exp1 effects of goodness-of-fit on associations and category learning------------


#Load in association ratings data
dExp1rate = read.csv("Exp1-associations.csv")
dExp1rateSat =(dExp1rate[dExp1rate$Sat.Light== "1",])


# Model predicting associations for seen (saturated) colors from co-occurrence frequency and two fit factors: shape-color & shape-name
m1 = lmer(Ratings ~ 1 + FreqC*ShapeNameC*ShapeColorC + (1 + FreqC|Subj), dExp1rateSat)
summary(m1)
Anova(m1, type = 3, test = "F")
r.squaredLR(m1)



#Load category learning data
dExp1cat <- read.csv("Exp1-catLearning.csv")

#Model predicting correct response from trial bin (every 40 trials) and two fit factors (Table S7)
m1Cat <- glmer(Correct ~TrialBin*ShapeNameC*ShapeColorC +(1+TrialBin|Subj),data=dExp1cat,family=binomial, control = gcontrol) 
summary(m1Cat)


#Plot category learning data for every 40 trials
dExp1cat$MeanCorrect<- with(dExp1cat, ave(Correct, TrialBin,Condition, FUN = function(x) mean(x, na.rm = TRUE)))
dExp1cat$ShapeName = ifelse(dExp1cat$Condition == 1 | dExp1cat$Condition == 2, "fit","noFIt")
dExp1cat$ShapeColor = ifelse(dExp1cat$Condition == 1 | dExp1cat$Condition == 3, "fit","noFit")
plotPoint = ggplot(data=dExp1cat, aes(x = TrialBin, y = MeanCorrect)) +
  geom_point(aes(shape=ShapeName, size = 3,  color = ShapeColor)) +
  geom_line(aes(group = c(Condition)))+
  labs(x = 'Trial Bin', y = 'Mean Accuracy') +
  scale_color_hue()+
  theme_dark() + 
  ylim(.75,1)
plotPoint




# Supplemental: Assessing goodness-of-fit of the features and comparing Experiment 1 to baseline----------------


# Color-word association ratings -------------------------------------------------------------------------------


#load in the baseline data dat
dCW <- read.csv("GoodFit-ColorWord.csv") 

#Filter baseline to just saturated colors for model predicting ratings from color warmness and species name
dSatBase =(dCW[dCW$Sat.Light== "1",])
mSatBase = lmer(Ratings ~  WarmnessC*NameC + (1 + (WarmnessC*NameC)|Subj), dSatBase,  control= control)
summary(mSatBase)
Anova(mSatBase, type = 3, test = "F")


#Filter baseline to light, muted, and dark colors for model predicting ratings from color warmness, species name, and lightness (Table S8)
dUnseenBase =(dCW[dCW$Sat.Light != "1",])
mUnseenBase = lmer(Ratings ~  WarmnessC*NameC*LightC + (1 + (WarmnessC*NameC)|Subj), dUnseenBase,  control= control)
summary(mUnseenBase)
Anova(mUnseenBase, type = 3, test = "F")



#load in Experiment 1 data to compare with baseline saturated data
dExp1rate <- read.csv("Exp1-associations.csv") 

#filter to just saturated colors
dExp1rateSat = dExp1rate[dExp1rate$Sat.Light == "1",]

#re-organize to group by warm vs. cool-biased species
dExp1rateSatFilk = dExp1rateSat[dExp1rateSat$NameStr == "Filk", ]
dExp1rateSatSlub = dExp1rateSat[dExp1rateSat$NameStr == "Slub", ]
dExp1rateSatWarmFilk = dExp1rateSatFilk[dExp1rateSatFilk$Condition == 1 | dExp1rateSatFilk$Condition == 4, ]
dExp1rateSatCoolFilk = dExp1rateSatFilk[dExp1rateSatFilk$Condition == 2 | dExp1rateSatFilk$Condition == 3, ]
dExp1rateSatWarmSlub = dExp1rateSatSlub[dExp1rateSatSlub$Condition == 2 | dExp1rateSatSlub$Condition == 3, ]
dExp1rateSatCoolSlub = dExp1rateSatSlub[dExp1rateSatSlub$Condition == 1 | dExp1rateSatSlub$Condition == 4, ]
keep = c("Subj", "Ratings", "LightC" ,"ColorIDF", "NameC","NameStr", "WarmnessC", "Warmness", "group", "groupStr")
dExp1rateSatWarmFilk = dExp1rateSatWarmFilk[keep]
dExp1rateSatCoolFilk = dExp1rateSatCoolFilk[keep]
dExp1rateSatWarmSlub = dExp1rateSatWarmSlub[keep]
dExp1rateSatCoolSlub = dExp1rateSatCoolSlub[keep]
dSatBase =dSatBase[keep]
#merge with saturated baseline data
dSatAllWarm = rbind(dSatBase, dExp1rateSatWarmFilk, dExp1rateSatWarmSlub)
dSatAllCool = rbind(dSatBase, dExp1rateSatCoolFilk, dExp1rateSatCoolSlub)


#Model comparing experiment 1 to baseline for warm colors (Table S9)
mCompareWarm = lmer(Ratings ~  WarmnessC*NameC*group + (1 + (WarmnessC)|Subj), dSatAllWarm,  control= control)
summary(mCompareWarm)
Anova(mCompareWarm, type = 3, test = "F")


#Plot association by warmness for saturated colors only - warm biased Filks
ColorWordSatC = aggregate(x = dSatAllWarm$Ratings,             
                          by = list( dSatAllWarm$Warmness, dSatAllWarm$groupStr, dSatAllWarm$NameStr),              
                          FUN = mean) 
ColorWordSatPlotC = ggplot(data=ColorWordSatC, aes(x = Group.1, y = x, fill = Group.3)) +
  geom_point(shape = 21, aes(size = Group.1, fill = Group.3, stroke = 2)) + 
  geom_smooth(method='lm', se = T)+
  labs(y = 'Rating', x = 'Warmness ') +
  facet_wrap(~Group.2)+
  scale_color_hue()+
  theme_dark()+
  ylim(0,1)
ColorWordSatPlotC


#Model comparing experiment 1 to baseline for cool colors (Table S9)
mCompareCool = lmer(Ratings ~  WarmnessC*NameC*group + (1 + (WarmnessC)|Subj), dSatAllCool,  control= control) 
summary(mCompareCool)
Anova(mCompareCool, type = 3, test = "F")


#Plot association by warmness for saturated colors only - cool biased Slubs
ColorWordSatCool = aggregate(x = dSatAllCool$Ratings,             
                             by = list( dSatAllCool$Warmness, dSatAllCool$groupStr, dSatAllCool$NameStr),              
                             FUN = mean) 
ColorWordSatPlotCool = ggplot(data=ColorWordSatCool, aes(x = Group.1, y = x, fill = Group.3)) +
  geom_point(shape = 21, aes(size = Group.1, fill = Group.3, stroke = 2)) + 
  geom_smooth(method='lm', se = T)+
  labs(y = 'Rating', x = 'Warmness ') +
  facet_wrap(~Group.2)+
  scale_color_hue()+
  theme_dark()+
  ylim(0,1)
ColorWordSatPlotCool




# Shape-color association ratings  ------------------------------------------------------------

#Load in the data
dSC <- read.csv("GoodFit-ShapeColor.csv")

#Model predicting ratings from body shape (pointy vs. curvy) and color warmness
mSC <- lmer(Ratings ~ WarmnessC*ShapeC + (WarmnessC*ShapeC|Subj), data =dSC, control = control)
summary(mSC)
Anova(m1, type = 3, test = "F")


#Separate colors into warm vs. cool for separate models
WarmC = dSC[dSC$ColorID == "R" | dSC$ColorID == "O" | dSC$ColorID == "Y",]
CoolC = dSC[dSC$ColorID == "G" | dSC$ColorID == "C" | dSC$Color == "B",]


#Model predicting ratings from body shape for warm colors
mSCwarm <- lmer(Ratings ~ ShapeC + (ShapeC|Subj), data = WarmC, control = control)
summary(mSCwarm)
Anova(mSCwarm, type = 3, test = "F")

#Model predicting ratings from body shape for cool colors
mSCcool <- lmer(Ratings ~ ShapeC + (ShapeC|Subj), data = CoolC)
summary(mSCcool)
Anova(mSCcool, type = 3, test = "F")


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




# Shape-word association ratings --------------------------------------------------------------

#load in the data
dSW <- read.csv("GoodFit-ShapeWord.csv")

#model predicitng ratings from name (filk vs. slub) and body shape (pointy vs. curvy)
mSW <- lmer(Ratings ~ NameC*ShapeC + (NameC*ShapeC|Subj), data = dSW, control = control)
summary(mSW)
Anova(mSW, type = 3, test = "F")

#Figure made in Excel 




# Supplemental: Exp2 effects of typicality on alien category learning ----------------------------------------------------

#load in data
dExp2cat = read.csv("Exp2-catLearning.csv")


#Predicting a correct response from trial bin (every 30 trials) and typicality condition
mExp2cat <- glmer(Correct ~TrialBin*TypicalityC +(1+TrialBin|Subj),data=dExp2cat,family=binomial, control = gcontrol) 
summary(mExp2cat)


#plot accuracy across trial bins (Figure S5)
dExp2cat.summary <- dExp2cat %>%
  group_by(TrialBin, TypicalityStr) %>%
  summarise(
    sd = sd(Correct, na.rm = TRUE),
    Correct = mean(Correct),
    N    = length(Correct/Subj),
    se   = sd / sqrt(N)
  )
dExp2cat.summary

plotExp2cat = ggplot(data=dExp2cat.summary, aes(x = TrialBin, y = Correct, color = TypicalityStr)) +
  geom_point(aes(shape=TypicalityStr, size = 3,  color = TypicalityStr)) +
  geom_line(aes(group = c(TypicalityStr)))+
  labs(x = 'Trial Bin', y = 'Mean Accuracy') +
  scale_color_hue()+
  geom_errorbar(aes(ymin = Correct-se, ymax = Correct+se, color = TypicalityStr),width = 0.2) + 
  theme_dark() + 
  ylim(.75,1)
plotExp2cat





