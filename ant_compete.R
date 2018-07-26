################################
## ant cookie analysis ####
## C Nell
## March 2018
################################

setwd("/Users/colleennell/Dropbox/ant_cookie/R")

library(tidyverse)
library(reshape2)
library(broom)
library(lme4)
library(emmeans)
library(estimability)
library(ggplot2)

#################
## QUESTIONS ####

# Time, thermal differences among species
#	Is discovery time and recruitment time shorter for the Argentine ant than for the natives?

# Competitive ability
#	Does the Argentine ant find faster the bait when is not competing with other ants? And the native species when they’re not competing against de Argentine?
# Do native species find faster the bait when they’re not competing with the Argentine ant?

# Recruitment strategy
# Which species has the most numerous recruitment? And the fastest discovery time?
# Is always the Argentine ant the one who colonized the baits when there’s no removal or some species colonize it better?
# Does the Argentine ant displace other species when they’re competing? Or any of the native species displaces the Argentine ant?

#################

ants<-read.csv('data/antcookie_sp_raw.csv')
View(ants)
str(ants)

## which species had most numerous recruitment?
aggregate(ANT_MAX~ANT_SP, FUN=max, data=ants) # LH = 660
aggregate(ANT_MAX~ANT_SP+COMPETE, FUN=max, data=ants)# compete - LH 660 in not competing, 600 compete. 

## highest average recruitment?
aggregate(ANT_MAX~ANT_SP, FUN=mean, data=ants)#mean
#aggregate(ANT_MAX~ANT_SP, FUN=se, data=ants)# se
aggregate(ANT_MAX~ANT_SP+COMPETE, FUN=mean, data=ants)%>%dplyr::select(everything(),mean = ANT_MAX)%>%#compete
  left_join(aggregate(ANT_MAX~ANT_SP+COMPETE, FUN=se, data=ants), by=c('ANT_SP','COMPETE'))%>%dplyr::select(everything(), se=ANT_MAX)

# mean and se by ant species
sp.means<-ants%>%group_by(ANT_SP)%>%summarize_at(vars(TIME_COMPETE, TIME_DISCOVER, TIME_RECRUIT, ANT_MAX), funs(max(.,na.rm=TRUE), mean(.,na.rm=TRUE), min(.,na.rm=TRUE), se))
View(sp.means)

#melt datafrma to plot all means
sp.means.melt<-sp.means%>%melt(id.vars=c('ANT_SP'))%>%separate(variable, into=c('variable','event','stat'))%>%dcast(ANT_SP+variable+event~stat)%>%mutate(subset='all',metric=paste(variable, event))
sp.means.melt

ggplot(sp.means.melt, aes(ANT_SP, mean))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0)+
  geom_point()+
  zamia_theme+
  facet_wrap(~metric, scales='free_y')+
  labs(x='Ant species', y='')

# Is always the Argentine ant the one who colonized the baits when there’s no removal or some species colonize it better?
# not enough data
removal<-ants%>%filter(REMOVAL == 'NO')
compete<-removal%>%filter(ANT_COMPETE == 1)
str(removal) # 26 obs (but includes 1 line per ant species, so fewr)
str(compete) # 7 times when actually competed (14 obs for comp, leaving 12 removals where the other ant did not show)

removal

# which species has the highest time to recruitment?

##max mean and min for each species when no removal
rem.means<-removal%>%group_by(ANT_SP)%>%summarize_at(vars(TIME_COMPETE, TIME_DISCOVER, TIME_RECRUIT, ANT_MAX), funs(max(.,na.rm=TRUE), mean(.,na.rm=TRUE), min(.,na.rm=TRUE), se))
rem.means.melt<-rem.means%>%melt(id.vars=c('ANT_SP'))%>%separate(variable, into=c('variable','event','stat'))%>%dcast(ANT_SP+variable+event~stat)%>%mutate(subset='removal',metric=paste(variable, event))

#species means when no removal (competing)
ggplot(rem.means.melt, aes(ANT_SP, mean))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0)+
  geom_point()+
  zamia_theme+
  facet_wrap(~metric, scales='free_y')+
  labs(x='Ant species', y='')

##max mean and min for each species when competing
comp.means<-compete%>%group_by(ANT_SP)%>%summarize_at(vars(TIME_COMPETE, TIME_DISCOVER, TIME_RECRUIT, ANT_MAX), funs(max(.,na.rm=TRUE), mean(.,na.rm=TRUE), min(.,na.rm=TRUE), se))
comp.means.melt<-comp.means%>%melt(id.vars=c('ANT_SP'))%>%separate(variable, into=c('variable','event','stat'))%>%dcast(ANT_SP+variable+event~stat)%>%mutate(subset = 'compete',metric=paste(variable, event))

#species means when competing
ggplot(comp.means.melt, aes(ANT_SP, mean))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0)+
  geom_point()+
  zamia_theme+
  facet_wrap(~metric, scales='free_y')+
  labs(x='Ant species', y='')

## all plotted together
longy<-rbind(comp.means.melt, rem.means.melt, sp.means.melt)
ggplot(longy, aes(ANT_SP, mean))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, color=subset), width=0, alpha=.7)+
  geom_point(aes(color=subset), alpha=.75)+
  zamia_theme+
  facet_wrap(~metric, scales='free_y')+
  labs(x='Ant species', y='')

## max and min
longy<-rbind(comp.means.melt, rem.means.melt, sp.means.melt)
ggplot(longy, aes(x=ANT_SP))+
  geom_point(aes(x=ANT_SP, y=min,color=subset), alpha=.75)+
  zamia_theme+
  facet_wrap(~metric, scales='free_y')+
  labs(x='Ant species', y='')+theme(legend.position='top')

##########
##temperature

# what is the minimum temp to discover for each each species?
temps<-ants%>%group_by(ANT_SP)%>%
  dplyr::select(ANT_SP, TEMP_DISCOVER, TEMP_RECRUIT, TEMP_COMPETE)%>%
  melt(id.vars=c('ANT_SP'))%>%filter(!is.na(value))%>% #all the temperatures the species were observed at
  group_by(ANT_SP)%>%
  summarize_if(is.numeric, funs(min(., na.rm=TRUE), max(., na.rm=TRUE), mean(., na.rm=TRUE), diff(range(., na.rm=TRUE))))

# plot of ant activity periods
ggplot(temps)+
  geom_linerange(aes(x = reorder(ANT_SP, max), ymin = min, ymax=max, color=ANT_SP), stat='identity', color='darkgrey', size=2, alpha=.95)+
  theme(panel.background = element_blank(), axis.line = element_line(color='black'))+
  labs(x='Ant species', y='Temperature (C)')+coord_flip()

## does temp affect time to discovery - yes
disc_time_temp<-lm(TIME_DISCOVER~ANT_SP+TEMP_DISCOVER:ANT_SP+TEMP_START, data=ants)
#summary(disc_time_temp)

#temp start mod
disc_time_temp<-lmer(log(1+TIME_DISCOVER)~ANT_SP*TEMP_START+(1|PLOT/FLAG), data=ants)
Anova(disc_time_temp, type='III')## effect of TEMP_START, TEMP_DISCOVER, marg ANT_SP - just LR or ORIGIN is sig
plot(allEffects(disc_time_temp))

#temp discover mod
disc_time_temp<-lmer(log(1+TIME_DISCOVER)~ANT_SP*TEMP_START+(1|PLOT/FLAG)+(1|TEMP_DISCOVER), data=ants)
Anova(disc_time_temp, type='III')## effect of TEMP_START, TEMP_DISCOVER, marg ANT_SP - just LR or ORIGIN is sig
plot(allEffects(disc_time_temp))

# time to discovery was affected by temp start, temperature, and ant origin
disc_time_temp_re<-lmer(log(1+TIME_DISCOVER)~TEMP_DISCOVER*ANT_SP+TEMP_START+(1|PLOT/FLAG), data=ants)
Anova(disc_time_temp_re, type='III')
plot(allEffects(disc_time_temp_re))

###RECRUITMENT

#temp recruit mod
rtime_temp<-lmer(log(1+TIME_RECRUIT)~ANT_SP*TEMP_DISCOVER+(1|PLOT/FLAG), data=ants)
Anova(rtime_temp, type='III')## nada
plot(allEffects(rtime_temp))

#temp discover mod
rtime_temp<-lmer(log(1+TIME_RECRUIT)~ANT_SP+TEMP_DISCOVER+(1|PLOT/FLAG), data=ants)
Anova(rtime_temp, type='III')## effect of TEMP_START, TEMP_DISCOVER, marg ANT_SP - just LR or ORIGIN is sig
plot(allEffects(rtime_temp))

str(ants)
remdf<-ants%>%mutate(gone=ifelse(REMOVAL != 'NO', 'NO','YES'))%>%
  dplyr::select(PLOT:FLAG,ANT_SP,ORIGIN,TIME_DISCOVER,TIME_RECRUIT,ANT_MAX,gone)%>%
  melt(id.vars=c('PLOT','FLAG','gone','ORIGIN','ANT_SP'))%>%
  dcast(PLOT+FLAG~gone+variable+ORIGIN)%>%
  mutate(LR_comp = log(YES_TIME_RECRUIT_EXOTIC/YES_TIME_RECRUIT_NATIVE), 
         LR_alone=log(NO_TIME_RECRUIT_EXOTIC/NO_TIME_RECRUIT_NATIVE))
View(remdf)  
colnames(remdf)
trytry<-lmer(YES_)