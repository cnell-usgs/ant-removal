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
cookie<-read.csv('data/antcookie_data.csv', nrows = 57)%>%mutate(COMPETE = ifelse(grepl('-', ANT_SP), 'YES', 'NO'))
dim(cookie)
cookie

# rows with 2 ant species - data values are same for both species so create new line for each species - 7 to 14 rows
sp.df<-cookie%>%filter(COMPETE == 'YES')%>%separate(ANT_SP, by='-', into=c('sp1', 'sp2'))%>%
  melt(id=colnames(cookie%>%select(-ANT_SP)), value.name='ANT_SP')
dub.df<-sp.df%>%filter(variable == 'sp2')%>%
  select(NATIVE_SP, PLOT, FLAG, DAY, REMOVAL,ANT_SP, TEMP_START, TIME_COMPETE, TEMP_COMPETE, TIME_DISCOVER=TIME_DISCOVER_COMPETE, TEMP_DISCOVER=TEMP_DISCOVER_COMPETE, TIME_RECRUIT=TIME_RECRUIT_COMPETE, 
         TEMP_RECRUIT=TEMP_RECRUIT_COMPETE, ANT_MAX=ANT_MAX_COMPETE, ANT_FINAL=ANT_FINAL_COMEPTE, COMPETE)%>%
  mutate(ANT_COMPETE = 2)
fir.df<-sp.df%>%filter(variable == 'sp1')%>%mutate(ANT_COMPETE = 1)%>%select(colnames(dub.df))
compete.df<-rbind(fir.df,dub.df)

ant_cook<-rbind(cookie%>%filter(COMPETE != 'YES')%>%mutate(ANT_COMPETE=0)%>%select(colnames(dub.df)), compete.df)%>%
  mutate(ORIGIN = ifelse(ANT_SP == 'LH', 'EXOTIC','NATIVE'))%>%transform(PLOT=as.factor(PLOT), COMPETE=as.factor(COMPETE))
dim(ant_cook)
View(ant_cook)

write.csv(ant_cook, 'data/antcookie_sp_raw.csv', row.names=FALSE)

#################
## temperature

# what is the minimum temp to discover for each each species?
temps<-ant_cook%>%group_by(ANT_SP)%>%
  select(ANT_SP, TEMP_DISCOVER, TEMP_RECRUIT, TEMP_COMPETE)%>%
  melt(id.vars=c('ANT_SP'))%>%filter(!is.na(value))%>% #all the temperatures the species were observed at
  group_by(ANT_SP)%>%
  summarize_if(is.numeric, funs(min(., na.rm=TRUE), max(., na.rm=TRUE), mean(., na.rm=TRUE), diff(range(., na.rm=TRUE))))

# temp active?

# plot of ant activity periods
ggplot(temps)+
  geom_linerange(aes(x = reorder(ANT_SP, max), ymin = min, ymax=max, color=ANT_SP), stat='identity', color='darkgrey', size=2, alpha=.95)+
  theme(panel.background = element_blank(), axis.line = element_line(color='black'))+
  labs(x='Ant species', y='Temperature (C)')+coord_flip()

## Q1 Does the temperature affect the time of discovery/recruitment? For all the species? ####
str(ant_cook)# log transform TIME

#TIME_DISCOVER
# include TEMP_START in model - account for variation in temp associated with experiment start - ie if already warm enough
discover_time_temp<-lm(log(1+TIME_DISCOVER)~ANT_SP+TEMP_DISCOVER:ANT_SP+TEMP_START, data=ant_cook)
#summary(discover_time_temp)
Anova(discover_time_temp, type='III')## effect of TEMP_START, TEMP_DISCOVER, marg ANT_SP - just LR or ORIGIN is sig

# time to discovery was affected by temp start, temperature, and ant origin
discover_time_temp_re<-lmer(log(1+TIME_DISCOVER)~TEMP_DISCOVER*ANT_SP+(1|TEMP_START), data=ant_cook)
tidy(Anova(discover_time_temp_re, type='II'))

# get emm for ants
disc_temp<-data.frame(emmeans(discover_time_temp, ~ANT_SP))%>%mutate(`Time to`= 'discover')

sp.disc.time<-ggplot(disc_temp, aes(ANT_SP, exp(emmean)-1))+geom_point()+
  geom_errorbar(aes(ymin=exp(emmean)-1-exp(SE), ymax=exp(emmean)-1+exp(SE)), width=0)+
  theme(panel.background = element_blank(), axis.line = element_line(color='black'))+
  labs(x='Origin', y='Time to discovery')+ylim(0,NA)


# time fo trecruitment differs with ant species
sp.recr.time<-ggplot(recruit_temp, aes(ANT_SP, exp(emmean)-1))+geom_point()+
  geom_errorbar(aes(ymin=exp(emmean)-1-exp(SE), ymax=exp(emmean)-1+exp(SE)), width=0)+
  theme(panel.background = element_blank(), axis.line = element_line(color='black'))+
  labs(x='Ant species', y='Time to recruitment')+ylim(0,NA)

plot_grid(sp.disc.time,sp.recr.time)

# normality of residuals
shapiro.test(resid(discover_time_temp))

# TIME_DISCOVER
recruit_time_temp<-lm(log(1+TIME_RECRUIT)~ANT_SP+ANT_SP:TEMP_RECRUIT+TEMP_DISCOVER, data=ant_cook)
#summary(recruit_time_temp)
Anova(recruit_time_temp, type='III')

# effect of ANT_SP, TEMP_RECRUIT
recruit_temp<-data.frame(emmeans(recruit_time_temp, ~ANT_SP))%>%mutate(`Time to`= 'recruit')
emm.df<-rbind(recruit_temp, disc_temp)

ggplot(emm.df, aes(ANT_SP, exp(emmean)-1))+
  geom_errorbar(aes(ymin=exp(emmean)-1-exp(SE), ymax=exp(emmean)-1+exp(SE)), width=0)+
  geom_point(aes(color=`Time to`),size=2)+
  theme(panel.background = element_blank(), axis.line = element_line(color='black'))+
  labs(x='Ant species', y='Time')+ylim(0,NA)

##############

# normality of residuals
shapiro.test(resid(recruit_time_temp))
## effect of TEMPERATURE and ANT_SP on TIME_RECRUIT, no interaction


# correlation of time and temp recurit
ggplot(ant_cook, aes(TEMP_DISCOVER, TIME_DISCOVER))+
  geom_point(aes(color=ANT_SP))+
  geom_smooth(aes(color=ANT_SP),method='lm', se=F, alpha=.5)+
  theme(panel.background = element_blank(), axis.line = element_line(color='black'))+
  labs(x='Temperature at recruitment', y='Time')

ggplot(ant_cook, aes(TEMP_RECRUIT, TIME_RECRUIT))+
  geom_point(aes(color=ANT_SP))+
  geom_smooth(aes(color=ANT_SP),method='lm', se=F, alpha=.5)+
  theme(panel.background = element_blank(), axis.line = element_line(color='black'))+
  labs(x='Temperature at recruitment', y='Time')

s##	Is discovery time and recruitment time shorter for the Argentine ant than for the natives?
# No - LH was around same time to recruitment as native species
# No - Had longer time to discovery than native species, or was not different than other ANT_SP

################

## test for an effect of ant species on
# discovery time
disc.aov<-aov(log(1+TIME_DISCOVER)~ANT_SP, data=ant_cook)
summary(disc.aov)
shapiro.test(resid(disc.aov)) 
TukeyHSD(disc.aov) # pairwise differences - compare LH to other species

# recruitment time
recr.aov<-aov(TIME_RECRUIT~ANT_SP, data=cookie)
summary(recr.aov) # yes

# most numerous recruitment
max.aov<-lm(ANT_MAX~ANT_SP:TEMP_RECRUIT, data=ant_cook)
Anova(max.aov, type='III')
plot(allEffects(max.aov))
maxrecr<-data.frame(emmeans(max.aov, ~ANT_SP))
maxrecr

ggplot(maxrecr, aes(ANT_SP, emmean))+
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=0)+
  geom_point(size=2)+
  theme(panel.background = element_blank(), axis.line = element_line(color='black'))+
  labs(x='Ant species', y='Max recruitment')+ylim(0,NA)

#################
# Competitive ability
#	Does the Argentine ant find faster the bait when is not competing with other ants? no
# And the native species when they’re not competing against de Argentine?  NOt
# Do native species find faster the bait when they’re not competing with the Argentine ant?

#compare time to recruit by competing
comp.aov<-aov(log(1+TIME_DISCOVER)~TEMP_DISCOVER+COMPETE+ANT_SP, data=ant_cook)
Anova(comp.aov, type='II')
TukeyHSD(comp.aov)
# time to recruit is faster when comepting - but which species?
ant_cook%>%group_by(ANT_SP)%>%summarize(mean=mean(ANT_MAX, na.rm=TRUE), se=se(ANT_MAX))
# LH had the most ants
orig.comp<-data.frame(emmeans(comp.aov, ~ANT_SP|COMPETE))
# Which species has the most numerous recruitment? And the fastest discovery time?
ggplot(orig.comp, aes(ANT_SP, exp(emmean)-1))+
  geom_errorbar(aes(ymin=exp(emmean)-1-exp(SE), ymax=exp(emmean)-1+exp(SE)), width=0)+
  geom_point(aes(color=COMPETE))+
  theme(panel.background = element_blank(), axis.line = element_line(color='black'))+
  labs(x='Ant species', y='Time to recruitment')+ylim(0,NA)
# with or without interaction?
# compete is significant, not ant sp

##########
# Recruitment strategy
# Is always the Argentine ant the one who colonized the baits when there’s no removal or some species colonize it better?
# Does the Argentine ant displace other species when they’re competing? Or any of the native species displaces the Argentine ant?
ant_cook

# recruitment anddiscovery in competition vs without
mod<-lmer(TIME_RECRUIT~ANT_SP*COMPETE+(1|TEMP_RECRUIT), data=ant_cook)
Anova(mod, type='III') # no interaction
plot(allEffects(mod))

ant_cook%>%filter(REMOVAL=='NO')%>%mutate(winner=ifelse(ANT_COMPETE==0, ))

#times when they compete
compete<-ant_cook%>%filter(ANT_COMPETE==1)
compete ## there are only 7 data points

#removals
removal<-ant_cook%>%filter(REMOVAL=='NO')
removal

comps<-cookie%>%filter(REMOVAL=='NO')
comps%>%select(ANT_SP, COMPETE) ## 19 data points
dim(comps%>%filter(COMPETE=='YES'))
dim(comps%>%filter(COMPETE=='NO'))
7/19 # compete 36.8% of times in removal

# does LH always recruit first? (ANT_COMPETE==1)
removal
#removal%>%group_by(COMPETE,ANT_SP)%>%
 # summarize(n_first=length(ANT_COMPETE == 1), n_second = length(ANT_COMPETE == 2))
# when competing LH recruited first3 times, second 4 times




