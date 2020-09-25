library(ggplot2)
library(emmeans)
library(mosaic)
library(dplyr)

# Cancer data 
cancer <- read.file('cancer.txt')
print(cancer)
print(sapply(cancer, class))
print(any(is.na(cancer)))


print(head(cancer,10))
print(summary(cancer))
print(str(cancer))


print(favstats(Survival ~ Organ ,data=cancer))

ggplot(cancer,aes(y=Survival,color=Organ))+geom_boxplot()+
  theme_bw()



ggplot(cancer,aes(x=Survival,color=Organ,fill=Organ))+
  geom_histogram(aes(y=..density..),position='identity',bins = 35,alpha=.4)+
  geom_density(alpha=.4)+
  theme_bw()+
  scale_color_brewer(palette = 'Dark2')


ggplot(cancer,aes(x=Organ,y=Survival,fill=Organ,color=Organ))+
  geom_jitter(alpha=.5)+
  theme_linedraw()

# Transforming cancer data log transformation

logTransformedSurvavial <- cancer %>% mutate(logSurvival=log(Survival))
logTransformedSurvavial <- logTransformedSurvavial %>% select(-Survival)
print(logTransformedSurvavial)

ggplot(logTransformedSurvavial,aes(y=logSurvival,color=Organ))+
  geom_boxplot()+
  theme_bw()

ggplot(logTransformedSurvavial,aes(x=logSurvival,color=Organ,fill=Organ))+
  geom_histogram(aes(y=..density..),position='identity',bins = 35,alpha=.4)+
  geom_density(alpha=.4)+
  theme_bw()+
  scale_color_brewer(palette = 'Dark2')

########################################

# fitting the cancer dataset into linear model

lmModel <- lm(Survival ~ Organ, data=cancer)
print(lmModel)
print(summary(lmModel))


########################################

# fitting the log transformed data into lmMoodel

lmLogModel <- lm(logSurvival ~ Organ, data = logTransformedSurvavial)
print(summary(lmLogModel))

# least significant difference test,

lsMeans <- lsmeans(lmModel,'Organ',data=cancer)
LSD <- contrast(lsMeans,method='pairwise',adjust = 'none')
print(LSD)

# 95% confidence interval
print(confint(LSD))

#Anova test
print(anova(lmModel))
