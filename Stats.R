####The influence of soil temperature on bud burst####
####analysing and visualising treatment####
#read soil temperature dat
setwd('C:\\Users\\Frederik\\Documents\\Studiumbewerbung\\Greifswald\\Experimental Plant Ecology\\Ecological Exp')
library(readxl)
library(dplyr)
dat <- read_excel('soiltempSummary.xlsx')

#data check
str(dat)
summary(dat)

#subset data by temperature
dat0C <- dat %>% slice(1:5439)
dat6C <- dat %>% slice(5440:10878)
dat12C <- dat %>% slice(10879:16317)
datISO  <- dat%>% slice(16318:34447)
datNO <- dat%>% slice(34448:52577)

#plot soil temperature: boxplot
?boxplot
boxplot(Soiltemp ~ Sensor, data=dat0C, ylab = 'Temperature (?C)')
boxplot(Soiltemp ~ Sensor, data=dat6C, ylab = 'Temperature (?C)')
boxplot(Soiltemp ~ Sensor, data=dat12C, ylab = 'Temperature (?C)')
boxplot(Soiltemp ~ Sensor, data=datISO, ylab = 'Temperature (?C)')
boxplot(Soiltemp ~ Sensor, data=datNO, ylab = 'Temperature (?C)')

#summerize all in one plot 0C, 6C, 12C, Iso, NoISO
boxplot(Soiltemp~Treatment, data = datsum, ylab = 'Temperature (?C)')

#signifant difference between groups?
library(multcomp)
library(sciplot)
library(lmerTest)
?lmer
mod <- lmer(Soiltemp ~ Treatment + (1|Sensor), data =datsum)
#check diagnostics
par(mfrow = c(1,2))
plot(fitted(mod), resid(mod), xlab = 'fitted', ylab = 'residuals')
qqnorm(resid(mod), main = "")
qqline(resid(mod), main = "", col = 2)
#transformation, because parametric assumtions are not satisfied
t <- lmer(rank(Soiltemp) ~ Treatment + (1|Sensor), data =datsum)
#testing the model
anova(t)
# anova implies a significance
# posthoc testing: pairwise testing with Tukey
phtuk <- glht(t, linfct = mcp(Treatment = 'Tukey'))
?glht
summary(phtuk)

####time series of bud burst####
buddat <- read.csv2('budburst-timeseries.csv')
buddat[buddat == 9999] <- NA
str(buddat)
cl<- rainbow(33)
?rainbow
plot(buddat$tree1 ~ buddat$post.treat..Days, type = 'l', 
     ylab = 'open buds', xlab = 'days post treatment', ylim = c(0,100))
for (i in 3:58){
  lines(buddat$post.treat..Days, buddat[,i], col = cl[i])
}

#with data ordered by column
budnew <- read_excel('bud-burstnew.xlsx')
budnew[budnew == 9999] <- NA
str(budnew)
budnew$tree <- as.factor(budnew$tree)
#plot(budnew$`open buds`~budnew$`post treat. Days`, type ='l')
library(ggplot2)
qplot(x=`post treat. Days`, y=`open buds`, 
      data=budnew, 
      colour=tree, 
      xlab = 'days post treatment',
      ylab = 'open buds') +
  geom_line()

#plot data ordered by column and group by treatment
#subset to only climate chamber trees
dattreat <- budnew %>% slice(1:1188)
dattreat$treatment <- as.factor(dattreat$treatment)
str(dattreat)
theme_set(theme_bw())
ggplot(dattreat, aes(x =`post treat. Days`, y =`open buds`,
                     group = interaction(tree, treatment),
                     colour = treatment)) +
  geom_line()

#signifant difference between treatments?
library(multcomp)
library(sciplot)
library(lmerTest)
?lmer
mod1 <- lmer(`open buds` ~ treatment + (1|tree), data =dattreat)
#check diagnostics
par(mfrow = c(1,2))
plot(fitted(mod1), resid(mod1), xlab = 'fitted', ylab = 'residuals')
qqnorm(resid(mod1), main = "")
qqline(resid(mod1), main = "", col = 2)
#transformation, because parametric assumtions are not satisfied
t <- lmer(rank(`open buds`) ~ treatment + (1|tree), data =dattreat)
plot(fitted(t), resid(t), xlab = 'fitted', ylab = 'residuals')
qqnorm(resid(t), main = "")
qqline(resid(t), main = "", col = 2)
#testing the model
anova(t)
# anova implies a significance
# posthoc testing: pairwise testing with Tukey
phtuk <- glht(t, linfct = mcp(treatment = 'Tukey'))
?glht
summary(phtuk)
