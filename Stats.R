####The influence of soil temperature on bud burst####
####analysing and visualising treatment####
#read soil temperature dat
setwd('C:\\Users\\Janus\\Desktop\\UNI LENC\\WS20_21\\Exped 2\\Exped-fucks')
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
<<<<<<< HEAD
boxplot(Soiltemp ~ Sensor, data=dat0C, ylab = 'Temperature (?C)')
boxplot(Soiltemp ~ Sensor, data=dat6C, ylab = 'Temperature (?C)')
boxplot(Soiltemp ~ Sensor, data=dat12C, ylab = 'Temperature (?C)')
boxplot(Soiltemp ~ Sensor, data=datISO, ylab = 'Temperature (?C)')
boxplot(Soiltemp ~ Sensor, data=datNO, ylab = 'Temperature (?C)')

#summerize all in one plot 0C, 6C, 12C, Iso, NoISO
boxplot(Soiltemp~Treatment, data = datsum, ylab = 'Temperature (?C)')
=======
boxplot(Soiltemp ~ Sensor, data=dat0C, ylab = 'Temperature (°C)')
boxplot(Soiltemp ~ Sensor, data=dat6C, ylab = 'Temperature (°C)')
boxplot(Soiltemp ~ Sensor, data=dat12C, ylab = 'Temperature (°C)')
boxplot(Soiltemp ~ Sensor, data=datISO, ylab = 'Temperature (°C)')
boxplot(Soiltemp ~ Sensor, data=datNO, ylab = 'Temperature (°C)')

#summerize all in one plot 0C, 6C, 12C, Iso, NoISO
boxplot(Soiltemp~Treatment, data = datsum, ylab = 'Temperature (°C)')
>>>>>>> 1592faa45e8341d9d8ff2679ea6c24ff11834872

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

<<<<<<< HEAD
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

?qplot
library(gitcreds)
gitcreds_get()
library(credentials)
set_github_pat()
=======
?qplot
>>>>>>> 1592faa45e8341d9d8ff2679ea6c24ff11834872
