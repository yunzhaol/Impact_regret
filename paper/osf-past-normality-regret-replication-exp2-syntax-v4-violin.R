######################################
# load libraries

if(!require(Rcpp)){install.packages('Rcpp', dependencies = TRUE)}
#if(!require(jmv)){install.packages('jmv', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))}
if(!require(jmv)){install.packages('jmv', dependencies = TRUE)}
if(!require(rstudioapi)){install.packages('rstudioapi', dependencies = TRUE)}
if(!require(effsize)){install.packages('effsize', dependencies = TRUE)}
if(!require(Hmisc)){install.packages('effsize', dependencies = TRUE)}
if(!require(psych)){install.packages('psych', dependencies = TRUE)}
if(!require(ggplot2)){install.packages('ggplot2', dependencies = TRUE)}
if(!require(dplyr)){install.packages('dplyr', dependencies = TRUE)}
if(!require(MBESS)){install.packages('MBESS', dependencies = TRUE)}
if(!require(jtools)){install.packages('jtools', dependencies = TRUE)}

library(jtools)
library(MBESS)
library(psych)
library(ggplot2)
library(dplyr) 
library(Hmisc)
library(effsize)
library(rstudioapi)
library(jmv)
library(Rcpp)


# setting formatting options
options(scipen=999.99, digits =7)

# a tweak to point RStudio to the current directory of the R file
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)

# load our dataset 
data <- read.csv("osf-past-normality-regret-replication-exp2-data-v2.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
str(data)

# number of participants
NROW(na.omit(data$gender))

#Show demographics
#Gender (1-male; 2-female)
#set value labels
data$gender<-factor(data$gender,levels = c(1,2,3), labels=c("Male", "Female", "Rather not say"))
data$gender
table(data$gender)

#Age
data$age
data$age[data$age==99] <- NA
mean(data$age, na.rm = TRUE)
sd(data$age, na.rm = TRUE)

###################
# Robbery-Scenario

#Preparation for ANOVA
#Bring data it format with two rows (Compensation and Condition)

# calculate condition
data$Sc3condition <- 0
data$compensationagg <- 0
data$regretagg <- 0

for (i in 1:nrow(data)){
  if (!is.na(data$Sc3_C1_text[i])){
    data$Sc3condition[i] <- 1
    data$compensationagg[i] <- data$sc3_c1_compensation[i]
    data$regretagg[i] <- data$sc3_c1_regret[i]
  }
  else if (!is.na(data$Sc3_C2_text[i])){
    data$Sc3condition[i] <- 2
    data$compensationagg[i] <- data$sc3_c2_compensation[i]
    data$regretagg[i] <- data$sc3_c2_regret[i]
  }
  else if (!is.na(data$Sc3_C3_text[i])){
    data$Sc3condition[i] <- 3
    data$compensationagg[i] <- data$sc3_c3_compensation[i]
    data$regretagg[i] <- data$sc3_c3_regret[i]
  }
  else {
    data$Sc3condition[i] <- NA
    data$compensationagg[i] <- NA
    data$regretagg[i] <- NA
  }
}

#value labels
data$Sc3conditionl<-factor(data$Sc3condition,levels = c(1,2,3), labels=c("Routine", "Self-produced exception", "Other-produced exception"))


# let's have a look at this
table(data$Sc3conditionl)
table(data$Sc3conditionl)

#Adjust Copensation-Scale to original paper (Values from 0 to 10, instead of 1 to 11)
data$compensationaggrecoded = data$compensationagg-1

#Adjust Regret-Scale (Values from 1 to 5, instead of 0 to 4)
data$regretaggrecoded = data$regretagg+1


# Label Variables
names (data$compensationaggrecoded) <- c("0", "100,000", "200,000", "300,000", "400,000", "500,000", "600,000", "700,000", "800,000", "900,000", "1,000,000")
names (data$regretaggrecoded) <- c("no regret", "weak regret", "medium regret", "strong regret", "very strong regret")


########
# Compensation
########

# get descriptives
describeBy(data$compensationaggrecoded, data$Sc3conditionl, mat=TRUE)

# plot it, doesn't look normally distributed
hist(data$compensationagg)

# test for normality
shapiro.test(data$compensationagg)

# run an omnibus ANOVA for compensation
# from https://blogs.uoregon.edu/rclub/2015/11/03/anova-contrasts-in-r/ 
model <- aov(compensationaggrecoded ~ Sc3conditionl, data = data)
summary(model)

# contrasts
TukeyHSD(model)
t.test(
  data[which(data$Sc3conditionl=="Routine"),]$compensationaggrecoded, 
  data[which(data$Sc3conditionl=="Self-produced exception"),]$compensationaggrecoded)
t.test(
  data[which(data$Sc3conditionl=="Routine"),]$compensationaggrecoded, 
  data[which(data$Sc3conditionl=="Other-produced exception"),]$compensationaggrecoded)
t.test(
  data[which(data$Sc3conditionl=="Self-produced exception"),]$compensationaggrecoded, 
  data[which(data$Sc3conditionl=="Other-produced exception"),]$compensationaggrecoded)
wilcox.test(
  data[which(data$Sc3conditionl=="Routine"),]$compensationaggrecoded, 
  data[which(data$Sc3conditionl=="Self-produced exception"),]$compensationaggrecoded)
wilcox.test(
  data[which(data$Sc3conditionl=="Routine"),]$compensationaggrecoded, 
  data[which(data$Sc3conditionl=="Other-produced exception"),]$compensationaggrecoded)
wilcox.test(
  data[which(data$Sc3conditionl=="Self-produced exception"),]$compensationaggrecoded, 
  data[which(data$Sc3conditionl=="Other-produced exception"),]$compensationaggrecoded)
smd(
  data[which(data$Sc3conditionl=="Routine"),]$compensationaggrecoded, 
  data[which(data$Sc3conditionl=="Self-produced exception"),]$compensationaggrecoded)
smd(
  data[which(data$Sc3conditionl=="Routine"),]$compensationaggrecoded, 
  data[which(data$Sc3conditionl=="Other-produced exception"),]$compensationaggrecoded)
smd(
  data[which(data$Sc3conditionl=="Self-produced exception"),]$compensationaggrecoded, 
  data[which(data$Sc3conditionl=="Other-produced exception"),]$compensationaggrecoded)


# plot it out
data.plot = data[which(!is.na(data$Sc3conditionl)),]
groups <- group_by(data.plot, Sc3conditionl) # this just prepares it for us to calculate eveyrthing within each condition
plot.data <- summarise(groups,
                       mean = mean(compensationaggrecoded, na.rm=TRUE),
                       sd = sd(compensationaggrecoded, na.rm=TRUE),
                       n = n(),
                       se=sd/sqrt(n),
                       ci = qt(0.975, df=n-1)*se)
plot.data # take a peek
ggplot(plot.data, aes(x=Sc3conditionl, y=mean, group = factor(1))) +
  geom_line() +
  geom_point() + 
  xlab("Condition") +
  ylab("Compensation") +
  expand_limits(y=0) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1) +
  ggtitle("Mean compensation by condition")

threeconditionscompensation <- ggplot(data, aes(x=Sc3conditionl, y=compensationaggrecoded)) + 
  geom_violin() + geom_violin(trim=FALSE) + 
  scale_x_discrete(limits=c("Routine", "Other-produced exception", "Self-produced exception")) +
  geom_jitter(shape=16, position=position_jitter(height=0.1, width=0.2)) +
  scale_color_grey() + scale_fill_grey() + 
  theme_apa() +
  theme(axis.title.x=element_blank(), 
        axis.text = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        panel.grid.major = element_line(colour="grey", size = (0.1)),
        panel.grid.minor = element_line(size = (0.1), colour="grey"),
        text=element_text(family="Arial"))+
  xlab("Condition") +
  ylab("Compensation") +
  scale_y_continuous(minor_breaks = seq(-10, 15, 5)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.90), width = 0.2) +
  stat_summary(fun.y=mean, geom="point", shape=18,
               size=3)
threeconditionscompensation


########
# Regret
########


# get descriptives
describeBy(data$regretaggrecoded, data$Sc3conditionl, mat=TRUE)


# run an omnibus ANOVA
model <- aov(regretaggrecoded ~ Sc3conditionl, data = data)
summary(model)

# contrasts
TukeyHSD(model)
t.test(
  data[which(data$Sc3conditionl=="Routine"),]$regretaggrecoded, 
  data[which(data$Sc3conditionl=="Self-produced exception"),]$regretaggrecoded)
t.test(
  data[which(data$Sc3conditionl=="Routine"),]$regretaggrecoded, 
  data[which(data$Sc3conditionl=="Other-produced exception"),]$regretaggrecoded)
t.test(
  data[which(data$Sc3conditionl=="Self-produced exception"),]$regretaggrecoded, 
  data[which(data$Sc3conditionl=="Other-produced exception"),]$regretaggrecoded)
smd(
  data[which(data$Sc3conditionl=="Routine"),]$regretaggrecoded, 
  data[which(data$Sc3conditionl=="Self-produced exception"),]$regretaggrecoded)
smd(
  data[which(data$Sc3conditionl=="Routine"),]$regretaggrecoded, 
  data[which(data$Sc3conditionl=="Other-produced exception"),]$regretaggrecoded)
smd(
  data[which(data$Sc3conditionl=="Self-produced exception"),]$regretaggrecoded, 
  data[which(data$Sc3conditionl=="Other-produced exception"),]$regretaggrecoded)

# plot it out
data.plot = data[which(!is.na(data$Sc3conditionl)),]
groups <- group_by(data.plot, Sc3conditionl) # this just prepares it for us to calculate eveyrthing within each condition
plot.data <- summarise(groups,
                       mean = mean(regretaggrecoded, na.rm=TRUE),
                       sd = sd(regretaggrecoded, na.rm=TRUE),
                       n = n(),
                       se=sd/sqrt(n),
                       ci = qt(0.975, df=n-1)*se)
plot.data # take a peek
ggplot(plot.data, aes(x=Sc3conditionl, y=mean, group = factor(1))) +
  geom_line() +
  geom_point() +
  xlab("Condition") +
  ylab("Regret") +
  expand_limits(y=0) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1) +
  ggtitle("Mean regret by condition")

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

threeconditionsregret <- ggplot(data, aes(x=Sc3conditionl, y=regretaggrecoded)) + 
  geom_violin() + geom_violin(trim=FALSE) + 
  scale_x_discrete(limits=c("Routine", "Other-produced exception", "Self-produced exception")) +
  geom_jitter(shape=16, position=position_jitter(height=0.1, width=0.2)) +
  scale_color_grey() + scale_fill_grey() + 
  theme_apa() +
  theme(axis.title.x=element_blank(), 
        axis.text = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        panel.grid.major = element_line(colour="grey", size = (0.1)),
        panel.grid.minor = element_line(size = (0.1), colour="grey"),
        text=element_text(family="Arial"))+
  xlab("Condition") +
  ylab("Regret") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.90), width = 0.2) +
  stat_summary(fun.y=mean, geom="point", shape=18,
                 size=3)
threeconditionsregret



########
# COMBINING EXCEPTIONAL conditions
########

#Preparation for t-test (Original analysis)
#Bring data it format with two rows (Compensation and Condition)

# calculate condition
data$Sc3condition1 <- 0
data$compensationagg1 <- 0
data$regretagg1 <- 0
for (i in 1:nrow(data)){
  if (!is.na(data$Sc3_C1_text[i])){
    data$Sc3condition1[i] <- 1
    data$compensationagg1[i] <- data$sc3_c1_compensation[i]
    data$regretagg1[i] <- data$sc3_c1_regret[i]
  }
  else if (!is.na(data$Sc3_C2_text[i])){
    data$Sc3condition1[i] <- 2
    data$compensationagg1[i] <- data$sc3_c2_compensation[i]
    data$regretagg1[i] <- data$sc3_c2_regret[i]
  }
  else if (!is.na(data$Sc3_C3_text[i])){
    data$Sc3condition1[i] <- 2
    data$compensationagg1[i] <- data$sc3_c3_compensation[i]
    data$regretagg1[i] <- data$sc3_c3_regret[i]
  }
  else {
    data$Sc3condition1[i] <- NA
    data$compensationagg1[i] <- NA
    data$regretagg1[i] <- NA
  }
}

#value labels
data$Sc3conditionl1<-factor(data$Sc3condition1,levels = c(1,2), labels=c("Routine", "Exception"))

# let's have a look at this
table(data$Sc3conditionl1)

#Adjust Copensation-Scale to original paper (Values from 0 to 10, instead of 1 to 11)
data$compensationaggrecoded1 = data$compensationagg1-1

#Adjust Regret Scale (Values 1 to 5, indead of 0 to 4)
data$regretaggrecoded1 = data$regretagg1+1

# Label Variables
names (data$compensationaggrecoded1) <- c("0", "100,000", "200,000", "300,000", "400,000", "500,000", "600,000", "700,000", "800,000", "900,000", "1,000,000")
names (data$regretaggrecoded1) <- c("no regret", "weak regret", "medium regret", "strong regret", "very strong regret")



#######
# Compensation
########

# get descriptives
describeBy(data$compensationaggrecoded1, data$Sc3conditionl1, mat=TRUE)

# run an omnibus ANOVA for compensation
# from https://blogs.uoregon.edu/rclub/2015/11/03/anova-contrasts-in-r/ 
model <- aov(compensationaggrecoded1 ~ Sc3conditionl1, data = data)
summary(model)

# contrasts
TukeyHSD(model)
t.test(
  data[which(data$Sc3conditionl1=="Routine"),]$compensationaggrecoded1, 
  data[which(data$Sc3conditionl1=="Exception"),]$compensationaggrecoded1)

smd(
  data[which(data$Sc3conditionl1=="Routine"),]$compensationaggrecoded1, 
  data[which(data$Sc3conditionl1=="Exception"),]$compensationaggrecoded1)

exceptioncombinedcompensationplot <- ggplot(data, aes(x=Sc3conditionl1, y=compensationaggrecoded1)) + 
  geom_violin() + geom_violin(trim=FALSE) + 
  scale_x_discrete(limits=c("Routine", "Exception")) +
  geom_jitter(shape=16, position=position_jitter(height=0.1, width=0.2)) +
  scale_color_grey() + scale_fill_grey() + 
  theme_apa() +
  theme(axis.title.x=element_blank(), 
        axis.text = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        panel.grid.major = element_line(colour="grey", size = (0.1)),
        panel.grid.minor = element_line(size = (0.1), colour="grey"),
        text=element_text(family="Arial"))+
  xlab("Condition") +
  ylab("Compensation") +
  scale_y_continuous(minor_breaks = seq(-10, 15, 5)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.90), width = 0.2) +
  stat_summary(fun.y=mean, geom="point", shape=18,
               size=3)
exceptioncombinedcompensationplot


########
# Regret
########

# get descriptives
describeBy(data$regretaggrecoded1, data$Sc3conditionl1, mat=TRUE)

# run an omnibus ANOVA for compensation
# from https://blogs.uoregon.edu/rclub/2015/11/03/anova-contrasts-in-r/ 
model <- aov(regretaggrecoded1 ~ Sc3conditionl1, data = data)
summary(model)

# contrasts
TukeyHSD(model)
t.test(
  data[which(data$Sc3conditionl1=="Routine"),]$regretaggrecoded1, 
  data[which(data$Sc3conditionl1=="Exception"),]$regretaggrecoded1)

smd(
  data[which(data$Sc3conditionl1=="Routine"),]$regretaggrecoded1, 
  data[which(data$Sc3conditionl1=="Exception"),]$regretaggrecoded1)

exceptioncombinedregretplot <- ggplot(data, aes(x=Sc3conditionl1, y=regretaggrecoded1)) + 
  geom_violin() + geom_violin(trim=FALSE) + 
  scale_x_discrete(limits=c("Routine", "Exception")) +
  geom_jitter(shape=16, position=position_jitter(height=0.1, width=0.2)) +
  scale_color_grey() + scale_fill_grey() + 
  theme_apa() +
  theme(axis.title.x=element_blank(), 
        axis.text = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        panel.grid.major = element_line(colour="grey", size = (0.1)),
        panel.grid.minor = element_line(size = (0.1), colour="grey"),
        text=element_text(family="Arial"))+
  xlab("Condition") +
  ylab("Regret") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.90), width = 0.2) +
  stat_summary(fun.y=mean, geom="point", shape=18,
               size=3)
exceptioncombinedregretplot
