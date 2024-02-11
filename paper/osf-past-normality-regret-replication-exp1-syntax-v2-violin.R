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
data <- read.csv("osf-past-normality-regret-replication-data.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
str(data)

# To check exclusions based on pre-registration criteria uncomment the following
# Criteria:
# Serious lower than 5
# English understanding lower than 5
# data <- data[ which(4<data$serious 
#                     & data$engunder > 4), ]


#Show demographics
#Age
data$gender
table(data$gender)
#Gender
data$age
data$age[data$age==99] <- NA
mean(data$age, na.rm = TRUE)
sd(data$age, na.rm = TRUE)

###################
# EXPERIMENT 1 (Hitchhiker-Scenario)

# JAMOVI requires factors, while R imports as numeric. So, need to convert from numeric to factor.
data$Sc1_regret<- factor(data$Sc1_regret)
data$sc1_socnorms1<- factor(data$sc1_socnorms1)
data$sc1_socnorms2<- factor(data$sc1_socnorms2)
data$sc1_combinednorms<- factor(data$sc1_combinednorms)

# Let's label the values better, so it's easier to understand the output.
data$Sc1_regret <- ordered(data$Sc1_regret, levels = c(1,2), labels = c("Exception Jones", "Routine Smith"))
data$sc1_socnorms1 <- ordered(data$sc1_socnorms1, levels = c(1,2), labels = c("Exception Jones", "Routine Smith"))
data$sc1_socnorms2 <- ordered(data$sc1_socnorms2, levels = c(1,2), labels = c("Exception Jones", "Routine Smith"))
data$sc1_combinednorms <- ordered(data$sc1_combinednorms, levels = c(1,2), labels = c("Exception Jones", "Routine Smith"))

# Let's label the variables better, so we'll remember what those mean and it's easier to understand the output when those are reported.
label(data$Sc1_regret) <- "Who experiences higher regret (direct replication)" 
label(data$sc1_socnorms1) <- "Descriptive norms - which is more common?" 
label(data$sc1_socnorms2) <- "Injunctive norms - who is more criticized by society?" 
label(data$sc1_combinednorms) <- "Who experiences higher regret, when asking participants to consider the norm" 


# Let's run the JAMOVI imported syntax 

# Descriptives for the main variables.
# Plots appear in the R Studio Plots section
jmv::descriptives(
  data=data,
  vars=c(
    "Sc1_regret",
    "sc1_socnorms1",
    "sc1_socnorms2",
    "sc1_combinednorms"),
  freq=TRUE)


# binomial Z
jmv::propTest2(
  data=data,
  vars=c(
    "Sc1_regret",
    "sc1_socnorms1",
    "sc1_socnorms2",
    "sc1_combinednorms"),
  ci=TRUE)

jmv::propTestN(
  data=data,
  var="Sc1_regret",
  expected=TRUE,
  ratio=c(1, 1))

jmv::propTestN(
  data=data,
  var="sc1_socnorms1",
  expected=TRUE,
  ratio=c(1, 1))

jmv::propTestN(
  data=data,
  var="sc1_socnorms2",
  expected=TRUE,
  ratio=c(1, 1))

jmv::propTestN(
  data=data,
  var="sc1_combinednorms",
  expected=TRUE,
  ratio=c(1, 1))



# You really don't need JAMOVI
# You can also do this with regular R syntax, which to me is easier.

# counts
summary(data$Sc1_regret)
summary(data$sc1_socnorms1)
summary(data$sc1_socnorms2)
summary(data$sc1_combinednorms)
plot(data$Sc1_regret)
plot(data$sc1_socnorms1)
plot(data$sc1_socnorms2)
plot(data$sc1_combinednorms)

# R's way of doing binomial
# we need to count, while excluding NAs
x1 <- sum((data$Sc1_regret[!is.na(data$Sc1_regret)])=="Exception Jones")
n1 <- length(data$Sc1_regret[!is.na(data$Sc1_regret)])
prop.test(x1, n1, p=0.5, correct = FALSE)

x2 <- sum((data$sc1_socnorms1[!is.na(data$sc1_socnorms1)])=="Exception Jones")
n2 <- length(data$sc1_socnorms1[!is.na(data$sc1_socnorms1)])
prop.test(x2, n2, p=0.5, correct = FALSE)

x3 <- sum((data$sc1_socnorms2[!is.na(data$sc1_socnorms2)])=="Exception Jones")
n3 <- length(data$sc1_socnorms2[!is.na(data$sc1_socnorms2)])
prop.test(x3, n3, p=0.5, correct = FALSE)

x4 <- sum((data$sc1_combinednorms[!is.na(data$sc1_combinednorms)])=="Exception Jones")
n4 <- length(data$sc1_combinednorms[!is.na(data$sc1_combinednorms)])
prop.test(x4, n4, p=0.5, correct = FALSE)


# or...
# binom.test(x, n,  p = 0.5,
#            alternative = c("two.sided", "less", "greater"),
#            conf.level = 0.95)


###################
# EXPERIMENT 2 (Car Accident-Scenario)

#Measure correction: All particpants who indicated (5- somwhat agree) on the question regarding random chance (data$Sc2_random_1 or data$Sc2_random_2) had accidently assigned the value '56' instead of '5' in Qualtrics
data$Sc2_random_1[data$Sc2_random_1==56] <- 5
data$Sc2_random_2[data$Sc2_random_2==56] <- 5

# Conversion from numeric to factors 
data$Sc2_regret<- factor(data$Sc2_regret)
data$Sc2_lucky<- factor(data$Sc2_lucky)

# Label values
data$Sc2_regret <- ordered(data$Sc2_regret, levels = c(1,2), labels = c("Routine Adams", "Exception White"))
data$Sc2_lucky <- ordered(data$Sc2_lucky, levels = c(1,2), labels = c("Adams less lucky", "White less lucky"))
names (data$Sc2_random_1) <- c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")
names (data$Sc2_random_2) <- c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")

# Label Variables
label(data$Sc2_regret) <- "Who feels more upset (direct replication)"
label(data$Sc2_random_1) <- "Adam's (Routine) accident is a random coincidence"
label(data$Sc2_random_2) <- "White's' (Exception) accident is a random coincidence"
label(data$Sc2_lucky) <- "Who is less lucky"

# Descriptives for main variables
jmv::descriptives(
  data=data,
  vars=c(
    "Sc2_regret",
    "Sc2_random_1",
    "Sc2_random_2",
    "Sc2_lucky"),
  freq=TRUE)



# binomial Z
jmv::propTest2(
  data=data,
  vars=c(
    "Sc2_regret",
    "Sc2_lucky"),
  ci=TRUE)

jmv::propTestN(
  data=data,
  var="Sc2_regret",
  expected=TRUE,
  ratio=c(1, 1))

jmv::propTestN(
  data=data,
  var="Sc2_lucky",
  expected=TRUE,
  ratio=c(1, 1))

#Chi2 without Jamovi for regret and luck

x5 <- sum((data$Sc2_regret[!is.na(data$Sc2_regret)])=="Exception White")
n5 <- length(data$Sc2_regret[!is.na(data$Sc2_regret)])
prop.test(x5, n5, p=0.5, correct = FALSE)

x6 <- sum((data$Sc2_lucky[!is.na(data$Sc2_lucky)])=="Exception White")
n6 <- length(data$Sc2_lucky[!is.na(data$Sc2_lucky)])
prop.test(x6, n6, p=0.5, correct = FALSE)

#t-test for "random chance"-Variable

data$Sc2_random_1
data$Sc2_random_2
table(data$Sc2_random_1)
table(data$Sc2_random_2)
summary(data$Sc2_random_1)
summary(data$Sc2_random_2)
sd(data$Sc2_random_1, na.rm = TRUE)
sd(data$Sc2_random_2, na.rm = TRUE)
t.test(data$Sc2_random_1,data$Sc2_random_2, paired=TRUE)
hist(data$Sc2_random_1, main = "Histogram of routine randomness", xlab = "Adam's (Routine) accident is a random coincidence")
hist(data$Sc2_random_2, main = "Histogram of exception randomness", xlab = "White's' (Exception) accident is a random coincidence")


###################
#EXPERIMENT 3 (Robbery-Scenario)

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
ci.smd(smd=smd(data[which(data$Sc3conditionl=="Routine"),]$compensationaggrecoded, 
               data[which(data$Sc3conditionl=="Self-produced exception"),]$compensationaggrecoded), 
       n.1=length(data[which(data$Sc3conditionl=="Routine"),]$compensationaggrecoded), 
       n.2=length(data[which(data$Sc3conditionl=="Self-produced exception"),]$compensationaggrecoded), 
       conf.level=0.95)
ci.smd(smd=smd(data[which(data$Sc3conditionl=="Routine"),]$compensationaggrecoded, 
               data[which(data$Sc3conditionl=="Other-produced exception"),]$compensationaggrecoded), 
       n.1=length(data[which(data$Sc3conditionl=="Routine"),]$compensationaggrecoded), 
       n.2=length(data[which(data$Sc3conditionl=="Other-produced exception"),]$compensationaggrecoded), 
       conf.level=0.95)
ci.smd(smd=smd(data[which(data$Sc3conditionl=="Self-produced exception"),]$compensationaggrecoded, 
               data[which(data$Sc3conditionl=="Other-produced exception"),]$compensationaggrecoded), 
       n.1=length(data[which(data$Sc3conditionl=="Self-produced exception"),]$compensationaggrecoded), 
       n.2=length(data[which(data$Sc3conditionl=="Other-produced exception"),]$compensationaggrecoded), 
       conf.level=0.95)


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
  expand_limits(y=4) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1)

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
ci.smd(smd=smd(data[which(data$Sc3conditionl=="Routine"),]$regretaggrecoded, 
               data[which(data$Sc3conditionl=="Self-produced exception"),]$regretaggrecoded), 
       n.1=length(data[which(data$Sc3conditionl=="Routine"),]$regretaggrecoded), 
       n.2=length(data[which(data$Sc3conditionl=="Self-produced exception"),]$regretaggrecoded), 
       conf.level=0.95)
ci.smd(smd=smd(data[which(data$Sc3conditionl=="Routine"),]$regretaggrecoded, 
               data[which(data$Sc3conditionl=="Other-produced exception"),]$regretaggrecoded), 
       n.1=length(data[which(data$Sc3conditionl=="Routine"),]$regretaggrecoded), 
       n.2=length(data[which(data$Sc3conditionl=="Other-produced exception"),]$regretaggrecoded), 
       conf.level=0.95)
ci.smd(smd=smd(data[which(data$Sc3conditionl=="Self-produced exception"),]$regretaggrecoded, 
               data[which(data$Sc3conditionl=="Other-produced exception"),]$regretaggrecoded), 
       n.1=length(data[which(data$Sc3conditionl=="Self-produced exception"),]$regretaggrecoded), 
       n.2=length(data[which(data$Sc3conditionl=="Other-produced exception"),]$regretaggrecoded), 
       conf.level=0.95)




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
  expand_limits(y=3) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1)



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

ci.smd(smd=smd(data[which(data$Sc3conditionl1=="Exception"),]$compensationaggrecoded1,
               data[which(data$Sc3conditionl1=="Routine"),]$compensationaggrecoded1), 
       n.1=length(data[which(data$Sc3conditionl1=="Routine"),]$compensationaggrecoded1), 
       n.2=length(data[which(data$Sc3conditionl1=="Exception"),]$compensationaggrecoded1), 
       conf.level=0.95)

# what was the effect in the original study?
ci.smd(ncp=2.17, n.1=58, n.2 = 105, conf.level=0.95)

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

ci.smd(smd=smd(data[which(data$Sc3conditionl1=="Exception"),]$regretaggrecoded1,
               data[which(data$Sc3conditionl1=="Routine"),]$regretaggrecoded1), 
       n.1=length(data[which(data$Sc3conditionl1=="Routine"),]$regretaggrecoded1), 
       n.2=length(data[which(data$Sc3conditionl1=="Exception"),]$regretaggrecoded1), 
       conf.level=0.95)

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

