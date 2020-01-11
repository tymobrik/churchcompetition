
#CHURCH COMPETITION HYPOTHESIS REVISITED: NEW EVIDENCE FROM UKRAINE, 1992-2012
#
#Corresponding author:
#Tymofii Brik
#Carlos III, Social Science
#tbrik@clio.uc3m.es; tymofiisoc@gmail.com
#
#DATA SOURCES
#Survey 1: Institute of Sociology of National Academy of Science of Ukraine, 1992-2006.
#Survey 2: Europeand Social Survey, Ukraine, 2002-2012
#Contextual data 1: Ukrainian national statustics
#Contextual data 2: REVIVAL project data (church communities in Ukraine, 1991-2013)

#*********************************************************************************************************
#Enter your directory here:
setwd("/Users/tymo-brik/Documents/Projects/Working papers/Manuscript SoR_2018_Intra-doctrinal Competition in Ukraine, 1992-2012/RR3/Replication_package_SoR")
#
library(AER)
library(arm)
library(car) 
library(classInt)
library(foreign)
library(grid)
library(gridExtra)
library(gplots)
library(ggplot2)
library(lattice)
library(lme4) 
library(lmtest)
library(mapproj)
library(maps)
library(mapdata)
library(mapproj)
library(maptools) 
library(multilevel)
library(pglm)
library(plm)
library(plyr)
library(psy)
library(psych)
library(RColorBrewer)
library(reshape)
library(rgeos)
library(stringr)
library(systemfit)
library(apsrtable)
library(lmtest)
#

#Download data
#Surveys
#1. isnasu - Surveys from ISNASU, 1992-2006
#2. ess_ua ??? European Social Survey for Ukraine, 2002-2012
#3. nationalstat_ua - GDP per capita and average salaries in Ukraine
#4. communities_annual - ReViVals data on religious communities in Ukraine
#5. appendix_sources- ReViVals data on religious communities in Ukraine
#6. idc_plot - ReViVals data on religious communities in Ukraine

#load data
#Step 1
isnasu <- read.spss("isnasu.sav", to.data.frame = TRUE, use.missings = TRUE) 
ess_ua <- as.data.frame(read.spss("ess_ua_2002-2012.sav"))
idc_plot <- read.csv("idc_index_plot.csv")
communities_annual <- read.csv("communities_all_estimated_newnames.csv") 
appendix_sources <- read.csv("data_communities_sources_newnames.csv")
nationalstat_ua <- read.csv("nationalstat_ukraine.csv")
fixedmodels <- read.csv("fixedmodels.csv")
names(fixedmodels)[1] <- "oblast"

buildings <- read.csv("buildings_comp_2008.csv")
names(buildings)[1] <- "oblast"

head(appendix_sources)
#Step 2
idc <- idc_plot[,c(1:6)] #idc = herfindahl index reversed, real minus counterfactual
totalcomp <- idc_plot[,c(1, 7:11)]

colnames(idc)[2:6] <- paste("idc", c(1991,1993,2000,2008,2009), sep = "_")
colnames(totalcomp)[2:6] <- paste("comp", c(1991,1993,2000,2008,2009), sep = "_")
totalcomp[2:6] <- 1 - totalcomp[2:6]
#names(prot_rejected) <- c("oblast", paste("protrej", c(1999:2004), sep = "_"))
#************************************************************************************************

#Figures

#Step 1
#APA.FORMAT-format template for figures:
APA.FORMAT = theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        text = element_text(family = 'Times'))

#Figure 1. Religious communities (estimated absolute numbers). Ukraine, 1991-2013. 
communities_annual$Church <- factor(communities_annual$Church, levels(communities_annual$Church)[c(3,4,1,2,6,5)])

p_communities_annual <- ggplot(data = communities_annual, 
                               aes(x = Years, y = Communities, 
                                   shape = Church, colour = Church)) + 
  geom_line() + xlim(1991, 2013)+
  geom_point(size = 5)+
  geom_line() +
  APA.FORMAT + theme(legend.position = "bottom")
p_communities_annual

#Heat Map (Not in the paper)
idc_to_plot <- idc[,-5] #no 2008
idc_to_plot_long <- melt(idc_to_plot, id = c("oblast"))

idc_to_plot_long$oblast <- factor(idc_to_plot_long$oblast, levels(idc_to_plot_long$oblast))

idc_to_plot_long$idc <- idc_to_plot_long$value


idc_to_plot_long$oblast <- gsub(" oblast", "", idc_to_plot_long$oblast)
idc_to_plot_long$oblast <- gsub(", Autonomy Republic", "", idc_to_plot_long$oblast)

p_idc_reg <- ggplot(idc_to_plot_long, aes(variable, oblast)) + geom_tile(aes(fill = idc), colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + APA.FORMAT +
  scale_y_discrete(limits = rev(levels(idc_to_plot_long$oblast)))+
  labs(x = "IDC of the Orthodox Church", y = "Region/Oblast") 
p_idc_reg

totalcomp_to_plot_long <- melt(totalcomp, id = c("oblast"))
totalcomp_to_plot_long$oblast <- factor(totalcomp_to_plot_long$oblast, levels(totalcomp_to_plot_long$oblast))

totalcomp_to_plot_long$competition <- totalcomp_to_plot_long$value


totalcomp_to_plot_long$oblast <- gsub(" oblast", "", totalcomp_to_plot_long$oblast)
totalcomp_to_plot_long$oblast <- gsub(", Autonomy Republic", "", totalcomp_to_plot_long$oblast)


p_totalcomp_reg <- ggplot(totalcomp_to_plot_long, aes(variable, oblast)) + geom_tile(aes(fill = competition), colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + APA.FORMAT +
  scale_y_discrete(limits = rev(levels(idc_to_plot_long$oblast)))+
  labs(x = "Total Church Competition", y = "Region/Oblast") 
p_totalcomp_reg
#Figure 2. IDC of the Orthodox Church at the regional level. Ukraine, 1991-2009.
p_idc_to_plot_long <- ggplot(idc_to_plot_long, aes(factor(variable), value)) + 
  geom_boxplot(outlier.size = 0.5, col = "blue") +
  labs(x = "Year", y = "IDC of the Orthodox Church") +
  APA.FORMAT
p_idc_to_plot_long

#Fig 4
appendix_sources_long <- melt(appendix_sources, id=c("Source","Year"))

p_appendix_sources <- ggplot(data = appendix_sources_long, aes(x = Year, y = value, shape = Source, colour=Source)) + 
  geom_line() + xlim(1990,2015)+
  geom_point(size = 1)+
  facet_wrap(~variable)+
  APA.FORMAT
p_appendix_sources

#
jpeg("Figure1.Religious communities (estimated).res.jpeg", width = 3000, height = 3000, res=500)
p_communities_annual 
dev.off()

jpeg("Figure2old. IDC 1991-2009.jpeg", width = 3000, height = 2000, res=300)
p_idc_to_plot_long
dev.off()

jpeg("Figure5.Appendix. Religious_communities.jpeg", width = 3000, height = 2000, res=300)
p_appendix_sources
dev.off()

jpeg("Figure4.Heatmap.res.jpeg", width = 3000, height = 3000, res=600)
p_idc_reg
dev.off()

jpeg("Figure3.Heatmap.res.jpeg", width = 3000, height = 3000, res=600)
p_totalcomp_reg
dev.off()

#
#correlate number of priests
comp_alternative <- cbind(totalcomp, buildings, idc_to_plot)

cor(comp_alternative$comp_2008, comp_alternative$Comp_priests_2008)
cor(comp_alternative$comp_2008, comp_alternative$HHI_priests_2008)
#
plot(comp_alternative$comp_2008, comp_alternative$Comp_priests_2008)
plot(comp_alternative$comp_2008, comp_alternative$HHI_priests_2008)
#
#************************************************************************************************

#STEP 2

#Making new variables and matching the data
isnasu$oblast <- isnasu$v394 

#Merge ISNASU and external data
isnasu_idc <-  merge(isnasu, idc, by = "oblast")
isnasu_idc <- merge(isnasu_idc, prot_rejected, by = "oblast")


#Making DEP VAR
table(isnasu_idc$v387)
table(isnasu_idc$v388)
table(isnasu_idc$v389)
table(isnasu_idc$v390)
#Greek C
isnasu_idc$greekc <- 0
isnasu_idc$greekc[which(isnasu_idc$v387 == "Greek Catholic")] <- 1
isnasu_idc$greekc[which(isnasu_idc$v388  == "Greek Catholic")] <- 1
isnasu_idc$greekc[which(isnasu_idc$v389  == "Greek Catholic")] <- 1
isnasu_idc$greekc[which(isnasu_idc$v390  == "Greek Catholic")] <- 1
table(isnasu_idc$greekc)
#Orthodox
isnasu_idc$orth <- 0
isnasu_idc$orth[which(isnasu_idc$v387  == "Orthodox")] <- 1
isnasu_idc$orth[which(isnasu_idc$v388  == "Orthodox Kiev")] <- 1
isnasu_idc$orth[which(isnasu_idc$v388  == "Orthodox Moscow")] <- 1
isnasu_idc$orth[which(isnasu_idc$v389  == "Orthodox Kiev")] <- 1
isnasu_idc$orth[which(isnasu_idc$v389  == "Orthodox Moscow")] <- 1
isnasu_idc$orth[which(isnasu_idc$v390  == "Orthodox Ukrainian church")] <- 1

table(isnasu_idc$v388,isnasu_idc$v2)
table(isnasu_idc$v389,isnasu_idc$v2)
#Rel by Years
table(isnasu_idc$v387,isnasu_idc$v2)
table(isnasu_idc$v388,isnasu_idc$v2)
table(isnasu_idc$v389,isnasu_idc$v2)
table(isnasu_idc$v390,isnasu_idc$v2)

#Being religious 
isnasu_idc$religious <- 1
isnasu_idc$religious[which(isnasu_idc$v387  == "Not religious")] <- 0
isnasu_idc$religious[which(isnasu_idc$v388  == "Not religious")] <- 0
isnasu_idc$religious[which(isnasu_idc$v389  == "Not religious")] <- 0
isnasu_idc$religious[which(isnasu_idc$v390  == "Not religious")] <- 0

#
isnasu_idc$kiev <- 0
isnasu_idc$moscow <- 0
isnasu_idc$kiev[which(isnasu_idc$v388  == "Orthodox Kiev")] <- 1
isnasu_idc$moscow[which(isnasu_idc$v388  == "Orthodox Moscow")] <- 1
isnasu_idc$kiev[which(isnasu_idc$v389  == "Orthodox Kiev")] <- 1
isnasu_idc$moscow[which(isnasu_idc$v389  == "Orthodox Moscow")] <- 1

#Checking the distribution 
plot(tapply(isnasu_idc$religious, isnasu_idc$v2, sum) / table(isnasu_idc$v2), type = "l",xlab = "X-axis label")
tapply(isnasu_idc$religious, isnasu_idc$v2, sum) / table(isnasu_idc$v2)

plot(tapply(isnasu_idc$kiev, isnasu_idc$v2, sum) / table(isnasu_idc$v2), type = "l",xlab = "X-axis label")
tapply(isnasu_idc$kiev, isnasu_idc$v2, sum) / table(isnasu_idc$v2)

plot(tapply(isnasu_idc$moscow, isnasu_idc$v2, sum) / table(isnasu_idc$v2), type = "l",xlab = "X-axis label")
tapply(isnasu_idc$moscow, isnasu_idc$v2, sum) / table(isnasu_idc$v2)
#Please note, that the ISNASU sampling design changed in 2002 after the census of 2001
#was conducted. Thus, we can observe some abrupt changes in the data.
#Early versions of this paper included robustness checks to account for this.

#************************************************************************************************

#Making a new grouping variable "group": a region in a given year.
isnasu_idc$group <- paste(isnasu_idc$v394,isnasu_idc$v2, sep = "_")

#Individual variables

#Occupation
table(isnasu_idc$v337)
isnasu_idc$occ <- NA
isnasu_idc$occ[which(isnasu_idc$v337  == "Working politician" |
                          isnasu_idc$v337 == "Working manager" |
                          isnasu_idc$v337 == "Working government" |
                          isnasu_idc$v337 == "Working technical professional" |
                          isnasu_idc$v337 == "Working professional" |
                          isnasu_idc$v337 == "Working militia or army" |
                          isnasu_idc$v337 == "Working propr big business" |
                          isnasu_idc$v337 == "Working small propr" |
                          isnasu_idc$v337 == "Working servise" |
                          isnasu_idc$v337 == "Working skilled" |
                          isnasu_idc$v337 == "Working low manual l" |
                          isnasu_idc$v337 == "Working agro" |
                          isnasu_idc$v337 == "Farmer" |
                          isnasu_idc$v337 == "Dont have stable job, only part time")] <- "1.Working"
                          isnasu_idc$occ[which(isnasu_idc$v337 == "Pensioner dont work")] <- "2.Dont work"
                          isnasu_idc$occ[which(isnasu_idc$v337 == "Dont work, no income")] <- "2.Dont work"
                          isnasu_idc$occ[which(isnasu_idc$v337 == "Oficially unemployed")] <- "2.Dont work"
                          isnasu_idc$occ[which(isnasu_idc$v337 == "Other")] <- "2.Dont work"
                          isnasu_idc$occ[which(isnasu_idc$v337 == "Housekeeper")] <- "2.Dont work"
                          isnasu_idc$occ[which(isnasu_idc$v337 == "Difficult to say")] <- "2.Dont work"
                          isnasu_idc$occ[which(isnasu_idc$v337 == "Student")] <- "3.Student"

table(isnasu_idc$occ)

#Has a job
isnasu_idc$working <- 0
isnasu_idc$working[which(isnasu_idc$occ == "1.Working")] <- 1
table(isnasu_idc$working)

#Higher education
isnasu_idc$indiv_higheduc <- 0
isnasu_idc$indiv_higheduc[isnasu_idc$v381 == "High Master or PhD" |
                            isnasu_idc$v381 == "High (bachelor)"] <- 1
table(isnasu_idc$indiv_higheduc)

#Speaking mostly Ukrainian at home
table(isnasu_idc$v147)
isnasu_idc$lang_ukr <- 0
isnasu_idc$lang_ukr[which(isnasu_idc$v147 == "More Ukrainian")] <- 1
table(isnasu_idc$lang_ukr)

#Ukrainian nationality
table(isnasu_idc$v382)
isnasu_idc$ukrainian <- 0
isnasu_idc$ukrainian[isnasu_idc$v382 == "Ukrainian"] <- 1
table(isnasu_idc$ukrainian)

#Gender
isnasu_idc$men <- 0
isnasu_idc$men[isnasu_idc$v376 == "Male"] <- 1
table(isnasu_idc$men)

#Loss of a partner
table(isnasu_idc$v379)
table(isnasu_idc$v380)
table(isnasu_idc$v379,isnasu_idc$v380)
table(isnasu_idc$v379,isnasu_idc$v2)
table(isnasu_idc$v380,isnasu_idc$v2)

table(isnasu_idc$v380, isnasu_idc$v2)
isnasu_idc$loss <- 0
isnasu_idc$loss[isnasu_idc$v379 == "widow"  | isnasu_idc$v380 == "Widow"] <- 1
table(isnasu_idc$loss)

#Bad health
table(isnasu_idc$v218)
isnasu_idc$badhealth <- 0
isnasu_idc$badhealth[isnasu_idc$v218 == "Very bad"] <- 1
isnasu_idc$badhealth[isnasu_idc$v218 == "Bad"] <- 1
table(isnasu_idc$badhealth)

#Age-groups as dummies
table(isnasu_idc$v378)
isnasu_idc$young <- 0
isnasu_idc$middle <- 0
isnasu_idc$older <- 0

isnasu_idc$young[isnasu_idc$v378 == "young (<30)"] <- 1
isnasu_idc$middle[isnasu_idc$v378 == "middle 30-55"] <- 1
isnasu_idc$older[isnasu_idc$v378 == "older >55"] <- 1

isnasu_idc$young <- as.factor(isnasu_idc$young)
isnasu_idc$middle <- as.factor(isnasu_idc$middle)
isnasu_idc$older <- as.factor(isnasu_idc$older)

table(isnasu_idc$young);table(isnasu_idc$middle);table(isnasu_idc$older)
#************************************************************************************************

#Contextual variables. 

#Context is mostly Orthodox
isnasu_idc$i <- 1

orthodox_pop <- tapply(isnasu_idc$orth, isnasu_idc$group, sum)
population_tot <- tapply(isnasu_idc$i, isnasu_idc$group, sum)
orth_contextual <- as.data.frame(orthodox_pop/population_tot)
names(orth_contextual) <- "orth_reg"
orth_contextual$group <- row.names(orth_contextual)

isnasu_idc <- merge(isnasu_idc,orth_contextual, by="group")

#Time_t = amount of years after 1991.
isnasu_idc$Year <- as.numeric(as.character(isnasu_idc$v2))
isnasu_idc$time <- isnasu_idc$Year - 1991

#Language, unemployement, and education
isnasu_idc$i <- 1
allpeople <- tapply(isnasu_idc$i, isnasu_idc$group, sum) #people per group
workpeople <- tapply(isnasu_idc$working, isnasu_idc$group, sum) #working people
speakukr <- tapply(isnasu_idc$lang_ukr, isnasu_idc$group, sum) #ukr language
areukr <- tapply(isnasu_idc$ukrainian, isnasu_idc$group, sum) #ukr nationality
higheduc <- tapply(isnasu_idc$indiv_higheduc, isnasu_idc$group, sum)

employment <- as.data.frame(workpeople / allpeople) 
language <- as.data.frame(speakukr / allpeople)
identity <- as.data.frame(areukr / allpeople)
educpeople <- as.data.frame(higheduc / allpeople)

employment$group <- row.names(employment)
language$group <- row.names(language)
identity$group <- row.names(identity)
educpeople$group <- row.names(educpeople)

names(employment) <- c("empl_reg", "group")
names(language) <- c("ukrlang.reg", "group")
names(identity) <- c("identity.reg", "group")
names(educpeople) <- c("higheduc.reg", "group")

isnasu_idc <- merge(isnasu_idc,employment, by = "group")
isnasu_idc <- merge(isnasu_idc,language, by = "group")
isnasu_idc <- merge(isnasu_idc,identity, by = "group")
isnasu_idc <- merge(isnasu_idc,educpeople, by = "group")

#Unemployment rates
isnasu_idc$unempl <- 1 - isnasu_idc$empl

#A region is predominantly Ukrainian speaking (dummy) if >= 60% prefer to speak Ukrainian
summary(isnasu_idc$ukrlang.reg)
summary(isnasu_idc$identity.reg)

#Ukr language and Ukra identities per reg - 80% correlation
cor(isnasu_idc$ukrlang.reg,isnasu_idc$identity.reg)
plot(isnasu_idc$ukrlang.reg,isnasu_idc$identity.reg)
#************************************************************************************************

#STEP 3
sort(table(isnasu_idc$group)) #some of the regions are too small - exclude them

isnasu_idc <- isnasu_idc[isnasu_idc$group != "Poltavska oblast_1992", ]
isnasu_idc <- isnasu_idc[isnasu_idc$group != "Luganska oblast_1992", ]
isnasu_idc <- isnasu_idc[isnasu_idc$group != "Odesska oblast_1992", ]
isnasu_idc <- isnasu_idc[isnasu_idc$group != "Khersonska oblast_1992", ]

#For models without outliers, i.e. Western Ukrainian regions
isnasu_idc_nowest <- isnasu_idc[isnasu_idc$oblast != "Ivano-Frankivska oblast" &
                                      isnasu_idc$oblast != "Lvivska oblast" &
                                      isnasu_idc$oblast != "Ternopilska oblast", ]


#************************************************************************************************

#STEP 4

#Descriptives
table(isnasu_idc$v2)
#Table 1. Descriptive statistics for the variables. Ukraine (1992-2006)
length(isnasu_idc$religious)
length(unique(isnasu_idc$group))
#
names(isnasu_idc)
summary(isnasu_idc$religious)
#
summary(isnasu_idc$working)
summary(isnasu_idc$badhealth)
summary(isnasu_idc$lang_ukr)
summary(isnasu_idc$loss)
summary(isnasu_idc$indiv_higheduc)
#
summary(isnasu_idc$unempl)
sd(isnasu_idc$unempl)
summary(isnasu_idc$ukrlang.reg)
sd(isnasu_idc$ukrlang.reg)
summary(isnasu_idc$ukrlang.reg.dum)
summary(isnasu_idc$higheduc.reg)
sd(isnasu_idc$higheduc.reg)
#
summary(isnasu_idc$time)
sd(isnasu_idc$time)
#
summary(isnasu_idc$men)
summary(isnasu_idc$v378)/length(isnasu_idc$v378)
#************************************************************************************************

#STEP 5

#
#Models
#
#####PLEASE NOTE!#####
#All the modesl are estimated with Stata. Here I employ R to
#make visualizations

#####************#####
#Model3
#Table 2. An impact of nationalistic Orthodox church competition on religious affiliation.
#ISNASUU surveys (1992-2006). Mixed effects logistic regression.

#
isnasu_idc$idc_1991.scale <- scale(isnasu_idc$idc_1991, center = TRUE, scale = TRUE)
isnasu_idc$unempl.scale <- scale(isnasu_idc$unempl, center = TRUE, scale = TRUE)
isnasu_idc$ukrlang.reg.scale <- scale(isnasu_idc$ukrlang.reg, center = TRUE, scale = TRUE)
isnasu_idc$higheduc.reg.scale <- scale(isnasu_idc$higheduc.reg, center = TRUE, scale = TRUE)
isnasu_idc$time.scale <- scale(isnasu_idc$time, center = TRUE, scale = TRUE)
#
tapply(isnasu_idc$idc_1991, isnasu_idc$oblast, mean)
tapply(isnasu_idc$idc_1991.scale, isnasu_idc$oblast, mean)

isnasu_idc$idc_1991_reverse <- isnasu_idc$idc_1991
isnasu_idc$idc_1991_reverse.scale <- scale(isnasu_idc$idc_1991, center = TRUE, scale = TRUE)
#
table(isnasu_idc$religious, isnasu_idc$Year)

prop.table(table(isnasu_idc$religious, isnasu_idc$Year), 2) 

#
mod_pp2_re <- glmer(religious ~
                      idc_1991.scale*working+
                      idc_1991.scale*badhealth+
                      idc_1991.scale*lang_ukr+
                      idc_1991.scale*loss+
                      idc_1991.scale*indiv_higheduc+
                 unempl.scale+
                 ukrlang.reg.scale+
                 higheduc.reg.scale+
                 time.scale+
                 men+middle+older+(1|group), data = isnasu_idc, family = "binomial")
summary(mod_pp2_re)

mod_pp2_re_vis <- glmer(religious ~
                      idc_1991*working+
                      idc_1991*badhealth+
                      idc_1991*lang_ukr+
                      idc_1991*loss+
                      idc_1991*indiv_higheduc+
                      unempl.scale+
                      ukrlang.reg.scale+
                      higheduc.reg.scale+
                      time.scale+
                      men+middle+older+(1|group), data = isnasu_idc, family = "binomial")
summary(mod_pp2_re_vis)
#
#IDC N - IDC in 1991. If the difference is LARGE 
#If the difference is 0... if the difference is large, then IDC increased -
#religiosity declines

isnasu_idc_robust$value <- as.numeric(isnasu_idc_robust$value)
summary(isnasu_idc_robust$value)
names(isnasu_idc_robust)

isnasu_idc_tostata_nowest

isnasu_idc_robust_nowest <- isnasu_idc_robust[#isnasu_idc_robust$oblast.x != "Volynska oblast" &
                                              #isnasu_idc_robust$oblast.x != "Odesska oblast" &
                                               isnasu_idc_robust$oblast.x != "Ivano-Frankivska oblast" &
                                               isnasu_idc_robust$oblast.x != "Zakarpatska oblast" &
                                                 isnasu_idc_robust$oblast.x != "Kyiv city" &
                                         isnasu_idc_robust$oblast.x != "Lvivska oblast" &
                                         isnasu_idc_robust$oblast.x != "Ternopilska oblast", ]

#Export to Stata
names(isnasu_idc)
isnasu_idc_tostata <- isnasu_idc[c("religious", "idc_1991", "idc_1991.scale", "time.scale","working",
                                   "badhealth", "lang_ukr", "loss", "indiv_higheduc",
                                   "unempl.scale", "ukrlang.reg.scale", "higheduc.reg.scale",
                                   "men","middle", "older", "group")]

isnasu_idc_tostata_nowest <- isnasu_idc[isnasu_idc$oblast != "Ivano-Frankivska oblast" &
                                          isnasu_idc$oblast != "Lvivska oblast" &
                                          isnasu_idc$oblast != "Ternopilska oblast", ]



write.dta(isnasu_idc_tostata, "isnasu_idc.dta")
write.dta(isnasu_idc_tostata_nowest, "isnasu_idc_nowest.dta")
#
tempdat_pp2 <-  isnasu_idc[, c("religious", "idc_1991",
                                "idc_1991.scale", 
                                "time.scale","working",
                                "badhealth", 
                                "lang_ukr", 
                                "loss", 
                                "indiv_higheduc",
                                "unempl.scale", "ukrlang.reg.scale", "higheduc.reg.scale",
                                "men","middle", "older", "group")]
#
summary(mod_pp2_re)
tempdat_pp2$lang_ukr  <- as.factor(tempdat_pp2$lang_ukr )
isnasu_idc$lang_ukrf <- factor(isnasu_idc$lang_ukr)
#
jvalues <- with(isnasu_idc, seq(from = min(idc_1991), to = max(idc_1991), length.out = 100))
#    
birobs <- list()
biprobs <-  lapply(levels(isnasu_idc$lang_ukrf), function(stage) {
  tempdat_pp2$lang_ukr [] <- stage
  lapply(jvalues, function(j) {
    tempdat_pp2$idc_1991 <- j
    predict(mod_pp2_re_vis, newdata = tempdat_pp2, type = "response")
  })
})
#
plotdat_pp2 <- lapply(biprobs, function(X) {
  temp <- t(sapply(X, function(x) {
    c(M=mean(x), quantile(x, c(.25, .75)))
  }))
  temp <- as.data.frame(cbind(temp, jvalues))
  colnames(temp) <- c("PredictedProbability", "Lower", "Upper", "idc_1991")
  return(temp)
})
#
plotdat_pp2.2 <- do.call(rbind, plotdat_pp2)
#
plotdat_pp2.2$Prefer.Ukrainain  <- factor(rep(levels(isnasu_idc$lang_ukrf), each = length(jvalues)))
#
pp_ukr_pp2 <-   ggplot(plotdat_pp2.2, aes(x = idc_1991, y = PredictedProbability, linetype= Prefer.Ukrainain)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Prefer.Ukrainain), alpha = .15) +
  geom_line(aes(colour = Prefer.Ukrainain), size = 2, alpha = 0.2) +
  ylim(c(0, 1)) +
  xlab("IDC in 1991")+
  ylab("Religious affiliation (predicted probabilities)")+
  ggtitle("Model 3")+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
  theme(plot.title = element_text(size=22))+
  theme(plot.title = element_text(hjust = 0.5))

pp_ukr_pp2+APA.FORMAT

#
jpeg('Figure4. Interaction effects: IDC in 1991 and language preferences.jpg', width=3000, height=1500, res=300)
pp_ukr_pp2+APA.FORMAT
dev.off()
#
#
#ROBUST WITH IDC in 2000

isnasu_idc$Year
class(isnasu_idc$Year)

table(isnasu_idc$Year)

names(fixedmodels)[1] <- "oblast"
table(fixedmodels$oblast)
table(isnasu_idc$oblast)

cor(isnasu_idc$idc_1991, isnasu_idc$idc_1993)
cor(isnasu_idc$idc_1991, isnasu_idc$idc_2000)

idc_1991agr <- aggregate(isnasu_idc$idc_1991, list(isnasu_idc$oblast, isnasu_idc$v2), mean)
idc_1993agr <- aggregate(isnasu_idc$idc_1993, list(isnasu_idc$oblast, isnasu_idc$v2), mean)
idc_2000agr <- aggregate(isnasu_idc$idc_2000, list(isnasu_idc$oblast, isnasu_idc$v2), mean)

cor(idc_1991agr [, 3], idc_1993agr [, 3])
cor(idc_1991agr [, 3], idc_2000agr [, 3])

plot(idc_1991agr [, 3], idc_2000agr [, 3])

isnasu_idc.robust <- merge(isnasu_idc, fixedmodels, by="oblast")
names(isnasu_idc.robust)

isnasu_idc.robust.1994 <- isnasu_idc.robust[isnasu_idc.robust$Year >= 1994, ]
isnasu_idc.robust.1999 <- isnasu_idc.robust[isnasu_idc.robust$Year >= 1999, ]
isnasu_idc.robust.2000 <- isnasu_idc.robust[isnasu_idc.robust$Year >= 2000, ]
isnasu_idc.robust.2001 <- isnasu_idc.robust[isnasu_idc.robust$Year >= 2001, ]
isnasu_idc.robust.2002 <- isnasu_idc.robust[isnasu_idc.robust$Year >= 2002, ]
isnasu_idc.robust.2003 <- isnasu_idc.robust[isnasu_idc.robust$Year >= 2003, ]
isnasu_idc.robust.2004 <- isnasu_idc.robust[isnasu_idc.robust$Year >= 2004, ]

isnasu_idc.robust.list <- split(isnasu_idc.robust, isnasu_idc.robust$Year)
length(isnasu_idc.robust.list)

fixedmodels.forlong <- fixedmodels[, 1:9]
fixedmodels.long <- melt(fixedmodels.forlong)

relagr <- aggregate(isnasu_idc.robust$religious, list(isnasu_idc.robust$oblast, isnasu_idc.robust$v2), mean)
ukrlang <- aggregate(isnasu_idc.robust$ukrlang.reg, list(isnasu_idc.robust$oblast, isnasu_idc.robust$v2), mean)




head(fixedmodels.long)
head(relagr)

fixedmodels.long$variable <- as.numeric(substr(fixedmodels.long$variable, 4, 7))

names(relagr) <- c("oblast", "year", "relpeople")
names(fixedmodels.long) <- c("oblast", "year", "idc")
names(ukrlang) <- c("oblast", "year", "ukrainian_language")

ddd <- merge(relagr, fixedmodels.long, by = c("oblast", "year"))
ddd <- merge(ddd, ukrlang, by = c("oblast", "year"))



help(cor)
cor(ddd$relpeople, ddd$idc, use = "complete.obs")

hist(ddd$ukrpeople)

ddd$ukrpeople.ag <- 0
ddd$ukrpeople.ag[ddd$ukrpeople >= 0.8] <- 1

ddd.nowest <- ddd[ddd$oblast != "Ivano-Frankivska oblast" &
                           ddd$oblast != "Lvivska oblast" &
                           ddd$oblast != "Ternopilska oblast", ]


fig4 <- ggplot(data = ddd, aes(x = relpeople, y = idc,
                       size=ukrainian_language)) + 
  #geom_point(color='blue') +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(x = "Share of religious people", y = "IDC of the Orthodox Church") +
  APA.FORMAT

fig5 <- ggplot(data = ddd.nowest, aes(x = relpeople, y = idc,
                       size=ukrainian_language)) + 
  #geom_point(color='blue') +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(x = "Share of religious people", y = "IDC of the Orthodox Church") +
  APA.FORMAT


jpeg("fig4.shares.all.jpeg", width = 3000, height = 2000, res=300)
fig4
dev.off()

jpeg("fig5.shares.outl.jpeg", width = 3000, height = 2000, res=300)
fig5
dev.off()


ddd$idcsq <- ddd$idc * ddd$idc
aggrmodel <- lm(relpeople ~ idc * ukrpeople, data = ddd)
summary(aggrmodel)

summary(msplit2)

plot(ddd$idc, ddd$relpeople)


table(isnasu_idc.robust.1999$Year)
table(isnasu_idc.robust.2000$Year)
table(isnasu_idc.robust.2004$Year)

isnasu_idc.robust.1994$idc.scale <- scale(isnasu_idc.robust.1994$idc1994, center = TRUE, scale = TRUE)
isnasu_idc.robust.1999$idc.scale <- scale(isnasu_idc.robust.1999$idc1999, center = TRUE, scale = TRUE)
isnasu_idc.robust.2000$idc.scale <- scale(isnasu_idc.robust.2000$idc2000, center = TRUE, scale = TRUE)
isnasu_idc.robust.2001$idc.scale <- scale(isnasu_idc.robust.2001$idc2001, center = TRUE, scale = TRUE)
isnasu_idc.robust.2002$idc.scale <- scale(isnasu_idc.robust.2002$idc2002, center = TRUE, scale = TRUE)
isnasu_idc.robust.2003$idc.scale <- scale(isnasu_idc.robust.2003$idc2003, center = TRUE, scale = TRUE)
isnasu_idc.robust.2004$idc.scale <- scale(isnasu_idc.robust.2004$idc2004, center = TRUE, scale = TRUE)

mod_pp3_re <- glmer(religious ~
                      idc.scale*working+
                      idc.scale*badhealth+
                      idc.scale*lang_ukr+
                      idc.scale*loss+
                      idc.scale*indiv_higheduc+
                      unempl.scale+
                      ukrlang.reg.scale+
                      higheduc.reg.scale+
                      time.scale+
                      men+middle+older+(1|group), data = isnasu_idc.robust.2004, family = "binomial")
summary(mod_pp3_re)

#1993, results. MAIN - NEG NOT SIG; INTERACT - POS NOT SIG. 
#1994, results. MAIN - NEG NOT SIG; INTERACT - POS NOT SIG. 
#1999, results. MAIN - NEG & SIG ; INTERACT - POS NOT SIG.  
#2000, results. MAIN - NEG & SIG ; INTERACT - POS NOT SIG. 
#2001, results. MAIN - NEG NOT SIG; INTERACT - NEG NOT SIG. 
#2002, results. MAIN - NEG NOT SIG; INTERACT - NEG NOT SIG. 
#2003, results. MAIN - NEG NOT SIG; INTERACT - NEG SIG. 
#2004, results. MAIN - NEG NOT SIG; INTERACT - NEG NOT SIG. 

names(isnasu_idc.robust.list[[1]])
table(isnasu_idc.robust.list[[1]]$Year)

msplit1 <- glm(freligious ~ idc1992 * working + idc1992 * badhealth + 
                idc1992 * lang_ukr + idc1992 * loss + idc1992 * indiv_higheduc + 
      unempl.scale + ukrlang.reg.scale + higheduc.reg.scale + time.scale, 
    family = binomial(link = "logit"), data = isnasu_idc.robust.list[[1]])

summary(msplit1)

table(isnasu_idc.robust.list[[2]]$Year)
msplit2 <- glm(religious ~ idc1994 * working + idc1994 * badhealth + 
                 idc1994 * lang_ukr + idc1994 * loss + idc1994 * indiv_higheduc + 
                 unempl.scale + ukrlang.reg.scale + higheduc.reg.scale + time.scale, 
               family = binomial(link = "logit"), data = isnasu_idc.robust.list[[2]])

summary(msplit2)
#**********************************************************************************************
#
#Repeating similar calculations for ESS
#Some packages to work with insturmental variables

#Models 5 to 7 (Table 5)
#Instrumental variable (Models 8,9; Table 6 and 7)

#ESS

#widowed
ess_ua$widow <- "not widowed"
ess_ua$widow[ess_ua$marital == "Widowed"] <- "widowed"
ess_ua$widow[ess_ua$maritala == "Widowed"] <- "widowed"
ess_ua$widow[ess_ua$maritalb == "Widowed/civil partner died"] <- "widowed"
table(ess_ua$widow)
#
#Age
summary(ess_ua$agea)
ess_ua$agea[ess_ua$agea == "999"] <- NA
ess_ua$eduyrs[ess_ua$eduyrs == "99" | ess_ua$eduyrs == "88"] <- NA
ess_ua$agea_n <- ess_ua$agea/100
summary(ess_ua$agea)
summary(ess_ua$agea_n); sd(ess_ua$agea_n, na.rm=T)
#
#Educ
names(ess_ua)

table(ess_ua$edlvua)
table(ess_ua$edlvaua)
table(ess_ua$edlvdua)

ess_ua$terted <- 0
ess_ua$terted[ess_ua$edlvua == "Completed high education (specialist, master, post-graduate,"] <- 1
ess_ua$terted[ess_ua$edlvua == "First stage of high education (bachelor)"] <- 1
ess_ua$terted[ess_ua$edlvaua == "Postgraduate studies, scientific degree"] <- 1
ess_ua$terted[ess_ua$edlvaua == "Completed high education (specialist degree, master degree)"] <- 1
ess_ua$terted[ess_ua$edlvaua == "Basic high education (bachelor degree)"] <- 1
ess_ua$terted[ess_ua$edlvdua == "Aspirantura, vchena stupin"] <- 1
ess_ua$terted[ess_ua$edlvdua == "Povna vyshcha osvita (magistr)"] <- 1
ess_ua$terted[ess_ua$edlvdua == "Povna vyshcha osvita (specialist)"] <- 1
ess_ua$terted[ess_ua$edlvdua == "Bazova vyshcha osvita (bakalavr)"] <- 1
table(ess_ua$terted)
#
#Religion
table(ess_ua$essround)
dim(ess_ua)

table(ess_ua$rlgdnua)
table(ess_ua$rlgdeua)

ess_ua$kiev <- 0
ess_ua$moscow <- 0

ess_ua$kiev[ess_ua$rlgdnua == "Ukrainian orthodox church (Kyiv patriarchate)"] <- 1
ess_ua$kiev[ess_ua$rlgdeua == "Ukrainian orthodox church (Kyiv patriarchate)"] <- 1

ess_ua$moscow[ess_ua$rlgdnua == "Orthodox church of the Moscow patriarchate"] <- 1
ess_ua$moscow[ess_ua$rlgdeua == "Orthodox church of the Moscow patriarchate"] <- 1

table(ess_ua$kiev)
table(ess_ua$moscow)

for (i in 1:7){
  ess_ua$prayoften[ess_ua$pray == names(table(ess_ua$pray))[i]] <- 7-i  
}

for (i in 1:7){
  ess_ua$attendance[ess_ua$rlgatnd == names(table(ess_ua$rlgatnd))[i]] <- 7-i  
}

table(ess_ua$rlgdgr)
for (i in 1:11){
  ess_ua$subjrel[ess_ua$rlgdgr == names(table(ess_ua$rlgdgr))[i]] <- i 
}

table(ess_ua$attendance); table(ess_ua$rlgatnd)
table(ess_ua$subjrel); table(ess_ua$rlgdgr)
table(ess_ua$prayoften); table(ess_ua$pray)
#
#Right Left scale
table(ess_ua$lrscale)
ess_ua$lr <- NA
for (i in 1:11){
  ess_ua$lr[ess_ua$lrscale == names(table(ess_ua$lrscale))[i]] <- i 
}
table(ess_ua$lr); table(ess_ua$lrscale)
#
#uemp3m uemp12m uemp5yr uemplap uemplip
table(ess_ua$uemp3m)
table(ess_ua$uemp12m)
table(ess_ua$uemp5yr)
table(ess_ua$uemplap)
table(ess_ua$uemplip)
#
#table wrkctra
#Doing last 7 days: education -> edctn
#Contract -> wrkctra
#Not active -> doing last 7 days was searching for a job uempla
#Not active -> doing last 7 days was NOT searching for a job uempli
table(ess_ua$edctn)
table(ess_ua$wrkctra)
table(ess_ua$uempla)
table(ess_ua$uempli)
#
#EMPL STATUS
table(ess_ua$wrkctra)
ess_ua$emplstatus <- 0
ess_ua$emplstatus[ess_ua$wrkctra == "Unlimited"] <- 1
ess_ua$emplstatus[ess_ua$wrkctra == "Limited"] <- 1
ess_ua$emplstatus[ess_ua$wrkctra == "No contract"] <- 0
table(ess_ua$emplstatus, ess_ua$essround)
#
#HEALTH
table(ess_ua$health)
ess_ua$badhealth <- NA
for (i in 1:5){
  ess_ua$badhealth[ess_ua$health == names(table(ess_ua$health))[i]] <- i 
}
table(ess_ua$badhealth)
#
#GENDER
table(ess_ua$gndr)
ess_ua$male <- 0
ess_ua$male[ess_ua$gndr == "Male"] <- 1
table(ess_ua$male)
#YEARS EDUC
table(ess_ua$eduyrs)
#table(ess_ua$attendance)
#table(ess_ua$subjrel)
#table(ess_ua$prayoften)
#
names(ess_ua)
table(ess_ua$lnghom1)
ess_ua$UKR <- 0
ess_ua$UKR[ess_ua$lnghom1 == "Ukrainian"] <- 1
table(ess_ua$UKR)
dim(ess_ua)
#
names(ess_ua)
table(ess_ua$prtvtua)
table(ess_ua$prtvtaua)
table(ess_ua$prtvtbua)
table(ess_ua$prtvtcua)
#
ess_ua$polit_prorus <- 0
ess_ua$polit_prorus[ess_ua$prtvtua == "Election bloc of Political Parties 'Nataliya Vitrenko Bloc'" |
                       ess_ua$prtvtua == "Communist Party of Ukraine" |
                       ess_ua$prtvtua == "Communist Party of Ukraine (renew)" |
                       ess_ua$prtvtua == "Social-Democratic Party of Ukraine (united)" |
                       ess_ua$prtvtua == "Election bloc of Political Parties 'Ruses bloc'" |
                       ess_ua$prtvtaua == "Communist party of Ukraine" |
                       ess_ua$prtvtaua == "Bloc of Natalya Vitrenko 'Peoples' opposition'" |
                       ess_ua$prtvtaua == "Party of regions" |
                       ess_ua$prtvtbua == "Communist party of Ukraine" |
                       ess_ua$prtvtbua == "Party of regions" | 
                       ess_ua$prtvtcua == "Communist Party of Ukraine" |
                       ess_ua$prtvtcua == "Party of Regions" ] <- 1
table(ess_ua$polit_prorus)
table(ess_ua$polit_prorus,ess_ua$essround)
#
dim(ess_ua)
#
#Making Orthodox subsample
table(ess_ua$rlgdnm)
table(ess_ua$rlgdnm,ess_ua$polit_prorus)
ess_ua$orthi <- 0
ess_ua$orthi[ess_ua$rlgdnm == "Eastern Orthodox"] <- 1
table(ess_ua$orthi)
ess_ua_orth <- ess_ua[ess_ua$orthi == 1, ]
#
names(ess_ua)
table(ess_ua$cregion,ess_ua$essround)
table(ess_ua$regunit,ess_ua$essround)
table(ess_ua$regionua,ess_ua$essround)

ess_ua$oblast <- paste(ess_ua$cregion,ess_ua$regionua, sep="")
ess_ua$oblast <- gsub("NA","",ess_ua$oblast)
sum(table(ess_ua$oblast))
table(ess_ua$oblast)
#
#Share ukr
ess_ua$i <- 1
allukr <- tapply(ess_ua$UKR,ess_ua$oblast,sum)
allreg <- tapply(ess_ua$i,ess_ua$oblast,sum)
shareukr <- as.data.frame(allukr/allreg)
names(shareukr) <- "share_ukr"
shareukr$oblast <- row.names(shareukr)
head(shareukr)
#
#Share tert educ
alleduc <- tapply(ess_ua$terted,ess_ua$oblast,sum)
shareeduc <- as.data.frame(alleduc/allreg)
names(shareeduc) <- "share_educ"
shareeduc$oblast <- row.names(shareeduc)
head(shareeduc)
#
#Share unempl
allempl <- tapply(ess_ua$emplstatus,ess_ua$oblast,sum)
shareempl <- as.data.frame(allempl/allreg)
names(shareempl) <- "share_empl"
shareempl$oblast <- row.names(shareempl)
head(shareempl)
#
names(ess_ua)
ess_ua <- merge(ess_ua, shareukr, by="oblast")
ess_ua <- merge(ess_ua, shareeduc, by="oblast")
ess_ua <- merge(ess_ua, shareempl, by="oblast")

ess_ua_merged <- merge(ess_ua, idc, by="oblast")
#ess_ua_merged <- merge(ess_ua_merged, IV, by="oblast")

nationalstat_ua$oblast <- nationalstat_ua[,1]
ess_ua_merged <- merge(ess_ua_merged, nationalstat_ua, by="oblast")

names(ess_ua_merged)

table(ess_ua_merged$essround)

ess_ua_merged$ukrstat_gdp <- NA
ess_ua_merged$ukrstat_gdp[ess_ua_merged$essround == 2] <- mean(ess_ua_merged$gdpperc2004)
ess_ua_merged$ukrstat_gdp[ess_ua_merged$essround == 3] <- mean(ess_ua_merged$gdpperc2005)
ess_ua_merged$ukrstat_gdp[ess_ua_merged$essround == 4] <- mean(ess_ua_merged$gdpperc2007)
ess_ua_merged$ukrstat_gdp[ess_ua_merged$essround == 5] <- mean(ess_ua_merged$gdpperc2009)
ess_ua_merged$ukrstat_gdp[ess_ua_merged$essround == 6] <- mean(ess_ua_merged$gdpperc2011)

table(ess_ua_merged$ukrstat_gdp,ess_ua_merged$essround)

ess_ua_merged$ukrstat_salary <- NA
ess_ua_merged$ukrstat_salary[ess_ua_merged$essround == 2] <- mean(ess_ua_merged$aversalar2003)
ess_ua_merged$ukrstat_salary[ess_ua_merged$essround == 3] <- mean(ess_ua_merged$aversalar2005)
ess_ua_merged$ukrstat_salary[ess_ua_merged$essround == 4] <- mean(ess_ua_merged$aversalar2007)
ess_ua_merged$ukrstat_salary[ess_ua_merged$essround == 5] <- mean(ess_ua_merged$aversalar2009)
ess_ua_merged$ukrstat_salary[ess_ua_merged$essround == 6] <- mean(ess_ua_merged$aversalar2011)

table(ess_ua_merged$ukrstat_salary,ess_ua_merged$essround)

ess_ua_merged$ukrstat_unempl <- NA
ess_ua_merged$ukrstat_unempl[ess_ua_merged$essround == 5] <- mean(ess_ua_merged$unempl2009)
ess_ua_merged$ukrstat_unempl[ess_ua_merged$essround == 6] <- mean(ess_ua_merged$unempl2011)

table(ess_ua_merged$ukrstat_unempl,ess_ua_merged$essround)

ess_ua_merged$share_unempl <- 1-ess_ua_merged$share_empl
ess_ua_merged$ukrstat_gdp_mln <- ess_ua_merged$ukrstat_gdp*100000
#
#Descr
summary(ess_ua_merged$subjrel)
sd(ess_ua_merged$subjrel, na.rm=T)

summary(ess_ua_merged$attendance)
sd(ess_ua_merged$attendance, na.rm=T)

summary(ess_ua_merged$prayoften)
sd(ess_ua_merged$prayoften, na.rm=T)

summary(ess_ua_merged$emplstatus)
summary(ess_ua_merged$UKR)
table(ess_ua_merged$widow)

summary(ess_ua_merged$terted)
summary(ess_ua_merged$polit_prorus)

summary(ess_ua_merged$badhealth)
sd(ess_ua_merged$badhealth, na.rm=T)

summary(ess_ua_merged$share_unempl)
sd(ess_ua_merged$share_unempl, na.rm=T)

summary(ess_ua_merged$share_ukr)
sd(ess_ua_merged$share_ukr, na.rm=T)

summary(ess_ua_merged$male)
summary(ess_ua_merged$agea, na.rm=T)
sd(ess_ua_merged$agea, na.rm=T)


summary(ess_ua_merged$ukrstat_gdp_mln, na.rm=T)
sd(ess_ua_merged$ukrstat_gdp_mln, na.rm=T)

summary(ess_ua_merged$ukrstat_salary, na.rm=T)
sd(ess_ua_merged$ukrstat_salary, na.rm=T)

names(ess_ua_merged)
#**********************************************************************************************
#
ess_ua_merged$essround
ess_ua_merged$oblas
ess_ua_merged$group <- paste(ess_ua_merged$oblast, ess_ua_merged$essround)

#The same models are repeated in Stata
ess_1 <- lm(subjrel~idc_1991*UKR+
              idc_1991*polit_prorus+
              emplstatus+
              widow+badhealth+
              UKR+terted+
              share_ukr+share_educ+share_unempl+
              ukrstat_gdp_mln+ukrstat_salary+
              male+agea, data=ess_ua_merged, weight=pweight)
summary(ess_1)

##
#CENTERED
names(ess_ua_merged)

ess_ua_merged$idc_1991.centered <- scale(ess_ua_merged$idc_1991, scale = FALSE)
ess_ua_merged$share_ukr.centered <- scale(ess_ua_merged$share_ukr, scale = FALSE)
ess_ua_merged$share_educ.centered <- scale(ess_ua_merged$share_educ, scale = FALSE)
ess_ua_merged$share_unempl.centered <- scale(ess_ua_merged$share_empl, scale = FALSE)
ess_ua_merged$gdp.centered <- scale(ess_ua_merged$ukrstat_gdp_mln, scale = FALSE)
ess_ua_merged$salary.centered <- scale(ess_ua_merged$ukrstat_salary, scale = FALSE)
ess_ua_merged$agea.centered <- scale(ess_ua_merged$agea, scale = FALSE)
ess_ua_merged$subjrel.centered <- scale(ess_ua_merged$subjrel, scale = FALSE)
ess_ua_merged$attendance.centered <- scale(ess_ua_merged$attendance, scale = FALSE)
ess_ua_merged$prayoften.centered <- scale(ess_ua_merged$prayoften, scale = FALSE)

names(ess_ua_merged)

ess_ua_merged$idc_1993.centered <- scale(ess_ua_merged$idc_1993, scale = FALSE)
ess_ua_merged$idc_2000.centered <- scale(ess_ua_merged$idc_2000, scale = FALSE)

ess_1.mixed <- lmer(subjrel.centered~idc_1991.centered*UKR+
                      idc_1991.centered*polit_prorus+
              emplstatus+
              widow+badhealth+
              UKR+terted+
              share_ukr.centered+
              share_educ.centered+
              share_unempl.centered+
              gdp.centered+
              salary.centered+
              male+
              agea.centered+(1|group), data=ess_ua_merged, weight=pweight)
summary(ess_1.mixed)
Anova(ess_1.mixed)
AIC(ess_1.mixed)
#
ess_2.mixed <- lmer(attendance.centered~idc_1991.centered*UKR+
                      idc_1991.centered*polit_prorus+
                      emplstatus+
                      widow+badhealth+
                      UKR+terted+
                      share_ukr.centered+
                      share_educ.centered+
                      share_unempl.centered+
                      gdp.centered+
                      salary.centered+
                      male+
                      agea.centered+(1|group), data=ess_ua_merged, weight=pweight)
summary(ess_2.mixed)
Anova(ess_2.mixed)
AIC(ess_2.mixed)
#
ess_3.mixed <- lmer(prayoften.centered~idc_1991.centered*UKR+
                      idc_1991.centered*polit_prorus+
                      emplstatus+
                      widow+badhealth+
                      UKR+terted+
                      share_ukr.centered+
                      share_educ.centered+
                      share_unempl.centered+
                      gdp.centered+
                      salary.centered+
                      male+
                      agea.centered+(1|group), data=ess_ua_merged, weight=pweight)
summary(ess_3.mixed)
Anova(ess_3.mixed)
AIC(ess_3.mixed)
#
ess_ua_merged_stata <- ess_ua_merged[c("subjrel.centered", "attendance.centered",
                                       "prayoften.centered", "pweight", 
                                       "idc_1991.centered", "UKR", "polit_prorus", 
                                       "emplstatus", "widow", "badhealth", "terted",
                                       "share_ukr.centered", "share_educ.centered",
                                       "share_unempl.centered",
                                       "gdp.centered", "salary.centered", 
                                       "male", "agea.centered", "group")]
#Save for Stata
dim(ess_ua_merged_stata)
write.dta(ess_ua_merged_stata, "ess_ua_stata.dta")
#ADDING FIXED EFFECTS MODELS

idc_robust <- read.csv("idc_robust.csv")
names(idc_robust)
idc_robust <- idc_robust[, c(1:12)]

idc_robust_long <- melt(idc_robust, id = c("oblast"))
idc_robust_long$year <- substr(idc_robust_long$variable, 4, 7)

idc_robust_long$group <- paste(idc_robust_long$oblast, 
                               idc_robust_long$year, sep = "_")
names(idc_robust)

#isnasu_idc_new <- isnasu_idc[isnasu_idc$Year <= 2001 & isnasu_idc$Year >= 1994, ]
isnasu_idc_new <- isnasu_idc

isnasu_idc_new <- isnasu_idc[isnasu_idc$Year <= 2001 & isnasu_idc$Year >= 1994, ]
table(isnasu_idc_new$religious)
table(isnasu_idc_new$kiev, isnasu_idc_new$v2)

rlg <- aggregate(isnasu_idc_new$religious, list(isnasu_idc_new$oblast, isnasu_idc_new$v2), mean)
orth <- aggregate(isnasu_idc_new$orth, list(isnasu_idc_new$oblast, isnasu_idc_new$v2), mean)
kyiv <- aggregate(isnasu_idc_new$kiev, list(isnasu_idc_new$oblast, isnasu_idc_new$v2), mean)
mcw <- aggregate(isnasu_idc_new$moscow, list(isnasu_idc_new$oblast, isnasu_idc_new$v2), mean)
wrk <- aggregate(isnasu_idc_new$working, list(isnasu_idc_new$oblast, isnasu_idc_new$v2), mean)
hlth <- aggregate(isnasu_idc_new$badhealth, list(isnasu_idc_new$oblast, isnasu_idc_new$v2), mean)
ukr <- aggregate(isnasu_idc_new$lang_ukr, list(isnasu_idc_new$oblast, isnasu_idc_new$v2), mean)
loss <- aggregate(isnasu_idc_new$loss, list(isnasu_idc_new$oblast, isnasu_idc_new$v2), mean)
educ <- aggregate(isnasu_idc_new$indiv_higheduc, list(isnasu_idc_new$oblast, isnasu_idc_new$v2), mean)

newd <- data.frame(oblast = kyiv[, 1], year = kyiv[, 2], rlg = rlg[, 3], orth = orth[, 3],
                   kyiv = kyiv[, 3], mcw = mcw[, 3], wrk = wrk[, 3], 
                   hlth = hlth[, 3], ukr = ukr[, 3],loss = loss[, 3], educ = educ[, 3])

dim(newd)
head(newd)
summary(newd)
names(fixedmodels)

fixedmodels_idc <- fixedmodels[, c(1:8)]
names(fixedmodels_idc)[1] <- "oblast"

fixedmodels_comp <- fixedmodels[, c(1, 9:15)]
names(fixedmodels_comp)[1] <- "oblast"

#
idc_robust

fixedmodels_idc_long <- melt(fixedmodels_idc, id = c("oblast"))
fixedmodels_comp_long <- melt(fixedmodels_comp, id = c("oblast"))
idc_robust_long <- melt(idc_robust, id = c("oblast"))

fixedmodels_idc_long$oblast <- paste(fixedmodels_idc_long$oblast, "oblast")
fixedmodels_idc_long$variable <- (gsub("idc", "", fixedmodels_idc_long$variable))

fixedmodels_comp_long$oblast <- paste(fixedmodels_comp_long$oblast, "oblast")
fixedmodels_comp_long$variable <- (gsub("comp", "", fixedmodels_comp_long$variable))


idc_robust_long$year <- substr(idc_robust_long$variable, 4, 7)
head(idc_robust_long)

names(fixedmodels_idc_long)[3] <- "idc"
names(fixedmodels_comp_long)[3] <- "cmpt"
names(fixedmodels_comp_long)[3] <- "idc_change1991"


vs <- cbind(fixedmodels_idc_long, fixedmodels_comp_long[,3], fixedmodels_comp_long[,3])
head(vs)
names(vs)[c(2, 4, 5)] <- c("year","cmpt", "idc_change1991")

head(vs)
head(newd)
names(newd)
names(idc_robust_long)

newd$group <- paste(newd$oblast, newd$year, sep = "_")
vs$group <- paste(vs$oblast, vs$year, sep = "_")
idc_robust_long$group <- paste(idc_robust_long$oblast, idc_robust_long$year, sep = "_")


dd <- merge(newd, idc_robust_long, by = "group")

dd.c <- dd
names(dd)
head(dd)
dim(dd)

dd.c[, c(4:12, 15)] <- scale(dd.c[, c(4:12, 15)], scale = TRUE)

coplot(rlg ~ year.x|oblast.x, type = "l", data = dd)
coplot(orth ~ year.x|oblast.x, type = "l", data = dd)
#
plotmeans(rlg ~ oblast.x, data = dd)
plotmeans(rlg ~ year.x, data = dd)

names(dd.c)
fixed1 <- plm(rlg ~ value*ukr + wrk + hlth + loss + educ, data = dd.c, 
             index = c("year.x", "oblast.x"), model = "within" )
summary(fixed1)

fixed2 <- plm(orth ~ value*ukr + wrk + hlth + educ, data = dd.c, 
              index = c("year.x", "oblast.x"), model = "within" )
summary(fixed2)

fixed3 <- plm(mcw ~ value*ukr + wrk + hlth + loss + educ, data = dd.c, 
             index = c("year.x", "oblast.x"), model = "within" )
summary(fixed3)

fixed4 <- plm(kyiv ~ value*ukr + wrk + hlth + loss + educ, data = dd.c, 
              index = c("year.x", "oblast.x"), model = "within" )
summary(fixed4)
#fixef(fixed3)    
#pFtest(fixed2, ols2)   

