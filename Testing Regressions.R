############################################################
# regression
# Feb 1, 2019
############################################################
#rm(list = ls())

library(dplyr)
library(plm)
library(ggplot2)
library(DataCombine)
library(lmtest)
setwd("Desktop")
final_data=read.csv("final_dataset.csv")

#all_data=read.csv("cleaned_data.csv")
#all_data = all_data %>% 
#filter(Donor_Group %in% c("Bilaterals"))  
  #filter(Year == 2012)
#summary(all_data)

#all_data_rev <- all_data %>% 
#select(Donor_Group:PerCap_GDP) %>% 
#group_by(Year, Recipient) %>% 
#summarize_all(funs(mean))

#create log variables
#create more log variables
#all_data_rev$log_PerCap_DAH <- (as.numeric(all_data_rev$PerCap_DAH) + 1) %>% log()
#all_data_rev$log_PerCap_GDP <- (as.numeric(all_data_rev$PerCap_GDP) + 1) %>% log()
#all_data_rev$log_PerCap_FDI <- (as.numeric(all_data_rev$PerCap_FDI) + 1) %>% log()
#all_data_rev$logFDI <- (as.numeric(all_data_rev$FDI) + 1) %>% log()
#all_data_rev$logGDP <- (as.numeric(all_data_rev$GDP) + 1) %>% log()
#all_data_rev$logDAH <- (as.numeric(all_data_rev$DAH) + 1) %>% log()

#header
final_data<- final_data %>%
  select(Year, Recipient, Trade_Openness, Accountability, Percent_DALY, log_PerCap_DAH, log_PerCap_FDI, log_PerCap_GDP)
head(final_data)
options(dplyr.width = Inf)

#merge back on income group
income_groupdata <- read.csv("cleaned_data.csv") 
income_groupdata <- income_groupdata %>%
  select(Recipient,Income_Group,Year)
final_datar <- left_join(final_data,income_groupdata, by= c("Recipient","Year")) %>% distinct()

#stat for income group - disease burden
KK = final_datar %>% 
  filter(Year == 2013) %>% 
  arrange(desc(Percent_DALY))

BB =final_datar %>% 
  arrange(Trade_Openness)

#time lag
final_data2<-slide(final_data, Var="Trade_Openness", GroupVar="Recipient",slideBy=-1)
final_data3<-slide(final_data2, Var="Accountability", GroupVar="Recipient",slideBy=-1)
final_data4<-slide(final_data3, Var="Percent_DALY", GroupVar="Recipient",slideBy=-1)
final_data5<-slide(final_data4, Var="log_PerCap_GDP", GroupVar="Recipient",slideBy=-1)
final_data6<-slide(final_data5, Var="log_PerCap_FDI", GroupVar="Recipient",slideBy=-1)

final_data6 <-final_data6 %>%
  rename(Trade_Openness_lagged="Trade_Openness-1", Accountability_lagged="Accountability-1", Percent_DALY_lagged="Percent_DALY-1",log_PerCap_FDI_lagged="log_PerCap_FDI-1", log_PerCap_GDP_lagged="log_PerCap_GDP-1")

summary(final_data6)

tw <- which(final_data2$Recipient=='Afghanistan')
temp_data <- final_data2[tw, ]

summary(final_data)
nrow(final_data)

#plot bar chart
library(ggplot2)
final_data %>% 
  group_by(Year) %>% 
  summarise(Total_DAH = sum(log_PerCap_DAH, na.rm = T)) %>% 
  ggplot(aes(x = Year, y = Total_DAH)) +
  geom_bar(stat = "identity",color = "royalblue",fill="royalblue")+ theme_bw()+ylab("Total  DAH, millions of 2018 USD")


#plot aid vs. daly
x <- ggplot(final_data, aes(x=Percent_DALY, y=log_PerCap_DAH)) + geom_point(color = "royalblue",fill="royalblue")+theme_bw()+ylab("Logged Per Capita DAH")+xlab("% DALY due to NCDs" )+ylim(0,0.02) + ggtitle("Relationship Between NCD Burden and Development Assistance")+theme(plot.title = element_text(hjust = 0.5))
x

#panel fixed data regression
class(all_data_rev) <- "data.frame"

fixed<-plm(log_PerCap_DAH ~ log_PerCap_GDP_lagged * Percent_DALY_lagged + log_PerCap_GDP_lagged +log_PerCap_FDI_lagged + Percent_DALY_lagged + Trade_Openness_lagged + Accountability_lagged,data=final_data6,index=c("Recipient","Year"),model="within", effect = "twoways")
summary(fixed)
coeftest(fixed, vcov.=function(x) vcovHC(x, type = "HC0", cluster = "group"))

cluster.bs.plm(mod=emp.1, dat=final_data6, cluster="time", ci.level = 0.95, 
               boot.reps = 1000, cluster.se = TRUE, report = TRUE, 
               prog.bar = TRUE)

#without myanmar
nomyan<-final_data6%>%
  filter(Recipient!="Myanmar")

withoutmyan<-plm(log_PerCap_DAH ~ log_PerCap_GDP_lagged * Percent_DALY_lagged + log_PerCap_GDP_lagged +log_PerCap_FDI_lagged + Percent_DALY_lagged + Trade_Openness_lagged + Accountability_lagged,data=nomyan,index=c("Recipient","Year"),model="within", effect = "twoways")
summary(withoutmyan)

#residuals for panel fixed
fitted_values <- fitted(fixed)
resid_values <- residuals(fixed)

plot_df <- data.frame(fitted_values, resid_values)
rs<-ggplot(aes(x=fitted_values,y=resid_values), data = plot_df)+geom_point()+geom_hline(yintercept=0)+labs(x="fitted values",y="residuals")
rs

#looking for heteroskedasticity
library(lmtest)
bptest(fixed)
#looks like in plm, homoskedastic

#multicollinearity/correlation eyeball with pairs
final_data <- final_data %>%
  rename(Level_of_Democracy=Accountability)
pairs(~log_PerCap_DAH+log_PerCap_GDP +log_PerCap_FDI + Percent_DALY + Trade_Openness + Level_of_Democracy,data=final_data)

pairs


#attempt to see diagnostic plots: cook's distance, residuals and fitted values, leverage -error (lengths don't match?)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(fixed, las = 1)

#vif-doesn't work
library(tidyverse)
library(caret)
library(foreign)
car::vif(fixed)

#ols for comparsion
ols<-lm(log_PerCap_DAH ~ log_PerCap_GDP * Percent_Deaths + log_PerCap_GDP +log_PerCap_FDI + Percent_Deaths + Trade_Openness+Accountability,data=all_data_rev)
summary(ols)

#ols also homoskedastic? contradicts residual model?
library(lmtest)
bptest(lm(log_PerCap_DAH ~ log_PerCap_GDP * Percent_Deaths + log_PerCap_GDP +log_PerCap_FDI + Percent_Deaths + Trade_Openness+Accountability,data=all_data_rev))

#seeing if fixed better than ols
fixef(fixed)
pFtest(fixed, ols)

#panel random
random<-plm(log_PerCap_DAH ~ log_PerCap_GDP * Percent_Deaths + log_PerCap_GDP +log_PerCap_FDI + Percent_Deaths + Trade_Openness+Accountability,data=all_data_rev,index=c("Recipient","Year"),model="random")
summary(random)

#testing if fixed or random better
phtest(fixed, random)

library(lmtest)
bptest(log_PerCap_DAH ~ log_PerCap_GDP * Percent_Deaths + log_PerCap_GDP +log_PerCap_FDI + Percent_Deaths + Trade_Openness+Accountability,data=all_data_rev,studentize=F)
