
library(tidyverse)
library(missForest)
library(stargazer)

##### 2016 adult data
person_2016data = read.csv("2016adults_coded.csv")

# filter out unavailable data
person_2016data_filtered <- person_2016data %>% filter(qg12 > 0 & qg12 != 'NA' & qn4001 >= 0 & rswt_natcs16!= 'NA' & rswt_natpn1016!= 'NA')

filtered.2016 = data.frame(pid = person_2016data_filtered$pid, year = 2016, income = person_2016data_filtered$qg12, 
                           party = person_2016data_filtered$qn4001, gender = person_2016data_filtered$cfps_gender, 
                           age = person_2016data_filtered$cfps_age, house_regis = person_2016data_filtered$pa301, 
                           edu = person_2016data_filtered$cfps2016edu, edu_year = person_2016data_filtered$cfps2016eduy_im,
                           marital = person_2016data_filtered$qea0, urban = person_2016data_filtered$urban16,
                           cross_weight = person_2016data_filtered$rswt_natcs16, panel_weight = person_2016data_filtered$rswt_natpn1016)

# changing all negative codes to NA
filtered.2016[filtered.2016 == -9 ] <- NA
filtered.2016[filtered.2016 == -8] <- NA
filtered.2016[filtered.2016 == -1] <- NA
# impute
filtered.2016 = missForest(filtered.2016,verbose = TRUE)
filtered.2016 = filtered.2016$ximp
# 1 = college, 0 = no college
filtered.2016$education = ifelse(filtered.2016$edu>=5, 1, 0)
table(filtered.2016$education)

# Add employment sector
filtered.2016_jobtype = filtered.2016
filtered.2016_jobtype$jobtype = person_2016data_filtered$qg2
filtered.2016_jobtype <- filtered.2016_jobtype %>% filter(jobtype != -1 & jobtype != 9)
# jobtype 1 = state/collectively owned, 2 = private, 3 = foreign invested, 4 = other
filtered.2016_jobtype$sector = ifelse(filtered.2016_jobtype$jobtype< 4, 1, filtered.2016_jobtype$jobtype)
filtered.2016_jobtype$sector = ifelse(filtered.2016_jobtype$sector == 4, 2, filtered.2016_jobtype$sector)
filtered.2016_jobtype$sector = ifelse(filtered.2016_jobtype$sector == 5, 3, filtered.2016_jobtype$sector)
filtered.2016_jobtype$sector = ifelse(filtered.2016_jobtype$sector > 3, 4, filtered.2016_jobtype$sector)
filtered.2016_jobtype$sector<-factor(filtered.2016_jobtype$sector, c(1,2,3,4), 
                                     labels=c('state/collectively owned', 'private', 'foreign','other ownership'))
# Create regression model
job_2016_ols = lm(log(income) ~ education  + marital.status + I(age**2) + gender + house.regis + Urban + sector , 
                  data = filtered.2016_jobtype, weights = cross_weight)
# summary(job_2016_ols)
stargazer(job_2016_ols,
          title = "Regression Results for Income in 2016", align = F, type = "text", no.space = TRUE, out = "poster_reg.html")
# change numerical values to characters for variables
filtered.2016_jobtype$marital.status<-factor(filtered.2016_jobtype$marital.status, c(0,1), labels=c('Unmarried', 'Married'))
filtered.2016_jobtype$education<-factor(filtered.2016_jobtype$education, c(0,1), labels=c('No College', 'College'))

# Creating plot
boxplot(filtered.2016_jobtype$income, horizontal=TRUE, main="Annual Income Distribution in 2016")

ggplot(data = filtered.2016_jobtype, aes(x = age, y = log_income, color = education)) +
  geom_point(alpha = 0.3) + theme_bw() + ggtitle("Age Vs. Income Varying by College Degree") +
  xlab("Age") + ylab("Log Income")

ggplot(data = filtered.2016_jobtype, aes(x = age, y = log_income, color = marital.status)) +
  geom_point(alpha = 0.3) + theme_bw() + ggtitle("Age Vs. Income Varying by Marrital Status") +
  xlab("Age") + ylab("Log Income")

ggplot(data = filtered.2016_jobtype, aes(x = age, y = log_income, color = education)) +
  geom_point(alpha = 0.3) + facet_grid(~ marital.status) + theme_bw() + xlab("Age") + ylab("Log Income") + 
  ggtitle(str_wrap("Age Vs. Income Varying by College Degree for Unmarried and Married Individuals", width = 55)) 
