
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, modelsummary, MatchIt, cobalt)

#Q1

#Remove all SNPs, 800-series plans, and prescription drug only plans (i.e., plans that do not offer Part C benefits).
#Provide a box and whisker plot showing the distribution of plan counts by county over time. Do you think that the 
#number of plans is sufficient, too few, or too many?

data <- read_rds("data/output/final_ma_data.rds")

data <- data %>% 
subset(year >= 2010 & year <= 2015 & snp == "No" & premium_partc == 0 & (planid < 800 | planid >= 900))

library(dplyr)

#boxplot
data %>%
  group_by(county) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = "", y = count)) +
  geom_boxplot() +
  labs(title = "Boxplot of Counts by County", x = "County", y = "Count")


table(data$premium_partc)

table(data$county)

colnames(data)


#Q2

#Provide bar graphs showing the distribution of star ratings in 2010, 2012, and 2015. 
#How has this distribution changed over time?

data%>%
filter(year == 2010)%>%
ggplot(aes(x = Star_Rating)) +
geom_histogram()

data%>%
filter(year == 2012)%>%
ggplot(aes(x = Star_Rating)) +
geom_histogram()

data%>%
filter(year == 2015)%>%
ggplot(aes(x = Star_Rating)) +
geom_histogram()

#Q3
#Plot the average benchmark payment over time from 2010 through 2015. 
#How much has the average benchmark payment risen over the years?

q3 <- data%>%
group_by(year)%>%
summarise(avg_benchmark = mean(payment_partc, na.rm = T))

ggplot(q3, aes(x = year, y = avg_benchmark))+
geom_line()

colnames(data)

#Q4

#Plot the average share of Medicare Advantage (relative to all Medicare eligibles) over time from 2010 through 2015. 
#Has Medicare Advantage increased or decreased in popularity? How does this share correlate with benchmark payments?

table(data$plan_name)

q4 <- data %>%
  group_by(year) %>%
  mutate(med_prop = avg_enrollment/avg_eligibles)  

ggplot(q4, aes(x = year, y = med_prop)) +
geom_line()



#Q5
#Calculate the running variable underlying the star rating. Provide a table showing the number of plans that are 
#rounded up into a 3-star, 3.5-star, 4-star, 4.5-star, and 5-star rating.
data_raw <- data %>%
  mutate(raw_rating=rowMeans(
    cbind(breastcancer_screen,rectalcancer_screen,cv_cholscreen,diabetes_cholscreen,
          glaucoma_test,monitoring,flu_vaccine,pn_vaccine,physical_health,
          mental_health,osteo_test,physical_monitor,primaryaccess,
          hospital_followup,depression_followup,nodelays,carequickly,
          overallrating_care,overallrating_plan,calltime,
          doctor_communicate,customer_service,osteo_manage,
          diabetes_eye,diabetes_kidney,diabetes_bloodsugar,
          diabetes_chol,antidepressant,bloodpressure,ra_manage,
          copd_test,betablocker,bladder,falling,appeals_timely,
          appeals_review),
    na.rm=T)) %>%
  select(contractid, planid, fips, avg_enrollment, state, county, raw_rating, partc_score,
         avg_eligibles, avg_enrolled, premium_partc, risk_ab, Star_Rating,
         bid, avg_ffscost, ma_rate)

q5 <- data_raw %>%
  select(raw_rating) %>%
  mutate(three = sum(ifelse(raw_rating >= 2.75 & raw_rating < 3.25, 1, 0), na.rm = T), three_five = sum(ifelse(raw_rating >= 3.25 & raw_rating < 3.75, 1, 0), na.rm = T), four = sum(ifelse(raw_rating >= 3.75 & raw_rating < 4.25, 1, 0), na.rm = T), four_five = sum(ifelse(raw_rating >= 4.25 & raw_rating < 4.75, 1, 0), na.rm = T), five = sum(ifelse(raw_rating <= 3.75, 1, 0), na.rm = T))%>%
  head(n=1)


q5


colnames(data)
table(data$partcd_score)


#Q6
#Using the RD estimator with a bandwidth of 0.125, provide an estimate of the effect of receiving a 3-star versus a 
#2.5 star rating on enrollments. Repeat the exercise to estimate the effects at 3.5 stars, and summarize your results 
#in a table
data.rd1 <- data_raw %>%
  filter(Star_Rating==2.5 | Star_Rating==3)%>%
  mutate(score = raw_rating - 2.25,
         treat = (score>=0),
         window1 = (score>=-.125 & score<=.125),
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share),
         score_treat=score*treat)
star25.1 <- lm(mkt_share ~ score + treat, data=data.rd1)
star25.2 <- lm(mkt_share ~ score + treat, data= (data.rd1 %>% filter(window1==TRUE)))
star25.3 <- lm(mkt_share ~ score + treat + score_treat, data= (data.rd1 %>% filter(window1==TRUE)))
est1 <- as.numeric(star25.1$coef[3])
est2 <- as.numeric(star25.2$coef[3])
est3 <- as.numeric(star25.3$coef[3])


rows <- tribble(~term, ~ m1, ~ m2, ~ m3 ,
                'Bandwidth', "0.25", "0.125", "0.125")
attr(rows, 'position')  <- 7

modelsummary(list(star25.1, star25.2, star25.3),
          keep=c("score", "treatTRUE", "score_treat"),
          coef_map=c("score"="Raw Score", 
                    "treatTRUE"="Treatment",
                    "score_treat"="Score x Treat"),
          gof_map=c("nobs", "r.squared"),
          add_rows=rows)

data.rd2 <- data_raw %>%
  filter(Star_Rating==3 | Star_Rating==3.5) %>%
  mutate(score = raw_rating - 2.25,
         treat = (score>=0),
         window1 = (score>=-.125 & score<=.125),
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share),
         score_treat=score*treat)
star35.1 <- lm(mkt_share ~ score + treat, data=data.rd2)
star35.2 <- lm(mkt_share ~ score + treat, data= (data.rd2 %>% filter(window1==TRUE)))
star35.3 <- lm(mkt_share ~ score + treat + score_treat, data= (data.rd2 %>% filter(window1==TRUE)))
est1 <- as.numeric(star35.1$coef[3])
est2 <- as.numeric(star35.2$coef[3])
est3 <- as.numeric(star35.3$coef[3])


rows <- tribble(~term, ~ m1, ~ m2, ~ m3 ,
                'Bandwidth', "0.25", "0.125", "0.125")
attr(rows, 'position')  <- 7

modelsummary(list(star35.1, star35.2, star35.3),
          keep=c("score", "treatTRUE", "score_treat"),
          coef_map=c("score"="Raw Score", 
                    "treatTRUE"="Treatment",
                    "score_treat"="Score x Treat"),
          gof_map=c("nobs", "r.squared"),
          add_rows=rows)



#Q7
#Repeat your results for bandwidhts of 0.1, 0.12, 0.13, 0.14, and 0.15 (again for 3 and 3.5 stars). 
#Show all of the results in a graph. How sensitive are your findings to the choice of bandwidth?
data.rd3 <- data_raw %>%
  filter(Star_Rating==2.5 | Star_Rating==3)%>%
  mutate(score = raw_rating - 2.25,
         treat = (score>=0),
         window2 = (score>=-.1 & score<=.1),
         window3 = (score>=-.12 & score<=.12),
         window4 = (score>=-.13 & score<=.13),
         window5 = (score>=-.14 & score<=.14),
         window6 = (score>=-.15 & score<=.15),
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share),
         score_treat=score*treat)
star25.2 <- lm(mkt_share ~ score + treat + score_treat, data= (data.rd3 %>% filter(window2==TRUE)))
star25.3 <- lm(mkt_share ~ score + treat + score_treat, data= (data.rd3 %>% filter(window3==TRUE)))
star25.4 <- lm(mkt_share ~ score + treat + score_treat, data= (data.rd3 %>% filter(window4==TRUE)))
star25.5 <- lm(mkt_share ~ score + treat + score_treat, data= (data.rd3 %>% filter(window5==TRUE)))
star25.6 <- lm(mkt_share ~ score + treat + score_treat, data= (data.rd3 %>% filter(window6==TRUE)))

est2 <- as.numeric(star25.2$coef[3])
est3 <- as.numeric(star25.3$coef[3])
est4 <- as.numeric(star25.4$coef[3])
est5 <- as.numeric(star25.5$coef[3])
est5 <- as.numeric(star25.6$coef[3])

bandwidth <- c("0.1", "0.12", "0.13", "0.14", "0.15")
estimates <- c(est1, est2, est3, est4, est5)

three_stars <- data.frame(bandwidth,estimates)
three_stars


data.rd4 <- data_raw %>%
  filter(Star_Rating==3 | Star_Rating==3.5)%>%
  mutate(score = raw_rating - 2.25,
         treat = (score>=0),
         window2 = (score>=-.1 & score<=.1),
         window3 = (score>=-.12 & score<=.12),
         window4 = (score>=-.13 & score<=.13),
         window5 = (score>=-.14 & score<=.14),
         window6 = (score>=-.15 & score<=.15),
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share),
         score_treat=score*treat)
star35.2 <- lm(mkt_share ~ score + treat + score_treat, data= (data.rd4 %>% filter(window2==TRUE)))
star35.3 <- lm(mkt_share ~ score + treat + score_treat, data= (data.rd4 %>% filter(window3==TRUE)))
star35.4 <- lm(mkt_share ~ score + treat + score_treat, data= (data.rd4 %>% filter(window4==TRUE)))
star35.5 <- lm(mkt_share ~ score + treat + score_treat, data= (data.rd4 %>% filter(window5==TRUE)))
star35.6 <- lm(mkt_share ~ score + treat + score_treat, data= (data.rd4 %>% filter(window6==TRUE)))

est2 <- as.numeric(star35.2$coef[3])
est3 <- as.numeric(star35.3$coef[3])
est4 <- as.numeric(star35.4$coef[3])
est5 <- as.numeric(star35.5$coef[3])
est5 <- as.numeric(star35.6$coef[3])

bandwidth <- c("0.1", "0.12", "0.13", "0.14", "0.15")
estimates <- c(est1, est2, est3, est4, est5)

three_five_stars <- data.frame(bandwidth,estimates)
three_five_stars


#Q8
#Examine (graphically) whether contracts appear to manipulate the running variable. In other words, look at the 
#distribution of the running variable before and after the relevent threshold values. What do you find?

dens3 <- rddensity(data.rd1$score, c=0)
rdplotdensity(dens3, ma.rd1$score)

ggplot(data.rd1, aes(x=score)) + 
  geom_bar(width=.025) + theme_bw() +
  labs(
    x="Running Variable",
    y="Number of Plans",
    title="Distribution of Raw Scores"
  ) 

#Q9
#Similar to question 4, examine whether plans just above the threshold values have different characteristics than 
#contracts just below the threshold values. Use HMO and Part D status as your plan characteristics

ggplot(data.rd1, aes(x=partd)) + 
  geom_bar(width=.025) + theme_bw() +
  labs(
    x="Running Variable",
    y="Number of Plans",
    title="Distribution of Raw Scores"
  ) 

  data.rd1$partd

match.dat <- matchit(treat~partd, 
                     data=data.rd1 %>% 
                       filter(window2==TRUE, 
                              !is.na(treat), 
                              !is.na(premium_partc), 
                              !is.na(ma_rate)),
                     method=NULL, distance="mahalanobis")
love.plot(match.dat, abs=TRUE)     

colnames(data)

table(data$partd)

ma.rd275 <- data_raw %>%
  filter(Star_Rating==2.5 | Star_Rating==3) %>%
  mutate(score = raw_rating - 2.75,
         treat = (score >= 0),
         window1 = (score >= -.175 & score <= .175),
         window2 = (score >= -.125 & score <= .125),
         mkt_share = avg_enrollment / avg_eligibles,
         ln_share = log(mkt_share),
         score_treat = score * treat)
         
         
  ma.rd275 <- ma.rd275 %>%
  mutate(hmo = ifelse(plan_type == "HMO/HMOPOS", 1, 0), 
         part_d = ifelse(partd == "Yes", 1, 0))

est275 <- rdrobust(y=ma.rd275$mkt_share, x=ma.rd275$score, c=0,
                 h=0.125, p=1, kernel="uniform", vce="hc0",
                 masspoints="off")

  match.dat <- matchit(treat~hmo + part_d, 
                     data=ma.rd275 %>% 
                       filter(window2==TRUE, 
                              !is.na(treat), 
                              !is.na(hmo), 
                              !is.na(part_d)),
                     method=NULL, distance="mahalanobis")

love.plot(match.dat, abs=TRUE)  

library(ggplot2)

q9 <- data_raw %>%
  mutate(hmo = ifelse(plan_type == "HMO/HMOPOS", 1, 0), 
         dummy_partd = ifelse(partd == "Yes", 1, 0)) %>%
  group_by(raw_rating) %>%
  summarise(prop_hmo = mean(hmo), prop_d = mean(dummy_partd))

q9$dummy_partd

q9 %>%
  mutate(bin = ntile(prop_hmo, n = 30)) %>%
  group_by(bin) %>%
  summarise(xmean = mean(raw_rating), ymean = mean(prop_hmo)) %>%
  ggplot(aes(x = xmean, y = ymean)) + 
  geom_point() +
  geom_vline(xintercept = 2.75, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 3.25, linetype = "dashed", color = "blue") +
  scale_x_continuous(limits = c(2.5, 3.5))

q9 %>%
  mutate(bin = ntile(prop_d, n = 20)) %>%
  group_by(bin) %>%
  summarise(xmean = mean(raw_rating), ymean = mean(prop_d)) %>%
  ggplot(aes(x = xmean, y = ymean)) + 
  geom_point() +
  geom_vline(xintercept = 2.75, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 3.25, linetype = "dashed", color = "blue") +
  scale_x_continuous(limits = c(2.5, 3.5))

#covariate balance from homework 2


#Q10
#Summarize your findings from 5-9. What is the effect of increasing a star rating on enrollments?
# Briefly explain your results

