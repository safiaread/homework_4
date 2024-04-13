
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, fixest, xtable, modelsummary, rdrobust, MatchIt, cobalt, gridExtra)
data <- read_rds("/Users/safiaread/Desktop/homework_4/data/output/final_ma_data.rds")
data <- data %>% 
subset(year >= 2010 & year <= 2015 & snp == "No" & (planid < 800 | planid >= 900))

#no need to limit partd because the way the data was inputted

#Q1

#Remove all SNPs, 800-series plans, and prescription drug only plans (i.e., plans that do not offer Part C benefits).
#Provide a box and whisker plot showing the distribution of plan counts by county over time. Do you think that the 
#number of plans is sufficient, too few, or too many?


#boxplot
table(data$premium_partc)

table(data$county)

colnames(data)

figure_q1<- data %>%
  group_by(fips, year) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = factor(year), y = count)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Number of Plans by County", x = "County", y = "Count")+
  ylim(0,40)

figure_q1

#Have dropped outliers on boxplot

#Q2

#Provide bar graphs showing the distribution of star ratings in 2010, 2012, and 2015. 
#How has this distribution changed over time?

figure_2_2010 <- data%>%
filter(year == 2010)%>%
ggplot(aes(x = Star_Rating)) +
geom_histogram()+
xlab("Star Rating")+
ylab("Count of Plans")+
ggtitle("2010 Distribution of Plan's Star Ratings")

figure_2_2012 <- data%>%
filter(year == 2012)%>%
ggplot(aes(x = Star_Rating)) +
geom_histogram()+
xlab("Star Rating")+
ylab("Count of Plans")+
ggtitle("2012 Distribution of Plan's Star Ratings")

figure_2_2015 <- data%>%
filter(year == 2015)%>%
ggplot(aes(x = Star_Rating)) +
geom_histogram()+
xlab("Star Rating")+
ylab("Count of Plans")+
ggtitle("2015 Distribution of Plan's Star Ratings")

#Q3
#Plot the average benchmark payment over time from 2010 through 2015. 
#How much has the average benchmark payment risen over the years?

q3 <- data%>%
group_by(year)%>%
summarise(avg_benchmark = mean(ma_rate, na.rm = T))
#he grouped by ssa but makes mine look insane

figure_q3 <- ggplot(q3, aes(x = year, y = avg_benchmark))+
geom_line()+
labs(title = "Average Benchmark Payment from 2010-2015", x = "Year", y = "Average Benchmark Payment")

colnames(data)

#Q4

#Plot the average share of Medicare Advantage (relative to all Medicare eligibles) over time from 2010 through 2015. 
#Has Medicare Advantage increased or decreased in popularity? How does this share correlate with benchmark payments?

table(data$plan_name)
#Attempt 1

 #q4 <- data %>%
 # group_by(year) %>%
 # summarize(med_prop = mean(avg_enrolled / avg_eligibles, na.rm = TRUE))

q4 <- data %>%
  group_by(year) %>%
  summarize(med_prop = first(avg_enrolled, na.rm = TRUE) / first(avg_eligibles, na.rm = TRUE), bench = mean(ma_rate, na.rm = TRUE))


table(is.na(data$avg_enrollment))
table(is.na(data$avg_enrolled))

figure_q4 <- ggplot(q4, aes(x = year, y = med_prop)) +
geom_line() +
labs(title = "Average Share of Medicare Advantage from 2010-2015", x = "Year", y = "Average Share of Medicare Advantage")


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
         bid, avg_ffscost, ma_rate, plan_type, partd)

q5 <-data_raw %>%
  select(raw_rating, Star_Rating) %>%
  mutate(three = sum(ifelse(raw_rating >= 2.75 & raw_rating < 3 & Star_Rating == 3, 1, 0), na.rm = TRUE), 
         three_five = sum(ifelse(raw_rating >= 3.25 & raw_rating < 3.5 & Star_Rating == 3.5, 1, 0), na.rm = TRUE), 
         four = sum(ifelse(raw_rating >= 3.75 & raw_rating < 4 & Star_Rating == 4, 1, 0), na.rm = TRUE), 
         four_five = sum(ifelse(raw_rating >= 4.25 & raw_rating < 4.5 & Star_Rating == 4.5, 1, 0), na.rm = TRUE), 
         five = sum(ifelse(raw_rating >= 4.75 & raw_rating < 5.00 & Star_Rating == 5, 1, 0), na.rm = TRUE))


#mutate error
table_q5 <- q5[1,3:7]


#Q6
#Using the RD estimator with a bandwidth of 0.125, provide an estimate of the effect of receiving a 3-star versus a 
#2.5 star rating on enrollments. Repeat the exercise to estimate the effects at 3.5 stars, and summarize your results 
#in a table
ma.rd275 <- data_raw %>%
  filter(Star_Rating==2.5 | Star_Rating==3) %>%
  mutate(score = raw_rating - 2.75, 
         treat = (score >= 0),
         window2 = (score >= -.125 & score <= .125),
         mkt_share = avg_enrolled / avg_eligibles,
         ln_share = log(mkt_share),
         score_treat = score * treat)


#est275 <- rdrobust(y=ma.rd275$mkt_share, x=ma.rd275$score, c=0,
#                 h=0.125, p=1, kernel="uniform", vce="hc0",
#                 masspoints="off")

#est275$coef[1]
              
#summary(est275)

#rows <- tribble(~term, ~ m1, ~ m2, ~ m3 ,
              #  'Bandwidth', "0.25", "0.125", "0.#125")
#attr(rows, 'position')  <- 7

#modelsummary(list(star25.1, star25.2, star25.3),
         # keep=c("score", "treatTRUE", "score_treat"),
         # coef_map=c("score"="Raw Score", 
        #            "treatTRUE"="Treatment",
         #           "score_treat"="Score x Treat"),
        #  gof_map=c("nobs", "r.squared"),
        #  add_rows=rows)

#rows <- tribble(~term, ~ m1, ~ m2, ~ m3 ,
              #  'Bandwidth', "0.25", "0.125", "0.125")
#attr(rows, 'position')  <- 7

#modelsummary(list(star35.1, star35.2, star35.3),
          #keep=c("score", "treatTRUE", "score_treat"),
          #coef_map=c("score"="Raw Score", 
                   # "treatTRUE"="Treatment",
                    #"score_treat"="Score x Treat"),
          #gof_map=c("nobs", "r.squared"),
          #add_rows=rows)

ma.rd325 <- data_raw %>%
  filter(Star_Rating==3 | Star_Rating==3.5) %>%
  mutate(score = raw_rating - 3.25,
         treat = (score >= 0),
         window2 = (score >= -.125 & score <= .125),
         mkt_share = avg_enrolled / avg_eligibles,
         ln_share = log(mkt_share),
         score_treat = score * treat)


#est325 <- rdrobust(y=ma.rd325$mkt_share, x=ma.rd325$score, c=0,
#                 h=0.125, p=1, kernel="uniform", vce="hc0",
#                 masspoints="off")
              
#summary(est325)

est325$coef[1]

est275 <- lm(mkt_share ~ treat + score + treat_score, 
   data = ma.rd275 %>%
            filter(raw_rating >= (2.75 - 0.125) & raw_rating <= (2.75 + 0.125) & Star_Rating %in% c(2.5, 3.0)) %>%
            mutate(treat = (Star_Rating == 3.0),
                   score = ifelse(raw_rating == 2.75, 1, 0),
                   treat_score = treat * score))

est325 <- lm(mkt_share ~ treat + score + treat_score, 
   data = ma.rd325 %>%
            filter(raw_rating >= (3.25 - 0.125) & raw_rating <= (3.25 + 0.125) & Star_Rating %in% c(3.0, 3.5)) %>%
            mutate(treat = (Star_Rating == 3.5),
                   score = ifelse(raw_rating == 3.25, 1, 0),
                   treat_score = treat * score))
    
est275$coef[3]
table_q6 <- data.frame(est275$coef,est325$coef)


#Q7

# Create for loop with tidy function from broom package
bw <- c(0.1, 0.12, 0.13, 0.14, 0.15)

estimate_three <- rep(NA, 5)
estimate_threefive <- rep(NA, 5)
for(i in 1:length(bw)){
  first_reg <- lm(mkt_share ~ treat + score + treat_score, 
   data = ma.rd275 %>%
            filter(raw_rating >= (2.75 - bw[i]) & raw_rating <= (2.75 + bw[i]) & Star_Rating %in% c(2.5, 3.0)) %>%
            mutate(treat = (Star_Rating == 3.0),
                   score = ifelse(raw_rating == 2.75, 1, 0),
                   treat_score = treat * score))
  estimate_three[i] <- first_reg$coef[3]

  second_reg <- lm(mkt_share ~ treat + score + treat_score, 
   data = ma.rd325 %>%
            filter(raw_rating >= (3.25 - bw[i]) & raw_rating <= (3.25 + bw[i]) & Star_Rating %in% c(3.0, 3.5)) %>%
            mutate(treat = (Star_Rating == 3.5),
                   score = ifelse(raw_rating == 3.25, 1, 0),
                   treat_score = treat * score))
  estimate_threefive[i] <- second_reg$coef[3]

  i <- i+1
}

bandwidth <- c("0.1", "0.12", "0.13", "0.14", "0.15")

joint_loop <- data.frame(bandwidth,estimate_three, estimate_threefive)
joint_loop

figure_q7 <- ggplot(data = joint_loop, aes(x = bandwidth)) +
  geom_point(aes(y = estimate_three, color = "3 Star")) +
  geom_point(aes(y = estimate_threefive, color = "3.5 Star")) +
  labs(x = "Bandwidth", y = "Regression Coefficient", title = "Regression Results for 3 and 3.5 Stars") +
  scale_color_manual(values = c("3 Star" = "green", "3.5 Star" = "red")) +
  guides(color = guide_legend(title = "Star Rating"))

#label graph

#rdrobust(y=ma.rd375$mkt_share, x=ma.rd375$score, c=0,
            #     h=0.125, p=1, kernel="uniform", vce="hc0",
           #      masspoints="off")

#Q8
#Examine (graphically) whether contracts appear to manipulate the running variable. In other words, look at the 
#distribution of the running variable before and after the relevent threshold values. What do you find?

figure_q8_1 <- ggplot(data_raw, aes(x=raw_rating)) + 
  geom_density() + theme_bw() +
  labs(
    x="Running Variable",
    y="Number of Plans",
    title="Distribution of Raw Scores for 2.75 Cutoff"
  ) +
  geom_vline(xintercept = 2.75, linetype = "dashed", color = "red")+
  scale_x_continuous(limits=c(2.5,3))
  #add scale limits

  figure_q8_2 <-ggplot(data_raw, aes(x=raw_rating)) + 
  geom_density() + theme_bw() +
  labs(
    x="Running Variable",
    y="Number of Plans",
    title="Distribution of Raw Scores for 3.25 Cutoff"
  ) +
  geom_vline(xintercept = 3.25, linetype = "dashed", color = "red")+
  scale_x_continuous(limits=c(3,3.5))

  
  #scale_x_continous()
  #add scale limits


#Q9
#Similar to question 4, examine whether plans just above the threshold values have different characteristics than 
#contracts just below the threshold values. Use HMO and Part D status as your plan characteristics


q9 <- data_raw %>%
  mutate(hmo = ifelse(plan_type == "HMO/HMOPOS", 1, 0), 
         dummy_partd = ifelse(partd == "Yes", 1, 0)) %>%
  group_by(raw_rating) %>%
  summarise(prop_hmo = mean(hmo), prop_d = mean(dummy_partd))



q9$dummy_partd

figure_q9_1 <- q9 %>%
  mutate(bin = ntile(prop_hmo, n = 30)) %>%
  group_by(bin) %>%
  summarise(xmean = mean(raw_rating), ymean = mean(prop_hmo)) %>%
  ggplot(aes(x = xmean, y = ymean)) + 
  geom_point() +
  geom_vline(xintercept = 2.75, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 3.25, linetype = "dashed", color = "blue") +
  scale_x_continuous(limits = c(2.5, 3.5))+
  labs(title = "HMOs")

figure_q9_2 <- q9 %>%
  mutate(bin = ntile(prop_d, n = 20)) %>%
  group_by(bin) %>%
  summarise(xmean = mean(raw_rating), ymean = mean(prop_d)) %>%
  ggplot(aes(x = xmean, y = ymean)) + 
  geom_point() +
  geom_vline(xintercept = 2.75, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 3.25, linetype = "dashed", color = "blue") +
  scale_x_continuous(limits = c(2.5, 3.5))+
  labs(title = "Part D")

combined_plots <- grid.arrange(figure_q9_1, figure_q9_2, ncol = 2)

table_q9_1 <- q9 %>%
  filter(raw_rating >= 2.5 & raw_rating <= 3) %>%
  mutate(three_bench = ifelse(raw_rating < 2.75, "Under", "Over")) %>%
  group_by(three_bench) %>%
  summarise(proportion_hmo = mean(prop_hmo), proportion_partd = mean(prop_d))  %>%
  rename(`Raw_Score_of_2.75` = three_bench, `Proportion_HMO` = proportion_hmo, `Proportion_Part_D` = proportion_partd)

table_q9_2 <- q9 %>%
  filter(raw_rating >= 3 & raw_rating <= 3.5) %>%
  mutate(three_bench = ifelse(raw_rating < 3.25, "Under", "Over")) %>%
  group_by(three_bench) %>%
  summarise(proportion_hmo = mean(prop_hmo), proportion_partd = mean(prop_d)) %>%
  rename(`Raw_Score_of_3.25` = three_bench, `Proportion_HMO` = proportion_hmo, `Proportion_Part_D` = proportion_partd)

#covariate balance from homework 2


#Q10
#Summarize your findings from 5-9. What is the effect of increasing a star rating on enrollments?
# Briefly explain your results

rm(list=c("data", "q3", "q4", "q5", "ma.rd275", "ma.rd325")) # nolint
save.image("submission_3/Hwk4_workspace.Rdata")