library(tidyverse)
library("dplyr")
library(ggplot2)
library(RColorBrewer)
library(scales)
library(gridExtra)
library(ggeasy)

install.packages("ggeasy")

#Prior substance use on 322
#Recoded substance use disorders - use to get data on those with active disorders 302
#Imputed substance use disorders -includes those in recovery 
#Substance abuse - get general abuse data 239
#substance use - get general use data 126
#mental health variables page 412

#substance use(excluding marijuana and tobacco) that required rehabilitation ever and see if there is a relationship
#between depression (and possibly other mental health issues), if there is time look at other factors too 

#Analyze how many people had a substance problem that required treatment ever and 12 months
# Show demographics of substance use treatment age, general location and income
#Show how many people with substance problem believe they had a mental health problem 
#Show demographics of substance use treatment age, general location and income
#histogram of weeks with depression 
#Show different types of treatment recieved 
#logistic regression to see if there is relationship between substance use and mental health 

#91 or 991 or 9991, etc. 93 or 993 or 9993, etc.
#94 or 994 or 9994, etc.
#97 or 997 or 9997, etc. 
#98 or 998 or 9998, etc. 
#99 or 999 or 9999, etc.
#= NEVER USED [DRUG(s) OF INTEREST]
#= USED [DRUG] BUT NOT IN THE PERIOD OF INTEREST
#= DON’T KNOW
#= REFUSED
#= BLANK (i.e., not answered; not asked the question) = LEGITIMATE SKIP
#The following analogous codes also were assigned as part of the logical editing process. 
#Exceptions are noted in the documentation for specific variables.
#81 or 981 or 9981, etc.
#83 or 983 or 9983, etc.
#85 or 985 or 9985, etc.
#89 or 989 or 9989, etc.
#= NEVER USED [DRUG(s) OF INTEREST] Logically assigned
#= USED [DRUG] BUT NOT IN THE PERIOD OF INTEREST Logically assigned
#= BAD DATA Logically assigned (i.e., usually inconsistent with other data)
#= LEGITIMATE SKIP Logically assigned

# 1 = Yes
# 2 = No

#AMDEYR - MDE in past 12 months
#QUESTID2 - identifier
#CASURCVR - believe they are recovered from substance problem 
#CAMHRCVR - believe they are recovered from mental health issues 
#CAMHPROB - believe they ever had problem with mental health issue
#TXEVRRCVD - ever received substance treatment
#TXYRUSEADB RCVD TREATMENT IN PAST 12 MOS FOR ALCOHOL, DRUGS, OR BOTH
#TXYRHOSOV RCVD TXFILL1 TRMT IN HOSP/OVERNIGHT PST 12 MOS
#TXYRHOSAD HOSP/OVERNIGHT TRMT FOR ALC, DRUGS, OR BOTH
#TXYRRESOV RCVD TXFILL1 TRMT REHAB CENTR/OVERNIGHT PST 12 MO
#TXYRRESAD RES ALC/DRG REH TRMT FOR ALC, DRUGS, OR BOTH
#TXYROUTPT outpatient treatment for substance use in past 12 MOS
#TXYROUTAD OUTPATIENT TRMT FOR ALC, DRUGS, OR BOTH
#TXYRMHCOP received treatment for substance use at mental health facility in past 12 months
#TXYRMHCAD MNT HEALTH CNTR TRMT FOR ALC, DRUGS, OR BOTH
#TXYREMRGN RCVD substance TRMT IN EMERGENCY ROOM PAST 12 MOS
#TXYREMRAD EMERGENCY ROOM TRMT FOR ALC, DRUGS, OR BOTH
#TXYRDRPAD PRIV DR.'S OFFICE TRMT FOR ALC, DRUGS, OR BOTH
#TXYRPRISN RCVD TXFILL1 TRMT IN PRISON/JAIL PAST 12 MONTHS
#TXYRPRIAD PRISON/JAIL TRMT FOR ALC, DRUGS, OR BOTH
#TXYRSLFHP RCVD TXFILL1 TRMT IN SELF-HELP GROUP PAST 12 MOS
#TXYRSLFAD SELF-HELP GROUP TRMT FOR ALC, DRUGS, OR BOTH
#TXYROTHER RCVD TRMT IN SOME OTHER PLACE PAST 12 MOS
#TXYROTHSP2 other place treatment was recieved 
#IMPWEEKS NUM WEEKS HAVE DIFFICULTIES BECAUSE OF MENTL HLTH
#SUICTHNK SERIOUSLY THINK ABOUT KILLING SELF PST 12 MOS
#SUIPLANYR ADULT MAKE PLANS TO KILL SELF PST 12 MOS
#ADLSI2WK PERIOD OF TIME depression LASTED EVERY DAY FOR 2 WKS /LNGR
#AUINPYR STAY OVNT IN HOSP FOR MH TRMT PST 12 MOS
#AUOPTYR RCVD OUTPATIENT MH TRMT PST 12 MOS
#age3 - age
#INCOME - total family income 1 - less than 20,000 2 - 20000-49999, 3 - 50000-74999, 4 - 75000 or more
#COUTYP4 - city or town 1 - large metro, 2 - small metro, 3 - nonmetro 

#load in data 
NSDUH <- PUF2021_100622

NSDUH <- data.frame(NSDUH)


#select data needed and rename variables
NSDUH <- NSDUH %>% select(QUESTID2 , AGE3, income, COUTYP4, txevrrcvd, 
                          txyruseadb, txyrhosov,
                          txyrresov, txyroutpt, 
                          txyrmhcop , txyremrgn, 
                          txyrdrpad, txyrprisn, txyrslfhp, 
                          txyrother, TXYROTHSP2, impweeks,
                          suicthnk, suiplanyr, ADLSI2WK,
                          auinpyr, auoptyr, amdeyr,
                          casurcvr, camhrcvr, camhprob) %>% 
  
  rename(id = QUESTID2 , age = AGE3, income = income, citysize = COUTYP4, rec_sub_trtm = txevrrcvd, 
                          sub_trtm_12mo = txyruseadb, hos_trtm = txyrhosov,
                         rehab_trtm = txyrresov, outpatient_trtm = txyroutpt, 
                        mhfacility_trtm = txyrmhcop , ER_trtm = txyremrgn, 
                        private_trtm = txyrdrpad, prison_trtm = txyrprisn, selfhelp_trtm = txyrslfhp, 
                        other_trtm = txyrother, type_other_trtm = TXYROTHSP2, num_weeks_mh = impweeks,
                        think_suicide = suicthnk, plan_suicide = suiplanyr, dep_two_weeks = ADLSI2WK,
                        hos_overnight_mh = auinpyr, outpatient_mh = auoptyr, MDE_pastyear = amdeyr,
                        think_recovered_sub = casurcvr, think_recovered_mh = camhrcvr, think_had_mh = camhprob)
  
#filter to only keep respondents who are 18 years or older and 
#those who responded to ever receiving treatment for SUD
  filter(NSDUH, rec_sub_trtm == 1 | rec_sub_trtm == 2 & age >=4) 
  
 #re-coding variables in data set 
 clean_NSDUH <- NSDUH %>%
   mutate(rec_sub_trtm = factor(rec_sub_trtm, 
                                levels = c(1,2), 
                                labels = c("yes", "no") )) %>% 
  
   mutate(age = factor(age, 
                                 levels = c(4,5,6,7,8,9,10,11),
                                 labels = c("18-20", "21-23", " 24-25", "26-29", "30-34", 
                                            "35-49", "50-64", "65 and older"))) %>% 
   mutate(income = factor(income, 
                       levels = c(1,2,3,4),
                       labels = c("less than $20,000", "$20,000 - $49,999", "$50,000 - $74,999", "$75,000 or more"))) %>% 
   
   mutate( citysize = factor(citysize, 
                       levels = c(1,2,3),
                       labels = c("Large city", "Small city/town", "Rural"))) %>% 
   
   mutate(sub_trtm_12mo = factor(sub_trtm_12mo, 
                                  levels = c(1,2),
                                  labels = c("yes", "no"))) %>% 
   
   mutate(hos_trtm = factor(hos_trtm, 
                                 levels = c(1,2),
                                 labels = c("yes", "no"))) %>% 
  
  mutate(rehab_trtm = factor(rehab_trtm, 
                                 levels = c(1,2),
                                 labels = c("yes", "no"))) %>% 
   
  mutate(outpatient_trtm = factor(outpatient_trtm, 
                              levels = c(1,2),
                              labels = c("yes", "no"))) %>% 
  
  mutate(mhfacility_trtm = factor(mhfacility_trtm, 
                            levels = c(1,2),
                            labels = c("yes", "no"))) %>% 
 
  mutate(ER_trtm = factor(ER_trtm, 
                              levels = c(1,2),
                              labels = c("yes", "no"))) %>% 
  
  mutate(private_trtm = factor(private_trtm, 
                              levels = c(1,2),
                              labels = c("yes", "no"))) %>% 
  
  mutate(prison_trtm = factor(prison_trtm, 
                              levels = c(1,2),
                              labels = c("yes", "no"))) %>% 
  
  mutate(selfhelp_trtm = factor(selfhelp_trtm, 
                              levels = c(1,2),
                              labels = c("yes", "no"))) %>% 
   
  mutate(other_trtm = factor(other_trtm, 
                              levels = c(1,2),
                              labels = c("yes", "no"))) %>% 

  mutate(type_other_trtm = factor(type_other_trtm, 
                              levels = c(2,6,8,9,40, 41, 42, 44, 45, 46, 47, 48, 49, 52, 53, 56, 64, 65, 67, 90 ),
                              labels = ("other"))) %>% 
  
  # mutate(num_weeks_mh = factor(num_weeks_mh, 
  #                             levels = c(1,2),
  #                             labels = c("yes", "no"))) %>% 
   
   mutate(think_suicide = factor(think_suicide, 
                              levels = c(1,2),
                              labels = c("yes", "no"))) %>% 
   
   mutate(plan_suicide = factor(plan_suicide, 
                                 levels = c(1,2),
                                 labels = c("yes", "no"))) %>% 
   
   mutate(dep_two_weeks = factor(dep_two_weeks, 
                              levels = c(1,2),
                              labels = c("yes", "no"))) %>% 
   
   mutate(hos_overnight_mh = factor(hos_overnight_mh, 
                              levels = c(1,2),
                              labels = c("yes", "no"))) %>% 
   
   mutate(outpatient_mh = factor(outpatient_mh, 
                              levels = c(1,2),
                              labels = c("yes", "no"))) %>% 
   
   mutate(MDE_pastyear = factor(MDE_pastyear, 
                              levels = c(1,2),
                              labels = c("yes", "no"))) %>% 
   
   mutate(think_recovered_sub = factor(think_recovered_sub, 
                              levels = c(1,2),
                              labels = c("yes", "no"))) %>% 
   
   mutate(think_recovered_mh = factor(think_recovered_mh, 
                              levels = c(1,2),
                              labels = c("yes", "no"))) %>% 
   
   mutate(think_had_mh = factor(think_had_mh, 
                              levels = c(1,2),
                              labels = c("yes", "no")))
   
 plot_theme <- function() {
   theme(plot.title=element_text(hjust=0.5,size=16),axis.text.y = element_text(size=12),axis.text.x = element_text(size=12),
         axis.title.y = element_text(size=14),axis.title.x = element_text(size=14),
         legend.text = element_text(size = 12),legend.title = element_text(size = 14))     
 }
 
 
 # Show bar plot of respondents who ever required substance use treatment 
 clean_NSDUH %>% 
   filter(!is.na(rec_sub_trtm)) %>% 
   ggplot(aes(x = rec_sub_trtm, y = after_stat(count/sum(count)), fill = rec_sub_trtm)) +
   geom_bar() +
   plot_theme() + 
   scale_y_continuous(labels = percent) +
   ggtitle("Received Treatment for SUD") +
   xlab(" ") +
   ylab("Percent") +
   scale_fill_manual(name=" ",values=c("hotpink","darkgrey")) 
 
 #Show bar plot of respondents who required substance use treatment in the past year
 clean_NSDUH %>%
   filter(!is.na(sub_trtm_12mo)) %>% 
   ggplot(aes(x = sub_trtm_12mo, y = after_stat(count/sum(count)), fill = sub_trtm_12mo)) +
   geom_bar() +
   plot_theme() + 
   scale_y_continuous(labels = percent) +
   ggtitle("Received SUD treatment (past 12 months)") +
   xlab(" ") +
   ylab("Percent") +
   scale_fill_manual(name=" ",values=c("hotpink","darkgrey"))
 
 
 #Age of respondents who have received treatment 
 clean_NSDUH %>% 
   filter(rec_sub_trtm == "yes") %>% 
   filter(!is.na(age)) %>% 
   ggplot(aes(x = age, y = after_stat(count/sum(count)))) +
   geom_bar(fill = "hotpink") +
   plot_theme() + 
   scale_y_continuous(labels = percent) +
   ggtitle("Age of Respondents that Received Treatment") +
   xlab(" ") +
   ylab("Percent")
  
  
 #Respondents from rural areas 
 rural <- clean_NSDUH %>% 
    filter(!is.na(rec_sub_trtm)) %>% 
    filter(citysize == "Rural") %>% 
    ggplot(aes(x = rec_sub_trtm, y = after_stat(count/sum(count)))) +
    geom_bar(fill = "hotpink") +
    plot_theme() + 
    scale_y_continuous(labels = percent) +
    ggtitle("Rural area") +
    xlab(" ") +
    ylab("Percent")
  
 #respondents from small city/town  
  smallcity <- clean_NSDUH %>% 
    filter(!is.na(rec_sub_trtm)) %>% 
    filter(citysize == "Small city/town") %>% 
    ggplot(aes(x = rec_sub_trtm, y = after_stat(count/sum(count)))) +
    geom_bar(fill = "hotpink") +
    plot_theme() + 
    scale_y_continuous(labels = percent) +
    ggtitle("Small city/town") +
    xlab(" ") +
    ylab("Percent")
  
  
  #respondents from large cities
  largecity <- clean_NSDUH %>% 
    filter(!is.na(rec_sub_trtm)) %>% 
    filter(citysize == "Large city") %>% 
    ggplot(aes(x = rec_sub_trtm, y = after_stat(count/sum(count)))) +
    geom_bar(fill = "hotpink") +
    plot_theme() +
    scale_y_continuous(labels = percent) +
    ggtitle("Large city") +
    xlab(" ") +
    ylab("Percent")
  
  grid.arrange(rural, smallcity, largecity, nrow = 2)
  
  #compare where respondents received treatment
  NSDUH_long <- clean_NSDUH %>% 
    rename(Hospital = hos_trtm, Rehab = rehab_trtm, 
            Outpatient = outpatient_trtm, "MH facility" = mhfacility_trtm, "Emergency Room" = ER_trtm,
            "Private Facility" = private_trtm, "Jail/Prison" = prison_trtm, "Self Help" =selfhelp_trtm)
  
  NSDUH_long <- gather(NSDUH_long, key = "Facility", value="response", c("Hospital", "Rehab", 
                                                                          "Outpatient", "MH facility", "Emergency Room",
                                                                          "Private Facility", "Jail/Prison", "Self Help"))
  
  NSDUH_long %>%
  filter(rec_sub_trtm == "yes") %>%
  filter(response == "yes")%>% 
    ggplot(aes(x = Facility, y = after_stat(count/sum(count)), fill = response)) +
    geom_bar(fill = "hotpink", position = 'dodge') +
    plot_theme() +
    scale_y_continuous(labels = percent) +
    ggtitle("Treatment Facility Type") +
    xlab(" ") +
    ylab("Percent") +
    easy_rotate_x_labels(angle = 90) 
  
  #Respondents who received treatment and think they had MH issues
  clean_NSDUH %>% 
    filter(rec_sub_trtm == "yes") %>% 
    filter(!is.na(think_had_mh)) %>% 
    ggplot(aes(x = think_had_mh, y = after_stat(count/sum(count)), fill = think_had_mh)) +
    geom_bar() +
    plot_theme() + 
    scale_y_continuous(labels = percent) +
    ggtitle("Think They Had MHI (past year)") +
    xlab(" ") +
    ylab("Percent") +
    scale_fill_manual(name=" ",values=c("hotpink","darkgrey")) 
  
  #Respondents who had depression lasting two weeks or longer
  clean_NSDUH %>% 
    filter(rec_sub_trtm == "yes") %>% 
    filter(!is.na(dep_two_weeks)) %>% 
    ggplot(aes(x = dep_two_weeks, y = after_stat(count/sum(count)), fill = dep_two_weeks)) +
    geom_bar() +
    plot_theme() + 
    scale_y_continuous(labels = percent) +
    ggtitle("Depression Lasting Two Weeks or Longer") +
    xlab(" ") +
    ylab("Percent") +
    scale_fill_manual(name=" ",values=c("hotpink","darkgrey")) 
  
   #number of weeks of MHI
  clean_NSDUH %>% 
    filter(rec_sub_trtm == "yes") %>% 
    filter(num_weeks_mh >= 2) %>% 
    ggplot(aes(x = num_weeks_mh)) +
    geom_histogram(fill = "hotpink", binwidth = 4) +
    plot_theme() +
    ggtitle("Length of MHI") +
    xlab("Length (weeks)") +
    ylab("Number of Respondents")

    
  
  
  
  #Respondents who believe they had MHI who think they are recovered
  clean_NSDUH %>% 
    filter(rec_sub_trtm == "yes" & think_had_mh == "yes") %>% 
    filter(!is.na(think_recovered_mh)) %>% 
    ggplot(aes(x = think_recovered_mh, y = after_stat(count/sum(count)), fill = think_recovered_mh)) +
    geom_bar() +
    plot_theme() + 
    scale_y_continuous(labels = percent) +
    ggtitle("Think They Are Recovered From MHI") +
    xlab(" ") +
    ylab("Percent") +
    scale_fill_manual(name=" ",values=c("hotpink","darkgrey")) 
  
  

  # use this for weeks depressed
  # ggplot(data = clean_NSDUH, mapping = aes(x = , y = )) +
  #   geom_point(alpha = 0.1, aes(color = citysize)) 
  
  # NSDUH_long <- gather(clean_NSDUH, key = "measure", value="value", c("hos_trtm", "rehab_trtm", 
  #                                                                     "outpatient_trtm", "mhfacility_trtm", "ER_trtm", 
  #                                                                     "private_trtm", "prison_trtm", "selfhelp_trtm"))
  # 
  # 
  # NSDUH_long %>%
  #  filter(rec_sub_trtm == "yes") %>%
  #  filter(!is.na(value)) %>%
  #  ggplot(aes(x = value, y = after_stat(count/sum(count)))) +
  #  geom_bar(fill = "hotpink")+
  #  plot_theme() +
  #  scale_y_continuous(labels = percent) +
  #  ggtitle("Place of treatment") +
  #  xlab(" ") +
  #  ylab("Percent") +
  #  facet_wrap(~measure)
  
  
  
   # Use the group_by() and summarise() functions to calculate percentages and make a table 
  # clean_NSDUH %>% 
  #   group_by(citysize) %>% 
  #   summarise(Percentage = n() / nrow(clean_NSDUH) * 100) 
  
  
  # clean_NSDUH %>% 
  # filter(!is.na(rec_sub_trtm)) %>% 
  #   ggplot(aes(x = rec_sub_trtm, y = after_stat(count/sum(count)), fill = citysize)) +
  #   geom_bar(position = 'dodge') +
  #   plot_theme() + 
  #   scale_y_continuous(labels = percent) +
  #   ggtitle("Respondents from Large city") +
  #   xlab("Received treatment ") +
  #   ylab("Percent") +
  #   scale_fill_manual(name=" ",values=c("hotpink","purple", "orange"), guide=guide_legend(reverse=TRUE)) 
  
  
  #facetwrap
  # NSDUH_long <- gather(clean_NSDUH, key="measure", value="value", c("citysize", "rec_sub_trtm"))
  # 
  # # ggplot(NSDUH_long, aes(x= , y=value))+
  # #   geom_bar(stat='identity', fill="")+
  # #   facet_wrap(~measure)
  
  #rotate label angles
  #ggeasy::easy_rotate_x_labels(angle = 90)
  
  # use this for weeks depressed
  # ggplot(data = NSDUH, mapping = aes(x = citysize, y = rec_sub_trtm)) +
  #   geom_point(alpha = 0.1, aes(color = citysize)) 
  
  
  #look at perecentage of patients who went to self-help vs paid treatment based on income
  
  

#mutate(id, rec_sub_trtm, 
       #sub_trtm_12mo, hos_trtm,
       #rehab_trtm, outpatient_trtm, 
       #mhfacility_trtm, ER_trtm, 
       #private_trtm, prison_trtm, selfhelp_trtm, 
       #other_trtm, type_other_trtm, num_weeks_mh,
       #think_suicide, plan_suicide, dep_two_weeks,
       #hos_overnight_mh, outpatient_mh, MDE_pastyear,
       #think_recovered_sub, think_recovered_mh, think_had_mh)


