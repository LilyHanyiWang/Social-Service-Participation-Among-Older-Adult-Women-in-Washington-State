library(tidyverse)
library(mlmRev)
library(lme4)
library(arm)
library(haven)
library(dplyr)
library(table1)

## ------------- working directories for Hanyi ----------
wd_hanyi = '/Users/hanyiwang/Desktop/Social-Service-Participation-Among-Older-Adult-Women-in-Washington-State'
path_hanyi = c("../SIPPdata/pu2021.dta",
               "../SIPPdata/sipp.csv")

## ------------- read data ----------
setwd(wd_hanyi)

pu2021 = read_dta(path_hanyi[1],col_select = c(
  'TEHC_ST',
  
  'SSUID','PNUM','MONTHCODE','TAGE','ESEX','ERACE','TRACE','EORIGIN','EEDUC','EHLTSTAT',
  
  'RGA_MNYN', 'RSNAP_MNYN' ,'RSSI_MNYN' ,'RTANF_MNYN','RWIC_MNYN', 'ECHLD_MNYN' ,'ECVDMNYN', 'EOTHAS_MNYN',
  'ETRANS_MNYN' ,'EUC1MNYN','EUC2MNYN', 'EUC3MNYN',
  
  'TMDPAY','TOTCMDPAY'))

## ------------- filter population of interest ----------
sipp = pu2021 %>% filter(TAGE >= 50 & ESEX ==2 & TEHC_ST=='53')
sipp = pu2021 %>% filter(TAGE >= 50 & ESEX ==2 )

table1(~ RGA_MNYN+RSNAP_MNYN + RSSI_MNYN+RTANF_MNYN + RWIC_MNYN +ECHLD_MNYN +ECVDMNYN +
         EOTHAS_MNYN +ETRANS_MNYN +EUC1MNYN +EUC2MNYN+ EUC3MNYN,data=sipp)


table1(~ ECHLD_MNYN +ECVDMNYN +EUC1MNYN +EUC2MNYN+ EUC3MNYN,data=sipp)

# a lot of missing data in ECHLD_MNYN, ECVDMNYN, EUC1MNYN, EUC2MNYN, EUC3MNYN.


## ------------- modify data set ----------

# UQID: unique individual id, from merging SSUID (unique household ID) and PNUM (individual ID within household). 
sipp = sipp %>% unite('UQID',c('SSUID','PNUM'),sep='',remove = FALSE)

#### demographic ####
sipp = sipp %>%
  #### month
  mutate(MONTHCODE=factor(MONTHCODE)) %>%
  
  #### demographic
  mutate(ESEX=factor(ESEX,levels=1:2,labels=c('Male','Female'))) %>%
  mutate(ERACE=factor(ERACE,levels=1:4,labels=c('White alone','Black alone','Asian alone','Residual'))) %>%
  mutate(TRACE=factor(TRACE,levels=1:10,
                      labels=c('White alone','Black alone','American Indian or Alaska Native alone (AIAN)',
                               'Asian alone','Native Hawaiian or Other Pacific Islander alone (HP)',
                               'White-Black','White-AIAN', 'White-Asian','Black-AIAN','Other 2 or more races'))) %>%
  mutate(EORIGIN=factor(EORIGIN,levels=1:2,labels=c('Is Spanish, Hispanic, or Latino','No'))) %>%
  mutate(EEDUC=factor(EEDUC,levels=31:46,
                      labels=c( 'Less than 1st grade','1st, 2nd, 3rd or 4th grade', '5th or 6th grade',
                                '7th or 8th grade','9th grade','10th grade','11th grade','12th grade, no diploma',
                                'High School Graduate','Some college credit, but less than 1 year',
                                '1 or more years of college, no degree','Associate’s degree' , 'Bachelor’s degree',
                                'Master’s degree' ,'Professional School degree' ,'Doctorate degree'))) %>%
  mutate(EHLTSTAT=factor(EHLTSTAT,levels=1:5,labels=c('Excellent','Very good','Good','Fair','Poor')))
  
  #### social service covariates
sipp = sipp %>%
  mutate(RGA_MNYN=as.numeric(RGA_MNYN ==1)) %>%
  mutate(RSNAP_MNYN=as.numeric(RSNAP_MNYN==1)) %>%
  mutate(RSSI_MNYN=as.numeric(RSSI_MNYN==1)) %>%
  mutate(RTANF_MNYN=as.numeric(RTANF_MNYN==1)) %>%
  mutate(RWIC_MNYN=as.numeric(RWIC_MNYN==1)) %>%
  mutate(ECHLD_MNYN=as.numeric(ECHLD_MNYN==1)) %>%
  mutate(ECVDMNYN=as.numeric(ECVDMNYN==1)) %>%
  mutate(EOTHAS_MNYN=as.numeric(EOTHAS_MNYN==1)) %>%
  mutate(ETRANS_MNYN=as.numeric(ETRANS_MNYN==1)) %>%
  mutate(EUC1MNYN=as.numeric(EUC1MNYN==1)) %>%
  mutate(EUC2MNYN=as.numeric(EUC2MNYN==1)) %>%
  mutate(EUC3MNYN=as.numeric(EUC3MNYN==1))

#### SS_SCORE #####
# names(sipp)
## social service participation score: SS_SCORE = sum of all social services 
# exclude variables with a lot of missing: ECHLD_MNYN, ECVDMNYN, EUC1MNYN, EUC2MNYN, EUC3MNYN.
names(sipp)[c(10,11,15,16,17,18,19)] # variables included in ss_score
sipp$SS_SCORE = rowSums(sipp[,c(10,11,15,16,17,18,19)], na.rm=TRUE)

table1(~factor(SS_SCORE),data=sipp)

sipp = sipp %>%
  mutate(RGA_MNYN= factor(RGA_MNYN,levels=c(1,0),labels=c('Yes','No'))) %>%
  mutate(RSNAP_MNYN=factor(RSNAP_MNYN,levels=c(1,0),labels=c('Yes','No'))) %>%
  mutate(RSSI_MNYN=factor(RSSI_MNYN,levels=c(1,0),labels=c('Yes','No'))) %>%
  mutate(RTANF_MNYN=factor(RTANF_MNYN,levels=c(1,0),labels=c('Yes','No'))) %>%
  mutate(RWIC_MNYN=factor(RWIC_MNYN,levels=c(1,0),labels=c('Yes','No'))) %>%
  mutate(ECHLD_MNYN=factor(ECHLD_MNYN,levels=c(1,0),labels=c('Yes','No'))) %>%
  mutate(ECVDMNYN=factor(ECVDMNYN,levels=c(1,0),labels=c('Yes','No'))) %>%
  mutate(EOTHAS_MNYN=factor(EOTHAS_MNYN,levels=c(1,0),labels=c('Yes','No'))) %>%
  mutate(ETRANS_MNYN=factor(ETRANS_MNYN,levels=c(1,0),labels=c('Yes','No'))) %>%
  mutate(EUC1MNYN=factor(EUC1MNYN,levels=c(1,0),labels=c('Yes','No'))) %>%
  mutate(EUC2MNYN=factor(EUC2MNYN,levels=c(1,0),labels=c('Yes','No'))) %>%
  mutate(EUC3MNYN=factor(EUC3MNYN,levels=c(1,0),labels=c('Yes','No')))

table1(~ RGA_MNYN+RSNAP_MNYN + RSSI_MNYN+RTANF_MNYN + RWIC_MNYN +ECHLD_MNYN +ECVDMNYN +
         EOTHAS_MNYN +ETRANS_MNYN +EUC1MNYN +EUC2MNYN+ EUC3MNYN,data=sipp)

#### HI #### 
## healthcare cost in total: HI = TMDPAY+TOTCMDPAY
sipp$HI = rowSums(sipp[,24:25], na.rm=TRUE)

table1(~HI,data=sipp)
 
## ------------- store as new dataset 'sipp' to ensure easier access for modelling ----------
write.csv(sipp,"path_hanyi[2]")

