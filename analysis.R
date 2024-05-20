library(tidyverse)
library(countrycode)
library(fixest)
library(texreg)
library(plm)
library(sf)

load("../data/processed/final_up.RData")

### Table 1; elections, democracy, clientelism ----

#Sanford 2021 regression
reg_1.1 <- final_up %>%
    feols(forest.diff ~ democracy_BX*election_DPI + forest.l + PCGDP.l + Pop.growth.l | FID + yr, cluster = ~ctr+yr)

#Sanford 2021 regression removing cells where election==1 but no CLEA data
reg_1.2 <- final_up %>% filter(!is.na(cst) | election_DPI==0) %>%
    feols(forest.diff ~ democracy_BX*election_DPI + forest.l + PCGDP.l + Pop.growth.l | FID + yr, cluster = ~ctr+yr)

#Our new target measure on all data
reg_1.3 <- final_up %>%
    feols(forest.diff ~ target*election_DPI + forest.l + PCGDP.l + Pop.growth.l | FID + yr, cluster = ~ctr+yr)

#Our new target measure regression removing cells where election==1 but no CLEA data
reg_1.4 <- final_up %>% filter(!is.na(cst) | election_DPI==0) %>%
    feols(forest.diff ~ target*election_DPI + forest.l + PCGDP.l + Pop.growth.l | FID + yr, cluster = ~ctr+yr)


### Table 2; comparing units of analysis: district vs. national ----

(reg_2.1 <- final_up %>% mutate(type = case_when(election_DPI==0 ~ "a_no_elec",
                                                comp_i<0.9 & comp_tvs>0.9 ~ "uncomp_dis_comp_nat",
                                                comp_i>0.9 & comp_tvs>0.9 ~ "comp_dis_comp_nat", 
                                                comp_i>0.9 & comp_tvs<0.9 ~ "comp_dis_uncomp_nat",
                                                comp_i<0.9 & comp_tvs<0.9 ~ "uncomp_both",
                                                .default=NULL)) %>%
    feols(forest.diff ~ type + forest.l + PCGDP.l + Pop.growth.l | FID + yr, cluster = ~ctr+yr))
    
(reg_2.2 <- final_up %>%
    mutate(type = case_when(comp_i<0.9 & comp_tvs<0.9 ~ "a_uncomp_both",
                            comp_i<0.9 & comp_tvs>0.9 ~ "uncomp_dis_comp_nat",
                            comp_i>0.9 & comp_tvs>0.9 ~ "comp_dis_comp_nat", 
                            comp_i>0.9 & comp_tvs<0.9 ~ "comp_dis_uncomp_nat")) %>% 
    feols(forest.diff ~ type + forest.l + PCGDP.l + Pop.growth.l | FID + yr, cluster = ~ctr+yr))


(reg_2.3 <- final_up %>% filter(target==1) %>% mutate(type = case_when(election_DPI==0 ~ "a_no_elec",
                                                comp_i<0.9 & comp_tvs>0.9 ~ "uncomp_dis_comp_nat",
                                                comp_i>0.9 & comp_tvs>0.9 ~ "comp_dis_comp_nat", 
                                                comp_i>0.9 & comp_tvs<0.9 ~ "comp_dis_uncomp_nat",
                                                comp_i<0.9 & comp_tvs<0.9 ~ "uncomp_both",
                                                .default=NULL)) %>%
    feols(forest.diff ~ type + forest.l + PCGDP.l + Pop.growth.l | FID + yr, cluster = ~ctr+yr))

(reg_2.4 <- final_up %>% filter(target==1) %>%
    mutate(type = case_when(comp_i<0.9 & comp_tvs<0.9 ~ "a_uncomp_both",
                            comp_i<0.9 & comp_tvs>0.9 ~ "uncomp_dis_comp_nat",
                            comp_i>0.9 & comp_tvs>0.9 ~ "comp_dis_comp_nat", 
                            comp_i>0.9 & comp_tvs<0.9 ~ "comp_dis_uncomp_nat")) %>% 
    feols(forest.diff ~ type + forest.l + PCGDP.l + Pop.growth.l | FID + yr, cluster = ~ctr+yr))

(reg_2.5 <- final_up %>% filter(target==0) %>% 
    mutate(type = case_when(election_DPI==0 ~ "a_no_elec",
                            comp_i<0.9 & comp_tvs>0.9 ~ "uncomp_dis_comp_nat",
                            comp_i>0.9 & comp_tvs>0.9 ~ "comp_dis_comp_nat", 
                            comp_i>0.9 & comp_tvs<0.9 ~ "comp_dis_uncomp_nat",
                            comp_i<0.9 & comp_tvs<0.9 ~ "uncomp_both",
                            .default=NULL)) %>%
    feols(forest.diff ~ type + forest.l + PCGDP.l + Pop.growth.l | FID + yr, cluster = ~ctr+yr))

(reg_2.6 <- final_up %>% filter(target==0) %>%
    mutate(type = case_when(comp_i<0.9 & comp_tvs<0.9 ~ "a_uncomp_both",
                            comp_i<0.9 & comp_tvs>0.9 ~ "uncomp_dis_comp_nat",
                            comp_i>0.9 & comp_tvs>0.9 ~ "comp_dis_comp_nat", 
                            comp_i>0.9 & comp_tvs<0.9 ~ "comp_dis_uncomp_nat")) %>% 
    feols(forest.diff ~ type + forest.l + PCGDP.l + Pop.growth.l | FID + yr, cluster = ~ctr+yr))

# REGRESSION TABLES ----

tab1 <- texreg(list(reg_1.1,reg_1.2,reg_1.3,reg_1.4),
                 include.rsquared = F, include.adjrs = TRUE,
                 caption = "Regressions of forest change on regime type and elections",
                 float.pos = "h")

tab2 <- texreg(list(reg_2.1,reg_2.2,reg_2.3,reg_2.4,reg_2.5,reg_2.6),
                 booktabs = T,
                 include.rsquared = F, include.adjrs = TRUE,
                 caption = "Regressions of forest change on electoral competitiveness",
                 float.pos = "h")

#APPENDIX MATERIALS ----

#DIFFERENT TARGET THRESHOLDS:

final_up <- final_up %>% mutate(target = ifelse(v2x_polyarchy>0.4 & v2xnp_client>0.1,1,0))

#Excluding Autocracies (v2x_polyarchy<0.5):

#Replica of Tab. 1, col. 2, removing all polities which don't pass the polyarchy>0.5 test; i.e., it's debatable whether in these polities elections actually matter at all.
reg_0.2 <- final_up %>% 
    filter(v2x_polyarchy>0.5) %>% 
    feols(forest.diff ~ target*election_DPI + forest.l + PCGDP.l + Pop.growth.l | FID + yr, cluster = ~ctr+yr)

final_up <- final_up %>% 
    filter(v2x_polyarchy > 0.5) %>% 
    mutate(target = ifelse(v2xnp_client>0.2,1,0))

reg_0.4 <- final_up %>% 
    filter(target==1) %>%
    mutate(type = case_when(election_DPI==0 ~ "a_no_elec",
                            comp_i<0.9 & comp_tvs>0.9 ~ "uncomp_dis_comp_nat",
                            comp_i>0.9 & comp_tvs>0.9 ~ "comp_dis_comp_nat", 
                            comp_i>0.9 & comp_tvs<0.9 ~ "comp_dis_uncomp_nat")) %>% 
    feols(forest.diff ~ type + forest.l + PCGDP.l + Pop.growth.l | FID + yr, cluster = ~ctr+yr)

reg_0.5 <- final_up %>% 
    filter(election_DPI==1,target==1) %>%
    mutate(type = case_when(comp_i<0.9 & comp_tvs<0.9 ~ "a_uncomp",
                            comp_i<0.9 & comp_tvs>0.9 ~ "uncomp_dis_comp_nat",
                            comp_i>0.9 & comp_tvs>0.9 ~ "comp_dis_comp_nat", 
                            comp_i>0.9 & comp_tvs<0.9 ~ "comp_dis_uncomp_nat")) %>% 
    feols(forest.diff ~ type + forest.l + PCGDP.l + Pop.growth.l | FID + yr, cluster = ~ctr+yr)

reg_0.6 <- final_up %>% 
    filter(target==0) %>%
    mutate(type = case_when(election_DPI==0 ~ "a_no_elec",
                            comp_i<0.9 & comp_tvs>0.9 ~ "uncomp_dis_comp_nat",
                            comp_i>0.9 & comp_tvs>0.9 ~ "comp_dis_comp_nat", 
                            comp_i>0.9 & comp_tvs<0.9 ~ "comp_dis_uncomp_nat")) %>% 
    feols(forest.diff ~ type + forest.l + PCGDP.l + Pop.growth.l | FID + yr, cluster = ~ctr+yr)

reg_0.7 <- final_up %>% 
    filter(election_DPI==1,target==0) %>%
    mutate(type = case_when(comp_i<0.9 & comp_tvs<0.9 ~ "a_uncomp",
                            comp_i<0.9 & comp_tvs>0.9 ~ "uncomp_dis_comp_nat",
                            comp_i>0.9 & comp_tvs>0.9 ~ "comp_dis_comp_nat", 
                            comp_i>0.9 & comp_tvs<0.9 ~ "comp_dis_uncomp_nat")) %>% 
    feols(forest.diff ~ type + forest.l + PCGDP.l + Pop.growth.l | FID + yr, cluster = ~ctr+yr)


#Alternative Competitiveness Measures:


