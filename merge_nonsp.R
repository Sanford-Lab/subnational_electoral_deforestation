library(tidyverse)
library(countrycode)
library(sf)

load("../data/processed/df_out.RData")

# LOAD IN DATAFRAMES ----
df_out <- df_out %>% filter(!((!is.na(ctr)) & ctr!=un)) %>% #remove cases where datasets disagree about country (n=12)
    mutate(ctr=un,yr=year,ctr_n=Countryeng) %>%
    filter(!is.na(forest.diff)) %>%
    mutate(election_DPI=ifelse(!is.na(id),1,election_DPI)) %>%
    relocate(c(release,id,ctr_n,ctr,yr,cst),.before=area) %>%
    select(-c(x,y,Countryeng,GID_0,GID_1,GID_2,un,year,polity2,
              Polity_class,close80,close90,votediff,maj,margin,margin.norm)) %>%
    arrange(yr,id)

polity <- read_sav("../data/polity5/p5v2018.sav") %>%
    select(-c(regtrans,interim,durable,xropen,xrreg,xrcomp,prior,emonth,eday,eyear,
              eprec,bmonth,bday,byear,bprec,ccode,scode,post,change,d5,sf,p5)) %>%
    mutate(country = countryname(country, destination="un",warn=FALSE)) %>% 
    rename(ctr=country, yr=year) %>%
    filter(!is.na(ctr)) %>%
    distinct(ctr,yr, .keep_all = TRUE)

dpi <- read.csv("../data/DPI2020/DPI2020.csv") %>% 
    mutate(countryname = countryname(countryname, destination = "un",warn=FALSE)) %>% 
    select(countryname,year,execrurl,execreg,reelect,gov1vote,gov1rurl, 
        gov1reg,liec,eiec,legelec,allhouse,
        maj,housesys,mdmh) %>% 
    rename(yr=year, ctr=countryname) %>%
    filter(!is.na(ctr)) %>%
    distinct(ctr,yr, .keep_all = TRUE)

golder <- read.csv("../data/golder/es_data-v4_0.csv") %>% 
    mutate(date = format(as.Date(date, format="%B %d, %Y"),"%Y"), country = 
        countryname(country, destination="un",warn=FALSE),date=as.numeric(date)) %>%
    rename(ctr=country, yr=date) %>%
    filter(!is.na(ctr)) %>%
    select(ctr,yr,legislative_type,elecrule) %>%
    distinct(ctr,yr,.keep_all = TRUE)

#source("../code/wdi_reproduce.R")
load("../data/WDI/wdi.RData") 
wdi <- wdi %>% 
    mutate(ctr=countrycode(iso2c,origin="iso2c",destination="un",warn=FALSE)) %>% 
    rename(yr=year,ctr_n=country) %>% 
    select(-c(iso2c,iso3c,ctr_n)) %>%
    filter(!is.na(ctr))

vdem <- readRDS("../data/VDEM/V-Dem-CY-Core-v12.rds") %>% 
    select(country_name,country_id,year,v2x_polyarchy,v2excrptps,v2xnp_client) %>% 
    mutate(ctr=countrycode(country_id,origin="vdem",destination="un",warn=FALSE),yr=year) %>%
    select(-c(country_id,country_name,year)) %>%
    filter(!is.na(ctr))

# MERGE ----
final <- reduce(list(df_out, polity, golder, dpi, wdi, vdem), function(x, y)
    merge(x, y, by = c("ctr", "yr"), all.x = TRUE))

rm(df_out,dpi,golder,polity,wdi,vdem)

#UNIQUE CST VARIABLE ----
final <- final %>% mutate(cst_uniq = paste0(cst,"-",ctr))

#VARIABLE CLEANUP ----

final <- final %>% 
    mutate(mag = replace(mag,mag<0,NA), #DPI
           execrurl = ifelse(!(execrurl %in% c(0,1)),NA,execrurl),
           execreg = ifelse(!(execreg %in% c(0,1)),NA,execreg),
           reelect = ifelse(!(reelect %in% c(0,1)),NA,reelect),
           gov1vote = ifelse(between(gov1vote,0,100),gov1vote,NA),
           gov1reg = ifelse(between(gov1reg,0,1),gov1reg,NA),
           gov1rurl = ifelse(between(gov1rurl,0,1),gov1rurl,NA),
           liec = ifelse(between(liec,1,7),liec,NA),
           eiec = ifelse(between(eiec,1,7),eiec,NA),
           legelec = ifelse((legelec %in% c(0,1)),legelec,NA),
           allhouse = ifelse((allhouse %in% c(0,1)),allhouse,NA),
           housesys = ifelse((housesys %in% c("Plurality","PR",".5")),housesys,NA),
           housesys=ifelse((housesys == ".5"),"PR",housesys),
           democ = ifelse(democ %in% c(-88,-77,-66),NA,democ), #Polity
           autoc = ifelse(autoc %in% c(-88,-77,-66),NA,autoc),
           polity = ifelse(polity %in% c(-88,-77,-66),NA,polity),
           xconst = ifelse(xconst %in% c(-88,-77,-66),NA,xconst),
           parreg = ifelse(parreg %in% c(-88,-77,-66),NA,parreg),
           parcomp = ifelse(parcomp %in% c(-88,-77,-66),NA,parcomp),
           exrec = ifelse(exrec %in% c(-88,-77,-66),NA,exrec),
           exconst = ifelse(exconst %in% c(-88,-77,-66),NA,exconst),
           polcomp = ifelse(polcomp %in% c(-88,-77,-66),NA,polcomp),
           polity_label = case_when((polity < -5) ~ "Autocracy",
                                    (polity > 5) ~ "Democracy",
                                    (between(polity,-5,5)) ~ "Anocracy")) %>%
    group_by(ctr) %>% 
    fill(c(democ,autoc,polity,xconst,parreg,parcomp,exrec,exconst,polcomp)) %>% 
    mutate(comp_i = comp_i^2) %>% 
    filter(ctr_n!="Chile") %>% 
    left_join(vdem) %>%
    mutate(target = ifelse(v2x_polyarchy>0.5 & v2xnp_client>0.2,1,0)) %>% 
    left_join(clea_nat)

#save(final,file="../data/processed/final.RData")
