source("../code/support.R")

load("../data/clea/clea_lc_20201216.rdata")

polity <- read_sav("../data/polity5/p5v2018.sav") %>% mutate(country = 
    countryname(country, destination="un")) %>% rename(ctr=country, yr=year)

dpi <- read.csv("../data/DPI2020/DPI2020.csv") %>% mutate(countryname = 
                    countryname(countryname, destination = "un")) %>% select(countryname, ifs, year,
                            execrurl, execreg, reelect, gov1me,gov1seat, gov1vote, gov1rurl, 
                                gov1reg, gov2me, gov2seat,gov2vote, gov2rurl, gov2reg, gov3me, 
                                    gov3seat, gov3vote, gov3rurl,gov3reg,liec,eiec,polariz) %>% 
                                        rename(yr=year, ctr=countryname)

golder <- read.csv("../data/golder/es_data-v4_0.csv") %>% mutate(date = 
                    format(as.Date(date, format="%B %d, %Y"),"%Y"), country = 
                                    countryname(country, destination="un")) %>%
                                                    rename(ctr=country, yr=date)

#source("../code/wdi_reproduce.R")
load("../data/WDI/wdi.RData")
wdi <- wdi %>% mutate(ctr = countrycode(iso2c, origin="iso2c",destination="un", warn=
                            FALSE)) %>% rename(yr=year,ctr_n=country) %>% select(-c(iso2c))

clea_final <-
    reduce(list(clea_lc_20201216, polity, golder, dpi, wdi), function(x, y)
        merge(x,
              y, by = c("ctr", "yr"), all.x = TRUE)) %>%
    select(-c(ccode.y, ctr_n.y, pty)) %>%
    rename(ctr_n =
               ctr_n.x, ccode = ccode.x) %>%
    group_by(ctr, cst, yr) %>%
    mutate(
        pvs1 = replace(pvs1,pvs1<0,NA),
        # pvs1 = replace(pvs1,pvs1>1,1),
        comp_i = closeness_i(pvs1),
        comp_ii = closeness_ii(pvs1)
    ) %>%
    distinct(id, cst, .keep_all = TRUE)

rm(dpi,golder,polity,wdi,clea_lc_20201216)

#Aggregate all CLEA GeoReference Data into one file:
#source("../code/geodata_global.R")
load("../data/GRED_20190215/GRED_GLOBAL.RData")
glob_geo <- glob_geo %>% select(-c(yr,ctr_n,cst_n))

data_penult <- left_join(clea_final,glob_geo, by=c("ctr","cst")) %>% select(-c(pty_n,can,pev1,
                vot1,vv1,ivv1,to1,cv1,cvs1,pv1,pvs1,pev2,vot2,vv2,ivv2,to2,cv2,cvs2,pv2,pvs2,seat,year))

data_penult  <- st_as_sf(data_penult)
rm(glob_geo,clea_final)

#DEFORESTATION DATA
load("../data/Deforestation/full_upsampled_10.rdata")
full <- st_as_sf(full, coords = c("x","y"), crs = 4326)
full <- full %>% mutate(x=st_coordinates(full)[,1],
            y=st_coordinates(full)[,2])

sf_use_s2(FALSE) #Disable spherical geometry (s2) engine
final <- st_join(data_penult,full)
final <- final %>% filter(yr==year) %>% select(-c(Countryeng,year,un))
