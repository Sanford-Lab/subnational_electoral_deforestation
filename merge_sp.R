#Dependencies ----
library(sf)
library(tidyverse)

#Join Geometries onto Electoral Data
load("~/project/data/processed/clea_final.RData")

#Load in GRED data
load("~/data/processed/gred_glbl.RData")
glob_geo <- glob_geo %>% select(c(ctr,cst,yr,geometry))

clea_final <- clea_final %>% 
    filter(yr>1981) %>% #First satellite data year is 1982
    left_join(glob_geo, by=c("ctr","cst","GRED_yr"="yr")) %>% 
    st_as_sf() %>% 
    select(-GRED_yr)

rm(glob_geo)

load("~/data/full_upsampled_10.Rdata")
full <- st_as_sf(full, coords = c("x","y"), crs = 4326)
full <- full %>% mutate(x=st_coordinates(full)[,1],y=st_coordinates(full)[,2])

clea_split_yr <- split(clea_final,clea_final$yr)
full_split_yr <- split(full,full$year)

df_out <- map2_df(clea_split_yr, full_split_yr, ~ .x %>% 
                      st_join(.y,join=st_within,left=TRUE))

save(df_out,file="../data/processed/df_out.RData")
