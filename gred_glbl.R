#AGGREGATING GEOREFERENCE DATA REPRODUCE
source("../code/support.R")

add_parent_dir <- function(str){return(paste0("../data/GRED/",str))}

sfcoerce <- function(df){return(df %>% mutate(ctr_n=as.character(ctr_n),ctr=as.numeric(
    ctr),yr=as.numeric(yr),cst_n=as.character(cst_n),cst=as.numeric(cst)))}

GREDs <- list.files("../data/GRED", pattern="\\d$",recursive=TRUE, include.dirs = TRUE)
GREDs <- unlist(lapply(GREDs, add_parent_dir))
geos <- lapply(GREDs, read_sf)

#Fix errors ----
geos[[57]] <- geos[[57]] %>% rename(ctr_n=CTR_N,ctr=CTR,yr=YR,cst_n=CST_N,cst=CST)
geos <- lapply(geos, sfcoerce)

for(i in 1:length(geos)){
    geos[[i]] <- geos[[i]] %>% st_transform(4326)
    }

#Combine all ----
glob_geo <- reduce(geos, function(x,y) bind_rows(x,y))
rm(GREDs, geos, add_parent_dir)

glob_geo <- as.data.frame(glob_geo)
#save(glob_geo,file="../file/path/")

#For merging: isolate GRED year data for single-GRED-yr countries (all but US, Sweden, Brazil) ----
gred_yr <- glob_geo %>% distinct(ctr,yr) %>% filter(!(ctr %in% c(840,752,76))) %>% rename(GRED_yr=yr)
#save(gred_yr,file="../data/processed/gred_yr.RData")