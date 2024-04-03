#Packages
library(sf)
library(dplyr, warn.conflicts=FALSE)
library(terra)
library(spData)
library(spDataLarge)
library(tidyverse, quietly=TRUE, warn.conflicts=FALSE)
library(haven)
library(countrycode)
library(spdep)

#Functions

#Competitiveness Measure I
closeness_i <- function(vec){
    return((1 - (sort(vec, decreasing = TRUE)[1] - sort(vec, decreasing = TRUE)[2]))*100)
}

#Competitiveness Measure II; Votes - 50
closeness_ii <- function(vec){
    return(1 - max(vec))
}

#Function for calculating the number of times a district has swung from one election to the next
swings <- function(vec){
    return(sum(diff(vec) != 0))
}