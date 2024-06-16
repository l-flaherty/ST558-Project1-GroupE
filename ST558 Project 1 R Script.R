###########Written By Liam Flaherty And Jia Lu for ST558 Project 1##########
#####1. Load In Data And Required Packages#####
###1a. Load Required Packages###
install.packages("tidyverse")
install.packages("DBI")  
install.packages("RSQLite")
install.packages("Lahman")
install.packages("readr")
install.packages("readxl")
library(tidyverse)
library(DBI)
library(RSQLite)
library(Lahman)
library(readr)
library(readxl)



###1b. Load Data###
#Census data from 2010#
#First three characters are survey type#
#Next four characters are type of value from survey#
#Next two are year#
#e.g. AGE010180D is Resident population (complete count) 1980#
#e.g. EDU640180D is Years of school completed in 1980#
#e.g. EDU010180D is Public school enrollment Fall 1979-1980#
#e.g. EDU010189D is Public school enrollment Fall 1988-1989#

census=read_csv("EDU01a.csv")       #repo, so same file path#
str(census)                         #get a sense of the data#
summary(census)                     #continue to get a sense of the data#

census=census |>                   
  select(Area_name, STCOU, ends_with("D")) |>
  rename(area_name=Area_name) |>
  pivot_longer(cols=ends_with("D"),
               names_to="code",    
               values_to="observed")



###1c. parse strings###
#note, `=SORT(UNIQUE(RIGHT(range, 4))) used in Excel doc...#
#...to ensure that no data from before 1910#` 

survey_type=str_sub(census$code, start=1, end=7)

yy=as.numeric(str_sub(census$code, start=8, end=9))
range(yy)                                                    #all in 1900s here...#
survey_year=as.numeric(ifelse(yy<=10,                        #... but want to make robust#
                              paste0("200", yy),
                              paste0("19", yy)))

census=census |>
  mutate(survey_type=survey_type, survey_year=survey_year) |>
  select(area_name, STCOU, survey_type, survey_year, observed)

rm(yy, survey_type, survey_year)                             #no need to keep#



###1d. County vs. Non-county ###
unique(census$area_name)                           #investigate data patterns#

a=str_locate(census$area_name, ",")[,1]            #no commas in non-county#
noncounty=census[which(is.na(a)),]                 #keep non-county data together#
county=census[which(!is.na(a)),]                   #keep county data together#

unique(noncounty$area_name)                        #double check things work as expected#
unique(county$area_name)

class(county)=c("county", class(county))           #not really sure why necessary#
class(noncounty)=c("state", class(noncounty))      #not really sure why necessary#

county=county |>
  mutate(state=str_sub(area_name, nchar(county$area_name) - 1, nchar(county$area_name)),
         district=str_sub(area_name, 1, nchar(county$area_name)-4)) |>
  select(-area_name) |>
  select(state, district, everything())



###1e. Get division###
#From https://en.wikipedia.org/wiki/List_of_regions_of_the_United_States?useskin=vector#

d1=c("CONNECTICUT", "MAINE", "MASSACHUSETTS", "NEW HAMPSHIRE", "RHODE ISLAND", "VERMONT")
d2=c("NEW JERSEY", "NEW YORK", "PENNSYLVANIA")
d3=c("ILLINOIS", "INDIANA", "MICHIGAN", "OHIO", "WISCONSIN")
d4=c("IOWA", "KANSAS", "MINNESOTA", "MISSOURI", "NEBRASKA", "NORTH DAKOTA", "SOUTH DAKOTA")
d5=c("DELAWARE", "FLORIDA", "GEORGIA", "MARYLAND", "NORTH CAROLINA", "SOUTH CAROLINA", "VIRGINIA", "DISTRICT OF COLUMBIA", "District of Columbia", "WEST VIRGINIA")
d6=c("ALABAMA", "KENTUCKY", "MISSISSIPPI", "TENNESSEE")
d7=c("ARKANSAS", "LOUISIANA", "OKLAHOMA", "TEXAS")
d8=c("ARIZONA", "COLORADO", "IDAHO", "MONTANA", "NEVADA", "NEW MEXICO", "UTAH", "WYOMING")
d9=c("ALASKA", "CALIFORNIA", "HAWAII", "OREGON", "WASHINGTON")

region=list(d1,d2,d3,d4,d5,d6,d7,d8,d9)
division=vector()

for (i in 1:nrow(noncounty)) {
  j=1
  
  while(j<=length(region) && !(noncounty$area_name[i] %in% region[[j]])) {
    j=j+1
  }
  
  division[i]=ifelse(j<=length(region), j, "ERROR")
}

noncounty$division=division
noncounty=noncounty |> select(area_name, division, everything())

rm(d1,d2,d3,d4,d5,d6,d7,d8,d9,region,division,i,j)      #keep environment clean#





#####2. Function Writing#####
#xxxx double check that column name choices are okayxxxx#
#xxxxx double check that upon parsing strings, remove old column#

function_for_step_1_2=function(url, default_val="observed") {
  tmp=read_csv(url) |>
    select(Area_name, STCOU, ends_with("D")) |>
    rename(area_name = Area_name) |>   
    pivot_longer(cols = ends_with("D"),
                 names_to = "code",
                 values_to = default_val)
  return(tmp)
}

function_for_step_3=function(mytibble) {
  survey_type=str_sub(mytibble$code, start=1, end=7)
  yy=as.numeric(str_sub(mytibble$code, start=8, end=9))
  survey_year=as.numeric(ifelse(yy<=10,                        
                                paste0("200", yy),
                                paste0("19", yy)))
  tmp=mytibble |>
    mutate(survey_type=survey_type, survey_year=survey_year) |>
    select(area_name, STCOU, survey_type, survey_year, observed)
  return(tmp)
}

function_for_step_5=function(mytibble) {
  tmp=mytibble |> 
    mutate(state=str_sub(area_name, nchar(mytibble$area_name) - 1, nchar(mytibble$area_name)),
           district=str_sub(area_name, 1, nchar(mytibble$area_name)-4)) |>
    select(-area_name) |>
    select(state, district, everything())
  return(tmp)
}

function_for_step_6=function(mytibble) {
  d1=c("CONNECTICUT", "MAINE", "MASSACHUSETTS", "NEW HAMPSHIRE", "RHODE ISLAND", "VERMONT")
  d2=c("NEW JERSEY", "NEW YORK", "PENNSYLVANIA")
  d3=c("ILLINOIS", "INDIANA", "MICHIGAN", "OHIO", "WISCONSIN")
  d4=c("IOWA", "KANSAS", "MINNESOTA", "MISSOURI", "NEBRASKA", "NORTH DAKOTA", "SOUTH DAKOTA")
  d5=c("DELAWARE", "FLORIDA", "GEORGIA", "MARYLAND", "NORTH CAROLINA", "SOUTH CAROLINA", "VIRGINIA", "DISTRICT OF COLUMBIA", "District of Columbia", "WEST VIRGINIA")
  d6=c("ALABAMA", "KENTUCKY", "MISSISSIPPI", "TENNESSEE")
  d7=c("ARKANSAS", "LOUISIANA", "OKLAHOMA", "TEXAS")
  d8=c("ARIZONA", "COLORADO", "IDAHO", "MONTANA", "NEVADA", "NEW MEXICO", "UTAH", "WYOMING")
  d9=c("ALASKA", "CALIFORNIA", "HAWAII", "OREGON", "WASHINGTON")
  
  region=list(d1,d2,d3,d4,d5,d6,d7,d8,d9)
  division=vector()
  
  for (i in 1:nrow(mytibble)) {
    j=1
    
    while(j<=length(region) && !(mytibble$area_name[i] %in% region[[j]])) {
      j=j+1
    }
    
    division[i]=ifelse(j<=length(region), j, "ERROR")
  }
  
  tmp=mytibble |>
    select(area_name, division, everything())
   return(tmp) 
}

