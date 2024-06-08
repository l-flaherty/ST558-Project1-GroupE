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
min(yy)                                                      #all in 1900s here...#
survey_year=as.numeric(ifelse(yy<=10,                        #... but want to make robust#
                              paste0("20", yy),
                              paste0("19", yy)))
rm(yy)                                                       #no need to keep#





###1d. County vs. Non-county ###
unique(census$area_name)                           #investigate data patterns#

a=str_locate(census$area_name, ",")[,1]            #no commas in non-county#
noncounty=census[which(is.na(a)),]                 #keep non-county data together#
county=census[which(!is.na(a)),]                   #keep county data together#

unique(noncounty$area_name)                        #double check things work as expected#
unique(county$area_name)

class(county)=c("county", class(county))           #not really sure why necessary#







#####2. Function Writing#####

data_reshape=function(file) {
  a=read_csv(file)
  b=a |>
    select(Area_name, STCOU, ends_with("D")) |>
    rename(area_name=Area_name) |>
    pivot_longer(cols=ends_with("D"),
                 names_to="code",    
                 values_to="observed")
  return(b)
}
