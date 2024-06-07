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

census1=read_csv("EDU01a.csv")       #repo, so same file path#
str(census1)                         #get a sense of the data#
summary(census1)                     #continue to get a sense of the data#

census1=census1 |>                   
  select(Area_name, STCOU, ends_with("D")) |>
  rename(area_name=Area_name) |>
  pivot_longer(cols=ends_with("D"),
               names_to="code",    
               values_to="observed")

