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

url1="https://www4.stat.ncsu.edu/~online/datasets/EDU01a.csv"
census=read_csv(url1)
str(census)                         #get a sense of the data#
summary(census)                     #continue to get a sense of the data#

census=census |>                   
  select(Area_name, STCOU, ends_with("D")) |>
  rename(area_name=Area_name) |>
  pivot_longer(cols=ends_with("D"),
               names_to="code",    
               values_to="observed")
census


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
  select(area_name, STCOU, code, survey_type, survey_year, observed)

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
  select(area_name, state, district, everything())



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
#xxxxx double check that upon parsing strings, remove old column okay#
#xxxx look at defaults and naming#

function_for_step_1_2=function(url, default_var_name="observed") {
  tmp=read_csv(url) |>
    select(Area_name, STCOU, ends_with("D")) |>
    rename(area_name = Area_name) |>   
    pivot_longer(cols = ends_with("D"),
                 names_to = "code",
                 values_to = default_var_name)
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
    select(area_name, STCOU, code, survey_type, survey_year, observed)
  return(tmp)
}

function_for_step_5=function(mytibble) {
  tmp=mytibble |> 
    mutate(state=str_sub(area_name, nchar(mytibble$area_name) - 1, nchar(mytibble$area_name)),
           district=str_sub(area_name, 1, nchar(mytibble$area_name)-4)) |>
    select(-area_name) |>
    select(area_name, state, district, everything())
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
  
  mytibble$division=division
  
  tmp=mytibble |>
    select(area_name, division, everything())
   return(tmp) 
}

function_for_step_4_5_6=function(mytibble) {
  a=str_locate(mytibble$area_name, ",")[,1]            
  noncounty=mytibble[which(is.na(a)),]                 
  county=mytibble[which(!is.na(a)),]
  
  class(county)=c("county", class(county))           
  class(noncounty)=c("state", class(noncounty))      
  
  county=function_for_step_5(county)
  noncounty=function_for_step_6(noncounty)
  
  return(list(county,noncounty))
}



### 2a. Create Wrapper Function###
function_wrap=function(url, default_var_name="observed") {
  result=function_for_step_1_2(url, default_var_name) |>
    function_for_step_3()|>
    function_for_step_4_5_6()
  
  return(result)
}



###2b. Compare output To Make Sure Works###
test=function_wrap("https://www4.stat.ncsu.edu/~online/datasets/EDU01a.csv")
all.equal(test[[1]], county)
all.equal(test[[2]], noncounty)
rm(test)


###2c. Apply function to both datasources###
url2="https://www4.stat.ncsu.edu/~online/datasets/EDU01b.csv"

tibble1=function_wrap(url1)
tibble2=function_wrap(url2)

function_combine=function(mytib1, mytib2) {
  county_data=rbind(mytib1[[1]],mytib2[[1]])
  non_county_data=rbind(mytib1[[2]], mytib2[[2]])
  return(list(county_data, non_county_data))
}

combined_data=function_combine(tibble1,tibble2)





#####3. Summarizing #####
###3a. Non-county plotting function###
plot_state=function(mytibble, default_var_name="observed") {
  tmp=mytibble |>
    filter(division != "ERROR") |>
    group_by(division, survey_year) |>
    summarize(mean_observed=mean(mean(get(default_var_name))))
  
  ggplot(tmp, aes(x=survey_year, y=mean_observed, color=division)) +
    geom_line() +
    labs(x="Year", 
         y="Mean Enrollemnt Value", 
         color="Division", 
         title = "Mean Enrollment Value by Year and Division") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
}

plot.state(combined_data[[2]])


###3b. County plotting###
#x ask about sorting and outputX#
"plot.county=function(mytibble,
                     default_state='NC', 
                     default_filter='top', 
                     default_count=5,
                     default_var_name='observed') {
  
  area_name=paste0(mytibble$district, ", ", mytibble$state)
  mytibble$area_name=area_name
  
  tmp=mytibble |>
    filter(state=default_state) |>
    group_by(area_name) |>
    arrange(ifelse(default_filter=='top',
                   default_var_name,
                   desc(default_var_name))) |>
    slice(1:default_count)
}"



###3c. Using Our Functions###
#Other data sources#
url3="https://www4.stat.ncsu.edu/~online/datasets/PST01a.csv"
url4="https://www4.stat.ncsu.edu/~online/datasets/PST01b.csv"
url5="https://www4.stat.ncsu.edu/~online/datasets/PST01c.csv"
url6="https://www4.stat.ncsu.edu/~online/datasets/PST01d.csv"

#Data Processing For Each Of Data Sources#
url_list=list(url1, url2,url3,url4,url5,url6)

for (i in 1:6) {
  myvar=paste0("data",i)
  assign(myvar, function_wrap(url_list[[i]]))
}

#Combining Data Sources#
first_urls=function_combine(data1,data2)
second_urls=function_combine(data3,
  function_combine(data4,
    function_combine(data5, data6)))

#Running Functions On Combined Data Sources#
plot.state(first_urls[[2]])

"plot.county(first_urls[[1]],NC, top, 20, observed)
plot.county(first_urls[[1]],SC, bottom, 7, observed)
plot.county(first_urls[[1]])
plot.county(first_urls[[1]], PA, top, 8, observed)"

plot.state(second_urls[[2]])

"plot.county(second_urls[[1]],CA, top, 15, observed)
plot.county(second_urls[[1]],TX, bottom, 4, observed)
plot.county(second_urls[[1]])
plot.county(second_urls[[1]], NY, top, 10, observed)"

