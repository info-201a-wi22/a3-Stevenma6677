
library(dplyr)


library(ggplot2)


library(tidyverse)


library(maps)
library(readr)

urlfile = "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
incarceration_data <- read.csv(url(urlfile))


new_data <- incarceration_data %>%
  select( year, state, county_name, total_pop_15to64, female_pop_15to64, total_jail_adm_dcrp, female_jail_adm_dcrp, male_jail_adm_dcrp) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

 state_jail_adm_year_summary <- new_data %>%
  
  group_by(year) %>%
  summarize(
  adm_jail_state = sum(total_jail_adm_dcrp, na.rm = FALSE),
  female_jail_adm_state = sum(female_jail_adm_dcrp, na.rm = FALSE),
  male_jail_adm_state = sum(male_jail_adm_dcrp, na.rm = FALSE)
  )   



#plot1
year_trend <- ggplot(state_jail_adm_year_summary, aes(x=year)) +
  geom_line(aes(y = female_jail_adm_state, color = 'female admissions')) +
  geom_line(aes(y = male_jail_adm_state, color = 'male admissions'))
year_trend


# admissions in WA, admissions in CA, admissions in TN, 
admissions_WA <- new_data %>%
  filter(state == "WA") %>%
  summarize(
    female = sum(female_jail_adm_dcrp),
    male = sum(male_jail_adm_dcrp)
  )

admissions_CA <- new_data %>%
  filter(state == "CA") %>%
  summarize(
    female = sum(female_jail_adm_dcrp),
    male = sum(male_jail_adm_dcrp)
  )
  
admissions_TN <- new_data %>%
  filter(state == "TN") %>%
  summarize(
    female_admissions = sum(female_jail_adm_dcrp),
    male_adimissions = sum(male_jail_adm_dcrp)
  )
sex <- c("female_admissions",'female_admissions', 'female_admissions',"male_admissions", "male_admissions", "male_admissions")
state <- c("WA", "CA", "TN","WA", "CA", "TN")
data <- c(651977,2378390,1209111,2972140,10501271,441928)


admissions_com <- data.frame(sex,state,data)

#plot2
compare <- ggplot(data = admissions_com, aes(x=state, y=data, fill=sex)) +
  geom_bar(stat="identity")
compare

#data frame for female admissions in different state for recent year
female_recent <- incarceration_data %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarize(num_female_jail=sum(female_jail_pop, na.rm = FALSE)) 

state_shape <- map_data("state")%>%
  rename(state = region) %>%
  left_join(female_recent, by= "state")

map <- ggplot(state_shape)+
  geom_polygon(
    mapping = aes(x=long, y=lat, group=group, fill=num_female_jail),
    color="white",
    size = .1
  )+
  coord_map()+
  scale_fill_continuous(low="#EEA236", high="red")+
  labs(fill = "number of female_jail in 2018")+
  labs(title="Female admissions by Geography")
map
