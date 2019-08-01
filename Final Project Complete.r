library(fivethirtyeight)
library(tidyverse)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(ggplot2)


#load data
data("police_killings")
data("df_pop_county")
gdp_data <- read.csv("~/Desktop/Math 2820/GCP_Release_1.csv")
race_data <- read.csv("~/Desktop/Math 2820/cc-est2017-alldata.csv")


#examine data
head(police_killings)
dim(police_killings)


#showing killings on map
kills_df <- data.frame(region = police_killings$county_id, value = 1)
county_df <- data.frame(region = df_pop_county$region, value = 0)
complete_kills_df <- rbind(kills_df, county_df)
complete_kills_df <- distinct(complete_kills_df, region, .keep_all = TRUE)
county_choropleth(complete_kills_df,
                  title  = "Locations of killings by US police in 2015", num_colors = 1)

#visualizing data
ggplot(police_killings, aes(x=share_white)) + geom_bar(binwidth = 10) + labs(x = "% White", y = "Frequency") + ggtitle("Frequency of Killing vs. % White")

ggplot(police_killings, aes(x=share_hispanic)) + geom_bar(binwidth = 10) + labs(x = "% Hispanic", y = "Frequency") + ggtitle("Frequency of Killing vs. % Hipsanic")

ggplot(police_killings, aes(x=share_black)) + geom_bar(binwidth = 10) + labs(x = "% Black", y = "Frequency") + ggtitle("Frequency of Killing vs. % Black")

#adding a kill variable
police_killings <- police_killings %>%
  mutate(kill = 1)

#filtering to the year we want
race_data <- race_data %>%
  filter(YEAR == 5, AGEGRP == 3)

#In the BEA data, populations are given as raw numbers so we had to calculate proportions on our own
race_data <- race_data %>%
  mutate(kill = 0, county_fp = STATE * 1000 + COUNTY, share_white = ((WA_MALE + WA_FEMALE) / TOT_POP) * 100, share_black = ((BA_MALE + BA_FEMALE) / TOT_POP) * 100, share_hispanic = ((H_MALE + H_FEMALE) / TOT_POP) * 100)

#created two data frames with the data we want
police_df <- data.frame(county_fp = police_killings$county_id, share_white = police_killings$share_white, share_black = police_killings$share_black, share_hispanic = police_killings$share_hispanic, kill = police_killings$kill)
race_df <- data.frame(county_fp = race_data$county_fp, share_white = race_data$share_white, share_black = race_data$share_black, share_hispanic = race_data$share_hispanic, kill = race_data$kill)

#appends both data frames together on top of each other
all_race_data <- rbind(police_df, race_df)

#removes duplicate counties
all_race_data <- distinct(all_race_data, county_fp, .keep_all = TRUE)


#race models
white_model <- lm(kill ~ share_white, all_race_data)
summary(white_model)

black_model <- lm(kill ~ share_black, all_race_data)
summary(black_model)

hispanic_model <- lm(kill ~ share_hispanic, all_race_data)
summary(hispanic_model)

confint(white_model)
confint(black_model)
confint(hispanic_model)

#mutating data sets to model economic data
gdp_data <- gdp_data %>%
  filter(LineData==1)
police_econ_df <- data.frame(county_fp = police_killings$county_id, kill = 1)
gdp_econ_df <- data.frame(county_fp = gdp_data$FIPS, kill = 0, gdp = gdp_data$gdp)
all_gdp_data <- full_join(police_econ_df, gdp_econ_df, by = "county_fp")
head(all_gdp_data)
all_gdp_data <- all_gdp_data %>%
  mutate(kill = (kill.x + kill.y))

#plot of gdp vs kills
ggplot(all_gdp_data, aes(x=gdp, y = kill)) + geom_point() + labs(x = "County GDP", y = "Kill") + ggtitle("GDP of Counties with Killings")

#economic models
gdp_model <- lm(kill ~ gdp, all_gdp_data)
summary(gdp_model)
confint(gdp_model)

#combining variables
all_data <- full_join(all_gdp_data, all_race_data, by = "county_fp")
head(all_data)
all_data <- all_data %>%
  mutate(kill = (kill.x + kill.y))


#combined model
combined_model <- lm(kill + share_white ~ gdp, all_data)
summary(combined_model)

#showing at risk counties on map
at_risk <- all_data %>%
  filter(share_white > 70, share_white < 90, gdp < 150000000)
atrisk_df <- data.frame(region = at_risk$county_fp, value = 1)
counties_df <- data.frame(region = df_pop_county$region, value = 0)
complete_risk_df <- rbind(atrisk_df, county_df)
complete_risk_df <- distinct(complete_risk_df, region, .keep_all = TRUE)
county_choropleth(complete_risk_df,
                  title  = "Locations of at risk counties for killings", num_colors = 1)
