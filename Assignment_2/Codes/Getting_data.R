############################
##      Analysis of       ##
##  CO2 emission and GDP  ##
##                        ##
##    Data analysis 4     ##
##                        ##
##     Assignment II.     ##
############################



# GETTING DATA FROM WDI ---------------------------------------------------

# Clear memory
rm(list=ls())

# Call packages
#install.packages('WDI')
library(WDI)


# Search for variables which contains GDP
a <- WDIsearch('gdp.*capita.*constant') # NY.GDP.PCAP.PP.KD
b <- WDIsearch('population, total') # SP.POP.TOTL
c <- WDIsearch("co2") #	EN.ATM.CO2E.PC


# Get all the data 
data_raw <- WDI(indicator=c('NY.GDP.PCAP.PP.KD','SP.POP.TOTL', 'EN.ATM.CO2E.PC', 'EN.ATM.CO2E.KT'), 
                country="all", start=1992, end=2018)

# look if co2 per cap is same if I download or if I download co2 and devide by population
#### we get exactly the same values
data_raw$co2_per_cap <- data_raw$EN.ATM.CO2E.KT/data_raw$SP.POP.TOTL*1000
data_raw$EN.ATM.CO2E.PC == data_raw$co2_per_cap 

# Save the raw data file
my_path <- "c://Users/MViki/Documents/CEU/Winter_semester/DA_4/CEU-Data-analysis-4/Assignment_2/Data/"
write.csv(data_raw, paste0(my_path,'raw/CO2_GDP_raw.csv'), row.names = FALSE)



# DATA CLENING ------------------------------------------------------------

# Clear memory
rm(list=ls())

my_url <- "https://raw.githubusercontent.com/Viki-Meszaros/CEU-Data-analysis-4/main/Assignment_2/Data/raw/CO2_GDP_raw.csv"
df <- read_csv( my_url)


# Filter out grouped observations - most of these have a digit in their name
df <- df %>% filter( !grepl("[[:digit:]]", df$iso2c) )


# drop specific values
drop_id <- c("EU","HK","OE")
# Save the opposite
df <- df %>% filter( !grepl( paste( drop_id , collapse="|"), df$iso2c ) ) 


# Get the first letter from iso2c
fl_iso2c <- substr(df$iso2c, 1, 1)
retain_id <- c("XK","ZA","ZM","ZW")

# Save observations which are the opposite (use of !)
df <- df %>% filter( !( grepl( "X", fl_iso2c ) | grepl( "Z", fl_iso2c ) & 
                          !grepl( paste( retain_id , collapse="|"), df$iso2c ) ) ) 

# Clear non-needed variables
rm( drop_id, fl_iso2c , retain_id )


# We have 27 rows for each country so some data was available for all of them for every year
# We have 215 countries
countries <- df %>% 
  group_by(country) %>% 
  summarise(count = n())

unique(df$country)

# Rename columns
df <- df %>% rename( country = country,
                       pop=SP.POP.TOTL,
                       gdp=NY.GDP.PCAP.PP.KD,
                       co2=EN.ATM.CO2E.PC )



# CREATE BALANCED PANEL ---------------------------------------------------

# There is no data for CO2 emissions in 2017 and 2018, so out panel will be from 1992 till 2016 (25 years)
### Let's delete rows with missing values and create balanced flag
data_panel <- df %>%
  filter(!(is.na(gdp) | is.na(co2))) %>%
  group_by(country) %>%
  mutate(balanced = min(year) == 1992 & max(year) == 2016 & length(unique(year)) == 25) %>%
  ungroup() 

data_balanced <- data_panel %>%
  filter(balanced == TRUE)

# We ended up by 161 countries in the balanced data
unique(data_balanced$country)



# EXPLANATORY DATA ANALYSIS -----------------------------------------------
library(tidyverse)
library(data.table)

df %>% 
  group_by(year) %>% 
  summarise(avg_emm = mean(co2, na.rm = T)) %>% 
ggplot(aes(x = year, y = avg_emm)) +
  geom_line(color = "cyan4") +
  theme_light() +
  labs(title="Average CO2 emmision by year", y="CO2 emission")

df %>% 
  group_by(year) %>% 
  summarise(avg_gdp = mean(gdp, na.rm = T)) %>% 
ggplot(aes(x = year, y = avg_gdp)) +
  geom_line(color = "deeppink4") +
  theme_light() +
  labs(title="GDP per capita by year", y="GDP (USD)") 



# CREATE LOG VARIABLES, LAGS AND LEADS ------------------------------------

data_balanced <- data_balanced %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    lnpop=log(pop),
    lnco2=log(co2),
    lngdp=log(gdp),
    d_lnpop = lnpop - lag(lnpop),
    d_lnco2= lnco2- lag(lnco2),
    d_lngdp= lngdp- lag(lngdp),
    year = factor(year)
  ) %>%
  ungroup()


# ANALYSIS ----------------------------------------------------------------

library(tidyverse)
library(modelsummary)
library(haven)
library(stargazer)
library(car)
library(huxtable)
library(estimatr)
library(lmtest)
library(modelsummary)
library(fixest)

## 1. Cross-section OLS for 2000/1999 

d1 <- data_balanced[data_balanced$year==2000,]

reg1 <- lm(d_lnco2 ~ d_lngdp, data = d1)

summary(reg1)


## 2. Cross-section OLS for a year of your choice

d2 <- data_balanced[data_balanced$year==2015,]

reg2 <- lm(d_lnco2 ~ d_lngdp, data = d2)

summary(reg2)


#### look at all possible years
models <- NULL
years <- 1993:2016

for (i in seq_along(years)) {
  data <- data_balanced[data_balanced$year==years[i],]
  models[[i]] <- lm(d_lnco2 ~ d_lngdp, data = data)

  }

lapply(models, function(x){
  
})


## 3. First difference model, with time trend, no lags

reg3 <- lm_robust(d_lnco2 ~ d_lngdp,
                   data = data_balanced, 
                   se_type = "stata", 
                   clusters = country)

summary(reg3)


## 4. First difference model, with time trend, 2 year lags
lags_2 <- paste(paste0("lag(d_lngdp,", c(0:2), ")"), collapse = " + ")

reg4_formula <- as.formula(paste0("d_lnco2 ~ ", lags_2))

reg4 <- lm_robust(reg4_formula,
                     data = data_balanced, 
                     se_type = "stata", 
                     clusters = country
)


## 5. First difference model, with time trend, 6 year lags

lags_6 <- paste(paste0("lag(d_lngdp,", c(0:6), ")"), collapse = " + ")

reg4_formula <- as.formula(paste0("d_lnco2 ~ ", lags_6))

reg4 <- lm_robust(reg4_formula,
                  data = data_balanced, 
                  se_type = "stata", 
                  clusters = country
)



## 6. Fixed effects model with time and country fixed effects
reg6 <- lm_robust(lnco2 ~ lngdp + year,
                    data = data_balanced, 
                    se_type = "stata", 
                    fixed_effect =  ~ country,
                    clusters = country)

summary(reg6)


## 7. Long difference model 
reg7 <- lm_robust(d_lnco2 ~ lag(d_lngdp, 22),
                  data = data_balanced, 
                  se_type = "stata", 
                  fixed_effect =  ~ country,
                  clusters = country)
summary(reg7)








































