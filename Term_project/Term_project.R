############################
##      Analysis of       ##
##  Life expectancy and   ##
##     GDP per capita     ##           
##                        ##
##    Data analysis 4     ##
##                        ##
##     Term project       ##
############################



# GETTING DATA FROM WDI ---------------------------------------------------

# Clear memory
rm(list=ls())

# Call packages
#install.packages('WDI')
library(WDI)


# Search for variables which contains GDP, population and life expectancy
a <- WDIsearch('gdp.*capita.*constant') # NY.GDP.PCAP.PP.KD
b <- WDIsearch('population, total') # SP.POP.TOTL
c <- WDIsearch('life expectancy at birth') # SP.DYN.LE00.IN
d <- WDIsearch('health expenditure') # 	SH.XPD.CHEX.PP.CD


# Get all the data 
data_raw <- WDI(indicator=c('NY.GDP.PCAP.PP.KD','SP.POP.TOTL', 'SP.DYN.LE00.IN', 'SH.XPD.CHEX.PP.CD'), 
                country="all", start=2000, end=2018)


# Save the raw data file
my_path <- "c://Users/MViki/Documents/CEU/Winter_semester/DA_4/Term_project/Data/"
write.csv(data_raw, paste0(my_path,'raw/CO2_GDP_raw.csv'), row.names = FALSE)



# DATA CLENING ------------------------------------------------------------
library(tidyverse)

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
                     life_exp=SP.DYN.LE00.IN,
                     health_exp=SH.XPD.CHEX.PP.CD)

war <- data.table(read.csv(paste0(my_path, "raw/ucdp-prio-acd-201.csv")))

war <- war[, .N, by = list(location,year)]

df <- left_join(df, war, by = c("country" = "location", "year" = "year"))
df$N[is.na(df$N)] = 0

df %>% 
  group_by(N) %>% 
  summarise(cnt = n())

df <- df %>% rename(num_wars = N)

# CREATE BALANCED PANEL ---------------------------------------------------

# There is no data for CO2 emissions in 2017 and 2018, so out panel will be from 1992 till 2016 (25 years)
### Let's delete rows with missing values and create balanced flag
data_panel <- df %>%
  filter(!(is.na(gdp) | is.na(life_exp))) %>%
  group_by(country) %>%
  mutate(balanced = min(year) == 2000 & max(year) == 2018& length(unique(year)) == 19) %>%
  ungroup() 

data_balanced <- data_panel %>%
  filter(balanced == TRUE)

# We ended up by 176 countries in the balanced data
unique(data_balanced$country)




# EXPLANATORY DATA ANALYSIS -----------------------------------------------
library(data.table)

df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram(fill= "cyan4")+
  theme_light()


df %>% 
  group_by(year) %>% 
  summarise(avg_emm = mean(life_exp, na.rm = T)) %>% 
  ggplot(aes(x = year, y = avg_emm)) +
  geom_line(color = "cyan4") +
  theme_light() +
  labs(title="Average life expectency by year", y="Life expectency")

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
    lngdp=log(gdp),
    lnhealth_exp= log(health_exp),
    year = factor(year),
    c = factor(country)
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

d1 <- data_balanced[data_balanced$year==2007,]

reg1 <- lm(life_exp ~ lngdp, data = d1)

summary(reg1)



#### look at all possible years
models <- NULL
years <- 2001:2018
i <- 1

for (i in seq_along(years)) {
  data <- data_balanced[data_balanced$year==years[i],]
  models[[i]] <- lm(life_exp ~ lngdp, data = data)
  
}



## 2. First difference model, with time trend, no lags

reg2 <- lm_robust(life_exp ~ lngdp,
                  data = data_balanced, 
                  se_type = "stata", 
                  clusters = country)

summary(reg2)



## 3. Fixed effects model with time fixed effects
reg3 <- lm_robust(life_exp ~ lngdp + year,
                  data = data_balanced, 
                  se_type = "stata", 
                  fixed_effect =  ~ country,
                  clusters = country)

summary(reg3)


## 4. Fixed effects model with time and country fixed effects 
reg4 <- lm_robust(life_exp ~ lngdp + year + c,
                  data = data_balanced, 
                  se_type = "stata", 
                  fixed_effect =  ~ country,
                  clusters = country)
summary(reg4)



## 5. Fixed effects model with time and country fixed effects 
reg5 <- lm_robust(life_exp ~ lngdp + year + lnpop,
                  data = data_balanced, 
                  se_type = "stata", 
                  fixed_effect =  ~ country,
                  clusters = country)
summary(reg5)


## 6. Fixed effects model with time and country fixed effects 
reg6 <- lm_robust(life_exp ~ lngdp + year + lnpop + lnhealth_exp,
                  data = data_balanced, 
                  se_type = "stata", 
                  fixed_effect =  ~ country,
                  clusters = country)
summary(reg6)


## 7. Fixed effects model with time and country fixed effects 
reg7 <- lm_robust(life_exp ~ lngdp + year + lnpop + lnhealth_exp + num_wars,
                  data = data_balanced, 
                  se_type = "stata", 
                  fixed_effect =  ~ country,
                  clusters = country)
summary(reg7)



summary <- huxreg( "Simple FE" = reg2, "FE / time trend" = reg3, "FE / population confounder" = reg5, "FE / pop & healt exp" = reg6, "FE / pop, health exp & war" = reg7,
       statistics = c(N = "nobs", R2 = "r.squared"),
       coefs = c("ln (GDP per capita)" = "lngdp",
                 "ln(population)" = "lnpop",
                 "ln(health expenditure)" = "lnhealth_exp",
                 "Number of wars" = "num_wars",
                 "Constant"= "(Intercept)" ))

summary %>% 
  insert_row(c("Year dummies", "No", "Yes", "Yes", "Yes", "Yes"), after = 11) 















