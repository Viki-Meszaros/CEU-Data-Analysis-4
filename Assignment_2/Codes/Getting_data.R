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


# Get all the data - 2018 is the latest available data for life expectancy
data_raw <- WDI(indicator=c('NY.GDP.PCAP.PP.KD','SP.POP.TOTL', 'EN.ATM.CO2E.PC'), 
                country="all", start=1992, end=2018)

# Save the raw data file
my_path <- "c://Users/MViki/Documents/CEU/Winter_semester/DA_4/Assignment_2/Data/"
write.csv(data_raw, paste0(my_path,'raw/CO2_GDP_raw.csv'), )



# DATA CLENING ------------------------------------------------------------

# Clear memory
rm(list=ls())

my_url <- "https://raw.githubusercontent.com/Viki-Meszaros/CEU-Data-analysis-4/main/Assignment_2/Data/raw/CO2_GDP_raw.csv"
df <- read_csv( my_url )



















