# if running Source, run Source with echo
# source("pollution_analysis.R", echo=TRUE)

library(ggplot2)
library(plyr)
library(dplyr)

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
filenames <- c("summarySCC_PM25.rds", "Source_Classification_Code.rds")
cities <- data.frame(fips = c("24510", "06037"), city = c("Baltimore", "Los Angeles"))

# download data, as needed
if (!all(file.exists(filenames))) {
    temp_file <- tempfile(fileext = ".zip")
    download.file(url, temp_file)
    unzip(temp_file, exdir = getwd())
    unlink(temp_file)
}

# populate data, as needed
if (!exists("data_all")){
    data_all <- filenames %>% sapply(readRDS) %>% join_all() %>% left_join(cities)
    data_all$year = as.factor(data_all$year) # factorize year to plot as discrete units
}

title <- "1. National Emissions"; x = "Year"; y = "PM2.5 Tons"
data_all %>%
    summarize(pm25 = sum(Emissions), .by = year) %>% # sum emissions by year
    barplot(pm25 ~ year, data = ., xlab = x, ylab = y, main = title) # plot

title <- "2. Baltimore Emissions"; x = "Year"; y = "PM2.5 Tons"
data_all %>%
    filter(city == "Baltimore") %>% # filter to Baltimore
    summarize(pm25 = sum(Emissions), .by = year) %>% # sum emissions by year
    barplot(pm25 ~ year, data = ., xlab = x, ylab = y, main = title) # plot

title <- "3. Baltimore Emissions by Type"; x = "Year"; y = "PM2.5 Tons"
data_all %>%
    filter(city == "Baltimore") %>% # filter to city
    summarize(pm25 = sum(Emissions), .by = c(year, type)) %>% # sum emissions by year and type
    ggplot(data = ., mapping = aes(x = year, y = pm25)) + 
    facet_grid(~ type, scales = "free_y") +
    geom_col() +
    labs (title = title, x = x, y = y)

title <- "4. Coal Combustion Emissions"; x = "Year"; y = "PM2.5 Tons"
data_all %>%
    filter(grepl("\\bComb\\b.*\\bCoal\\b", EI.Sector, ignore.case = T)) %>% # filter for source
    summarize(pm25 = sum(Emissions), .by = year) %>%
    barplot(pm25 ~ year, data = ., xlab = x, ylab = y, main = title) # plot

title <- "5. Baltimore Vehicle Emissions"; x = "Year"; y = "PM2.5 Tons"
data_all %>%
    filter(city == "Baltimore") %>% # filter to city
    filter(grepl("\\bVehicles\\b", EI.Sector, ignore.case = T)) %>% # filter for source
    summarize(pm25 = sum(Emissions), .by = year) %>%
    barplot(pm25 ~ year, data = ., xlab = x, ylab = y, main = title) # plot

title <- "6. Vehicle Emission Comparison"; x = "Year"; y = "PM2.5 Tons"
data_all %>%
    filter(city %in% c("Baltimore", "Los Angeles") & grepl("\\bVehicles\\b", EI.Sector, ignore.case = T)) %>%
    summarize(pm25 = sum(Emissions), .by = c(city,year)) %>%
    ggplot(data = ., mapping = aes(x = year, y = pm25)) + 
    facet_wrap(~ city, scales = "free_y") +
    geom_col() +
    labs (title = title, x = x, y = y)

# if you like my work, you can connect with me on http://linkedin.com/in/oeryani/ 😊