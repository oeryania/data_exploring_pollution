library(ggplot2)
library(dplyr)

# read data
data_nei <- readRDS("summarySCC_PM25.rds")
data_scc <- readRDS("Source_Classification_Code.rds")
data_cities <- data.frame(fips = c("24510", "06037"), city = c("Baltimore", "Los Angeles"))
data_all <- left_join(data_nei, data_scc, ) %>% left_join(data_cities)
data_all$year = as.factor(data_all$year)

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