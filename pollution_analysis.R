library(ggplot2)
library(dplyr)

# read data
nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")
mrg <- left_join(nei, scc)

title <- "1. National Emissions"; x = "Year"; y = "PM2.5 Tons"
mrg %>%
    summarize(pm25 = sum(Emissions), .by = year) %>% # sum emissions by year
    plot(type = "l", xlab = x, ylab = y, main = title) # plot

title <- "2. Baltimore Emissions"; x = "Year"; y = "PM2.5 Tons"
mrg %>%
    filter(fips == "24510") %>% # filter to Baltimore
    summarize(pm25 = sum(Emissions), .by = year) %>% # sum emissions by year
    plot(type = "l", xlab = x, ylab = y, main = title) # plot

title <- "3. Baltimore Emissions by Type"; x = "Year"; y = "PM2.5 Tons"
mrg %>%
    filter(fips == "24510") %>% # filter to city
    summarize(pm25 = sum(Emissions), .by = c(year, type)) %>% # sum emissions by year and type
    qplot(year, pm25, data = ., facets = . ~ type, geom = "line", xlab = x, ylab = y, main = title) # plot

title <- "4. Coal Combustion Emissions"; x = "Year"; y = "PM2.5 Tons"
mrg %>%
    filter(grepl("\\bComb\\b.*\\bCoal\\b", EI.Sector, ignore.case = T)) %>% # filter for source
    summarize(pm25 = sum(Emissions), .by = year) %>%
    plot(type = "l", xlab = x, ylab = y, main = title) # plot

title <- "5. Baltimore Vehicle Emissions"; x = "Year"; y = "PM2.5 Tons"
mrg %>%
    filter(fips == "24510") %>% # filter to Baltimore
    filter(grepl("\\bVehicles\\b", EI.Sector, ignore.case = T)) %>% # filter for source
    summarize(pm25 = sum(Emissions), .by = year) %>%
    plot(type = "l", xlab = x, ylab = y, main = title) # plot

mtitle <- "6. Vehicle Emission Comparison"
par(mfrow = c(1,2), mar = c(5.1,5.1,4.1,2.1), oma = c(0,0,3,0))
d1 <- mrg %>% filter(fips == "24510" & grepl("\\bVehicles\\b", EI.Sector, ignore.case = T)) %>% summarize(pm25 = sum(Emissions), .by = year)
d2 <- mrg %>% filter(fips == "06037" & grepl("\\bVehicles\\b", EI.Sector, ignore.case = T)) %>% summarize(pm25 = sum(Emissions), .by = year)
r1 <- range(d1$pm25); r1len <- r1[2]-r1[1]
r2 <- range(d2$pm25); r2len <- r2[2]-r2[1]
yl <- c(0,max(r1len, r2len))
d1$pm25 <- d1$pm25 - r1[1]
d2$pm25 <- d2$pm25 - r2[1]
min1 <- filter(d1, pm25 == min(pm25))
min2 <- filter(d2, pm25 == min(pm25))

title <- "Baltimore"; x = "Year"; y = "PM2.5 Tons\n(relative to lowest point)"
d1 %>% plot(type = "l", ylim = yl, xlab = x, ylab = y, main = title) # plot
text(min1$year, min1$pm25, round(r1[1]), pos = 3)

title <- "Los Angeles County"; x = "Year"; y = "PM2.5 Tons\n(relative to lowest point)"
d2 %>% plot(type = "l", ylim = yl, xlab = x, ylab = y, main = title) # plot
text(min2$year, min2$pm25, round(r2[1]), pos = 4)

mtext(mtitle, outer = T, cex = 2)
