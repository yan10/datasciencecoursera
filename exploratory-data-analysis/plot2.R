## Load and process data
library(dplyr)
library(lubridate)
library(ggplot2)
library(Rmisc)
rawdata <- read.table("household_power_consumption.txt", sep = ";", header = TRUE, stringsAsFactors = FALSE)
#rawdata <- read.table("household_power_consumption.txt", sep = ";", header = TRUE, colClasses = c(rep('factor',2),rep('character',7)))
selectdata <- filter(rawdata, Date == "1/2/2007" | Date == "2/2/2007")
data <- mutate(selectdata, datetime = dmy_hms(paste(Date,Time, sep = " ")))
data <- mutate(data, Global_active_power = as.numeric(Global_active_power),
               Global_reactive_apower = as.numeric(Global_reactive_power),
               Voltage = as.numeric(Voltage),
               Global_intensity = as.numeric(Global_intensity),
               Sub_metering_1 = as.numeric(Sub_metering_1),
               Sub_metering_2 = as.numeric(Sub_metering_2),
               Sub_metering_3 = as.numeric(Sub_metering_3)
)


## Plot 2
g2 <- ggplot(data, aes(datetime, Global_active_power))
p2 <- g2 + geom_path()
print(p2)
ggsave(filename = "plot2.png",width = 5, height = 5)

