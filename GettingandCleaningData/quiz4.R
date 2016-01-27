stringASFactor = FALSE

if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl,destfile="./data/PUMS.csv",method="curl")
PUMSData <- read.csv("./data/PUMS.csv")

strsplit(names(PUMSData), 'wgtp')


if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileUrl,destfile="./data/GDP.csv",method="curl")
GDPData <- read.csv("./data/GDP.csv")

GDPData$X.3 <- as.numeric(gsub(',','',GDPData$X.3))
mean(GDPData$X.3, na.rm = TRUE)
