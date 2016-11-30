# Read in csv
test <- read.csv("rawWifi2016/201601.csv", header=FALSE)

# Reformatted rptHeader to be consistent
headers <- read.table("rawWifi2015/wip/rptHeader.txt", sep=",", header=TRUE)
colnames(test) <- colnames(headers)

# April 2016 data is different, need to reform either 2015 or 4/2016 onward
# Only get daily csv
wifi_2015 <- list.files("rawWifi2015", pattern="[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9].csv", full=TRUE)
wifi_2016 <- list.files("rawWifi2016", pattern="[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9].csv", full=TRUE)

# Put wifi info in rawWifi2015, rawWifi2016 in lists
wifi_2015.list <- lapply(wifi_2015, read.csv, header=FALSE)
wifi_2016.list <- lapply(wifi_2016, read.csv, header=FALSE)

# Bind all csv information for 2015 year into one data set
wifi_2015_data <- do.call('rbind', wifi_2015.list)
colnames(wifi_2015_data) <- colnames(headers)

wifi_2016_data <- do.call('rbind', wifi_2016.list)
colnames(wifi_2016_data) <- colnames(headers)

# Frequency of different campuses (table)
# Frequency of different location by campuses (tapply) summarize, function
lower_campus <- tolower(wifi_2015_data$Campus)
lower_campus <- gsub("ptz|pitzer.edu", "pit", lower_campus)
lower_campus <- gsub("cmc.*|claremont.*|.*cmc", "cmc", lower_campus)
lower_campus <- gsub("cuc.cmc", "cmc", lower_campus)
lower_campus <- gsub("scr.*", "scr", lower_campus)
lower_campus <- gsub("cgu.*", "cgu", lower_campus)
lower_campus <- gsub("hmc.*", "hmc", lower_campus)
lower_campus <- gsub("pom.*", "pom", lower_campus)
lower_campus <- gsub("kgi.*", "kgi", lower_campus)
campus_2015 <- table(lower_campus)
wifi_2015_data$Campus <- lower_campus

# Make date columns to be actual dates
# strptime: R built in date formatting

# Format data
wifi_2015_data <- wifi_2015_data[-which(wifi_2015_data$Device.Location == "-"), ]
wifi_2015_data <- wifi_2015_data[-which(wifi_2015_data$Campus == "-"), ]
wifi_2015_data <- wifi_2015_data[-which(wifi_2015_data$Campus == "123"), ]
wifi_2015_data <- wifi_2015_data[-which(wifi_2015_data$Duration == "0 mins"), ]
wifi_2015_data <- wifi_2015_data[-which(wifi_2015_data$Campus == "yale.edu"), ]
wifi_2015_data <- wifi_2015_data[-which(wifi_2015_data$Campus == "my.csun.edu"), ]
wifi_2015_data <- wifi_2015_data[-which(wifi_2015_data$Campus == "tulane.edu"), ]
wifi_2015_data <- wifi_2015_data[-which(wifi_2015_data$Campus == "smith.edu"), ]
wifi_2015_data <- wifi_2015_data[-which(wifi_2015_data$Campus == "swarthmore.edu"), ]
wifi_2015_data <- wifi_2015_data[-which(wifi_2015_data$Campus == "brynmawr.edu"), ]
wifi_2015_data <- wifi_2015_data[-which(wifi_2015_data$Campus == "cornell.edu"), ]
wifi_2015_data <- wifi_2015_data[-which(wifi_2015_data$Campus == "csusb.edu"), ]
wifi_2015_data <- wifi_2015_data[-which(wifi_2015_data$Campus == "gwmail.gwu.edu"), ]
wifi_2015_data <- wifi_2015_data[-which(wifi_2015_data$Campus == "haverford.edu"), ]
wifi_2015_data <- wifi_2015_data[-which(wifi_2015_data$Campus == "lsu.edu"), ]
wifi_2015_data <- wifi_2015_data[-which(wifi_2015_data$Campus == "jsd"), ]


plot(Duration ~ Campus, data=wifi_2015_data)

plot(Duration ~ Device.Location, data=wifi_2015_data)


# Make model Duration ~ Campus + Location
lmod <- lm(as.numeric(Duration) ~ Campus*Device.Location, data=wifi_2015_data)
summary(lmod)

# Plot histogram of schools
library(ggplot2)
ggplot(wifi_2015_data,aes(x=Campus)) + geom_bar()

# Histogram of location
ggplot(wifi_2015_data,aes(x=Device.Location)) + geom_bar()

# Make duration a numeric variable
wifi_2015_data$Duration <- as.numeric(wifi_2015_data$Duration)

# Plot average duration against school
ggplot(wifi_2015_data, aes(x=wifi_2015_data$Campus, y=wifi_2015_data$Duration)) + stat_summary(fun.y="mean", geom="bar")

# Plot median duration against school
ggplot(wifi_2015_data, aes(x=wifi_2015_data$Campus, y=wifi_2015_data$Duration)) + stat_summary(fun.y="median", geom="bar")

# Plot average duration against location
ggplot(wifi_2015_data, aes(x=wifi_2015_data$Device.Location, y=wifi_2015_data$Duration)) + stat_summary(fun.y="mean", geom="bar")

# Plot median duration against location
ggplot(wifi_2015_data, aes(x=wifi_2015_data$Device.Location, y=wifi_2015_data$Duration)) + stat_summary(fun.y="median", geom="bar")

mod <- lm(Duration ~ Device.Location + Campus, data = wifi_2015_data)
summary(mod)
# IDEA: People and places and time; location data and duration (aov), 
# campus and location data, campus and duration (interaction??)

library(plotly)

plot_ly(type="scatter3d",x=wifi_2015_data$Campus,y=wifi_2015_data$Device.Location,z=wifi_2015_data$ConnectStart,mode="markers")

ggplot(wifi_2015_data, aes(x=wifi_2015_data$ConnectStart, y=wifi_2015_data$Duration))


