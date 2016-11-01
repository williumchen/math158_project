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
single.data.frame <- do.call('rbind', wifi_2015.list)
colnames(single.data.frame) <- colnames(headers)

# Frequency of different campuses (table)
# Frequency of different location by campuses (tapply) summarize, function
lower_campus <- tolower(single.data.frame$Campus)
lower_campus <- gsub("ptz|pitzer.edu", "pit", lower_campus)
lower_campus <- gsub("cmc.*|claremont.*", "cmc", lower_campus)
lower_campus <- gsub("scr.*", "scr", lower_campus)
lower_campus <- gsub("cgu.*", "cgu", lower_campus)
lower_campus <- gsub("hmc.*", "hmc", lower_campus)
lower_campus <- gsub("pom.*", "pom", lower_campus)
lower_campus <- gsub("kgi.*", "kgi", lower_campus)
campus_2015 <- table(lower_campus)

# Make date columns to be actual dates
# strptime: R built in date formatting

(campus_2015)
