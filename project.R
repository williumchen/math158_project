# Read in csv
test <- read.csv("rawWifi2016/201601.csv", header=FALSE)

# Reformatted rptHeader to be consistent
headers <- read.table("rawWifi2015/wip/rptHeader.txt", sep=",", header=TRUE)
colnames(test) <- colnames(headers)

# April 2016 data is different, need to reform either 2015 or 4/2016 onward
# Only get daily csv
wifi_2015 <- list.files("rawWifi2015", pattern="[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9].csv", full=TRUE)

# Put wifi info in rawWifi2015, rawWifi2016 in lists
wifi_2015.list <- lapply(wifi_2015, read.csv, header=FALSE)

# Bind all csv information for 2015 year into one data set
wifi_2015_data <- do.call('rbind', wifi_2015.list)
colnames(wifi_2015_data) <- colnames(headers)

######## 
# 2015 #
########

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

# Make duration a numeric variable
temp2 <- gregexpr("[0-9]+", wifi_2015_data$Duration)
matches <- regmatches(wifi_2015_data$Duration, temp2)
newrow <- matches[1]
newDur <- c()
for (row in matches) {
  len <- length(row)
  if (len == 1) {
    newDur <- c(newDur, as.numeric(row))
  } else if (len == 2) {
    newDur <- c(newDur, as.numeric(row[1])*60 + as.numeric(row[2]))
  } else {
    newDur <- c(newDur, (as.numeric(row[1])*60*24) + (as.numeric(row[2])*60) + as.numeric(row[3]))
  }
}
wifi_2015_data$Duration <- newDur

wifi_2015_data <- wifi_2015_data[-which(wifi_2015_data$Duration == 1), ]
wifi_2015_data <- wifi_2015_data[-which(wifi_2015_data$Duration == 2), ]
wifi_2015_data <- wifi_2015_data[-which(wifi_2015_data$Duration == 3), ]
wifi_2015_data <- wifi_2015_data[-which(wifi_2015_data$Duration == 4), ]
wifi_2015_data <- wifi_2015_data[-which(wifi_2015_data$Duration > 900), ]

# Make heatmap dataframe
library(plyr)
temp2 <- gregexpr("[0-9]+", wifi_2015_data$ConnectStart)
matches <- regmatches(wifi_2015_data$ConnectStart, temp2)
time_data <- c()
for (row in matches) {
  if (!is.na(row[2])) {
    if (nchar(row[2]) == 1) {
      row[2] = paste("0", row[2], sep="")
    }
    time_data <- c(time_data, paste(row[1], row[2], sep="/"))
  } else {
    time_data <- c(time_data, paste("ERROR"))
  }
}
time_data <- sort(time_data)
wifi_2015_data$ConnectStart <- time_data

count_agg <- count(wifi_2015_data, c('ConnectStart','Campus'))
heat_count <- reshape(count_agg, idvar = "Campus", timevar = "ConnectStart", direction = "wide")
heat_count[is.na(heat_count)] <- 0
rownames(heat_count) <- unique(count_agg$Campus)
colnames(heat_count) <- unique(count_agg$ConnectStart)
heat_count <- subset(heat_count, select = -1)
heat_count <- subset(heat_count, select = -1)


heat_matrix <- t(data.matrix(heat_count))
heatmap <- heatmap(heat_matrix, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10))

count_agg2 <- aggregate(wifi_2015_data$Duration ~ wifi_2015_data$Campus + wifi_2015_data$ConnectStart, FUN = sum)
heat_count <- reshape(count_agg2, idvar = "wifi_2015_data$Campus", timevar = "wifi_2015_data$ConnectStart", direction = "wide")
heat_count[is.na(heat_count)] <- 0
rownames(heat_count) <- unique(count_agg2$`wifi_2015_data$Campus`)
colnames(heat_count) <- unique(count_agg2$`wifi_2015_data$ConnectStart`)
heat_count <- subset(heat_count, select = -1)
heat_count <- subset(heat_count, select = -1)

require(gplots)
heat_matrix <- t(data.matrix(heat_count))
heatmap <- heatmap(heat_matrix, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10))
# heatmap <- heatmap.2(heat_matrix, cellnote=heat_matrix, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10))

# Make model Duration ~ Campus + Location
lmod <- lm(as.numeric(Duration) ~ Campus*Device.Location, data=wifi_2015_data)
summary(lmod)


# Plot histogram of schools
library(ggplot2)
ggplot(wifi_2015_data,aes(x=Campus)) + geom_bar()

# Histogram of location
ggplot(wifi_2015_data,aes(x=Device.Location)) + geom_bar()

# Plot average duration against school
ggplot(wifi_2015_data, aes(x=wifi_2015_data$Campus, y=wifi_2015_data$Duration)) + stat_summary(fun.y="mean", geom="bar")

# Boxplot duration against school
ggplot(wifi_2015_data, aes(x=wifi_2015_data$Campus, y=log10(wifi_2015_data$Duration))) + geom_boxplot() + stat_summary(fun.y=mean, geom="line", aes(group=1))  + stat_summary(fun.y=mean, geom="point")

# Plot median duration against school
ggplot(wifi_2015_data, aes(x=wifi_2015_data$Campus, y=wifi_2015_data$Duration)) + stat_summary(fun.y="median", geom="bar")

# Plot average duration against location
ggplot(wifi_2015_data, aes(x=wifi_2015_data$Device.Location, y=wifi_2015_data$Duration)) + stat_summary(fun.y="mean", geom="bar")

# Boxplot duration against location
ggplot(wifi_2015_data, aes(x=wifi_2015_data$Device.Location, y=log10(wifi_2015_data$Duration))) + geom_boxplot() + stat_summary(fun.y=mean, geom="line", aes(group=1)) + stat_summary(fun.y=mean, geom="point")

# Plot median duration against location
ggplot(wifi_2015_data, aes(x=wifi_2015_data$Device.Location, y=wifi_2015_data$Duration)) + stat_summary(fun.y="median", geom="bar")

# IDEA: People and places and time; location data and duration (aov), 
# campus and location data, campus and duration (interaction??)

library(ggplot2)

ggplot(wifi_2015_data, aes(x=wifi_2015_data$ConnectStart, y=wifi_2015_data$Duration))

agg <- aggregate(wifi_2015_data$Duration ~ wifi_2015_data$Campus + wifi_2015_data$Device.Location, FUN = sum)
ggplot(data=agg, aes(x=agg$`wifi_2015_data$Campus`, y=agg$`wifi_2015_data$Duration`, fill=agg$`wifi_2015_data$Device.Location`))  + geom_bar(stat="identity") + scale_fill_discrete(name = "Campus") + labs(x="Campus", y="Duration (minutes)")
ggplot(data=agg, aes(x=agg$`wifi_2015_data$Device.Location`, y=agg$`wifi_2015_data$Duration`, fill=agg$`wifi_2015_data$Campus`))  + geom_bar(stat="identity") + scale_fill_discrete(name = "Campus") + labs(x="Location", y="Duration (minutes)")
######## 
# 2016 #
########

wifi_2016 <- list.files("rawWifi2016", pattern="[0-9][0-9][0-9][0-9][0-9][0-3][0-9][0-9].csv", full=TRUE)

wifi_2016.list <- lapply(wifi_2016, read.csv, header=FALSE)

wifi_2016_data <- do.call('rbind', wifi_2016.list)
colnames(wifi_2016_data) <- colnames(headers)

# Frequency of different campuses (table)
# Frequency of different location by campuses (tapply) summarize, function
lower_campus <- tolower(wifi_2016_data$Campus)
lower_campus <- gsub("ptz|pitzer.edu", "pit", lower_campus)
lower_campus <- gsub("cmc.*|claremont.*|.*cmc", "cmc", lower_campus)
lower_campus <- gsub("cuc.cmc", "cmc", lower_campus)
lower_campus <- gsub("scr.*", "scr", lower_campus)
lower_campus <- gsub("cgu.*", "cgu", lower_campus)
lower_campus <- gsub("hmc.*", "hmc", lower_campus)
lower_campus <- gsub("pom.*", "pom", lower_campus)
lower_campus <- gsub("kgi.*", "kgi", lower_campus)
campus_2016 <- table(lower_campus)
wifi_2016_data$Campus <- lower_campus

# Make date columns to be actual dates
# strptime: R built in date formatting

# Format data
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Device.Location == "-"), ]
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Campus == "-"), ]
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Campus == "123"), ]
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Campus == "yale.edu"), ]
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Campus == "tulane.edu"), ]
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Campus == "bristol.ac.uk"), ]
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Campus == "uvt.nl"), ]
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Campus == "us.es"), ]
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Campus == "ucdavis.edu"), ]
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Campus == "ubc.ca"), ]
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Campus == "swin.edu.au"), ]
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Campus == "student.unisg.ch"), ]
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Campus == "st-andrews.ac.uk"), ]
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Campus == "smith.edu"), ]
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Campus == "qmul.ac.uk"), ]
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Campus == "northeastern.edu"), ]
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Campus == "lsu.edu"), ]
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Campus == "gwmail.gwu.edu"), ]
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Campus == "eduroam.ucc.ie"), ]
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Campus == "ed.ac.uk"), ]
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Campus == "csusb.edu"), ]
wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Campus == "cam.ac.uk"), ]

# Make duration a numeric variable
temp1 <- gregexpr("[0-9]+", wifi_2016_data$Duration)
matches <- regmatches(wifi_2016_data$Duration, temp1)
newDur <- c()
for (row in matches) {
  len <- length(row)
  if (len == 1) {
    newDur <- c(newDur, as.numeric(row))
  } else if (len == 2) {
    newDur <- c(newDur, as.numeric(row[1])*60 + as.numeric(row[2]))
  } else {
    newDur <- c(newDur, (as.numeric(row[1])*60*24) + (as.numeric(row[2])*60) + as.numeric(row[3]))
  }
}
wifi_2016_data$Duration <- newDur

wifi_2016_data <- wifi_2016_data[-which(wifi_2016_data$Duration > 900), ]

# Plot histogram of schools
library(ggplot2)
ggplot(wifi_2016_data,aes(x=Campus)) + geom_bar()

# Histogram of location
ggplot(wifi_2016_data,aes(x=Device.Location)) + geom_bar()

# Plot average duration against school
ggplot(wifi_2016_data, aes(x=wifi_2016_data$Campus, y=wifi_2016_data$Duration)) + stat_summary(fun.y="mean", geom="bar")

# Plot median duration against school
ggplot(wifi_2016_data, aes(x=wifi_2016_data$Campus, y=wifi_2016_data$Duration)) + stat_summary(fun.y="median", geom="bar")

# Plot average duration against location
ggplot(wifi_2016_data, aes(x=wifi_2016_data$Device.Location, y=wifi_2016_data$Duration)) + stat_summary(fun.y="mean", geom="bar")

# Plot median duration against location
ggplot(wifi_2016_data, aes(x=wifi_2016_data$Device.Location, y=wifi_2016_data$Duration)) + stat_summary(fun.y="median", geom="bar")

# IDEA: People and places and time; location data and duration (aov), 
# campus and location data, campus and duration (interaction??)

library(ggplot2)

ggplot(wifi_2016_data, aes(x=wifi_2016_data$ConnectStart, y=wifi_2016_data$Duration))

agg2 <- aggregate(wifi_2016_data$Duration ~ wifi_2016_data$Campus + wifi_2016_data$Device.Location, FUN = sum)
ggplot(data=agg2, aes(x=agg2$`wifi_2016_data$Device.Location`, y=agg2$`wifi_2016_data$Duration`, fill=agg2$`wifi_2016_data$Campus`))  + geom_bar(stat="identity") + scale_fill_discrete(name = "Campus")

# Box Cox, ANOVA, Tukey
require(MASS)
dur_lm <- lm(wifi_2015_data$Duration ~ wifi_2015_data$Device.Location + wifi_2015_data$Campus)
summary(dur_lm)
# Interaction terms
# dur_inter_lm <- lm(wifi_2015_data$Duration ~ wifi_2015_data$Device.Location*wifi_2015_data$Campus)
# summary(dur_inter_lm)
anova(dur_lm)
(tci <- TukeyHSD(aov(wifi_2015_data$Duration ~ wifi_2015_data$Device.Location + wifi_2015_data$Campus)))
par(las=1, mar=c(5,5,5,5))
psig=as.numeric(apply(tci$`wifi_2015_data$Campus`[,2:3],1,prod)>=0)+1
plot(tci,col=psig,yaxt="n")
for (j in 1:length(psig)){
  axis(2,at=j,labels=rownames(tci$`wifi_2015_data$Campus`)[length(psig)-j+1],
       las=1,cex.axis=.8,col.axis=psig[length(psig)-j+1])
}

# Interaction plot
interaction.plot(wifi_2015_data$Device.Location, wifi_2015_data$Campus, wifi_2015_data$Duration) 
interaction.plot(wifi_2015_data$Campus, wifi_2015_data$Device.Location, wifi_2015_data$Duration) 

# Multi-level Tukey
# (tci <- TukeyHSD(aov(wifi_2015_data$Duration ~ wifi_2015_data$Device.Location:wifi_2015_data$Campus)))
plot(tci)
boxcox(dur_lm, plotit=T, lambda=seq(-0.40,-0.37,by=0.001))
dur_trans_lm <- lm(1/sqrt(wifi_2015_data$Duration) ~ wifi_2015_data$Device.Location + wifi_2015_data$Campus)
summary(dur_trans_lm)
anova(dur_trans_lm)
(tci <- TukeyHSD(aov(1/sqrt(wifi_2015_data$Duration) ~ wifi_2015_data$Device.Location + wifi_2015_data$Campus)))
par(las=1, mar=c(5,5,5,5))
psig=as.numeric(apply(tci$`wifi_2015_data$Campus`[,2:3],1,prod)>=0)+1
plot(tci,col=psig,yaxt="n")
for (j in 1:length(psig)){
  axis(2,at=j,labels=rownames(tci$`wifi_2015_data$Campus`)[length(psig)-j+1],
       las=1,cex.axis=.8,col.axis=psig[length(psig)-j+1])
}

# Regression diagnostics
par(mfrow=c(1,2))
qqnorm(residuals(dur_lm),main="")
qqline(residuals(dur_lm))
plot(fitted(dur_lm),residuals(dur_lm),xlab="Fitted",ylab="Residuals")
abline(h=0)
par(mfrow=c(1,1))

par(mfrow=c(1,2))
qqnorm(residuals(dur_trans_lm),main="")
qqline(residuals(dur_trans_lm))
plot(fitted(dur_trans_lm),residuals(dur_trans_lm),xlab="Fitted",ylab="Residuals")
abline(h=0)
par(mfrow=c(1,1))

# Interaction model
dur_inter_lm <- lm(wifi_2015_data$Duration ~ wifi_2015_data$Device.Location:wifi_2015_data$Campus)
summary(dur_inter_lm)

