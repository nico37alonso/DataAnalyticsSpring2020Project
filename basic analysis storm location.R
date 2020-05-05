storm_2000 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2000.csv" )
storm_2001 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2001.csv" )
storm_2002 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2002.csv" )
storm_2003 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2003.csv" )
storm_2004 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2004.csv" )
storm_2005 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2005.csv" )
storm_2006 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2006.csv" )
storm_2007 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2007.csv" )
storm_2008 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2008.csv" )
storm_2009 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2009.csv" )

storm_2010 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2010.csv" )
storm_2011 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2011.csv" )
storm_2012 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2012.csv" )
storm_2013 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2013.csv" )
storm_2014 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2014.csv" )
storm_2015 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2015.csv" )
storm_2016 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2016.csv" )
storm_2017 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2017.csv" )
storm_2018 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2018.csv" )
storm_2019 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2019.csv" )

data <- storm_2000
data <- rbind( data, storm_2001 )
data <- rbind( data, storm_2002 )
data <- rbind( data, storm_2003 )
data <- rbind( data, storm_2004 )
data <- rbind( data, storm_2005 )
data <- rbind( data, storm_2006 )
data <- rbind( data, storm_2007 )
data <- rbind( data, storm_2008 )
data <- rbind( data, storm_2009 )

data <- rbind( data, storm_2010 )
data <- rbind( data, storm_2011 )
data <- rbind( data, storm_2012 )
data <- rbind( data, storm_2013 )
data <- rbind( data, storm_2014 )
data <- rbind( data, storm_2015 )
data <- rbind( data, storm_2016 )
data <- rbind( data, storm_2017 )
data <- rbind( data, storm_2018 )
data <- rbind( data, storm_2019 )

tmp1 <- matrix(c(0), nrow = nrow(data), ncol = 1 )
tmp2 <- matrix(c(0), nrow = nrow(data), ncol = 1 )

for ( i in 1 : nrow(data) ){
  tmp1[i] = (data$YEARMONTH[i])%%100
  tmp2[i] = (data$YEARMONTH[i])%/%100
}

data <- cbind(data, tmp1 )
data <- cbind(data, tmp2 )
names(data)[12] <- "month"
names(data)[13] <- "year"

attach(data)
head(data)
data <- data[,-1]
tmp <- which(is.na(data$RANGE))
data <- data[-tmp, ]
tmp <- which(data$RANGE == 0 )
data <- data[-tmp, ]
tmp <- which(is.na(data$LATITUDE))
data <- data[-tmp,]
tmp <- which(is.na(data$LAT2))
data <- data[-tmp,]

summary(data)

attach(data)
par(mfrow=c(2,2))
summary(LATITUDE)
hist(LATITUDE, seq(-15, 75, 5), prob = TRUE)
lines(density(LATITUDE, na.rm = FALSE, bw = 5. ))
boxplot(LATITUDE)
plot(ecdf(LATITUDE), do.points = FALSE, verticals = TRUE)

qqnorm( LATITUDE )
qqline( LATITUDE, col = 2, lwd = 2 )

##########################################################

summary(data)

par(mfrow=c(2,2))
summary(LONGITUDE)
tmp <- which(LONGITUDE >0 )
LONGITUDE <- LONGITUDE[-tmp]
hist(LONGITUDE, seq(-175, -5, 10), prob = TRUE)
lines(density(LONGITUDE, na.rm = FALSE, bw = 5. ))
boxplot(LONGITUDE)
plot(ecdf(LONGITUDE), do.points = FALSE, verticals = TRUE)

qqnorm( LONGITUDE )
qqline( LONGITUDE, col = 2, lwd = 2 )

###########################################################

summary(data)

par(mfrow=c(2,2))
summary(RANGE)
tmp <- which(RANGE >20 )
RANGE <- RANGE[-tmp]
hist(RANGE, seq(0, 20, 1), prob = TRUE)
lines(density(RANGE, na.rm = FALSE, bw = 1. ))
boxplot(RANGE)
plot(ecdf(RANGE), do.points = FALSE, verticals = TRUE)

qqnorm( RANGE )
qqline( RANGE, col = 2, lwd = 2 )

################################################################

summary(data)

par(mfrow=c(2,2))
summary(month)
hist(month, seq(0, 12, 1), prob = TRUE)
lines(density(month, na.rm = FALSE, bw = 1. ))
boxplot(month)
plot(ecdf(month), do.points = FALSE, verticals = TRUE)

qqnorm( month )
qqline( month, col = 2, lwd = 2 )
