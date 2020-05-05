storm_2019 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2019.csv" )
library(cluster)
library(factoextra)

tmp <- which(storm_2019$YEARMONTH == 201906 )
data_2019 <- storm_2019[tmp, ]
tmp <- which( data_2019$LATITUDE < 25 )
data_2019 <- data_2019[-tmp, ]
tmp <- which( data_2019$LATITUDE > 45 )
data_2019 <- data_2019[-tmp, ]
tmp <- which( data_2019$RANGE > 20 )
data_2019 <- data_2019[-tmp, ]
tmp <- which( data_2019$RANGE == 0 )
data_2019 <- data_2019[-tmp, ]
tmp <- which( data_2019$LONGITUDE < -105)
data_2019 <- data_2019[-tmp, ]
tmp <- which( data_2019$LONGITUDE > -75)
data_2019 <- data_2019[-tmp, ]

data_2019_c <- data_2019[8:9]
plot(data_2019_c)

gap_stat_2019 = clusGap(data_2019_c, FUN = kmeans, K.max = 10, B = 20)
fviz_gap_stat(gap_stat_2019)
kmeans.cluster_2019 <- kmeans( data_2019_c, center = 3 )
kmeans.cluster_2019$centers
kmeans.cluster_2019$size
plot(data_2019_c, col = kmeans.cluster_2019$cluster, main = "Kmeans Cluster")
points( kmeans.cluster_2019$centers, col = 1:3, pch = 10, cex = 4)
sil_2019 = silhouette(kmeans.cluster_2019$cluster, dist(data_2019_c))
head(sil_2019[,1:3])
fviz_silhouette(sil_2019)

#################################################################################

storm_2018 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2018.csv" )

tmp <- which(storm_2018$YEARMONTH == 201806 )
data_2018 <- storm_2018[tmp, ]
tmp <- which( data_2018$LATITUDE < 25 )
data_2018 <- data_2018[-tmp, ]
tmp <- which( data_2018$LATITUDE > 45 )
data_2018 <- data_2018[-tmp, ]
tmp <- which( data_2018$RANGE > 20 )
data_2018 <- data_2018[-tmp, ]
tmp <- which( data_2018$RANGE == 0 )
data_2018 <- data_2018[-tmp, ]
tmp <- which( data_2018$LONGITUDE < -105)
data_2018 <- data_2018[-tmp, ]
tmp <- which( data_2018$LONGITUDE > -75)
data_2018 <- data_2018[-tmp, ]

data_2018_c <- data_2018[8:9]
plot(data_2018_c)

gap_stat_2018 = clusGap(data_2018_c, FUN = kmeans, K.max = 10, B = 20)
fviz_gap_stat(gap_stat_2018)
kmeans.cluster_2018 <- kmeans( data_2018_c, center = 2 )
kmeans.cluster_2018$centers
kmeans.cluster_2018$size
plot(data_2018_c, col = kmeans.cluster_2018$cluster, main = "Kmeans Cluster")
points( kmeans.cluster_2018$centers, col = 1:3, pch = 10, cex = 4)
sil_2018 = silhouette(kmeans.cluster_2018$cluster, dist(data_2018_c))
head(sil_2018[,1:3])
fviz_silhouette(sil_2018)

################################################################################

storm_2017 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2017.csv" )

tmp <- which(storm_2017$YEARMONTH == 201706 )
data_2017 <- storm_2017[tmp, ]
tmp <- which( data_2017$LATITUDE < 25 )
data_2017 <- data_2017[-tmp, ]
tmp <- which( data_2017$LATITUDE > 45 )
data_2017 <- data_2017[-tmp, ]
tmp <- which( data_2017$RANGE > 20 )
data_2017 <- data_2017[-tmp, ]
tmp <- which( data_2017$RANGE == 0 )
data_2017 <- data_2017[-tmp, ]
tmp <- which( data_2017$LONGITUDE < -105)
data_2017 <- data_2017[-tmp, ]
tmp <- which( data_2017$LONGITUDE > -75)
data_2017 <- data_2017[-tmp, ]

data_2017_c <- data_2017[8:9]
plot(data_2017_c)

gap_stat_2017 = clusGap(data_2017_c, FUN = kmeans, K.max = 10, B = 20)
fviz_gap_stat(gap_stat_2017)
kmeans.cluster_2017 <- kmeans( data_2017_c, center = 4 )
kmeans.cluster_2017$centers
kmeans.cluster_2017$size
plot(data_2017_c, col = kmeans.cluster_2017$cluster, main = "Kmeans Cluster")
points( kmeans.cluster_2017$centers, col = 1:3, pch = 10, cex = 4)
sil_2017 = silhouette(kmeans.cluster_2017$cluster, dist(data_2017_c))
head(sil_2017[,1:3])
fviz_silhouette(sil_2017)

#########################################################################

storm_2016 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2016.csv" )

tmp <- which(storm_2016$YEARMONTH == 201606 )
data_2016 <- storm_2016[tmp, ]
tmp <- which( data_2016$LATITUDE < 25 )
data_2016 <- data_2016[-tmp, ]
tmp <- which( data_2016$LATITUDE > 45 )
data_2016 <- data_2016[-tmp, ]
tmp <- which( data_2016$RANGE > 20 )
data_2016 <- data_2016[-tmp, ]
tmp <- which( data_2016$RANGE == 0 )
data_2016 <- data_2016[-tmp, ]
tmp <- which( data_2016$LONGITUDE < -105)
data_2016 <- data_2016[-tmp, ]
tmp <- which( data_2016$LONGITUDE > -75)
data_2016 <- data_2016[-tmp, ]

data_2016_c <- data_2016[8:9]
plot(data_2016_c)

gap_stat_2016 = clusGap(data_2016_c, FUN = kmeans, K.max = 10, B = 20)
fviz_gap_stat(gap_stat_2016)
kmeans.cluster_2016 <- kmeans( data_2016_c, center = 2 )
kmeans.cluster_2016$centers
kmeans.cluster_2016$size
plot(data_2016_c, col = kmeans.cluster_2016$cluster, main = "Kmeans Cluster")
points( kmeans.cluster_2016$centers, col = 1:3, pch = 10, cex = 4)
sil_2016 = silhouette(kmeans.cluster_2016$cluster, dist(data_2016_c))
head(sil_2016[,1:3])
fviz_silhouette(sil_2016)

##########################################################################

storm_2015 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2015.csv" )

tmp <- which(storm_2015$YEARMONTH == 201506 )
data_2015 <- storm_2015[tmp, ]
tmp <- which( data_2015$LATITUDE < 25 )
data_2015 <- data_2015[-tmp, ]
tmp <- which( data_2015$LATITUDE > 45 )
data_2015 <- data_2015[-tmp, ]
tmp <- which( data_2015$RANGE > 20 )
data_2015 <- data_2015[-tmp, ]
tmp <- which( data_2015$RANGE == 0 )
data_2015 <- data_2015[-tmp, ]
tmp <- which( data_2015$LONGITUDE < -105)
data_2015 <- data_2015[-tmp, ]
tmp <- which( data_2015$LONGITUDE > -75)
data_2015 <- data_2015[-tmp, ]

data_2015_c <- data_2015[8:9]
plot(data_2015_c)

gap_stat_2015 = clusGap(data_2015_c, FUN = kmeans, K.max = 10, B = 20)
fviz_gap_stat(gap_stat_2015)
kmeans.cluster_2015 <- kmeans( data_2015_c, center = 3 )
kmeans.cluster_2015$centers
kmeans.cluster_2015$size
plot(data_2015_c, col = kmeans.cluster_2015$cluster, main = "Kmeans Cluster")
points( kmeans.cluster_2015$centers, col = 1:3, pch = 10, cex = 4)
sil_2015 = silhouette(kmeans.cluster_2015$cluster, dist(data_2015_c))
head(sil_2015[,1:3])
fviz_silhouette(sil_2015)

###########################################################################

storm_2014 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2014.csv" )

tmp <- which(storm_2014$YEARMONTH == 201406 )
data_2014 <- storm_2014[tmp, ]
tmp <- which( data_2014$LATITUDE < 25 )
data_2014 <- data_2014[-tmp, ]
tmp <- which( data_2014$LATITUDE > 45 )
data_2014 <- data_2014[-tmp, ]
tmp <- which( data_2014$RANGE > 20 )
data_2014 <- data_2014[-tmp, ]
tmp <- which( data_2014$RANGE == 0 )
data_2014 <- data_2014[-tmp, ]
tmp <- which( data_2014$LONGITUDE < -105)
data_2014 <- data_2014[-tmp, ]
tmp <- which( data_2014$LONGITUDE > -75)
data_2014 <- data_2014[-tmp, ]

data_2014_c <- data_2014[8:9]
plot(data_2014_c)

gap_stat_2014 = clusGap(data_2014_c, FUN = kmeans, K.max = 10, B = 20)
fviz_gap_stat(gap_stat_2014)
kmeans.cluster_2014 <- kmeans( data_2014_c, center = 3 )
kmeans.cluster_2014$centers
kmeans.cluster_2014$size
plot(data_2014_c, col = kmeans.cluster_2014$cluster, main = "Kmeans Cluster")
points( kmeans.cluster_2014$centers, col = 1:3, pch = 10, cex = 4)
sil_2014 = silhouette(kmeans.cluster_2014$cluster, dist(data_2014_c))
head(sil_2014[,1:3])
fviz_silhouette(sil_2014)

#########################################################################

storm_2013 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2013.csv" )

tmp <- which(storm_2013$YEARMONTH == 201306 )
data_2013 <- storm_2013[tmp, ]
tmp <- which( data_2013$LATITUDE < 25 )
data_2013 <- data_2013[-tmp, ]
tmp <- which( data_2013$LATITUDE > 45 )
data_2013 <- data_2013[-tmp, ]
tmp <- which( data_2013$RANGE > 20 )
data_2013 <- data_2013[-tmp, ]
tmp <- which( data_2013$RANGE == 0 )
data_2013 <- data_2013[-tmp, ]
tmp <- which( data_2013$LONGITUDE < -105)
data_2013 <- data_2013[-tmp, ]
tmp <- which( data_2013$LONGITUDE > -75)
data_2013 <- data_2013[-tmp, ]

data_2013_c <- data_2013[8:9]
plot(data_2013_c)

gap_stat_2013 = clusGap(data_2013_c, FUN = kmeans, K.max = 10, B = 20)
fviz_gap_stat(gap_stat_2013)
kmeans.cluster_2013 <- kmeans( data_2013_c, center = 3 )
kmeans.cluster_2013$centers
kmeans.cluster_2013$size
plot(data_2013_c, col = kmeans.cluster_2013$cluster, main = "Kmeans Cluster")
points( kmeans.cluster_2013$centers, col = 1:3, pch = 10, cex = 4)
sil_2013 = silhouette(kmeans.cluster_2013$cluster, dist(data_2013_c))
head(sil_2013[,1:3])
fviz_silhouette(sil_2013)

#########################################################################

storm_2012 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2012.csv" )

tmp <- which(storm_2012$YEARMONTH == 201206 )
data_2012 <- storm_2012[tmp, ]
tmp <- which( data_2012$LATITUDE < 25 )
data_2012 <- data_2012[-tmp, ]
tmp <- which( data_2012$LATITUDE > 45 )
data_2012 <- data_2012[-tmp, ]
tmp <- which( data_2012$RANGE > 20 )
data_2012 <- data_2012[-tmp, ]
tmp <- which( data_2012$RANGE == 0 )
data_2012 <- data_2012[-tmp, ]
tmp <- which( data_2012$LONGITUDE < -105)
data_2012 <- data_2012[-tmp, ]
tmp <- which( data_2012$LONGITUDE > -75)
data_2012 <- data_2012[-tmp, ]

data_2012_c <- data_2012[8:9]
plot(data_2012_c)

gap_stat_2012 = clusGap(data_2012_c, FUN = kmeans, K.max = 10, B = 20)
fviz_gap_stat(gap_stat_2012)
kmeans.cluster_2012 <- kmeans( data_2012_c, center = 4 )
kmeans.cluster_2012$centers
kmeans.cluster_2012$size
plot(data_2012_c, col = kmeans.cluster_2012$cluster, main = "Kmeans Cluster")
points( kmeans.cluster_2012$centers, col = 1:3, pch = 10, cex = 4)
sil_2012 = silhouette(kmeans.cluster_2012$cluster, dist(data_2012_c))
head(sil_2012[,1:3])
fviz_silhouette(sil_2012)

#########################################################################

storm_2011 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2011.csv" )

tmp <- which(storm_2011$YEARMONTH == 201106 )
data_2011 <- storm_2011[tmp, ]
tmp <- which( data_2011$LATITUDE < 25 )
data_2011 <- data_2011[-tmp, ]
tmp <- which( data_2011$LATITUDE > 45 )
data_2011 <- data_2011[-tmp, ]
tmp <- which( data_2011$RANGE > 20 )
data_2011 <- data_2011[-tmp, ]
tmp <- which( data_2011$RANGE == 0 )
data_2011 <- data_2011[-tmp, ]
tmp <- which( data_2011$LONGITUDE < -105)
data_2011 <- data_2011[-tmp, ]
tmp <- which( data_2011$LONGITUDE > -75)
data_2011 <- data_2011[-tmp, ]

data_2011_c <- data_2011[8:9]
plot(data_2011_c)

gap_stat_2011 = clusGap(data_2011_c, FUN = kmeans, K.max = 10, B = 20)
fviz_gap_stat(gap_stat_2011)
kmeans.cluster_2011 <- kmeans( data_2011_c, center = 3 )
kmeans.cluster_2011$centers
kmeans.cluster_2011$size
plot(data_2011_c, col = kmeans.cluster_2011$cluster, main = "Kmeans Cluster")
points( kmeans.cluster_2011$centers, col = 1:3, pch = 10, cex = 4)
sil_2011 = silhouette(kmeans.cluster_2011$cluster, dist(data_2011_c))
head(sil_2011[,1:3])
fviz_silhouette(sil_2011)

###########################################################################

storm_2010 <- read.csv("C:/da/project/StormEvents_locations-ftp_v1.0_d2010.csv" )

tmp <- which(storm_2010$YEARMONTH == 201006 )
data_2010 <- storm_2010[tmp, ]
tmp <- which( data_2010$LATITUDE < 25 )
data_2010 <- data_2010[-tmp, ]
tmp <- which( data_2010$LATITUDE > 45 )
data_2010 <- data_2010[-tmp, ]
tmp <- which( data_2010$RANGE > 20 )
data_2010 <- data_2010[-tmp, ]
tmp <- which( data_2010$RANGE == 0 )
data_2010 <- data_2010[-tmp, ]
tmp <- which( data_2010$LONGITUDE < -105)
data_2010 <- data_2010[-tmp, ]
tmp <- which( data_2010$LONGITUDE > -75)
data_2010 <- data_2010[-tmp, ]

data_2010_c <- data_2010[8:9]
plot(data_2010_c)

gap_stat_2010 = clusGap(data_2010_c, FUN = kmeans, K.max = 10, B = 20)
fviz_gap_stat(gap_stat_2010)
kmeans.cluster_2010 <- kmeans( data_2010_c, center = 3 )
kmeans.cluster_2010$centers
kmeans.cluster_2010$size
plot(data_2010_c, col = kmeans.cluster_2010$cluster, main = "Kmeans Cluster")
points( kmeans.cluster_2010$centers, col = 1:3, pch = 10, cex = 4)
sil_2010 = silhouette(kmeans.cluster_2010$cluster, dist(data_2010_c))
head(sil_2010[,1:3])
fviz_silhouette(sil_2010)
