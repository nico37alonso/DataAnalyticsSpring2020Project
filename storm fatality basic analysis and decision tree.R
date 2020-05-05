library(rpart)
library(rpart.plot)
storm_fatalities_2000 <- read.csv("C:/da/project/StormEvents_fatalities-ftp_v1.0_d2000.csv" )
storm_fatalities_2001 <- read.csv("C:/da/project/StormEvents_fatalities-ftp_v1.0_d2001.csv" )
storm_fatalities_2002 <- read.csv("C:/da/project/StormEvents_fatalities-ftp_v1.0_d2002.csv" )
storm_fatalities_2003 <- read.csv("C:/da/project/StormEvents_fatalities-ftp_v1.0_d2003.csv" )
storm_fatalities_2004 <- read.csv("C:/da/project/StormEvents_fatalities-ftp_v1.0_d2004.csv" )
storm_fatalities_2005 <- read.csv("C:/da/project/StormEvents_fatalities-ftp_v1.0_d2005.csv" )
storm_fatalities_2006 <- read.csv("C:/da/project/StormEvents_fatalities-ftp_v1.0_d2006.csv" )
storm_fatalities_2007 <- read.csv("C:/da/project/StormEvents_fatalities-ftp_v1.0_d2007.csv" )
storm_fatalities_2008 <- read.csv("C:/da/project/StormEvents_fatalities-ftp_v1.0_d2008.csv" )
storm_fatalities_2009 <- read.csv("C:/da/project/StormEvents_fatalities-ftp_v1.0_d2009.csv" )
storm_fatalities_2010 <- read.csv("C:/da/project/StormEvents_fatalities-ftp_v1.0_d2010.csv" )
storm_fatalities_2011 <- read.csv("C:/da/project/StormEvents_fatalities-ftp_v1.0_d2011.csv" )
storm_fatalities_2012 <- read.csv("C:/da/project/StormEvents_fatalities-ftp_v1.0_d2012.csv" )
storm_fatalities_2013 <- read.csv("C:/da/project/StormEvents_fatalities-ftp_v1.0_d2013.csv" )
storm_fatalities_2014 <- read.csv("C:/da/project/StormEvents_fatalities-ftp_v1.0_d2014.csv" )
storm_fatalities_2015 <- read.csv("C:/da/project/StormEvents_fatalities-ftp_v1.0_d2015.csv" )
storm_fatalities_2016 <- read.csv("C:/da/project/StormEvents_fatalities-ftp_v1.0_d2016.csv" )
storm_fatalities_2017 <- read.csv("C:/da/project/StormEvents_fatalities-ftp_v1.0_d2017.csv" )
storm_fatalities_2018 <- read.csv("C:/da/project/StormEvents_fatalities-ftp_v1.0_d2018.csv" )
storm_fatalities_2019 <- read.csv("C:/da/project/StormEvents_fatalities-ftp_v1.0_d2019.csv" )
data <- storm_fatalities_2000
data <- rbind(data, storm_fatalities_2001)
data <- rbind(data, storm_fatalities_2002)
data <- rbind(data, storm_fatalities_2003)
data <- rbind(data, storm_fatalities_2004)
data <- rbind(data, storm_fatalities_2005)
data <- rbind(data, storm_fatalities_2006)
data <- rbind(data, storm_fatalities_2007)
data <- rbind(data, storm_fatalities_2008)
data <- rbind(data, storm_fatalities_2009)
data <- rbind(data, storm_fatalities_2010)
data <- rbind(data, storm_fatalities_2011)
data <- rbind(data, storm_fatalities_2012)
data <- rbind(data, storm_fatalities_2013)
data <- rbind(data, storm_fatalities_2014)
data <- rbind(data, storm_fatalities_2015)
data <- rbind(data, storm_fatalities_2016)
data <- rbind(data, storm_fatalities_2017)
data <- rbind(data, storm_fatalities_2018)
data <- rbind(data, storm_fatalities_2019)
summary(data)

head(data)
data <- data[,-11]
data <- data[,-7]
data <- data[,-4]
data <- data[,-4]

tmp1 <- matrix(c(0), nrow = nrow(data), ncol = 1 )
tmp2 <- matrix(c(0), nrow = nrow(data), ncol = 1 )

for ( i in 1 : nrow(data ) ){
  tmp1[i] = (data$FAT_YEARMONTH[i])%%100
  tmp2[i] = (data$FAT_YEARMONTH[i])%/%100
}

data <- cbind(data, tmp1 )
data <- cbind(data , tmp2 )
names(data )[8] <- "month"
names(data )[9] <- "year"
data <- data[,-1]
data <- data[,-2]

tmp <- which(is.na(data$FATALITY_AGE))
data <- data[-tmp,]

tmp <- which(data$FATALITY_AGE == 0 )
data <- data[-tmp, ]

tmp <- which(data$FATALITY_SEX == "F" )
tmp2 <- which(data$FATALITY_SEX == "M")
data <- rbind(data[tmp, ], data[tmp2, ])

attach(data)

summary(data)

par(mfrow=c(2,2))
summary(FATALITY_AGE)
hist(FATALITY_AGE, seq(0, 105, 5), prob = TRUE)
lines(density(FATALITY_AGE, na.rm = FALSE, bw = 10. ))
boxplot(FATALITY_AGE)
plot(ecdf(FATALITY_AGE), do.points = FALSE, verticals = TRUE)

qqnorm( FATALITY_AGE )
qqline( FATALITY_AGE, col = 2, lwd = 2 )

#################################################################

par(mfrow=c(2,2))
summary(month)
hist(month, seq(1, 12, 1), prob = TRUE)
lines(density(month,  na.rm = FALSE, bw = 1. ))
boxplot(month)
plot(ecdf(month), do.points = FALSE, verticals = TRUE)

qqnorm( month )
qqline( month, col = 2, lwd = 2 )

#################################################################

data_tmp <- data

data_tmp <- data_tmp[,-6]
data_tmp <- data_tmp[,-5]
data_tmp <- data_tmp[,-3]

summary(data_tmp)

heatmap(cor(data_tmp),Rowv = NA, Colv = NA)

data <- data[,-7]
summary(data)

Tree_1 <- rpart( FATALITY_LOCATION ~., data, method = "class" )
Tree_1
rpart.plot(Tree_1)
printcp(Tree_1)
plotcp(Tree_1)

which.min(Tree_1$cptable[, "xerror"])

Tree_1.cp <- Tree_1$cptable[5,"CP"]
Tree_1.cp

prune.tree <- prune(Tree_1, cp = Tree_1.cp )

rpart.plot(prune.tree)
printcp(prune.tree)

library(caret)

data_deal <- data[which(data$FATALITY_LOCATION == "Vehicle/Towed Trailer"), ]
data_deal <- rbind(data_deal, data[which(data$FATALITY_LOCATION == "Outside/Open Areas"), ])
data_deal <- rbind(data_deal, data[which(data$FATALITY_LOCATION == "Permanent Home"), ])
data_deal <- rbind(data_deal, data[which(data$FATALITY_LOCATION == "In Water"), ])
data_deal <- rbind(data_deal, data[which(data$FATALITY_LOCATION == "Other"),])

Tree_1_deal <- rpart( FATALITY_LOCATION ~., data_deal, method = "class" )
Tree_1_deal
rpart.plot(Tree_1_deal)
printcp(Tree_1_deal)
plotcp(Tree_1_deal)

which.min(Tree_1_deal$cptable[, "xerror"])

Tree_1_deal.cp <- Tree_1_deal$cptable[5,"CP"]
Tree_1_deal.cp

prune.tree_deal <- prune(Tree_1_deal, cp = Tree_1_deal.cp )

rpart.plot(prune.tree_deal)
printcp(prune.tree_deal)

summary(data_deal)

set.seed(1234)
tmp <- sample(5060,1012)
data_deal_test_1 <- data_deal[tmp, ]
rest <- data_deal[-tmp, ]

tmp <- sample(4048,1012)
data_deal_test_2 <- data_deal[tmp, ]
rest <- data_deal[-tmp, ]

tmp <- sample(3036,1012)
data_deal_test_3 <- data_deal[tmp, ]
rest <- data_deal[-tmp, ]

tmp <- sample(20324,1012)
data_deal_test_4 <- data_deal[tmp, ]
data_deal_test_5 <- data_deal[-tmp, ]

data_deal_train_1 <- rbind( data_deal_test_2, data_deal_test_3, data_deal_test_4, data_deal_test_5 )

Tree_deal_1 <- rpart( FATALITY_LOCATION ~., data_deal_train_1, method = "class" )
Tree_deal_1
rpart.plot(Tree_deal_1)
printcp(Tree_deal_1)


which.min(Tree_deal_1$cptable[, "xerror"])

Tree_deal_1.cp <- Tree_deal_1$cptable[4,"CP"]
Tree_deal_1.cp

prune.tree_deal_1 <- prune(Tree_deal_1, cp = Tree_deal_1.cp )

rpart.plot(prune.tree_deal_1)
printcp(prune.tree_deal_1)

prediction_1 <- predict(prune.tree_deal_1, data_deal_test_1, type = "class")
table(data_deal_test_1$FATALITY_LOCATION, prediction_1)
confusionMatrix(table(data_deal_test_1$FATALITY_LOCATION, prediction_1))


############################################################################
data_deal_train_2 <- rbind( data_deal_test_1, data_deal_test_3, data_deal_test_4, data_deal_test_5 )

Tree_deal_2 <- rpart( FATALITY_LOCATION ~., data_deal_train_2, method = "class" )
Tree_deal_2
rpart.plot(Tree_deal_2)
printcp(Tree_deal_2)


which.min(Tree_deal_2$cptable[, "xerror"])

Tree_deal_2.cp <- Tree_deal_2$cptable[4,"CP"]
Tree_deal_2.cp

prune.tree_deal_2 <- prune(Tree_deal_2, cp = Tree_deal_2.cp )

rpart.plot(prune.tree_deal_2)
printcp(prune.tree_deal_2)

prediction_2 <- predict(prune.tree_deal_2, data_deal_test_2, type = "class")
table(data_deal_test_2$FATALITY_LOCATION, prediction_2)
confusionMatrix(table(data_deal_test_2$FATALITY_LOCATION, prediction_2))


##############################################################################
data_deal_train_3 <- rbind( data_deal_test_1, data_deal_test_2, data_deal_test_4, data_deal_test_5 )

Tree_deal_3 <- rpart( FATALITY_LOCATION ~., data_deal_train_3, method = "class" )
Tree_deal_3
rpart.plot(Tree_deal_3)
printcp(Tree_deal_3)


which.min(Tree_deal_3$cptable[, "xerror"])

Tree_deal_3.cp <- Tree_deal_3$cptable[3,"CP"]
Tree_deal_3.cp

prune.tree_deal_3 <- prune(Tree_deal_3, cp = Tree_deal_3.cp )

rpart.plot(prune.tree_deal_3)

printcp(prune.tree_deal_3)

prediction_3 <- predict(prune.tree_deal_3, data_deal_test_3, type = "class")
table(data_deal_test_3$FATALITY_LOCATION, prediction_3)
confusionMatrix(table(data_deal_test_3$FATALITY_LOCATION, prediction_3))

###########################################################################

data_deal_train_4 <- rbind( data_deal_test_1, data_deal_test_2, data_deal_test_3, data_deal_test_5 )

Tree_deal_4 <- rpart( FATALITY_LOCATION ~., data_deal_train_4, method = "class" )
Tree_deal_4
rpart.plot(Tree_deal_4)
printcp(Tree_deal_4)


which.min(Tree_deal_4$cptable[, "xerror"])

Tree_deal_4.cp <- Tree_deal_4$cptable[3,"CP"]
Tree_deal_4.cp

prune.tree_deal_4 <- prune(Tree_deal_4, cp = Tree_deal_4.cp )

rpart.plot(prune.tree_deal_4)

printcp(prune.tree_deal_4)

prediction_4 <- predict(prune.tree_deal_4, data_deal_test_4, type = "class")
table(data_deal_test_4$FATALITY_LOCATION, prediction_4)
confusionMatrix(table(data_deal_test_4$FATALITY_LOCATION, prediction_4))

##########################################################################

data_deal_train_5 <- rbind( data_deal_test_1, data_deal_test_2, data_deal_test_3, data_deal_test_4 )

Tree_deal_5 <- rpart( FATALITY_LOCATION ~., data_deal_train_5, method = "class" )
Tree_deal_5
rpart.plot(Tree_deal_5)
printcp(Tree_deal_5)


which.min(Tree_deal_5$cptable[, "xerror"])

Tree_deal_5.cp <- Tree_deal_5$cptable[5,"CP"]
Tree_deal_5.cp

prune.tree_deal_5 <- prune(Tree_deal_5, cp = Tree_deal_5.cp )

rpart.plot(prune.tree_deal_5)

printcp(prune.tree_deal_5)

prediction_5 <- predict(prune.tree_deal_5, data_deal_test_5, type = "class")
table(data_deal_test_5$FATALITY_LOCATION, prediction_5)
confusionMatrix(table(data_deal_test_5$FATALITY_LOCATION, prediction_5))

###########################################################################
