---
title: "reproducible_research_project_1"
author: "Juan Osobampo"
date: "16/12/2020"
output: html_document
---


```r
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(gridExtra)
library(dplyr)
zipUrl <-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zipFile <- "repdata_data_activity.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

# unzip zip file containing data if data directory doesn't already exist
dataPath <- "activity.csv"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}
activity <- read.csv("activity.csv")
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
#Turn the column into date class
activity$date <- as.Date(activity$date, tryFormats = c("%Y-%m-%d"))
#Creating histogram
ag_act <- aggregate(activity$steps , by= list(activity$date), FUN = sum)

#Replace NA's in the DF to 0
ag_act2<- ag_act
ag_act2[is.na(ag_act2)] <- 0

plot1 <- ggplot(ag_act2, aes(Group.1, x)) + geom_histogram(stat='identity')+ xlab("Date") + ylab("Steps")+ ggtitle("Steps by Day")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

```r
plot1
```

![plot of chunk Changing type](figure/Changing type-1.png)

```r
#As there are  NA values in activity df, we change those to 0 to get means
activity2<- activity
activity2[is.na(activity2)] <- 0
p3 <- aggregate(activity2$steps , by= list(activity2$date), FUN = mean)
ps3 <- ggplot(p3, aes(Group.1, x)) + geom_histogram(stat='identity')+ xlab("Date") + ylab("Steps")+ ggtitle("Mean Steps by Day")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

```r
ps3
```

![plot of chunk step 3 of coursera assignment](figure/step 3 of coursera assignment-1.png)

```r
#Median of steps recorded by day
median_and_mean_steps <- summary(p3$x)
median_and_mean_steps
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00   23.53   36.09   32.48   44.48   73.59
```
#### As for the step 4 instruction, we are plotting the mean number of steps by day

```r
ps3 <- ggplot(p3, aes(Group.1, x)) + geom_line(stat='identity')+ xlab("Date") + ylab("Steps")+ ggtitle("Mean Steps by Day")
ps3
```

![plot of chunk step 4](figure/step 4-1.png)

```r
median_and_mean_steps_ps3 <- summary(ps3)
```

```
## data: Group.1, x [61x2]
## mapping:  x = ~Group.1, y = ~x
## faceting: <ggproto object: Class FacetNull, Facet, gg>
##     compute_layout: function
##     draw_back: function
##     draw_front: function
##     draw_labels: function
##     draw_panels: function
##     finish_data: function
##     init_scales: function
##     map_data: function
##     params: list
##     setup_data: function
##     setup_params: function
##     shrink: TRUE
##     train_scales: function
##     vars: function
##     super:  <ggproto object: Class FacetNull, Facet, gg>
## -----------------------------------
## geom_line: na.rm = FALSE, orientation = NA, flipped_aes = FALSE
## stat_identity: na.rm = FALSE
## position_identity
```

#### For the step 5, we must get the most steps possible in a 5-min interval


```r
days <- as.data.frame( weekdays(as.Date(activity2$date)))
ag_act3 <- as.data.frame(bind_cols( activity2, days))
max_steps <- max(activity2$steps)
match(max_steps, table= activity2$steps)
```

```
## [1] 16492
```

```r
activity2[16492,]
```

```
##       steps       date interval
## 16492   806 2012-11-27      615
```

```r
plot_step5 <- ggplot(ag_act3, aes(interval, steps)) + geom_line(stat='identity')+ xlab("5-min Intervals") + ylab("Steps")+ ggtitle("Steps by Interval")
plot_step5
```

![plot of chunk step 5](figure/step 5-1.png)

#### For the step 6, we are asked for an strategy to work with NA's in the data frame, an option is to turn them into 0
-6.1. To work with the data frame, we change de date column into Date class

```r
 #Turn the column into date class
activity$date <- as.Date(activity$date, tryFormats = c("%Y-%m-%d"))
```
-6.2. To avoid multiple same-day data, we use aggregate
 
 ```r
 #Creating histogram
 ag_act <- aggregate(activity$steps , by= list(activity$date), FUN = sum)
 ```
-6.3. Finally, we count the amount of NA, then change'em to 0 and store them into ag_act2

```r
# Count na
NA_number <- colSums(is.na(ag_act))
NA_number
```

```
## Group.1       x 
##       0       8
```

```r
# There are 8 NA
 #Replace NA's in the DF to 0
ag_act2<- ag_act
ag_act2[is.na(ag_act2)] <- 0
```

### For the step 7, we include  the NA(now 0) and plot it

```r
plot1 <- ggplot(ag_act2, aes(Group.1, x)) + geom_histogram(stat='identity')+ xlab("Date") + ylab("Steps")+ ggtitle("Steps by Day")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

```r
plot1
```

![plot of chunk step 7](figure/step 7-1.png)
###A dataset identical to the original is now stored un ag_act3 , being the main difference that NAs are now 0, in addition, there are the names of the day next to the dates

```r
ag_act3
```

```
##     steps       date interval weekdays(as.Date(activity2$date))
## 1       0 2012-10-01        0                             lunes
## 2       0 2012-10-01        5                             lunes
## 3       0 2012-10-01       10                             lunes
## 4       0 2012-10-01       15                             lunes
## 5       0 2012-10-01       20                             lunes
## 6       0 2012-10-01       25                             lunes
## 7       0 2012-10-01       30                             lunes
## 8       0 2012-10-01       35                             lunes
## 9       0 2012-10-01       40                             lunes
## 10      0 2012-10-01       45                             lunes
## 11      0 2012-10-01       50                             lunes
## 12      0 2012-10-01       55                             lunes
## 13      0 2012-10-01      100                             lunes
## 14      0 2012-10-01      105                             lunes
## 15      0 2012-10-01      110                             lunes
## 16      0 2012-10-01      115                             lunes
## 17      0 2012-10-01      120                             lunes
## 18      0 2012-10-01      125                             lunes
## 19      0 2012-10-01      130                             lunes
## 20      0 2012-10-01      135                             lunes
## 21      0 2012-10-01      140                             lunes
## 22      0 2012-10-01      145                             lunes
## 23      0 2012-10-01      150                             lunes
## 24      0 2012-10-01      155                             lunes
## 25      0 2012-10-01      200                             lunes
## 26      0 2012-10-01      205                             lunes
## 27      0 2012-10-01      210                             lunes
## 28      0 2012-10-01      215                             lunes
## 29      0 2012-10-01      220                             lunes
## 30      0 2012-10-01      225                             lunes
## 31      0 2012-10-01      230                             lunes
## 32      0 2012-10-01      235                             lunes
## 33      0 2012-10-01      240                             lunes
## 34      0 2012-10-01      245                             lunes
## 35      0 2012-10-01      250                             lunes
## 36      0 2012-10-01      255                             lunes
## 37      0 2012-10-01      300                             lunes
## 38      0 2012-10-01      305                             lunes
## 39      0 2012-10-01      310                             lunes
## 40      0 2012-10-01      315                             lunes
## 41      0 2012-10-01      320                             lunes
## 42      0 2012-10-01      325                             lunes
## 43      0 2012-10-01      330                             lunes
## 44      0 2012-10-01      335                             lunes
## 45      0 2012-10-01      340                             lunes
## 46      0 2012-10-01      345                             lunes
## 47      0 2012-10-01      350                             lunes
## 48      0 2012-10-01      355                             lunes
## 49      0 2012-10-01      400                             lunes
## 50      0 2012-10-01      405                             lunes
## 51      0 2012-10-01      410                             lunes
## 52      0 2012-10-01      415                             lunes
## 53      0 2012-10-01      420                             lunes
## 54      0 2012-10-01      425                             lunes
## 55      0 2012-10-01      430                             lunes
## 56      0 2012-10-01      435                             lunes
## 57      0 2012-10-01      440                             lunes
## 58      0 2012-10-01      445                             lunes
## 59      0 2012-10-01      450                             lunes
## 60      0 2012-10-01      455                             lunes
## 61      0 2012-10-01      500                             lunes
## 62      0 2012-10-01      505                             lunes
## 63      0 2012-10-01      510                             lunes
## 64      0 2012-10-01      515                             lunes
## 65      0 2012-10-01      520                             lunes
## 66      0 2012-10-01      525                             lunes
## 67      0 2012-10-01      530                             lunes
## 68      0 2012-10-01      535                             lunes
## 69      0 2012-10-01      540                             lunes
## 70      0 2012-10-01      545                             lunes
## 71      0 2012-10-01      550                             lunes
## 72      0 2012-10-01      555                             lunes
## 73      0 2012-10-01      600                             lunes
## 74      0 2012-10-01      605                             lunes
## 75      0 2012-10-01      610                             lunes
## 76      0 2012-10-01      615                             lunes
## 77      0 2012-10-01      620                             lunes
## 78      0 2012-10-01      625                             lunes
## 79      0 2012-10-01      630                             lunes
## 80      0 2012-10-01      635                             lunes
## 81      0 2012-10-01      640                             lunes
## 82      0 2012-10-01      645                             lunes
## 83      0 2012-10-01      650                             lunes
## 84      0 2012-10-01      655                             lunes
## 85      0 2012-10-01      700                             lunes
## 86      0 2012-10-01      705                             lunes
## 87      0 2012-10-01      710                             lunes
## 88      0 2012-10-01      715                             lunes
## 89      0 2012-10-01      720                             lunes
## 90      0 2012-10-01      725                             lunes
## 91      0 2012-10-01      730                             lunes
## 92      0 2012-10-01      735                             lunes
## 93      0 2012-10-01      740                             lunes
## 94      0 2012-10-01      745                             lunes
## 95      0 2012-10-01      750                             lunes
## 96      0 2012-10-01      755                             lunes
## 97      0 2012-10-01      800                             lunes
## 98      0 2012-10-01      805                             lunes
## 99      0 2012-10-01      810                             lunes
## 100     0 2012-10-01      815                             lunes
## 101     0 2012-10-01      820                             lunes
## 102     0 2012-10-01      825                             lunes
## 103     0 2012-10-01      830                             lunes
## 104     0 2012-10-01      835                             lunes
## 105     0 2012-10-01      840                             lunes
## 106     0 2012-10-01      845                             lunes
## 107     0 2012-10-01      850                             lunes
## 108     0 2012-10-01      855                             lunes
## 109     0 2012-10-01      900                             lunes
## 110     0 2012-10-01      905                             lunes
## 111     0 2012-10-01      910                             lunes
## 112     0 2012-10-01      915                             lunes
## 113     0 2012-10-01      920                             lunes
## 114     0 2012-10-01      925                             lunes
## 115     0 2012-10-01      930                             lunes
## 116     0 2012-10-01      935                             lunes
## 117     0 2012-10-01      940                             lunes
## 118     0 2012-10-01      945                             lunes
## 119     0 2012-10-01      950                             lunes
## 120     0 2012-10-01      955                             lunes
## 121     0 2012-10-01     1000                             lunes
## 122     0 2012-10-01     1005                             lunes
## 123     0 2012-10-01     1010                             lunes
## 124     0 2012-10-01     1015                             lunes
## 125     0 2012-10-01     1020                             lunes
## 126     0 2012-10-01     1025                             lunes
## 127     0 2012-10-01     1030                             lunes
## 128     0 2012-10-01     1035                             lunes
## 129     0 2012-10-01     1040                             lunes
## 130     0 2012-10-01     1045                             lunes
## 131     0 2012-10-01     1050                             lunes
## 132     0 2012-10-01     1055                             lunes
## 133     0 2012-10-01     1100                             lunes
## 134     0 2012-10-01     1105                             lunes
## 135     0 2012-10-01     1110                             lunes
## 136     0 2012-10-01     1115                             lunes
## 137     0 2012-10-01     1120                             lunes
## 138     0 2012-10-01     1125                             lunes
## 139     0 2012-10-01     1130                             lunes
## 140     0 2012-10-01     1135                             lunes
## 141     0 2012-10-01     1140                             lunes
## 142     0 2012-10-01     1145                             lunes
## 143     0 2012-10-01     1150                             lunes
## 144     0 2012-10-01     1155                             lunes
## 145     0 2012-10-01     1200                             lunes
## 146     0 2012-10-01     1205                             lunes
## 147     0 2012-10-01     1210                             lunes
## 148     0 2012-10-01     1215                             lunes
## 149     0 2012-10-01     1220                             lunes
## 150     0 2012-10-01     1225                             lunes
## 151     0 2012-10-01     1230                             lunes
## 152     0 2012-10-01     1235                             lunes
## 153     0 2012-10-01     1240                             lunes
## 154     0 2012-10-01     1245                             lunes
## 155     0 2012-10-01     1250                             lunes
## 156     0 2012-10-01     1255                             lunes
## 157     0 2012-10-01     1300                             lunes
## 158     0 2012-10-01     1305                             lunes
## 159     0 2012-10-01     1310                             lunes
## 160     0 2012-10-01     1315                             lunes
## 161     0 2012-10-01     1320                             lunes
## 162     0 2012-10-01     1325                             lunes
## 163     0 2012-10-01     1330                             lunes
## 164     0 2012-10-01     1335                             lunes
## 165     0 2012-10-01     1340                             lunes
## 166     0 2012-10-01     1345                             lunes
## 167     0 2012-10-01     1350                             lunes
## 168     0 2012-10-01     1355                             lunes
## 169     0 2012-10-01     1400                             lunes
## 170     0 2012-10-01     1405                             lunes
## 171     0 2012-10-01     1410                             lunes
## 172     0 2012-10-01     1415                             lunes
## 173     0 2012-10-01     1420                             lunes
## 174     0 2012-10-01     1425                             lunes
## 175     0 2012-10-01     1430                             lunes
## 176     0 2012-10-01     1435                             lunes
## 177     0 2012-10-01     1440                             lunes
## 178     0 2012-10-01     1445                             lunes
## 179     0 2012-10-01     1450                             lunes
## 180     0 2012-10-01     1455                             lunes
## 181     0 2012-10-01     1500                             lunes
## 182     0 2012-10-01     1505                             lunes
## 183     0 2012-10-01     1510                             lunes
## 184     0 2012-10-01     1515                             lunes
## 185     0 2012-10-01     1520                             lunes
## 186     0 2012-10-01     1525                             lunes
## 187     0 2012-10-01     1530                             lunes
## 188     0 2012-10-01     1535                             lunes
## 189     0 2012-10-01     1540                             lunes
## 190     0 2012-10-01     1545                             lunes
## 191     0 2012-10-01     1550                             lunes
## 192     0 2012-10-01     1555                             lunes
## 193     0 2012-10-01     1600                             lunes
## 194     0 2012-10-01     1605                             lunes
## 195     0 2012-10-01     1610                             lunes
## 196     0 2012-10-01     1615                             lunes
## 197     0 2012-10-01     1620                             lunes
## 198     0 2012-10-01     1625                             lunes
## 199     0 2012-10-01     1630                             lunes
## 200     0 2012-10-01     1635                             lunes
## 201     0 2012-10-01     1640                             lunes
## 202     0 2012-10-01     1645                             lunes
## 203     0 2012-10-01     1650                             lunes
## 204     0 2012-10-01     1655                             lunes
## 205     0 2012-10-01     1700                             lunes
## 206     0 2012-10-01     1705                             lunes
## 207     0 2012-10-01     1710                             lunes
## 208     0 2012-10-01     1715                             lunes
## 209     0 2012-10-01     1720                             lunes
## 210     0 2012-10-01     1725                             lunes
## 211     0 2012-10-01     1730                             lunes
## 212     0 2012-10-01     1735                             lunes
## 213     0 2012-10-01     1740                             lunes
## 214     0 2012-10-01     1745                             lunes
## 215     0 2012-10-01     1750                             lunes
## 216     0 2012-10-01     1755                             lunes
## 217     0 2012-10-01     1800                             lunes
## 218     0 2012-10-01     1805                             lunes
## 219     0 2012-10-01     1810                             lunes
## 220     0 2012-10-01     1815                             lunes
## 221     0 2012-10-01     1820                             lunes
## 222     0 2012-10-01     1825                             lunes
## 223     0 2012-10-01     1830                             lunes
## 224     0 2012-10-01     1835                             lunes
## 225     0 2012-10-01     1840                             lunes
## 226     0 2012-10-01     1845                             lunes
## 227     0 2012-10-01     1850                             lunes
## 228     0 2012-10-01     1855                             lunes
## 229     0 2012-10-01     1900                             lunes
## 230     0 2012-10-01     1905                             lunes
## 231     0 2012-10-01     1910                             lunes
## 232     0 2012-10-01     1915                             lunes
## 233     0 2012-10-01     1920                             lunes
## 234     0 2012-10-01     1925                             lunes
## 235     0 2012-10-01     1930                             lunes
## 236     0 2012-10-01     1935                             lunes
## 237     0 2012-10-01     1940                             lunes
## 238     0 2012-10-01     1945                             lunes
## 239     0 2012-10-01     1950                             lunes
## 240     0 2012-10-01     1955                             lunes
## 241     0 2012-10-01     2000                             lunes
## 242     0 2012-10-01     2005                             lunes
## 243     0 2012-10-01     2010                             lunes
## 244     0 2012-10-01     2015                             lunes
## 245     0 2012-10-01     2020                             lunes
## 246     0 2012-10-01     2025                             lunes
## 247     0 2012-10-01     2030                             lunes
## 248     0 2012-10-01     2035                             lunes
## 249     0 2012-10-01     2040                             lunes
## 250     0 2012-10-01     2045                             lunes
##  [ reached 'max' / getOption("max.print") -- omitted 17318 rows ]
```

```r
summary(ag_act3)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 32.48   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.:  0.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  weekdays(as.Date(activity2$date))
##  Length:17568                     
##  Class :character                 
##  Mode  :character                 
##                                   
##                                   
## 
```
### For the step 8 we must find the name of the day that corresponds the date, then we subset by the day of the week to plot the steps by 5-min intervals, to display all 7 plot we use the require(gridExtra) function

```r
monday <- subset.data.frame(ag_act3, ag_act3$`weekdays(as.Date(activity2$date))` == "lunes", select = c(steps, interval))
tuesday <- subset.data.frame(ag_act3, ag_act3$`weekdays(as.Date(activity2$date))` == "martes", select = c(steps, interval))
wednesday <- subset.data.frame(ag_act3, ag_act3$`weekdays(as.Date(activity2$date))` == "miércoles", select = c(steps, interval))
thursday <- subset.data.frame(ag_act3, ag_act3$`weekdays(as.Date(activity2$date))` == "jueves", select = c(steps, interval))
friday <- subset.data.frame(ag_act3, ag_act3$`weekdays(as.Date(activity2$date))` == "viernes", select = c(steps, interval))
saturday <- subset.data.frame(ag_act3, ag_act3$`weekdays(as.Date(activity2$date))` == "sábado", select = c(steps, interval))
sunday <- subset.data.frame(ag_act3, ag_act3$`weekdays(as.Date(activity2$date))` == "domingo", select = c(steps, interval))
require(gridExtra)
ps8.1 <- ggplot(monday, aes(interval, steps)) + geom_line(stat='identity')+ xlab("Monday 5-min Intervals") + ylab("Steps")+ ggtitle("Monday Steps by Interval")
ps8.2 <- ggplot(tuesday, aes(interval, steps)) + geom_line(stat='identity')+ xlab("Tuesday 5-min Intervals") + ylab("Steps")+ ggtitle("Tuesday Steps by Interval")
ps8.3 <- ggplot(wednesday, aes(interval, steps)) + geom_line(stat='identity')+ xlab("Wednesday 5-min Intervals") + ylab("Steps")+ ggtitle("Wednesday Steps by Interval")
ps8.4 <- ggplot(thursday, aes(interval, steps)) + geom_line(stat='identity')+ xlab("Thursday 5-min Intervals") + ylab("Steps")+ ggtitle("Thursday Steps by Interval")
ps8.5 <- ggplot(friday, aes(interval, steps)) + geom_line(stat='identity')+ xlab("Friday 5-min Intervals") + ylab("Steps")+ ggtitle("Friday Steps by Interval")
ps8.6 <- ggplot(saturday, aes(interval, steps)) + geom_line(stat='identity')+ xlab("Saturday 5-min Intervals") + ylab("Steps")+ ggtitle("Saturday Steps by Interval")
ps8.7 <- ggplot(sunday, aes(interval, steps)) + geom_line(stat='identity' )+ xlab("Sunday 5-min Intervals") + ylab("Steps")+ ggtitle("Sunday Steps by Interval")
require(gridExtra)
grid.arrange(ps8.1, ps8.2, ps8.3, ps8.4, ps8.5, ps8.6, ps8.7, ncol=7)
```

![plot of chunk step 8](figure/step 8-1.png)
