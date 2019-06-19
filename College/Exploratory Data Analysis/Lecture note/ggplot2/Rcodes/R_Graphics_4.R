
###### R Graphics Lecture 4 : Summarized Data Distributions ########

### install packages for graphics -- if you installed them, do not again
#pack_names <- c('ggplots','gcookbook')
#install.packages(pack_names)

### loading packages
library('ggplot2')      # install "ggplot2" package
library('gcookbook')    # install "gcookbook" for data sets 
# NOTE : same as "require('ggplot2')" and "require('gcookbook')"


## 4.1. Making a Basic Histogram 
ggplot(faithful, aes(x=waiting)) + geom_histogram()
# in the case that a variable is stored in a vector, not in a data frame
w<-rnorm(100)
ggplot(NULL, aes(x=w)) + geom_histogram()
# Set the width of each bin to 5
ggplot(faithful, aes(x=waiting)) + 
  geom_histogram(binwidth=5, fill='white', color='black')
# Divide the x range into 15 bins
binsize <- diff(range(faithful$waiting))/15
ggplot(faithful, aes(x=waiting)) +
  geom_histogram(binwidth=binsize, fill='white', color='black')

h <- ggplot(faithful, aes(x=waiting))
# origin = 31
h + geom_histogram(binwidth=8, fill='white', color='black', origin=31)
# origin = 35
h + geom_histogram(binwidth=8, fill='white', color='black', origin=35)


## 4.2. Making Multiple Histograms from Grouped Data
library(MASS)
ggplot(birthwt, aes(x=bwt)) + geom_histogram(fill='white', color='black') +
  facet_grid(smoke ~.)
# change the labels for smoke
birthwt1 <- birthwt
birthwt1$smoke <- factor(birthwt1$smoke, labels=c('No Smoke', 'Smoke'))
ggplot(birthwt1, aes(x=bwt)) + geom_histogram(fill='white', color='black') +
  facet_grid(smoke ~.)

ggplot(birthwt, aes(x=bwt)) + geom_histogram(fill='white', color='black') +
  facet_grid(race~.)
# scale='free'
ggplot(birthwt, aes(x=bwt)) + geom_histogram(fill='white', color='black') +
  facet_grid(race~., scale='free')
# change the facet
ggplot(birthwt, aes(x=bwt)) + geom_histogram(fill='white', color='black') +
  facet_grid(.~race)

# overlay the two histograms
ggplot(birthwt1, aes(x=bwt, fill=smoke)) +
  geom_histogram(position='identity', alpha=0.4)


## 4.3. Making a Density Curve
# default
ggplot(faithful, aes(x=waiting)) + geom_density()
# another way
ggplot(faithful, aes(x=waiting)) + geom_line(stat='density') + 
  expand_limits(y=0)
# expand limits
ggplot(faithful, aes(x=waiting)) + geom_density() + expand_limits(x=c(35,105))

# adding colors
ggplot(faithful, aes(x=waiting)) +
  geom_density(fill='blue', color=NA, alpha=0.2)+
  geom_line(stat='density') +
  xlim(35, 105)

# changing bandwidth
ggplot(faithful, aes(x=waiting)) +
  geom_line(stat='density', adjust=0.25, color='red') +
  geom_line(stat='density') + 
  geom_line(stat='density', adjust=2, color='blue')

# overlay the density curve onto histogram
ggplot(faithful, aes(x=waiting, y=..density..)) +
  geom_histogram(fill='cornsilk', color='grey60', size=0.2) +
  geom_density() + xlim(35,105)


## 4.4. Making Multiple Density Curves from Grouped Data
birthwt1 <- birthwt
birthwt1$smoke <- factor(birthwt1$smoke)
ggplot(birthwt1, aes(x=bwt, color=smoke)) + geom_density()
ggplot(birthwt1, aes(x=bwt, fill=smoke)) + geom_density(alpha=0.3)


## 4.5. Making a Basic Box Plot
ggplot(birthwt, aes(x=factor(race), y=bwt)) + geom_boxplot()
# changing width of boxes
ggplot(birthwt, aes(x=factor(race), y=bwt)) + geom_boxplot(width=0.5)
# changing the appearance of outliers
ggplot(birthwt, aes(x=factor(race), y=bwt)) + 
  geom_boxplot(outlier.size=3, outlier.shape=21)
# no grouping
ggplot(birthwt, aes(x=1, y=bwt)) + geom_boxplot() +
  scale_x_continuous(breaks=NULL) +
  theme(axis.title.x = element_blank())


## 4.6. Adding Means to a Box Plot
ggplot(birthwt, aes(x=factor(race), y=bwt)) + geom_boxplot() +
  stat_summary(fun.y='mean', geom='point', shape=23, size=3, fill='white')


## 4.7. Making a Dot Plot
countries2009 <- subset(countries, Year==2009 & healthexp>2000)
p <- ggplot(countries2009, aes(x=infmortality))

# default
p + geom_dotplot()  

# remove tick marks and axis label, and add rug
p + geom_dotplot(binwidth=0.25) + geom_rug() +
  scale_y_continuous(breaks=NULL) + # remove tick marks
  theme(axis.title.y=element_blank()) # remove axis label

p + geom_dotplot(binwidth=0.25, stackdir='center') + 
  scale_y_continuous(breaks=NULL) + # remove tick marks
  theme(axis.title.y=element_blank()) # remove axis label


## 4.8. Making a Density Plot of Two-Dimensional Data
p <- ggplot(faithful, aes(x=eruptions, y=waiting))
p + geom_point() + stat_density2d()
p + stat_density2d()
p + stat_density2d(aes(color=..level..))
p + stat_density2d(aes(fill=..density..), geom='raster', contour=FALSE)
p + geom_point() +
  stat_density2d(aes(alpha=..density..), geom='tile', contour=FALSE)
