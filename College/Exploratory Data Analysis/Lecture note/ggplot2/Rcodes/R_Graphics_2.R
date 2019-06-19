
###### R Graphics Lecture 2 : Line Graphs ########

### install packages for graphics -- if you installed them, do not again
#pack_names <- c('ggplots','gcookbook')
#install.packages(pack_names)

### loading packages
library('ggplot2')      # install "ggplot2" package
library('gcookbook')    # install "gcookbook" for data sets 
# NOTE : same as "require('ggplot2')" and "require('gcookbook')"


## 2.1. Making a Basic Line Graph 
BOD
str(BOD)
ggplot(BOD, aes(x=Time, y=demand)) + geom_line()  # x is numeric

ggplot(BOD, aes(x=factor(Time), y=demand, group=1)) + geom_line() # x is a factor

# enlarge y-axis
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() +
  ylim(0,max(BOD$demand))


## 2.2. Adding Points to a Line Graph
ggplot(BOD, aes(x=Time,y=demand)) + geom_line() + geom_point()

str(worldpop)
p <- ggplot(worldpop, aes(x=Year,y=Population)) + geom_line() + geom_point()
p
p + scale_y_log10()


## 2.3. Making a Line Graph with Multiple Lines
str(ToothGrowth)
library(plyr)
tg <- ddply(ToothGrowth, c('supp','dose'), summarise, length=mean(len))
tg

# map supp to color
ggplot(tg, aes(x=dose, y=length, color=supp)) + geom_line()

# map supp to linetype
ggplot(tg, aes(x=dose, y=length, color=supp, linetype=supp)) + geom_line()

# points along with the lines
ggplot(tg, aes(x=dose, y=length, shape=supp)) + geom_line() + 
  geom_point(size=4)

ggplot(tg, aes(x=dose, y=length, fill=supp)) + geom_line() + 
  geom_point(size=4, shape=21)

# avoiding overlapping
ggplot(tg, aes(x=dose, y=length, shape=supp)) + 
  geom_line(position=position_dodge(0.05)) + 
  geom_point(position=position_dodge(0.05), size=4)


## 2.4. Changing the Appearance of Lines and Points
ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line(linetype='dashed', size=1, color='blue') +
  geom_point(size=10, shape=22, color='darkred', fill='pink')

ggplot(BOD, aes(x=Time, y=demand)) + geom_line() +
  geom_point(size=10, shape=21, fill='white')


## 2.5. Making a Graph with a Shaded Area 
# use "geom_area()"
str(sunspot.year)
# Convert the sunspot.year data set into a data frame for this example
sunspotyear <- data.frame(
  Year = as.numeric(time(sunspot.year)),
  Sunspots = as.numeric(sunspot.year)
)

ggplot(sunspotyear, aes(x=Year, y=Sunspots)) + geom_area()

ggplot(sunspotyear, aes(x=Year, y=Sunspots)) +
  geom_area(fill='blue', alpha=0.2) + geom_line()
# "alpha" adjusts the transparancy
# "geom_line()" adds the line over the area

#### start here!!!

## 2.6. Making a Stacked Area Graph
str(uspopage)
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()

# chaning appearance
p <- ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup))

p +  geom_area(color='black', size=0.2, alpha=0.4) +
  scale_fill_brewer(palette='Blues', breaks=rev(levels(uspopage$AgeGroup))) +
  theme_bw()
# "theme_bw()" makes the background color white

# removing the border
p +  geom_area(color=NA, size=0.2, alpha=0.4) +
  scale_fill_brewer(palette='Blues', breaks=rev(levels(uspopage$AgeGroup))) +
  theme_bw()

# removing the border only for legend
p +  geom_area(color=NA, size=0.2, alpha=0.4) +
  scale_fill_brewer(palette='Blues', breaks=rev(levels(uspopage$AgeGroup))) +
  geom_line(position='stack', size=0.2) +
  theme_bw()


## 2.7. Adding a Confidence Region
str(climate)
# The variable "Unc10y" is the standard deviation of "Anomaly10y"
clim <- subset(climate, Source=='Berkeley',
               select=c('Year', 'Anomaly10y', 'Unc10y'))
# Extract the subset from "climate" whose "Source" equal to 'Berkeley'
# and keep only three variables
p <- ggplot(clim, aes(x=Year, y=Anomaly10y))
# shaded region
p + geom_ribbon(aes(ymin=Anomaly10y-Unc10y, ymax=Anomaly10y+Unc10y), alpha=0.2) +
  geom_line()
# dotted lines
p + geom_line(aes(y=Anomaly10y-Unc10y), color='grey50', linetype='dotted') +
  geom_line(aes(y=Anomaly10y+Unc10y), color='grey50', linetype='dotted') + 
  geom_line(lwd=1)







