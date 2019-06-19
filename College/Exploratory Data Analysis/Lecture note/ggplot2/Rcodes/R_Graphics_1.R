
###### R Graphics Lecture 1 : Bar Graphs ########

### install packages for graphics -- if you installed them, do not again
#pack_names <- c('ggplots','gcookbook')
#install.packages(pack_names)

### loading packages
library('ggplot2')      # load "ggplot2" package
library('gcookbook')    # load "gcookbook" for data sets 
# NOTE : same as "require('ggplot2')" and "require('gcookbook')"

### 1. BAR GRAPHS

## 1.1. Making a Basic Bar Graph -- x variable is a factor
pg_mean
str(pg_mean)
# method 1
ggplot(pg_mean, aes(x=group, y=weight)) + 
  geom_bar(stat='identity')
# method 2, creating the same graph by method 1
p <- ggplot(pg_mean, aes(x=group, y=weight))
p + geom_bar(stat='identity')

## 1.1. Making a Basic Bar Graph -- x variable is not a factor
str(BOD)
ggplot(BOD, aes(x=Time, y=demand)) + geom_bar(stat='identity')
# option 1 - Convert Time to a discrete (categorical) variable with factor()
ggplot(BOD, aes(x=factor(Time), y=demand)) + geom_bar(stat='identity')
# option 2 - Creating a new data set having a factor variable
BOD2 <- BOD
BOD2$fTime <- factor(BOD$Time)
str(BOD2)
ggplot(BOD2, aes(x=fTime, y=demand)) + geom_bar(stat='identity')

## changing color
ggplot(pg_mean, aes(x=group, y=weight)) + 
  geom_bar(stat='identity', fill='lightblue', color='black')
# "fill" for the color which fills in rectangles
# "color" for the color of lines (or points)
# "colour" (British) is available instead of "color" (American)


## 1.2. Grouping Bar Together
cabbage_exp
str(cabbage_exp)

# use "fill" and "geom_bar(position='dodge')"
p <- ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar))
p + geom_bar(stat='identity', position='dodge')

# change colors
p + geom_bar(stat='identity', position='dodge', color='black') + 
  scale_fill_brewer(palette='Pastel1')

# missing cases exist
ce <- cabbage_exp
ce[c(1,6),3] <- NA
ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat='identity', position='dodge', color='black') +
  scale_fill_brewer(palette='Pastel1')

# missing cases exist 2
ce2 <- cabbage_exp[-c(1,6),]
ggplot(ce2, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat='identity', position='dodge', color='black') +
  scale_fill_brewer(palette='Pastel1')


## 1.3. Making a Bar Graph of Counts
str(diamonds)
ggplot(diamonds, aes(x=cut)) + geom_bar()
# setting x by a continuous variable yields a histogram
ggplot(diamonds, aes(x=carat)) + geom_bar()
# same histogram
ggplot(diamonds, aes(x=carat)) + geom_histogram()


## 1.4. Using Colors in a Bar Graph
str(uspopchange)
head(uspopchange)
upc <- subset(uspopchange, rank(Change)>40)
upc
ggplot(upc, aes(x=Abb, y=Change, fill=Region)) + geom_bar(stat='identity')
# reorder "Abb" and change colors
ggplot(upc, aes(x=reorder(Abb,Change), y=Change, fill=Region)) +
  geom_bar(stat='identity', color='black') +
  scale_fill_manual(values=c('#669933','#FFCC66')) +
  xlab('State')


## 1.5. Coloring Negative and Positive Bars Differently
?climate  # same as help(climate)
# NOTE: A data set can be put in "help()" as an argument 
str(climate)
unique(climate$Source)
# "Berkeley" only with "Year" >= 1900
csub <- subset(climate, Source=='Berkeley' & Year >= 1900)
# create a variable "pos" indicating nonnegative value of "Anomaly10y"
csub$pos <- csub$Anomaly10y >= 0
head(csub)  # show the first 6 rows
tail(csub)  # show the last 6 rows
p <- ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos))
# default colors
p + geom_bar(stat='identity', position='identity')
# change colors and remove legend
p + geom_bar(stat='identity', position='identity', color='black', size=0.25) +
  scale_fill_manual(values=c('#CCEEFF','#FFDDDD'), guide=FALSE)
# NOTE: "size" tunes the thickness of borderline


## 1.6. Adjusting Bar Width and Spacing
# spacing - one group
p <- ggplot(pg_mean, aes(x=group, y=weight))
p + geom_bar(stat='identity') # default width=0.9
p + geom_bar(stat='identity', width=0.5)  # narrower
p + geom_bar(stat='identity', width=1)  # wider

# spacing - multiple groups
p <- ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar))
p + geom_bar(stat='identity', width=0.5, position='dodge')
p + geom_bar(stat='identity', width=0.5, position=position_dodge(0.7))
# EXERCISE: change "width" and value of "position_dodge()"


## 1.7. Making a Stacked Bar Graph
# remove "position" option (or aesthetic)
p + geom_bar(stat='identity')

# reverse the order of groups
library(plyr) # for "desc()"
# NOTE: "plyr" package was automatically installed when you installed "ggplot2"
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar, 
                        order=desc(Cultivar))) + 
  geom_bar(stat='identity')
# "desc()" make values in descending order


## 1.8. Adding Labels to a Bar Graph
p <- ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), 
                             y=Weight)) + 
  geom_bar(stat='identity')
# try this first
p + geom_text(aes(label=Weight))
# Below the top - use "vjust"
p + geom_text(aes(label=Weight), vjust=1.5, color='white')
# Above the top - use "vjust"
p + geom_text(aes(label=Weight), vjust=-0.2)




