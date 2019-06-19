
###### R Graphics Lecture 6 : Axes ########

### install packages for graphics -- if you installed them, do not again
#pack_names <- c('ggplots','gcookbook')
#install.packages(pack_names)

### loading packages
library('ggplot2')      # install "ggplot2" package
library('gcookbook')    # install "gcookbook" for data sets 
# NOTE : same as "require('ggplot2')" and "require('gcookbook')"


## 6.1. Swapping X- and Y-Axes 
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() + coord_flip()
# change the order of x
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() + coord_flip() +
  scale_x_discrete(limits=rev(levels(PlantGrowth$group)))


## 6.2. Setting the Range of a Continuous Axis
p <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()
p 
p + ylim(0, max(PlantGrowth$weight))
# "ylim()" is shorthand for setting the limits with "scale_y_continuous()"
p + scale_y_continuous(limits=c(0,max(PlantGrowth$weight)))

# note the difference:
p + scale_y_continuous(limits=c(5,6.5)) # redraw boxplots within the limit
p + coord_cartesian(ylim=c(5,6.5))  # magnify the limit region


## 6.3. Reversing a Continuous Axis
p + scale_y_reverse()


## 6.4. Changing the Order of Items on a Categorical Axis
p + scale_x_discrete(limits=c('trt1','ctrl','trt2'))
p + scale_x_discrete(limits=c('ctrl','trt1'))


## 6.5. Setting the Positions of Tick Marks
p
p + scale_y_continuous(breaks=c(4, 4.25, 4.5, 5, 6, 8))
mytick <- seq(3.5, 7, by=0.2)
p + scale_y_continuous(breaks=mytick) + 
  scale_x_discrete(limits=c('ctrl','trt1'),breaks='ctrl')


## 6.6. Removing Tick Marks and Labels
p + theme(axis.text.y=element_blank())  # remove y tick labels
p + theme(axis.ticks=element_blank()) # remove tick marks on X- and Y-axis
p + theme(axis.ticks=element_blank(), axis.text.y=element_blank())
p + scale_y_continuous(breaks=NULL) # remove grids on y


## 6.7. Changing the Text of Tick Labels
hwp <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
hwp
hwp + scale_y_continuous(breaks=c(50,56,60,66,72),
                         labels=c('Tiny', 'Really\nshort', 'Short', 
                                  'Medium', 'Tallish'))

# formatting inches into foot/inches
footinch_formatter <- function(x){
  foot <- floor(x/12)
  inch <- x %% 12
  return(paste(foot,"'", inch, "\"", sep=''))
}
footinch_formatter(56:64)
hwp + scale_y_continuous(labels=footinch_formatter)
hwp + scale_y_continuous(breaks=seq(48,72,by=4), labels=footinch_formatter)

# time formatter from minutes to HMS
timeHMS_formatter <- function(x){
  h <- floor(x/60)  # hour
  m <- floor(x %% 60) # minute
  s <- round(60*(x %% 1)) # Round to nearest second
  lab <- sprintf("%02d:%02d:%02d", h, m, s) # Format the strings as HH:MM:SS
  lab <- gsub("^00:", "", lab)  # Remove leading 00: if present
  lab <- gsub("^0", "", lab)  # Remove leading 0 if present
  return (lab)
}
timeHMS_formatter(c(.33, 50, 51, 51.25, 60, 60.1, 130.23, 1234))
# other built-in formatters in "scales" package
# comma() : adds commas to numbers, in the thousand, million, billion, etc. places
# dollar() : adds a dollar sign and rounds to the nearest cent
# percent() : multiplies by 100, rounds to the nearest integer, and adds a percent sign
# scientific() : gives numbers in scientific notation, like 3.30e+05


## 6.8. Changing the Appearance of Tick Labels
bp <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() + 
  scale_x_discrete(breaks=c('ctrl','trt1','trt2'),
                   labels=c('Control','Treatment 1', 'Treatment 2'))
bp
bp + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
bp + theme(axis.text.x=element_text(angle=30, hjust=1, vjust=1))
bp + theme(axis.text.x=element_text(family='Times', face='italic', color='darkred',
                                    size=rel(2)))
install.packages('extrafont')
library(extrafont)
font_import()  # take more than 5 minutes
fonts()

## 6.9. Changing the Text of Axis Labels
hwp <- ggplot(heightweight, aes(x=ageYear, y=heightIn, color=sex)) + geom_point()
hwp

# set the axis labels
hwp + xlab('Age in years') + ylab('Height in inches')
hwp + labs(x='Age in years', y='Height in inches')
hwp + scale_y_continuous(name='Height (inches)') +
  scale_x_continuous(name='Age\n(years)')


## 6.10. Removing Axis Labels
p <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()
p
p + theme(axis.title.x=element_blank())
p + xlab('')


## 6.11. Changing the Appearance of Axis Labels
hwp <- ggplot(heightweight, aes(x=ageYear, y=heightIn, color=sex)) + geom_point()
hwp + theme(axis.title.x=element_text(face='italic',color='darkred',size=14)) +
  theme(axis.title.y=element_text(face='italic',angle=90,color='darkred',size=14)) +
  ylab('Height\n(Inches)')


## 6.12. Showing Lines Along the Axes
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
p
p + theme(axis.line=element_line(color='black'))
p + theme_bw()
p + theme_bw() + theme(panel.border=element_blank(), panel.grid=element_blank(),
                       axis.line=element_line(color='black'))
p + theme_bw() + theme(panel.border=element_blank(), 
                       axis.line=element_line(color='black',size=4, lineend='square'))



