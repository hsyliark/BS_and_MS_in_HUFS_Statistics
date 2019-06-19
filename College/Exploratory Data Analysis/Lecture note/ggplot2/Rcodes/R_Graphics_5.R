
###### R Graphics Lecture 5 : Annotations ########

### install packages for graphics -- if you installed them, do not again
#pack_names <- c('ggplots','gcookbook')
#install.packages(pack_names)

### loading packages
library('ggplot2')      # install "ggplot2" package
library('gcookbook')    # install "gcookbook" for data sets 
# NOTE : same as "require('ggplot2')" and "require('gcookbook')"


## 5.1. Adding Text Annotations 
p <- ggplot(faithful, aes(x=eruptions, y=waiting)) + geom_point()

p + annotate('text', x=3, y=48, label='Group 1') +
  annotate('text', x=4.5, y=66, label='Group 2')

# change the appearance of texts
p + annotate('text', x=3, y=48, label='Group 1', family='serif',
             fontface='italic', color='darkred', size=6) +
  annotate('text', x=4.5, y=66, label='Group 2', family='serif',
           fontface='bold', color='magenta', size=6)

# change the position of texts
p + annotate('text', x=-Inf, y=Inf, label='Upper left', hjust=-0.2, vjust=2) +
  annotate('text', x=mean(range(faithful$eruptions)), y=-Inf, vjust=-0.4,
           label='Bottom middle')


## 5.2. Using Mathematical Expressions in Annotations
# A normal curve
p <- ggplot(data.frame(x=c(-3,3)), aes(x=x)) + stat_function(fun=dnorm)
p + annotate('text', x=2, y=0.3, parse=TRUE, size=6,
             label='f(x) == frac(1, sqrt(2*pi))* e^{-x^2 /2}')
  p + annotate('text', x=0, y=0.05, parse=TRUE, size=6,
             label="'Function:  ' * y == frac(1, sqrt(2*pi))* e^{-x^2 /2}")
# For including texts, use ''. 
# In this case you should use label="" (double qoutations)

?plotmath
demo(plotmath)  # demo for math expressions


## 5.3. Adding Lines
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn, color=sex)) + geom_point()
p + geom_hline(yintercept=60) + # horizontal line
  geom_vline(xintercept=14) # vertical line
# add angled line
p + geom_abline(intercept=37.4, slope=1.75)

# separate mean for each groups
library(plyr)
hw_means <- ddply(heightweight, 'sex', summarise, heightIn=mean(heightIn))
hw_means
p + geom_hline(data=hw_means, aes(yintercept=heightIn, color=sex), 
               linetype='dashed', size=1)


## 5.4. Adding Line Segments and Arrows
p <- ggplot(subset(climate, Source=='Berkeley'), aes(x=Year, y=Anomaly10y)) + 
  geom_line()
p + annotate('segment', x=1950, xend=1980, y=-0.25, yend=-0.25)
# arrow() from "grid" package
library(grid)
p + annotate('segment', x=1850, xend=1820, y=-0.8, yend=-0.95, color='blue',
            size=2, arrow=arrow()) +
  annotate('segment', x=1950, xend=1980, y=-0.25, yend=-0.25,
           arrow=arrow(ends='both', angle=90, length=unit(0.2,'cm'))) +
  annotate('text', x=1875, y=-0.8, color='red', size=7, label='minimum')


## 5.5. Adding a Shaded Rectangle
p <- ggplot(subset(climate, Source=='Berkeley'), aes(x=Year, y=Anomaly10y)) + 
  geom_line()
p + annotate('rect', xmin=1950, xmax=1980, ymin=-1, ymax=1, 
             alpha=0.1, fill='blue')


## 5.6. Adding Error Bars
ce <- subset(cabbage_exp, Cultivar=='c39')
# With a bar graph
ggplot(ce, aes(x=Date, y=Weight)) +
  geom_bar(fill='white', color='black') +
  geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), width=0.1)
# With a line graph
ggplot(ce, aes(x=Date, y=Weight)) +
  geom_line(aes(group=1)) + # note that x variable is a factor
  geom_point(size=4) +
  geom_errorbar(aes(ymin=Weight-se,ymax=Weight+se), width=0.1)

# barplot with dodge
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(position='dodge') +
  geom_errorbar(aes(ymin=Weight-se,ymax=Weight+se), position='dodge', 
                width=0.1)
# barplot with dodge that is adjusted
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(position='dodge') +
  geom_errorbar(aes(ymin=Weight-se,ymax=Weight+se), 
                position=position_dodge(0.9), width=0.1)

# line plot with dodge
pd <- position_dodge(0.3) # save the dodge spec.. We can use it repeatedly
ggplot(cabbage_exp, aes(x=Date, y=Weight, color=Cultivar, group=Cultivar)) +
  geom_errorbar(aes(ymin=Weight-se,ymax=Weight+se),
                width=0.1, size=0.25, color='black', position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2.5)


## 5.7. Adding Annotations to Individual Facets
p <- ggplot(mpg, aes(x=displ, y=hwy)) + geom_point() + facet_grid(.~drv)
# A data frame with labels for each facet
f_labels <- data.frame(drv=c('4','f','r'),label=c('4wd','Frond','Rear'))
p + geom_text(x=6, y=40, aes(label=label), data=f_labels)
# If you use annotate(), the label will appear in all facets
p + annotate('text', x=6, y=40, label='label text')

# This function returns a data frame with strings representing the regression
# equation, and the r^2 value
# These strings will be treated as R math expressions
lm_labels <- function(dat){
  mod <- lm(hwy~displ, data=dat)
  formula <- sprintf('italic(y) == %.2f %+ .2f * italic(x)',
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$displ,dat$hwy)
  r2 <- sprintf('italic(R^2) == %.2f', r^2)
  data.frame(formula=formula, r2=r2, stringAsFactors=FALSE)
}
library(plyr)
labels <- ddply(mpg, 'drv', lm_labels)
labels
p + geom_smooth(method=lm, se=FALSE) +
  geom_text(x=7, y=40, aes(label=formula), data=labels, 
            parse=TRUE, hjust=1) +
  geom_text(x=7, y=35, aes(label=r2), data=labels, parse=TRUE, 
            hjust=1)
# "hjust=1" aligns all texts to the right





