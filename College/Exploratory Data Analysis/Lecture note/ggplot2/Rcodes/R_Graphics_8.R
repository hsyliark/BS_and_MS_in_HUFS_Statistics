
###### R Graphics Lecture 8 : Legends ########

### install packages for graphics -- if you installed them, do not again
#pack_names <- c('ggplots','gcookbook')
#install.packages(pack_names)

### loading packages
library('ggplot2')      # install "ggplot2" package
library('gcookbook')    # install "gcookbook" for data sets 
# NOTE : same as "require('ggplot2')" and "require('gcookbook')"


## 8.1. Removing the Legend
p <- ggplot(PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot()
p
# remove the legend for fill
p + guides(fill=FALSE)
# same result
p + scale_fill_discrete(guide=FALSE)

p2 <-ggplot(PlantGrowth, aes(x=group, y=weight, fill=group, color=group)) + 
  geom_boxplot()
p2
p2 + guides(fill=FALSE)
p2 + guides(color=FALSE)
p2 + guides(fill=FALSE, color=FALSE)


## 8.2. Changing the Position of a Legend
p <- ggplot(PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot() +
  scale_fill_brewer(palette='Pastel2')
p + theme(legend.position='top')
p + theme(legend.position='bottom')
p + theme(legend.position='left')
p + theme(legend.position='right')
# place the bottom-right corner of the legend 
# in the bottom-right corner of the graphing area
p + theme(legend.position=c(1,0), legend.justification=c(1,0))
# try
p + theme(legend.position=c(1,0), legend.justification=c(0.5,0.5))
p + theme(legend.position=c(0.5,0.5), legend.justification=c(1,0))
p + theme(legend.position=c(1,1), legend.justification=c(1,1))

# opaque border
p + theme(legend.position=c(0.9, 0.2)) +
  theme(legend.background=element_rect(fill='white',color='black'))
# remove overall border of legend
p + theme(legend.position=c(0.9, 0.2)) +
  theme(legend.background=element_blank()) +  # remove overall border
  theme(legend.key=element_blank()) # remove border around each item


## 8.3. Changing a Legend Title
p <- ggplot(PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot()
p
p + labs(fill='Condition')
# same result
p + scale_fill_discrete(name='Condition')
# another way
p + guides(fill=guide_legend(title='Condition'))

hw <- ggplot(heightweight, aes(x=ageYear, y=heightIn, color=sex)) + 
  geom_point(aes(size=weightLb)) + scale_size_continuous(range=c(1,4))
hw
hw + labs(color='Male/Female', size='Weight\n(pound)')


## 8.4. Changing the Appearance of a Legend Title
p <- ggplot(PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot()
p + theme(legend.title=element_text(face='italic', family='Times', color='red',
                                    size=18))


## 8.5. Removing a Legend Title
ggplot(PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot() +
  guides(fill=guide_legend(title=NULL))


## 8.6. Changing the Labels in a Legend
p <- ggplot(PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot()
p + scale_fill_discrete(labels=c('Control', 'Treatment 1', 'Treatment 2'))
# change the order of labels in a legend
p + scale_fill_discrete(limits=c('trt1','trt2','ctrl'),
                        labels=c('Treatment 1', 'Treatment 2', 'Control'))

p <- ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex, color=sex)) +
  geom_point()
# see the difference 
p
p + scale_shape_discrete(labels=c('Female','Male'))
p + scale_color_discrete(labels=c('Female','Male'))
p + scale_shape_discrete(labels=c('Female','Male')) +
  scale_color_discrete(labels=c('Female','Male'))



