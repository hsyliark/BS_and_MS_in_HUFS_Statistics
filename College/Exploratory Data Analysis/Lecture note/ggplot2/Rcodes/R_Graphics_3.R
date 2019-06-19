
###### R Graphics Lecture 3 : Scatter Plots ########

### install packages for graphics -- if you installed them, do not again
#pack_names <- c('ggplots','gcookbook')
#install.packages(pack_names)

### loading packages
library('ggplot2')      # install "ggplot2" package
library('gcookbook')    # install "gcookbook" for data sets 
# NOTE : same as "require('ggplot2')" and "require('gcookbook')"


## 3.1. Making a Basic Scatter Plot 
str(heightweight)
head(heightweight[,c('ageYear','heightIn')])
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn))
p + geom_point(shape=21)
p + geom_point(size=1.5)


## 3.2. Grouping Data Points by a Variable Using Shape or Color
head(heightweight[,c('sex','ageYear','heightIn')])
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex, color=sex))
p + geom_point()
p + geom_point() + scale_shape_manual(values=c(1,2)) +
  scale_color_brewer(palette='Set1')


## 3.3. Using Different Point Shapes
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point(shape=3)
ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex)) +
  geom_point(size=4) + scale_shape_manual(values=c(1,4))

hw <- heightweight
# categorize into <100 and >=100 groups
hw$weightGroup <- cut(hw$weightLb, breaks=c(-Inf, 100, Inf),
                      labels=c('< 100', '>=100'))
ggplot(hw, aes(x=ageYear, y=heightIn, shape=sex, fill=weightGroup)) +
  geom_point(size=2.5) +
  scale_shape_manual(values=c(21,24)) +
  scale_fill_manual(values=c(NA,'black'), 
                    guide=guide_legend(override.aes=list(shape=21)))


## 3.4. Mapping a Continuous Variable to Color or Size
ggplot(heightweight, aes(x=ageYear, y=heightIn, color=weightLb)) + geom_point()
ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb)) + geom_point()

ggplot(heightweight, aes(x=ageYear, y=heightIn, fill=weightLb)) +
  geom_point(shape=21,size=2.5) +
  scale_fill_gradient(low='black',high='white')
ggplot(heightweight, aes(x=ageYear, y=heightIn, fill=weightLb)) +
  geom_point(shape=21,size=2.5) +
  scale_fill_gradient(low='black',high='white', guide=guide_legend())

ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb, color=sex)) +
  geom_point(alpha=0.5) +
  scale_size_area() +
  scale_color_brewer(palette='Set1')


## 3.5. Adding Fitted Regression Model Lines
sp <- ggplot(heightweight, aes(x=ageYear, y=heightIn))
sp + geom_point() + stat_smooth(method=lm)
sp + geom_point() + stat_smooth(method=lm, level=0.99)
sp + geom_point() + stat_smooth(method=lm, se=FALSE)
sp + geom_point() + stat_smooth() # default: method=loess

sps <- ggplot(heightweight, aes(x=ageYear, y=heightIn, color=sex)) + 
  geom_point() + scale_color_brewer(palette='Set1')
sps + stat_smooth(method=lm)
sps + stat_smooth(method=lm,fullrange=TRUE)


## 3.6. Adding Annotations with Model Coefficients
model <- lm(heightIn ~ ageYear, heightweight)
summary(model)
sp <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point() +
  stat_smooth(method=lm)
sp + annotate('text', label='r^2==0.42', parse=TRUE, x=16.5, y=52)

eqn <- as.character(as.expression(
  substitute(italic(y) == a + b * italic(x) * ',' ~~ italic(r)^2 ~ '=' ~ r2,
             list(a = format(coef(model)[1], digits=3),
                  b = format(coef(model)[2], digits=3),
                  r2 = format(summary(model)$r.squared, digits=2)
))))
parse(text=eqn)
sp + annotate('text', label=eqn, parse=TRUE, x=Inf, y=-Inf, hjust=1.1, vjust=-0.5)


## 3.7. Adding Marginal Rugs to a Scatter Plot
ggplot(faithful, aes(x=eruptions, y=waiting)) + geom_point() + geom_rug()
ggplot(faithful, aes(x=eruptions, y=waiting)) + geom_point() + 
  geom_rug(position='jitter', size=0.2)


## 3.8. Labeling Points in a Scatter Plot
cs <- subset(countries, Year==2009 & healthexp>2000)
str(cs)

sp <- ggplot(cs, aes(x=healthexp, y=infmortality)) + geom_point()
sp + geom_text(aes(label=Name), size=4)
sp + geom_text(aes(label=Name), size=4, vjust=-1)
sp + geom_text(aes(label=Name), size=4, hjust=-0.1)
# add text labels for subsets 1
sp + annotate('text', x=4350, y=5.4, label='Canada') +
  annotate('text', x=7400, y=6.8, label='USA')
# add text labels for subsets 2
cs$Name1 <- cs$Name
idx <- cs$Name1 %in% c('Canada', 'Ireland', 'United Kingdom', 'United States', 
                        'Japan', 'New Zealand', 'Iceland', 'Luxembourg', 
                       'Netherland', 'Switzerland')
cs$Name1[!idx] <- NA
ggplot(cs, aes(x=healthexp, y=infmortality, label=Name1)) + geom_point() + 
  geom_text(aes(x=healthexp+100, label=Name1), size=4, hjust=0) +
  xlim(2000, 9000)


## 3.9. Creating a Balloon Plot
cdat <- subset(countries, Year==2009 &
                 Name %in% c('Canada', 'Ireland', 'United Kingdom', 'United States',
                             'New Zealand', 'Iceland', 'Japan', 'Luxembourg',
                             'Netherland', 'Switzerland'))
p <- ggplot(cdat, aes(x=healthexp, y=infmortality, size=GDP)) +
  geom_point(shape=21, color='black', fill='cornsilk')
p
p + scale_size_area(max_size=15)
