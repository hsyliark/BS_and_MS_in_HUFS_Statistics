car <- read.csv("Car.csv")
head(car)
tail(car)

malibu <- car[car$Model=="Malibu",]
malibu

lm.out <- lm(Price~Mileage+Model+Liter+factor(Leather)+factor(Doors), data=car)
summary(lm.out)


my.car <- data.frame(Mileage=20000, Model="Malibu", Liter=3.1, Leather=0, Doors=4)
predict(lm.out, my.car)


lunch <- c(1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1)
gender <- c("F", "F", "M", "M", "F", "F", "F", "M", "M", "M", "M", "M")
class <- c(0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1)
weight <- c(48, 46, 65, 58, 55, 50, 45, 65, 96, 70, 65, 65)

dat <- data.frame(lunch=lunch, gender=gender, class=class, weight=weight)
head(dat)

attach(dat)
glm.out <- glm(lunch~gender+class+weight, family="binomial")
summary(glm.out)

my.friend <- data.frame(gender=c("M", "F"), class=c(0, 1), weight=c(65, 55))
predict(glm.out, my.friend, type="response")

