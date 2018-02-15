d <- read.csv("C:/Users/ts_ta/Documents/USF-Sem/Spring2018/SD/DataSets/Rossman/Rossmann_data_new.csv")
head(d)

attach(d)
ggplot(data = d,aes(x=Sales)) + geom_histogram(aes(y= ..density..),binwidth = 10,colour="Red",fill="white") + geom_density(alpha="0.6",fill="white")

#plots
install.packages("lmtest")
library(lmtest)
hist(Customers, probability = TRUE, ylim = c(0, 0.0020))
lines(density(Customers),col = "chocolate3",lwd = 2)
abline(v = mean(Customers),col = "royalblue",lwd = 2)
abline(v = median(Customers),col = "red",lwd = 2)
legend(x = "topright",c("Density plot", "Mean", "Median"),col = c("chocolate3", "royalblue", "red"),lwd = c(2, 2, 2))
ggplot(data = d,aes(x=Sales)) + geom_histogram(aes(y= ..density..),binwidth = 10,colour="blue",fill="red") + geom_density(alpha="0.6",fill="white")

boxplot(Sales, col.box="plum", xlab="Sales")
boxplot(Sales)$out

m1 <- lm(Sales~ CompetitionDistance + Customers + Customers*CompetitionDistance)
dwtest(m1)
plot(m1)


hist(d$Sales,xlab="Sales",ylab="Frequency of Sales",border="black",col = "lightblue")

hist(log(Sales),xlab="Sales",ylab="Frequency of Sales",border="black",col = "lightblue")


hist(d$Customers,xlab="Customers",ylab="Frequency of Customers",border="black",col = "Green")

hist(log(Customers),xlab="Customers",ylab="Frequency of Customers",border="black",col = "Green")


#POOLED MODEL
pool <- plm(Sales~as.factor(Assortment)+as.factor(Promo)+as.factor(Promo2)+as.factor(MONTH)+as.factor(YEAR)+Promo*CompetitionDistance,index = c("Store"),data = d,model = "pooling")
#The above model is not possible becuse it has duplicate
 v#alues means for this model to work plm require id and year to be unique
summary(pool)
hist(pool$residuals,col = "orange")
d[Promo==1]="YES"
d[Promo==0]="NO"

class(d$Promo)
any(table(d$Store, d$YEAR)!=1)

#FUNCTION FOR FINDING DUPLICATE IN  R
with(d, levels(Store)[tapply(YEAR, Store,
                               function(x) any(table(x) > 1))])

#POOLED MODEL customer

pool_customer <- plm(Customers~as.factor(Assortment)+as.factor(Promo)+as.factor(Promo2)+as.factor(MONTH)+as.factor(YEAR)+Promo*CompetitionDistance,index = "Store",data = d,model = "pooling")

summary(pool_customer)
hist(pool_customer$residuals,col = "light green")

#Fixed models

w <- plm(Sales~as.factor(Promo)+as.factor(MONTH)+Promo*CompetitionDistance,index = "Store",data = d,model = "within")
summary(w)
hist(w$residuals,col = "light green")

d <-  plm(Customers~as.factor(Promo)+as.factor(YEAR)+as.factor(SchoolHoliday),index = "Store",data = d,model = "within")
summary(d)
hist(d$residuals,col = "Purple")

##Random effect models
random <-  plm(Sales~as.factor(Promo)+as.factor(YEAR),data = d,index = c("Store"),model = "random")
random1 <- plm(Sales~as.factor(Promo)+as.factor(Promo2)+StateHoliday*CompetitionDistance,data = d,index = c("MONTH"),model = "random")
class(d$StateHoliday)
summary(random)
hist(random$residuals,col = "lightblue")
plot(random$residuals~random$fit)
summary(r)


random_customer <- plm(Customers~as.factor(Promo2)+as.factor(YEAR)+ StateHoliday,data = d,index = c("Store"),model = "random")

summary(random_customer)
hist(random_customer$residuals,col = "yellow")
