unzip("/Users/matthewheath/Desktop/airfare.zip")
airfare <- read.csv("/Users/matthewheath/airfare.csv")
sapply(airfare,class)
type_convert <- c("year", "quarter", "city_id1", "city_id2", "airport_id1", "airport_id2")
airfare[type_convert]<- lapply(airfare[type_convert], factor)
airfare1<-split(airfare, airfare$table)
oneA<-airfare1$`1a`
six<-airfare1$`6`
#Question 1

table(oneA$year)
table(six$year)
#gives total number of measurements for each year
summary(is.na(oneA))
oneA$missing_data=rowSums(is.na(oneA))
table(oneA$year, oneA$missing_data)
#shows number of missing values for each year
summary(is.na(six))
six$missing_data=rowSums(is.na(six))
table(six$year, six$missing_data)

table(oneA$quarter, oneA$missing_data)
table(six$quarter, six$missing_data)
#shows number of missing values for each quarter

six_by_year<-split(six, six$year)
six_2017<- six_by_year$'2017'
a2017<-table(six_2017$city1)
b2017<-table(six_2017$city2)
#created tables to show number of connections to and from each city per year

n1<- intersect(names(a2017), names(b2017))
list2017<- c(a2017[!(names(a2017) %in% n1)], b2017[!(names(b2017) %in% n1)], a2017[n1]+b2017[n1])
list2017[order(list2017)]
#combined tables and sorted them from least to greatest

six_2007<- six_by_year$'2007'
quarters2007<- split(six_2007, six_2007$quarter)
q1_2007<- quarters2007$`1`
a2007<-table(q1_2007$city1)
b2007<-table(q1_2007$city2)

n2<- intersect(names(a2007), names(b2007))
list2007<- c(a2007[!(names(a2007) %in% n2)], b2007[!(names(b2007) %in% n2)], a2007[n2]+b2007[n2])
list2007[order(list2007)]

six_1997<- six_by_year$'1997'
quarters1997<- split(six_1997, six_1997$quarter)
q1_1997<- quarters1997$`1`
a1997<-table(q1_1997$city1)
b1997<-table(q1_1997$city2)

n3<- intersect(names(a1997), names(b1997))
list1997<- c(a1997[!(names(a1997) %in% n3)], b1997[!(names(b1997) %in% n3)], a1997[n3]+b1997[n3])
list1997[order(list1997)]

diff1<-list2017-list2007
diff1[order(diff1)]
diff2<-list2017-list1997
diff2[order(diff2)]
#computes differences between two tables

passenger_sums<-aggregate(passengers ~ year + quarter, six, sum)
passenger_sums<- passenger_sums[order(passenger_sums$year),]
library(ggplot2)
ggplot(passenger_sums, aes(x=year, y=passengers, shape=quarter)) + 
  geom_point(stat="identity", fill="grey")+
  labs(title="     Passengers per Quarter", x="Year", y="Number of Passengers")

install.packages("readxl")
library(readxl)
cpi<- read_xlsx("/Users/matthewheath/cpi_1996_2017.xlsx")

cpi=as.data.frame(sapply(cpi, as.numeric))
cpi$q1=cpi$X__3
six<-merge(x=six, y=cpi[ ,c("CPI-All Urban Consumers (Current Series)", "q1")], by.x="year", by.y="CPI-All Urban Consumers (Current Series)", all.x=TRUE)
six$real_fare<- six$fare*(199.51/six$q1)
#Question 5

fare_time<- aggregate(real_fare ~ year, six, mean)
ggplot(fare_time, aes(x=year, y=real_fare, group=1))+
  geom_line()+labs(title="Average Fare vs. Year", x="Year", y="Real Fare ($)")

six_2015<-six_by_year$`2015`
oneA_by_year<- split(oneA, oneA$year)
oneA_2015<- oneA_by_year$`2015`
oneA_2015<-merge(x=oneA_2015, y=cpi[ ,c("CPI-All Urban Consumers (Current Series)", "q1")], by.x="year", by.y="CPI-All Urban Consumers (Current Series)", all.x=TRUE)
oneA_2015$real_fare<-oneA_2015$fare*(199.51/oneA_2015$q1)
oneA_model<-lm(oneA_2015$real_fare ~ oneA_2015$miles)
ggplot(oneA_2015, aes(x=miles, y=real_fare))+
  geom_point(color="dark blue", shape=1)+
  geom_smooth(method=lm, color="red")+
  labs(title="Fare vs. Distance (1a)", x="Distance of Flight (miles)", y= "Real Fare ($)")
six_2015<-merge(x=six_2015, y=cpi[ ,c("CPI-All Urban Consumers (Current Series)", "q1")], by.x="year", by.y="CPI-All Urban Consumers (Current Series)", all.x=TRUE)
six_2015$real_fare<-six_2015$fare*(199.51/six_2015$q1)
six_model<-lm(six_2015$real_fare~six_2015$miles)
ggplot(six_2015, aes(x=miles, y=real_fare))+
  geom_point(color="dark blue", shape=1)+
  geom_smooth(method=lm, color="red")+
  labs(title="Fare vs. Distance (6)", x="Distance of Flight (miles)", y= "Real Fare ($)")
coef(oneA_model)
coef(six_model)
resid_one<-residuals(oneA_model)
qqnorm(resid_one)
qqline(resid_one)
resid_six<-residuals(six_model)
qqnorm(resid_six)
qqline(resid_six)
plot(oneA_model, which=1, main="Residual Plot 1a")
plot(six_model, which=1, main="Residual Plot 6")
plot(oneA_model, which=5)
plot(six_model, which=5)
#created linear model/plot and performed diagnostic tests

oneA_model2<-lm(oneA_2015$real_fare ~ oneA_2015$passengers)
ggplot(oneA_2015, aes(x=passengers, y=real_fare))+
  geom_point(color="dark blue", shape=1)+
  geom_smooth(method=lm, color="red")+
  labs(title="Fare vs. Passengers (1a)", x="Number of Passengers", y= "Real Fare ($)")
six_model2<-lm(six_2015$real_fare~six_2015$passengers)
ggplot(six_2015, aes(x=passengers, y=real_fare))+
  geom_point(color="dark blue", shape=1)+
  geom_smooth(method=lm, color="red")+
  labs(title="Fare vs. Passengers (6)", x="Number of Passengers", y= "Real Fare ($)")
coef(oneA_model2)
coef(six_model2)
resid_one2<-residuals(oneA_model2)
qqnorm(resid_one2)
qqline(resid_one2)
resid_six2<-residuals(six_model2)
qqnorm(resid_six2)
qqline(resid_six2)
plot(oneA_model2, which=1, main="Residual Plot 1a")
plot(six_model2, which=1, main="Residual Plot 6")
summary(oneA_model2)
summary(six_model2)

oneA_2015$lg_real_fare<-oneA_2015$lg_fare*(199.51/oneA_2015$q1)
oneA_2015$fare_diff<-oneA_2015$lg_real_fare-oneA_2015$real_fare

six_2015$lg_real_fare<-six_2015$lg_fare*(199.51/six_2015$q1)
six_2015$fare_diff<-six_2015$lg_real_fare-six_2015$real_fare
fare_diff_2<- aggregate(fare_diff~ city1+city2, six_2015, mean)
#shows average differences for each pair of cities
hist(fare_diff_2$fare_diff, main="Fare Histogram", xlab="Difference in Fares")
plot(six_2015$lg_real_fare,six_2015$fare_dif, main="Fares", xlab = "Fare of Largest Carrier", ylab= "Difference in Fares")
ggplot(six_2015, aes(x= lg_carrier, y= fare_diff))+geom_boxplot()+labs(title="Fares across Carriers", x="Name of Carrier", y="Difference in Fares")

oneA<-merge(x=oneA, y=cpi[ ,c("CPI-All Urban Consumers (Current Series)", "q1")], by.x="year", by.y="CPI-All Urban Consumers (Current Series)", all.x=TRUE)
oneA$real_fare<-oneA$fare*(199.51/oneA$q1)
cities<- oneA[oneA$airport1%in%c("SFO", "OAK", "SJC", "SMF"), ]
cities2<- oneA[oneA$airport2%in%c("SFO", "OAK", "SJC", "SMF"), ]
cities<-rbind(cities, cities2)
ggplot(subset(cities,airport1%in%c("SFO", "OAK", "SJC", "SMF")), aes(x=airport1, y=real_fare)) +geom_boxplot()+ labs(title= "Airport 1", x=" Airport", y="fare")
ggplot(subset(cities,airport2%in%c("SFO", "OAK", "SJC", "SMF")), aes(x=airport2, y=real_fare)) +geom_boxplot()+ labs(title= "Airport 2", x=" Airport", y="fare")
#subset the data and showed fares in boxplot

sfo<-cities[cities$airport1%in%("SFO"),]
sfo2<-cities[cities$airport2%in%("SFO"),]
sfo<-rbind(sfo, sfo2)
ggplot(sfo, aes(x=year, y=real_fare))+geom_boxplot()+ labs(title= "SFO", y="fare")

oak<-cities[cities$airport1%in%("OAK"),]
oak2<-cities[cities$airport2%in%("OAK"),]
oak<-rbind(oak, oak2)
ggplot(oak, aes(x=year, y=real_fare))+geom_boxplot()+ labs(title= "OAK", y="fare")

sjc<-cities[cities$airport1%in%("SJC"),]
sjc2<-cities[cities$airport2%in%("SJC"),]
sjc<-rbind(sjc, sjc2)
ggplot(sjc, aes(x=year, y=real_fare))+geom_boxplot()+ labs(title= "SJC", y="fare")

smf<-cities[cities$airport1%in%("SMF"),]
smf2<-cities[cities$airport2%in%("SMF"),]
smf<-rbind(smf, smf2)
ggplot(smf, aes(x=year, y=fare))+geom_boxplot()+ labs(title= "SMF", y="fare")

table(sfo$miles>2500)
table(oak$miles>2500)
table(sjc$miles>2500)
table(smf$miles>2500)
#true= number of flights longer than 2500 miles

table(sfo$miles>2500, sfo$year)
table(oak$miles>2500, oak$year)
table(sjc$miles>2500, sjc$year)
table(smf$miles>2500, smf$year)
#same tables as before but sorted by year





