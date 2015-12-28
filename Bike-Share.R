setwd("E:\\HW1-ExploratoryData-Nyt\\BIKE")

file<-read.csv("E:\\HW1-ExploratoryData-Nyt\\BIKE\\day.csv",
               stringsAsFactors=FALSE,
               strip.white=TRUE,
               na.strings=c("NA",""))

###Histogranm of Bike Rentals by wEATHER

file$weathersit<-factor(file$weathersit,levels=c("1","2","3"),labels=c("Clear","Misty/Cloudy","Light Snow"))

clear<-subset(file,weathersit=="Clear")$cnt
misty<-subset(file,weathersit=="Misty/Cloudy")$cnt
snow<-subset(file,weathersit=="Light Snow")$cnt

par(mfrow=c(2,2))
hist(clear,
     prob=TRUE,
     xlab="Bike Rentals on A clear Day")
lines(density(clear))

hist(misty,
     prob=TRUE,
     xlab="Bike Rentals on A Misty Day")
lines(density(misty))

hist(snow,
     prob=TRUE,
     xlab="Bike Rentals on A Snowy Day")
lines(density(snow))

###Histogram of Bike rentals by Holiday or Non-Holiday
file$holiday <- factor(file$holiday, levels = c("0","1"), labels = c("NoHoliday","Holiday"))

NoHoliday<-subset(file,file$holiday=="NoHoliday")$cnt
Holiday<-subset(file,file$holiday=="Holiday")$cnt

hist(NoHoliday,
     prob=TRUE,
     xlab="Total Bike Rentals")
lines(density(NoHoliday))

file$season<-factor(file$season, levels=c("1","2","3","4"),labels=c("Spring","Summer","Fall","Winter"))

###Plot histogram by season of the year
summary(file$season)
attach(file)


spring<-subset(file,season=="Spring")$cnt
summer<-subset(file,season=="Summer")$cnt
fall<-subset(file,season=="Fall")$cnt
winter<-subset(file,season="Winter")$cnt

####Gistogram by time of SEASON
hist(spring,
     prob=TRUE,
     xlab="Bike Rentals in Spring"
)
lines(density(spring))

hist(summer,
     prob=TRUE,
     xlab="Bike Rentals in summer")
lines(density(summer))

hist(fall,
     prob=TRUE,
     xlab="Bike rentals in Fall")
lines(density(fall))

hist(winter,
     prob=TRUE,
     xlab="Bike rentals in Winter")
lines(density(winter))

##Histogram of bike rentals by day
file$days<-factor(file$weekday,levels=c("0","1","2","3","4","5","6"),
                  labels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
summary(file$days)

sun<-subset(file,days=="Sunday")$cnt
mon<-subset(file,days=="Monday")$cnt
tues<-subset(file,days=="Tuesday")$cnt
wed<-subset(file,days=="Wednesday")$cnt
thurs<-subset(file,days=="Thursday")$cnt
fri<-subset(file,days=="Friday")$cnt
sat<-subset(file,days=="Saturday")$cnt

par(mfrow=c(2,4))

hist(sun,
     prob=TRUE,
     xlab="Bike Rentals in Sunday")
lines(density(sun))

hist(mon,
     prob=TRUE,
     xlab="Bike Rentals in Monday")
lines(density(mon))

hist(tues,
     prob=TRUE,
     xlab="Bike Rentals in Tuesday")
lines(density(tues))

hist(wed,
     prob=TRUE,
     xlab="Bike Rentals in Wednesday")
lines(density(wed))

hist(thurs,
     prob=TRUE,
     xlab="Bike Rentals in Thursday")
lines(density(thurs))

hist(fri,
     prob=TRUE,
     xlab="Bike Rentals in Friday")
lines(density(fri))

hist(sat,
     prob=TRUE,
     xlab="Bike Rentals in Saturday")
lines(density(sat))


qplot(cnt, data = file) + facet_wrap(~ days, nrow=4) + geom_histogram(fill = "blue")


file$month<-factor(file$mnth,levels=c("1","2","3","4","5","6","7","8","9","10","11","12"),
                   labels=c("Jan","Feb","March","April","May","June","July","Aug","Sep","Oct","Nov","Dec"))

jan<-subset(file,month=="Jan")$cnt
feb<-subset(file,month=="Feb")$cnt
march<-subset(file,month=="March")$cnt
April<-subset(file,month=="April")$cnt
May<-subset(file,month=="May")$cnt
june<-subset(file,month=="June")$cnt
july<-subset(file,month=="July")$cnt
aug<-subset(file,month=="Aug")$cnt
sep<-subset(file,month=="Sep")$cnt
oct<-subset(file,month=="Oct")$cnt
nov<-subset(file,month=="Nov")$cnt
Dec<-subset(file,month=="Dec")$cnt

#Histogram of bike rentals by Monthh of the year
qplot(cnt, data = file, xlab="Count of Total Bike Rentals") + facet_wrap(~ month, nrow=4)

