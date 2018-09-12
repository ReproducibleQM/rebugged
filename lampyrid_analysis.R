# Needed packages are listed and installed in install.R


# lampyrid analysis
#bring data in from figshare
lampyrid<-read.csv(file="https://ndownloader.figshare.com/files/3686040",
                   header=T)

#we're going to be looking at the responses of lampyrids to environmental conditions
#this means making some choices about when to stat counting
#what day of the year should we start the analysis on? 
#giving it a start day of Mar 1
start<-60

#coding the variable like this makes it easy to re-run the code with different start dates
#to see what the effect of the start date has on our conclusions. this type of testing is 
#often referred to as 'sensitivity analysis'- ie seeing how sensitive your conclusions are to
#your  assumptions, guesses or starting points.

#clean data
#fix dates, make them ISO'ed
library(lubridate)
lampyrid$newdate<-mdy(lampyrid$DATE)
#extract year
lampyrid$year<-year(lampyrid$newdate)
#extract day of year. DOY is very useful for a phenology type analyses
#because you don't have to deal with day-of-month numbers starting over 
#in the middle of a phenological event.
lampyrid$DOY<-yday(lampyrid$newdate)
#use ISO week, so we start counting on Monday, not Jan 1, COOL! Our sampling usually 
#takes place Wed-Friday, so if we use week of year stating on Jan 1, there is a good chance that
#samples taken within a sampling week would get grouped incorrectly when we go to do the analysis.
lampyrid$week<-isoweek(lampyrid$newdate)

#let's look for the data problems we found we used OpenRefine and see if
#we can impliment our cleaning operations here- that way we have a complete
#record of EVERYTHING that happened to these data. Recall there were issues 
#with TREAT_DESC
#let's look at these columns individually and fix errors as we find them
#and we should also check for weirdness in our numeric values

summary(lampyrid)
#looks like there's one missing data point (NA) for adults. Let's ditch
#it so it doesn't cause any problems in subsequent analyses
lampyrid<-na.omit(lampyrid)
summary(lampyrid)

#looks good. Okay, TREAT_DESC:

summary(as.factor(lampyrid$TREAT_DESC))
#wow, we've got some spelling errors. Let's clean that up

lampyrid$TREAT_DESC<-gsub("Early succesional community", "Early successional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Early sucessional community", "Early successional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Succesional", "Successional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Sucessional", "Successional", lampyrid$TREAT_DESC)
#also shorten biologically based (organic) and conventional till for plotting purposes 
lampyrid$TREAT_DESC<-gsub("Biologically based \\(organic\\)", "Organic", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Conventional till", "Conventional", lampyrid$TREAT_DESC)

#also convert this column to factor (gsub sometimes turns it into character type)
lampyrid$TREAT_DESC<-as.factor(lampyrid$TREAT_DESC)
summary(lampyrid$TREAT_DESC)

#do the same for HABITAT
summary(as.factor(lampyrid$HABITAT))
#checks out. Let's make sure R is seeing it as a factor, and also rep and station while we're at it

lampyrid$HABITAT<-as.factor(lampyrid$HABITAT)
lampyrid$REPLICATE<-as.factor(lampyrid$REPLICATE)
lampyrid$STATION<-as.factor(lampyrid$STATION)

#one more check to see if the data looks clean
summary(lampyrid)

#so we have a small issue with these data. The counts will be strongly zero-biased because we 
# give each subsample its own observation. When it comes to modelling and plotting, we're going to
#want to have the subsamples combined (summed), but because sometimes we lost traps (weather, accidental loss)
#not all plots will have the same number of subsamples
#we will process our data set so that we've got our subsamples combined by plot date etc and create a vector with counts
library(reshape2)
#tell R where the data is by melting it, assigning IDs to the columns
lampyrid1<-melt(lampyrid, id=c("DATE","TREAT_DESC","HABITAT","REPLICATE","STATION","newdate", "year", "DOY", "week"))
#cast the data to count up the fireflies
lampyrid2<-dcast(lampyrid1, year+DOY+week+TREAT_DESC+HABITAT+REPLICATE~., sum)
#cast the data to count the traps
lampyrid3<-dcast(lampyrid1, year+DOY+week+TREAT_DESC+HABITAT+REPLICATE~., length)

#let's rename these new vectors within the data frame
names(lampyrid2)[7]<-"ADULTS"
names(lampyrid3)[7]<-"TRAPS"

#rename the data frame and combine the number of traps we counted into it from lampyrid3
lampyrid<-lampyrid2
lampyrid$TRAPS<-lampyrid3$TRAPS

#download weather data from KBS weather station
weather<-read.table(file="http://lter.kbs.msu.edu/datatables/7.csv",
                    header=T, sep=",", na.strings="")
#extract day of year, so we have a continuous variable running for each year.
#since we're in a temperate northern climate, this is convenient- not too 
#much insect action happening at the december-january transition, so we 
#can use the yearly break as a blocking variable for rowing season.
#it's convenient living where we do! 

weather$DOY<-yday(weather$date)
weather$week<-isoweek(weather$date)
#do a few simple plots to make sure the data makes sense -this is
#a good way to check that the importation was sucessful

plot(weather$DOY, weather$air_temp_mean)
plot(weather$DOY, weather$precipitation)

#because we don't have lampyrid records before 2004, let's cut out the data
#from before 2003 so we can process the weaqther data more quickly. Also our
#lampyrid data stops at the end of 2015 and for some reason the new
#weather station data breaks our code. DANGIT. so we'll cut off the weather
#data that's causing us problems- we don't need it anyway
weather<-subset(weather, weather$year>2003& weather$year<2016)


#lets also get rid of the vairables we don't need:
weather$flag_precip<-NULL
weather$flag_air_temp_mean<-NULL
weather$flag_air_temp_max<-NULL
weather$flag_air_temp_min<-NULL

#also, these data are sorted in decending order. It's easier to think of this 
#stuff in ascending order, so let's sort the data by year and DOY

weather<-weather[order(weather$year, weather$DOY),]

#lets's pre-process these weather data so we get rid of missing values
# we can write a function to do this for us.
#if missing data is rare, it is probably safe to assume that missing
#temperatures are similar to the weather on the day before or after.
#for the sake of simplicity, let's replace a missing value with the 
#value for that variable for the day before

#first, define the function

replace.missing<-function(vec){
  #create a vector to put our new values into
  N
