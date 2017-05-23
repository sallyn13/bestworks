#Installing necessary packages
install.packages("devtools")
library('devtools')

install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")
require (Rfacebook)

#Connecting R
fb_oauth=fbOAuth(app_id="250634588676083", app_secret="cf68d6118e520e5d93222434dce6f6de",extended_permissions = TRUE)

#Saving Authentication
save(fb_oauth, file="fb_oauth")
#Loading Authentication
load("fb_oauth")

#Viewing likes
my_likes=getLikes(user="me", token=fb_oauth)

#ShoeDazzle Analysis
token='EAACEdEose0cBAFuyYFFzMezesPEIF4J0YlABTzUJnhV7R20uTa8vZBX8IjB8wNVNZCyJhATRQjYl1ZAZA75frSP33bdjcqjA6nlejTFmqlClTG8LJ5Amkx2ZBRy6jZAjBPZAXqHpaLyNXW5bDRyvSASALSZC4jZAAW8Wg74MqA8w1H4cWyO79CbhEVRxHYrIMHJkZD'
n=1000
users=getUsers("101987301816",token,private_info = FALSE)

#Year 2017
Jan2017=getPage(page="101987301816",token,n, since="2017/01/01",until="2017/01/31")
Feb2017=getPage(page="101987301816",token,n, since="2017/02/01",until="2017/02/28")
March2017=getPage(page="101987301816",token,n, since="2017/03/01",until="2017/03/31")
April2017=getPage(page="101987301816",token,n, since="2017/04/01",until="2017/04/30")
Year2017=rbind.data.frame(Jan2017,Feb2017,March2017,April2017)
Year2017$Time=ymd_hms(Year2017$created_time,tz="")
Year2017$Time=format(Year2017$Time, format='%I:%M:%S %p')
Year2017$Date=as.Date(Year2017$created_time,format="%Y-%m-%d")
Year2017=Year2017[,c(1:2,12:13,3:11)]
Year2017=Year2017[,c(1:5,7:13,6)]
write.csv(Year2017,file="Facebook Posts 2017.csv")


#2016 Year 
Jan2016=getPage(page="101987301816",token,n, since="2016/01/01",until="2016/01/31")
Feb2016=getPage(page="101987301816",token,n, since="2016/02/01",until="2016/02/28")
March2016=getPage(page="101987301816",token,n, since="2016/03/01",until="2016/03/31")
April2016=getPage(page="101987301816",token,n, since="2016/04/01",until="2016/04/30")
May2016=getPage(page="101987301816",token,n, since="2016/05/01",until="2016/05/31")
June2016=getPage(page="101987301816",token,n, since="2016/06/01",until="2016/06/30")
July2016=getPage(page="101987301816",token,n, since="2016/07/01",until="2016/07/31")
Aug2016=getPage(page="101987301816",token,n, since="2016/08/01",until="2016/08/31")
Sept2016=getPage(page="101987301816",token,n, since="2016/09/01",until="2016/09/30")
Oct2016=getPage(page="101987301816",token,n, since="2016/10/01",until="2016/10/31")
Nov2016=getPage(page="101987301816",token,n, since="2016/11/01",until="2016/11/30")
Dec2016=getPage(page="101987301816",token,n, since="2016/12/01",until="2016/12/31")
Year2016=rbind.data.frame(Jan2016,Feb2016,March2016,April2016,May2016,June2016,July2016,Aug2016,Sept2016,Oct2016,Nov2016,Dec2016)
Year2016$Time=ymd_hms(Year2016$created_time,tz="")
Year2016$Time=format(Year2016$Time, format='%I:%M:%S %p')
Year2016$Date=as.Date(Year2016$created_time,format="%Y-%m-%d")
Year2016=Year2016[,c(1:2,12:13,3:11)]
Year2016=Year2016[,c(1:5,7:13,6)]
write.csv(Year2016,file="Facebook Posts 2016.csv")

#2016 Year Analysis 
Facebook.Posts.2016=read.csv("~/Desktop/Grad School /Spring 2017/TechStyle /Datasets/Facebook Posts 2016.csv", comment.char="#")
library("lubridate")
summary(Facebook.Posts.2016$Time)
#most posts times: 1:30pm, 5:30pm, 9:30am

#Morning bin: from 7:00am to 11:00am 
#Afternoon bin: 12:00pm to 4:00pm
#Evening bin: 5:00pm to 9:00pm
#Late Night bin: 10:00pm-1:00am

# Extract Time From created_time Column
time=(regmatches(Facebook.Posts.2016$created_time,gregexpr("T.*\\+",Facebook.Posts.2016$created_time)))
time=unlist(time)
time=gsub("T","",time)
time=gsub("\\+","",time)
Facebook.Posts.2016$MilTime=time

#Create Time of Day Assignments
Facebook.Posts.2016$time_of_day <- -999
# Assign Case to Morning - 06:00:00 to 10:59:59 +00 (1)
Facebook.Posts.2016$time_of_day[which(Facebook.Posts.2016$MilTime >= "23:00:00" & Facebook.Posts.2016$MilTime <= "23:59:59")] <- 1
Facebook.Posts.2016$time_of_day[which(Facebook.Posts.2016$MilTime >= "00:00:00" & Facebook.Posts.2016$MilTime <= "03:59:59")] <- 1
# Assign Case to Afternoon - 11:00:00 to 16:59:59 +00 (2)
Facebook.Posts.2016$time_of_day[which(Facebook.Posts.2016$MilTime >= "04:00:00" & Facebook.Posts.2016$MilTime <= "09:59:59")] <- 2
# Assign Case to Evening - 17:00:000 to 23:59:59 +00 (3)
Facebook.Posts.2016$time_of_day[which(Facebook.Posts.2016$MilTime >= "10:00:00" & Facebook.Posts.2016$MilTime <= "16:59:59")] <- 3
# Assign Case to Late Night - 00:00:00 to 05:59:59 +00 (4)
Facebook.Posts.2016$time_of_day[which(Facebook.Posts.2016$MilTime >= "17:00:00" & Facebook.Posts.2016$MilTime <= "22:59:59")] <- 4
# Check to confirm all cases have been assigned a time of day
length(which(Facebook.Posts.2016$time_of_day == -999))
# fb$miltime[which(fb$time_of_day == -999)]

#Labeling Times as Morning, Afternoon, Night, Late Night
Facebook.Posts.2016$Morning=ifelse(Facebook.Posts.2016$time_of_day =="1",1,0)
Facebook.Posts.2016$Afternoon=ifelse(Facebook.Posts.2016$time_of_day=="2",1,0)
Facebook.Posts.2016$Night=ifelse(Facebook.Posts.2016$time_of_day=="3",1,0)
Facebook.Posts.2016$Late_Night=ifelse(Facebook.Posts.2016$time_of_day=="4",1,0)

#Time of Day preference
#Morning
morningmodel=lm(Facebook.Posts.2016$Morning~Facebook.Posts.2016$likes_count+Facebook.Posts.2016$shares_count+Facebook.Posts.2016$comments_count+Facebook.Posts.2016$type)
summary(morningmodel)
#no significant variables 

afternoonmodel=lm(Facebook.Posts.2016$Afternoon~Facebook.Posts.2016$likes_count+Facebook.Posts.2016$shares_count+Facebook.Posts.2016$comments_count+Facebook.Posts.2016$type)
summary(afternoonmodel)
#likes is a siginificant variable 

eveningmodel=lm(Facebook.Posts.2016$Night~Facebook.Posts.2016$likes_count+Facebook.Posts.2016$shares_count+Facebook.Posts.2016$comments_count+Facebook.Posts.2016$type)
summary(eveningmodel)


nightmodel=lm(Facebook.Posts.2016$Late_Night~Facebook.Posts.2016$likes_count+Facebook.Posts.2016$shares_count+Facebook.Posts.2016$comments_count+Facebook.Posts.2016$type)
summary(nightmodel)
#comments is somewhat significant 


#Spotlight Analysis of Shares 
#standard deviation of likes
likesd=sd(Facebook.Posts.2016$likes_count)
#Center of likes 
likescenter=Facebook.Posts.2016$likes_count-mean(Facebook.Posts.2016$likes_count)
likeshigh=likescenter-likesd
likeslow=likescenter+likesd
likes_lowmodel=lm(Facebook.Posts.2016$shares_count~likeslow+likeslow*Facebook.Posts.2016$Morning)
likes_highmodel=lm(Facebook.Posts.2016$shares_count~likeshigh+likeshigh*Facebook.Posts.2016$Morning)
summary(likes_lowmodel)
summary(likes_highmodel)

#60 for shares, 30 comments, 10 likes 

#2017 Year Analysis 
Facebook.Posts.2017=read.csv("~/Desktop/Grad School /Spring 2017/TechStyle /Datasets/Facebook Posts 2017.csv", comment.char="#")
summary(Facebook.Posts.2017$Time)
#most posts at 1:00pm, 3:00pm, 6:00pm, and 12:00pm

time2=(regmatches(Facebook.Posts.2017$created_time,gregexpr("T.*\\+",Facebook.Posts.2017$created_time)))
time2=unlist(time2)
time2=gsub("T","",time2)
time2=gsub("\\+","",time2)
Facebook.Posts.2017$MilTime=time2

#Create Time of Day Assignments
Facebook.Posts.2017$time_of_day <- -999
# Assign Case to Morning - 06:00:00 to 10:59:59 +00 (1)
Facebook.Posts.2017$time_of_day[which(Facebook.Posts.2017$MilTime >= "23:00:00" & Facebook.Posts.2017$MilTime <= "23:59:59")] <- 1
Facebook.Posts.2017$time_of_day[which(Facebook.Posts.2017$MilTime >= "00:00:00" & Facebook.Posts.2017$MilTime <= "03:59:59")] <- 1
# Assign Case to Afternoon - 11:00:00 to 16:59:59 +00 (2)
Facebook.Posts.2017$time_of_day[which(Facebook.Posts.2017$MilTime >= "04:00:00" & Facebook.Posts.2017$MilTime <= "09:59:59")] <- 2
# Assign Case to Evening - 17:00:000 to 23:59:59 +00 (3)
Facebook.Posts.2017$time_of_day[which(Facebook.Posts.2017$MilTime >= "10:00:00" & Facebook.Posts.2017$MilTime <= "16:59:59")] <- 3
# Assign Case to Late Night - 00:00:00 to 05:59:59 +00 (4)
Facebook.Posts.2017$time_of_day[which(Facebook.Posts.2017$MilTime >= "17:00:00" & Facebook.Posts.2017$MilTime <= "22:59:59")] <- 4
# Check to confirm all cases have been assigned a time of day
length(which(Facebook.Posts.2017$time_of_day == -999))
# fb$miltime[which(fb$time_of_day == -999)]


#Macroeconomic Factors
Macroeconomic.Factors=read.csv("~/Desktop/Grad School /Spring 2017/TechStyle /Datasets/Macroeconomic Factors.csv")
Macroeconomic.Factors=Macroeconomic.Factors[-28,-c(5:13)]
