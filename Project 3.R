#Sally Nguyen
#Project 3

#Clearing the environment
rm(list=ls())

#Part 1

#Creating an empty dataframe that will hold the final data with wins, losses, and opponets for each season
final_d=data.frame()
d=data.frame()

#Sourcing in data from 1960 to 2010
for (i in 1960:2010){
  d1=read.fwf(paste("http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf",i,"gms.txt",sep=""),c(11,28,2,30,2))
  d=rbind(d,d1)
}

#Renaming the columns in the dataframe
names(d)=c("Date","Away Team","Away Score","Home Team","Home Score")

#Removing spaces in between team names
d$`Away Team`=as.character(gsub(" ", "", d$`Away Team`))
d$`Home Team`=as.character(gsub(" ", "", d$`Home Team`))

#Creating a Month column to fix season of bowl games
d$Month=as.numeric(substr(d$Date,1,2))

#Creating a Season column
d$Season=as.numeric(substr(d$Date,7,10))

#Ensuring teams that played in January are in the previous Season
for(i in 1:length(d$Date)){
  if(d$Month[[i]]==1){
    d$Season[[i]] = d$Season[[i]] - 1}
}

#Removing the Month column
d=d[,-6]

#Moving Season to the first column
d=d[c(colnames(d)[6],colnames(d)[1:5])]

#Creating a vector of teams by ID number to see all the unique teams
teams=levels(as.factor(c(d$`Away Team`, d$`Home Team`)))

#Installing plyr package and loading library to use count function
install.packages('plyr')
library(plyr)

#Specifying the start row and end row for each season
for (season in 1960:2010){
  print(season)
  startrow=grep(season,d[,1])[1]
  endrow=-1+startrow+count(d[,1]==season)[2,2]
  
#Creating a list of teams from the Away and Home columns for a particular season
teamlist=character()  
for (i in startrow:endrow){
  away_team=d[i,3]
  if(away_team %in% teamlist==FALSE){
    teamlist=c(teamlist,away_team) 
  }
  home_team=d[i,5]
  if (home_team %in% teamlist==FALSE){
    teamlist=c(teamlist,home_team)
  }
}

#Creating games dataframe to count how many times each team played for a particular season
games=data.frame()
nteam=length(teamlist)
games=data.frame(teamlist,numeric(nteam))
for (i in startrow:endrow){
  away_team=d[i,3]
  j=which(away_team==games[[1]])
  games[j,2]=games[j,2]+1
  home_team=d[i,5]
  j=which(home_team==games[[1]])
  games[j,2]=games[j,2]+1 
}
  
#Creating a list that shows which teams played more than 6 games for a particular season
div1=character()
for (i in 1:nteam){
  if (games[i,2]>=6){
    div1temp=as.character(games[[1]][i])
    div1=c(div1,div1temp)
  }
}
  
#Creating a final division 1 team dataframe that is binded with the original dataframe for a particular season
div1teams=data.frame()
for (i in startrow:endrow){
  away_team=d[i,3]
  home_team=d[i,5]
  if (away_team %in% div1 & home_team %in% div1){
    div1teams=rbind(div1teams,d[i,])
  }
}
  
#Creating dummy columns Winners and Losers in div1teams dataframe
div1teams$Winner='<>'
div1teams$Loser='<>'
  
#Deleting games that end in a tie 
div1teams=subset(div1teams, div1teams$`Away Score` != div1teams$`Home Score`)
  
#Creating new final dataframe to track wins and losses
ndiv1=length(div1)
df=data.frame(numeric(ndiv1),div1,numeric(ndiv1),numeric(ndiv1))
  
#Renaming final dataframe 
names(df)=c("Season","Team","Wins","Losses")
  
#Filling in Season column in dataframe
df$Season=season
  
#When the Away Team or Home Team wins in a particular season
for(i in 1:length(div1teams$Season)){
  if(div1teams$`Away Score`[[i]] > div1teams$`Home Score`[[i]]){
    div1teams$Winner[[i]] = as.character(paste(div1teams$`Away Team`[[i]]))}
  if(div1teams$`Home Score`[[i]] > div1teams$`Away Score`[[i]]){
    div1teams$Winner[[i]] = as.character(div1teams$`Home Team`[[i]])}
}
  
#When the Away Team or Home Team loses in a particular season
for(i in 1:length(div1teams$Season)){
  if(div1teams$`Away Score`[[i]] > div1teams$`Home Score`[[i]]){
    div1teams$Loser[[i]] = as.character(paste(div1teams$`Home Team`[[i]]))}
  if(div1teams$`Home Score`[[i]] > div1teams$`Away Score`[[i]]){
    div1teams$Loser[[i]] = as.character(paste(div1teams$`Away Team`[[i]]))}
}
  
#Counting the number of wins for each team in a particular season 
df$Wins=0
for (i in 1:length(df$Team)){
  for (j in 1:length(div1teams$Date)){
    if(df$Team[[i]] %in% div1teams$Winner[[j]]){
      df$Wins[[i]]=df$Wins[[i]]+1
    }
  }
}
  
#Counting the number of losses for each team in a particular seasosn 
df$Losses=0
for (i in 1:length(df$Team)){
  for (j in 1:length(div1teams$Date)){
    if(df$Team[[i]] %in% div1teams$Loser[[j]]){
      df$Losses[[i]]=df$Losses[[i]]+1
    }
  }
}
  
#Creating an Opponents list
Opponents=list()
for (i in 1:length(df$Team)){
  away=which(div1teams$`Away Team`== df$Team[i])
  home=which(div1teams$`Home Team`== df$Team[i])
  Opponents[[i]]=c(which(teamlist %in% div1teams$`Home Team`[away]), which(teamlist %in% div1teams$`Away Team`[home]))
}

#Putting Opponents list into final dataframe 
df$Opponents=Opponents
  
#Binding dataframe to create final dataframe with all the years
final_d=rbind(final_d,df)
}

#Saving dataframe as a script 
save(final_d,file="ncaa_all_teams.rdata")

#Part 2

#Clear environment 
rm(list=ls())

#Load ncaadataframe
load(file="ncaa_all_teams.rdata")

#Colley matrix function from Project 2
colley_matrix=function(Year){
df=final_d[final_d$Season==Year,]
#Create matrix A
A = matrix(diag(2+(df$Losses+df$Wins)), nrow=nrow(df))
for(i in 1:nrow(df)){
  for(j in 1:nrow(df)){
    if(i %in% (df$Opponents)[[j]]){
      A[i,j] = -1*length(which(i == df$Opponents[[j]][]))
    }
  }
}

#Creating b vector
b= matrix(1 + (df$Wins - df$Losses)/2,nrow=nrow((df)))
  
#Solving linear system where x equals the score
x= solve(A,b)
  
#Solution frame
solution= data.frame(df$Team,x)
solution= solution[order(-solution$x),]
print(solution)
}
colley_matrix(2010)

