library(ggplot2)
library(tidyverse)
instasetwd("~")
str(googleplaystore)
df<-as.data.frame(googleplaystore)

df$Last.Updated[df$Last.Updated=="1.0.19 April 1"]= NULL

df$Last.Updated <- as.character(df$Last.Updated)

df$Last.Updated <- as.Date(df$Last.Updated,format="%B %d, %Y",tryFormats = "%Y %m %B")
df$Rating <- as.numeric(df$Rating)
df$Reviews <- as.numeric(df$Reviews)    
df$Installs <- gsub(",", "", gsub("\\.", "", df$Installs))
df$Installs <- as.character(df$Installs)
df$Installs = substr(df$Installs,1,nchar(df$Installs)-1)
df$Installs <- as.numeric(df$Installs)


df$Size <- gsub(",", "", gsub("\\.", "", df$Size))
df$Size <- as.character(df$Size)
df$Size = substr(df$Size,1,nchar(df$Size)-1)
df$Size <- as.numeric(df$Size)


df$App <- as.character(df$App)


df$Category = as.character(df$Category)
df$Category[df$Category=="1.9"]<-NA
df$Category <- as.factor(df$Category)


df$Type = as.character(df$Type)
df$Type[df$Type=="0"]<-NA
df$Type[df$Type=="NaN"] <- NA
df$Type <- as.factor(df$Type)


df$Price <- as.character(df$Price)
df$Price = as.numeric(gsub("\\$", "", df$Price))

df$Genres = as.character(df$Genres)


###Analysis of the Types of Apps and their average User Rating

df<- na.omit(df)

cat = df %>% group_by(Category) %>% select(c(App,Category,Rating))
cat = as.data.frame(cat)
str(cat$Rating)

table(cat$Rating)

###Analyzing the Type of Apps and their Average Rating 

type<- df %>% group_by(Type) %>% select(c(Rating,Type, Installs, Category, Price))
type=as.data.frame(type)

table(df$Type)


###Installations by Type and Categories

ggplot(type, aes(x=Type, y=Installs, fill=Type))+geom_bar(stat="identity")+labs(x="Type",y="Installs",fill="Types",title="Installations by Type of Apps")

ggplot(type, aes(x=Category, y=Installs, fill=Category))+ geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, vjust=0.5))+ labs(title="Installations by Category of Apps",x="Categories",y="Installs",fill="Categories")

### Since games has more installation. Lets see most installed app under it
gp1<- subset(df, Category=="GAME", select = c(App, Installs, Rating))
gp1<-top_n(gp1, 10)
ggplot(gp1, aes(x=App, y=Installs, fill=Rating))+geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, vjust=0.5))+ labs(title="Most installed Apps under Game Category",subtitle="(All rated above 5)",x="Apps",y="Installs",fill="Rating")


###Most Family Apps installed
gp2<- subset(df, Category=="FAMILY", select = c(App, Installs, Rating))
gp2<-top_n(gp2, 10)
ggplot(gp2, aes(x=App, y=Installs, fill=Rating))+geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, vjust=0.5))+ labs(title="Most installed Apps under Family Category",subtitle="(All rated above 5)",x="Apps",y="Installs",fill="Rating")


### lets explore app price now
ggplot(df, aes(x=Price))+geom_density(linetype="dashed", color="Black", fill="blue")+ theme(axis.text.x = element_text(angle = 90, vjust=0.5))+labs(title="Lets see how are the App's Priced?",x="App Prices")

paid_data<- subset(df, Type=="Paid", select = c(App, Price, Rating,Installs))
ggplot(paid_data, aes(x=Installs))+geom_density(color="black",fill="blue")+labs(title="Installation density of Paid Apps",x="Installs")

###Let's Analyze content rating and Installs
ggplot(df, aes(x=Content.Rating, y=Installs, fill=Installs))+geom_bar(stat="identity")+labs(title="Content Rating and Installs",subtitle= "Analysis",x="Content Rating",y="Installs",fill="Installs")

### Checking which category has the highest rating
ggplot(df, aes(x=Rating, y=Category)) +
  geom_segment(aes(yend=Category), xend=0, colour="grey50") +
  geom_point(size=1, aes(colour=Type)) +
  scale_colour_brewer(palette="Set1", limits=c("Free", "Paid"), guide=FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(Type ~ ., scales="free_y", space="free_y") +
  ggtitle("Checking which Category has the highest Rating between Free and Paid Apps")

## Number of ratings
ggplot(df, aes(x= Category, y= Rating, fill = Type)) +
  geom_bar(position='dodge',stat='identity') +
  coord_flip() +
  ggtitle("Number Of App Ratings Based On Category and Type")
## Number of reviews
ggplot(df, aes(x= Category, y= Reviews, fill = Type)) +
  geom_bar(position='dodge',stat='identity') +
  coord_flip() +
  ggtitle("Number Of App Reviews Based On Category and Type")
## Number of app installs
ggplot(df, aes(x= Category, y= Installs, fill = Type)) +
  geom_bar(position='dodge',stat='identity') +
  coord_flip() +
  ggtitle("Number Of App Installs Based On Category and Type")
## content rating and rating
ggplot(df, aes(x= Content.Rating, y= Rating, fill = Type)) +
  geom_bar(position='dodge',stat='identity') +
  coord_flip() +
  ggtitle("Content Rating Based On Rating and Type")




ggplot(df,aes(x=Content.Rating,y=Rating,fill = Type)) +
  geom_boxplot()+
  ggtitle("Content Rating Based On Rating and Type(Boxplot)")


ggplot(df,aes(x=Type,y=Rating)) +
  geom_boxplot()+
  ggtitle("Rating Among Free and Paid Apps")




xbar <- tapply(df$Rating, df$Content.Rating, mean)
s <- tapply(df$Rating, df$Content.Rating, sd)
df$normal.density <- apply(df, 1, function(x){
  dnorm(as.numeric(x["Rating"]),
        xbar[x["Content Rating"]], s[x["Content Rating"]])})
ggplot(df, aes(x = Rating)) +
  geom_histogram(aes(y = ..density..), 
                 # bins = sqrt(nrow(bike)) + 2,
                 bins = 20,
                 fill = "grey", col = "black") +
  facet_grid(Content.Rating ~ .) +
  geom_density(col = "red", lwd = 1) +
  geom_line(aes(y = normal.density), col = "blue", lwd = 1) +
  ggtitle("Histograms of Rating Among Differnet Ages")

###
xbar <- tapply(df$Rating, df$Category, mean)
s <- tapply(df$Rating, df$Category, sd)
df$normal.density <- apply(df, 1, function(x){
  dnorm(as.numeric(x["Rating"]),
        xbar[x["Category"]], s[x["Category"]])})
ggplot(df, aes(x = Rating)) +
  geom_histogram(aes(y = ..density..), 
                 # bins = sqrt(nrow(bike)) + 2,
                 bins = 20,
                 fill = "grey", col = "black") +
  facet_grid(Category ~ .) +
  geom_density(col = "red", lwd = 1) +
  geom_line(aes(y = normal.density), col = "blue", lwd = 1) +
  ggtitle("Histograms of Rating Between Categories")

### Tests
# t test
t.test(df$Rating ~ df$Type,mu = 0,conf.level = 0.95,paired = FALSE,alternative = "two.sided",var.equal = FALSE)

##
df$Content.Rating = as.numeric(df$Rating)
t.test(df$Rating,df$Content.Rating,mu = 0,conf.level = 0.95,paired = FALSE,alternative = "two.sided",var.equal = FALSE)
##
t.test(df$Rating,df$Reviews, mu = 0, conf.level = 0.95,paired = FALSE,alternative = "two.sided",var.equal=FALSE)
##
df$Category = as.numeric(df$Category)
t.test(df$Rating,df$Category, mu = 0, conf.level = 0.95,paired = FALSE,alternative = "two.sided",var.equal=FALSE)
### F test
var.test(df$Rating,df$Category,alternative = "two.sided")


# density plot of rating
ggplot(df,aes(x = Rating)) +
  geom_density() + facet_grid(Type ~ .) +
  geom_vline(aes(xintercept= mean(Rating)),linetype="dashed")

# hist
ggplot(df,aes(x = Rating)) +
  geom_histogram()


###Linear Regression
x<-df$Size
y<-df$Rating

relation <- lm(y~x)
png(file = "linearregression between Rating and Reviews.png")
plot(y,x,col = "blue",main = "Rating and Reviews Regression",
abline(relation),cex = 1.3,pch = 16,xlab = "Review",ylab = "Rating")
dev.off()

# Gaussina Distribution of rating and review
x<-df$Rating
y<-dnorm(x,mean = mean(x), sd = sqrt(var(x)))
plot (x , y , main = " Gaussian distribution of Rating", xlab = " Rating ", ylab = " Density ")


x<-df$Reviews          
y<-dnorm(x,mean = mean(x), sd = sqrt(var(x)))
plot (x , y , main = " Gaussian distribution of Reviews", xlab = " Reviews ", ylab = " Density ")        

# Gaussin dust of price
x<- df$Price
y<-dnorm(x,mean = mean(x), sd = sqrt(var(x)))
plot (x , y , main = " Gaussian distribution of Price", xlab = " Price ", ylab = " Density ")
