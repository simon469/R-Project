#Read Dataset
mydata<-read.csv("c:/Users/RC/Desktop/myfile/G1_Allometry.csv",na.strings = c(""))
mydata
head(mydata) # display the first few rows of the dataset
head(mydata,10) # display the first 10 rows of the dataset
tail(mydata) # display the Last few rows of the dataset If not specified, it defaults to 6.
tail(mydata,10) #display the last 10 rows of the dataset
summary(mydata) # summary statistics of the dataset
str(mydata) # structure of the dataset
dim(mydata) # dimensions of the dataset
View(mydata) #View your dataset on R environment
nrow(mydata) #Know the number of rows
ncol(mydata) #Know the number of columns
colnames(mydata) #Know the names of columns
row.names(mydata) #Know the name of rows

#cleaning data

mydata$height<-gsub("â€œ","",mydata$height)
mydata$height<-gsub("â€" , "",mydata$height)
View(mydata)

# convert data types
mydata$height <- as.numeric(mydata$height)
mydata$branchmass<-as.integer(mydata$branchmass)
mydata$height<-as.integer(mydata$height)
mydata$leafarea<-as.integer(mydata$branchmass)
str(mydata)

#drop Na values
library(tidyr)
cleaned_data<-drop_na(mydata)
is.na(clean_cleaned)
View(cleaned_data)

#Delet rows with Missing value or NA

mydata[! complete.cases(mydata), ]
mydata[is.na(mydata$height), ]
mynewData<-mydata[ ! is.na(mydata$height), ]
View(mynewData)

#get all locations of NA
complete.cases(mydata) # FALSE means the row contains NA

#Get all rows contain missing data.
mydata[! complete.cases(mydata), ]
is.na(mydata$height)
mising

# remove outliers
mydata <- mydata[mydata$diameter < 60, ]
mydata

#Ignore NA values in dataset
median(mydata[ ,"height"], na.rm = T)
# calculate mean and standard deviation
mean_diameter <- mean(mydata$diameter)
mean_diameter
sd_leafarea <- sd(mydata$leafarea)
sd_leafarea

# identify duplicates
duplicates <- duplicated(mydata)
duplicates
# remove duplicates
myuniquedata <- unique(mydata)

#recoding

mydata$diameter1[mydata$diameter<=30]='small diameter'
mydata$diameter1[mydata$diameter>=40 & mydata$diameter>=50]='medium diameter'
mydata$diameter1[mydata$diameter>=60]='large diameter'
View(mydata)

#Sorting
mysort<-cleaned_data[order(cleaned_data$species,cleaned_data$diameter),]
View(mysort) #Data after sorting

#Filter the dataset according to specific criteria
filter1<-cleaned_data[cleaned_data$diameter < 30,]
View(filter1)
filter1<-clleaned_data[cleaned_data$species=="PIMO",c(1,3,4)]
View(filter1)

# Data Visualization
#Install tidyverse package

install.packages("tidyverse")

library(ggplot2)
completed_data<-mydata[complete.cases(mydata),]
draw1<-ggplot(completed_data,aes(x=diameter,y=leafarea))+geom_point()
draw1

#Data visualizing using Histogram

draw_hist<-ggplot(mydata, aes(diameter,fill=species))
draw_hist + geom_histogram(binwidth = 7,alpha=1)+ggtitle("The Diameters")+
  labs(x="The number of Diameter",y="The count")

#Data visualizing using Barchart

bar1<- ggplot(cleaned_data, aes(x= species,fill = branchmass))
bar1 + geom_bar() + theme_light()+labs(y="The Count",
                                      title="The Branchmass Rate")+
  facet_wrap(~ diameter) 

#Data visualizing using ScatterBlot

scatter1<-ggplot(mydata, aes(diameter,leafarea))
scatter1+ geom_point(aes(color=species),fill="white")+
  theme_light()+ stat_smooth(se=FALSE)



