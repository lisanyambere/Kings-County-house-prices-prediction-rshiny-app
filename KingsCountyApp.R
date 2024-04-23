##Importing relevant packages
library(tidyverse)
library(tidymodels)
library(ggplot2)
library(corrplot)
library(randomForest)
library(dplyr)
library(caTools)
library(Amelia)
library(caret)
library(rpart)
library(rpart.plot)
library(cluster)
library(party)
library(shiny)
library(shinydashboard)
library(leaflet)
## Importing the Kings County house prices data
data<-read.csv("Kings County Data Set.csv")
##Data exploration
head(data)
dim(data) # data has 19 variables/columns with 21613 entries/rows
summary(data)
str(data) #data structure
sum(is.na(data))#checking for missing values. there are 8 missing values
sapply(data,class)#cecking if the data types are consistent 
##Transforming Date from character to date
data$Date<-(substr(data$Date,1,8)) ##removes the extra characters from the data and returns the first eight values

data$Date<-as.Date(data$Date,format = "%Y%m%d")
head(data) ##confirming
sapply(data,class)

data$Year_built<-as.Date(data$Year_built,format="%Y") #traforming year built into date
data$renovation<-NULL
data$renovation[data$Year_renovated>0]=1 #1 means that the house has been renovated
data$renovation[data$Year_renovated==0]=0 #0 indicates no renovation has been done on the house
data$renovation<-as.factor(data$renovation) #factorizing renovation
data$View<-as.factor(data$View) #factorizing view
data$Waterfront<-as.factor(data$Waterfront)
head(data)
sapply(data,class)
##
missmap(data)
data<-na.omit(data) #dropping missing values
sum(is.na(data))
##

sum(duplicated(data)) #checkig for duplicates
#removing some columns
data <- data[, !colnames(data) %in% c("Year_renovated", "Sqft_above", "Sqft_basement","Sqft_lot","Id")]
head(data)
data<-data[,!colnames(data) %in%"Date"]
##outlier detection
boxplot(data$Price,data$Bedrooms,data$Bathrooms,data$Floors,data$Sqft_living,data$Sqft_lot,data$Grade,names=c("price","Bedroom","bathrooms","floors","Sqft_living","sqft_lot","grade"),main="Boxplots of numeric variables")
#
boxplot(data$Bedrooms)
summary(data$Bedrooms)  
data<-subset(data,data$Bedrooms!=33)#removing outlier
boxplot(data$Bedrooms)
##correlation matrix
numeric_col<-data[sapply(data, is.numeric)] #putting numeric columns together
data.cor<-cor(numeric_col)
corrplot(data.cor) #correlation matrix
data.cor
##MODELLING
#Linear regression
set.seed(110)
lmtrain<- sample.split(data$Price, SplitRatio = 0.8) #splittig dataset into training and testing sets

training1<- data[lmtrain,]

testing1<- data[-lmtrain,]

regr<- lm(Price~., data=training1)
summary(regr)

#evaluating using MSE
prediction1 <- predict(regr, newdata = testing1)
mse <- mean((prediction1 - testing1$Price)^2)
print(paste("mse:", mse))

##Decision trees
set.seed(110)
attach(data)
indtrain<-sample.split(data$Price, SplitRatio =0.8)

training2<-data[indtrain,]
testing2<-data[-indtrain,]

myc<-rpart.control(minsplit=1, minbucket=2, maxdepth=10, cp=0.01)
mypred<-rpart(Price~., data=training2, control=myc)
rpart.plot(mypred, main="Kings County House Sales")
 #evaluating
mypred<-rpart(Price~., data=training2, control=myc)
pred<-predict(mypred, newdata=testing2)

mse <- mean((pred - testing2$Price)^2)
print(mse)

##Random Forest model
rftrain<-sample.split(data$Price, SplitRatio =0.8)
training3<-data[rftrain,]
testing3<-data[-rftrain,]
rfmodel<- randomForest(Price~., data=training3, ntree = 200)
print(rfmodel)

#evaluating
rfpredict <- predict(rfmodel, newdata = testing3)
mse3 <- mean((rfpredict - testing3$Price)^2)
print(mse3)
## Validation
#The Random forest model performs better compared to other models




##Rshiny dashboard
ui<-fluidPage(
  titlePanel("King's County House Prices Prediction"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("Sqft_living","square footage",min = 500,max = 10000,value=2000),
      numericInput("Bedrooms","Number of bedrooms",3),
      numericInput("Bathrooms","Number of bathrooms",2),
      numericInput("Grade","Grade of the house",7),
      selectInput("renovation","Is the house renovated",choices=c("No"=0,"Yes"=1)),
      numericInput("Year_built","Year built",1980),
      selectInput("View","View of the house",choices = c("No"=0,"Yes"=1)),
      selectInput("Waterfront","Waterfront",choices = c("No"=0,"Yes"=1)),
      numericInput("Floors","Number of Floors",1),
      numericInput("Zipcode","Zipcode of the house",98125),
      numericInput("Condition","Condition of the house",3),
      numericInput("Latitude", "Latitude of the house", 47.5112),
      numericInput("Longitude", "Longitude of the house", -122.257),
      actionButton("predict","predict price")
      
    ),
    mainPanel(
      textOutput("prediction"),
      leafletOutput("map")
    )
  )
)

server<-function(input,output){
  prediction<-eventReactive(input$predict, {
    new_data<-data.frame(
      Sqft_living=input$Sqft_living,
      Bedrooms=as.integer(input$Bedrooms),
      Bathrooms=as.numeric(input$Bathrooms),
      Grade=as.integer(input$Grade),
      Floors=as.numeric(input$Floors),
      renovation = as.factor(input$renovation),
      Year_built = as.Date(input$Year_built),
      View = as.factor(input$View),
      Waterfront = as.factor(input$Waterfront),
      Zipcode=as.integer(input$Zipcode),
      Condition=as.integer(input$Condition),
      Latitude=as.numeric(input$Latitude),
      Longitude=as.numeric(input$Longitude)
      
    )
    predict(regr,new_data)
  })
  output$prediction<-renderText({
    if(input$predict>0){
      paste("Predicted price: $",round(prediction(), 2))
    }
  })
  output$map<-renderLeaflet({
    leaflet() %>%
    addTiles()%>%
    addMarkers(
      lng=input$Longitude,
      lat=input$Latitude,
      popup = paste("Predicted price: $",round(prediction(), 2))
    )
  })
  observe({
    if(input$predict>0){
      leafletProxy("map", data = data.frame(lat = input$Latitude, lng = input$Longitude)) %>%
      clearMarkers()%>%
      addMarkers(
        lng=input$Longitude,
        lat=input$Latitude,
        popup = paste("Predicted price: $",round(prediction(), 2))
      )
    }
  })
  
}
shinyApp(ui = ui, server = server)
sapply(training3,class)
S
