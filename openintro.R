setwd("/Users/mau/github/practice")

###Plot page 41 of the book, Figure 2.1###
loan50 <- read.delim("loan50.csv",header=TRUE,sep=",") #load the data

###Setting axes labels and tick marks in advance
l_xmarks <- c(0,50000,100000,150000,200000,250000,300000,350000)
l_xlabels <- c("$0","$50k","$100k","$150k","$200k","$250k","$300k","$350k")
l_ymarks <- c(0,10000,20000,30000,40000)
l_ylabels <- c("$0","$10k","$20k","$30k","$40k")

#Drawing a simple plot
plot(loan_amount~total_income,data=loan50,xlim=c(0,350000),ylim=c(0,40000),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
title(xlab="Total Income",ylab="Loan Amount",cex.lab=.9)
axis(1,tick=TRUE,at=l_xmarks,labels=l_xlabels,cex.axis=.8)
axis(2,tick=TRUE,at=l_ymarks,labels=l_ylabels,las=2,cex.axis=.8)
abline(h=l_ymarks,v=l_xmarks,col=c("grey95","grey95"))
points(loan_amount~total_income,data=loan50,pch=19,col="grey")
#box(col = 'red')

###Exploring the plot possibilities
# 1) 
#I want to modify the plot region and background colours
par(bg="#44ACAA44",fg="#000000FF")
plot(loan_amount~total_income,data=loan50,xlim=c(0,350000),ylim=c(0,40000),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
title(xlab="Total Income",ylab="Loan Amount",cex.lab=.9,col.lab="#44447799")
axis(1,tick=TRUE,at=l_xmarks,labels=l_xlabels,cex.axis=.8,col="#44447799",col.axis="#44447799")
axis(2,tick=TRUE,at=l_ymarks,labels=l_ylabels,las=2,cex.axis=.8,col="#44447799",col.axis="#44447799")
abline(h=l_ymarks,v=l_xmarks,col=c("grey95","grey95"))
points(loan_amount~total_income,data=loan50,pch=19,col="#8877AACC")
#By including the "bg" option in "par()" I can only colour the whole background 
#of the plot, which is a bit ugly.

# 2) 
#I can use a function to simulate the colouring of the plot rectangle area rather
#than the whole background of the plot. It can be used within the plot option 
#"panel.first": panel.first=plot_region(). Here I use it within the points function, 
#otherwise it would be evaluated too early and it would return an error
plot_region <- function() {
  points(0, 0, pch=16, cex=1e6, col="#FFFFFF") #FFFFFF: white
  grid(NA,ny=NULL,col="#EAF2F3",lty=1,lwd=2)
}
#I also want to use a Stata like background
par(bg="#EAF2F3") #(Stata ltbluishgray: #EAF2F3)
plot(loan_amount~total_income,data=loan50,xlim=c(0,350000),ylim=c(0,40000),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
title(xlab="Total Income",ylab="Loan Amount",cex.lab=.9,col.lab="#696969")
axis(1,tick=TRUE,at=l_xmarks,labels=l_xlabels,cex.axis=.8,col="#696969",col.axis="#696969")
axis(2,tick=TRUE,at=l_ymarks,labels=l_ylabels,las=2,cex.axis=.8,col="#696969",col.axis="#696969")
abline(h=l_ymarks,v=l_xmarks,col=c("grey95","grey95"))
points(loan_amount~total_income,data=loan50,pch=19,col="#8877AACC",panel.first=plot_region())

# 3) 
#Use a light grey (grey93) Stata like background
plot_region <- function() {
  points(0, 0, pch=16, cex=1e6, col="#FFFFFF")
  grid(NA,ny=NULL,col="#EDEDED",lty=1,lwd=2)
}
par(bg="#EDEDED") #(Azure mist: #F0FFFF; Indigo white: #EBF6F7; Italian ice: #E9F6EF; Indigo white seems the nearest to the Stata graph colour scheme)
plot(loan_amount~total_income,data=loan50,xlim=c(0,350000),ylim=c(0,40000),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
title(xlab="Total Income",ylab="Loan Amount",cex.lab=.9,col.lab="#44447799")
axis(1,tick=TRUE,at=l_xmarks,labels=l_xlabels,cex.axis=.8,col="#44447799",col.axis="#44447799")
axis(2,tick=TRUE,at=l_ymarks,labels=l_ylabels,las=2,cex.axis=.8,col="#44447799",col.axis="#44447799")
abline(h=l_ymarks,v=l_xmarks,col=c("#EDEDED","#EDEDED"))
points(loan_amount~total_income,data=loan50,pch=19,col="#8877AACC",panel.first=plot_region())
###

###Plot page 42 of the book, Figure 2.2###
county <- read.delim("county.csv",header=TRUE,sep=",")

c_xmarks <- c(0,10,20,30,40,50)
c_xlabels <- c("0%","10%","20%","30%","40%","50%")
c_ymarks <- c(0,20000,40000,60000,80000,100000,120000)
c_ylabels <- c("$0","$20k","$40k","$60k","$80k","$100k","$120k")

plot(median_hh_income~poverty,data=county,xlim=c(0,52),ylim=c(0,130000),col="#FFFFFF",xaxt="n",yaxt="n",xlab="",ylab="")
title(xlab="Poverty Rate (Percent)",ylab="Median Household Income",cex.lab=.8)
axis(1,at=c_xmarks,labels=c_xlabels,cex.axis=.8)
axis(2,at=c_ymarks,labels=c_ylabels,cex.axis=.8,las=2)
abline(h=c_ymarks,v=c_xmarks,col=c("#EDEDED","#EDEDED"))
points(median_hh_income~poverty,data=county,pch=21,lwd=1.3,cex=.6,bg="#808080",col="#C0C0C0")

#Then I estimate a non-linear model on the data, by using poverty and its square as independent variables
county$pov_sq <- county$poverty^2
nl_model <- lm(county$median_hh_income~county$poverty+county$pov_sq)
yhat <- predict(nl_model,newdata <- county)
points(yhat~county$poverty,xlim=c(0,52),ylim=c(0,130000),type="p",cex=.1)
###

###Plot page 45 of the book, Figure 2.6###
loan50 <- read.delim("loan50.csv",header=TRUE,sep=",") #load the data
hist(loan50$interest_rate,breaks=c(5,7.5,10,12.5,15,17.5,20,22.5,25,27.5),xlim=c(5,30),xaxt="n",main="Counts for the binned interest_rate data",xlab="Interest Rate")
axis(1,at=c(5,10,15,20,25),labels=c("5%","10%","15%","20%","25%"))
