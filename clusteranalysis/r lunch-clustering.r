#################################
##      Cluster Analysis       ##
## R Learning Lunch March. 3rd ##
#################################

################################
## This next line of code will clear your working environment to you can start "fresh"
rm(list=ls())
################################

# Install packaged you want to use first #
#Note: You may already have some of these packages.
# Once it's installed once, there is no need to install it again.
install.packages("NbClust")
install.packages("clValid")
install.packages("kohonen")
install.packages("psych")
install.packages("nFactors")
#########################################
#####
#
# Load in data
#
#####
setwd("C:/Users/asathish/Desktop/R Workshop/R Lunch Round 2")
DAT <- read.csv( "Raw Data.csv" , stringsAsFactors=FALSE , check.names=FALSE , colClasses="character" )
#N# QC
DAT[1:5,1:5]
DAT[(nrow(DAT)-5):nrow(DAT),1:5]
#P# QC looks good
set.seed(123)

###########################################################
#Recode Matrix Q1

col.raw <- grep( "Please indicate how much you agree or disagree with each of the following statements about Product A" , names(DAT) , fixed=TRUE , value=TRUE )
col.raw
#N# Loop through columns
for ( col in col.raw ) {
  #N# Define new column name
  col.new <- unlist(strsplit( col , split="-" , fixed=TRUE ))
  col.new <- col.new[2]
  col.new <- paste0( "PREP.Table_Statements." , col.new )
  #N# Convert to numeric	
  DAT[,col.new] <- NA
  DAT[,col.new][DAT[,col]=="Strongly Disagree"] <- 1
  DAT[,col.new][DAT[,col]=="Somewhat Disagree"] <- 2
  DAT[,col.new][DAT[,col]=="Neither Agree nor Disagree"] <- 3
  DAT[,col.new][DAT[,col]=="Somewhat Agree"] <- 4
  DAT[,col.new][DAT[,col]=="Strongly  Agree"] <- 5
  DAT[,col.new] <- as.numeric( DAT[,col.new] )
  #N# Standardize
#  DAT[,col.new] <- scale(DAT[,col.new])
  #N# QC
  print( col.new )
  print( table( DAT[,col] , round(DAT[,col.new],3) , useNA='a' ) )
  
}



#########################################################
##Recode Matrix Q2

col.raw <- grep( "For each statement below, please tell us your level of agreement..." , names(DAT) , fixed=TRUE , value=TRUE )
col.raw
#N# Loop through columns
for ( col in col.raw ) {
  #N# Define new column name
  col.new <- unlist(strsplit( col , split="-" , fixed=TRUE ))
  col.new <- col.new[2]
  col.new <- paste0( "PREP.Recycling." , col.new )
  #N# Convert to numeric	
  DAT[,col.new] <- NA
  DAT[,col.new][DAT[,col]=="Strongly Disagree"] <- 1
  DAT[,col.new][DAT[,col]=="Somewhat Disagree"] <- 2
  DAT[,col.new][DAT[,col]=="Neither Agree nor Disagree"] <- 3
  DAT[,col.new][DAT[,col]=="Somewhat Agree"] <- 4
  DAT[,col.new][DAT[,col]=="Strongly Agree"] <- 5
  DAT[,col.new] <- as.numeric( DAT[,col.new] )
  #N# Standardize
#  DAT[,col.new] <- scale(DAT[,col.new])
  #N# QC
  print( col.new )
  print( table( DAT[,col] , round(DAT[,col.new],3) , useNA='a' ) )
  
}

##############################################################
##Recode Matrix Q3

col.raw <- grep( "We would like to know more about your purchasing style. Please indicate how much agree with the f..." , names(DAT) , fixed=TRUE , value=TRUE )
col.raw
#N# Loop through columns
for ( col in col.raw ) {
  #N# Define new column name
  col.new <- unlist(strsplit( col , split="-" , fixed=TRUE ))
  col.new <- col.new[2]
  col.new <- paste0( "PREP.Buying_Style." , col.new )
  #N# Convert to numeric	
  DAT[,col.new] <- NA
  DAT[,col.new][DAT[,col]=="Strongly Disagree"] <- 1
  DAT[,col.new][DAT[,col]=="Somewhat Disagree"] <- 2
  DAT[,col.new][DAT[,col]=="Neither Agree nor Disagree"] <- 3
  DAT[,col.new][DAT[,col]=="Somewhat Agree"] <- 4
  DAT[,col.new][DAT[,col]=="Strongly Agree"] <- 5
  DAT[,col.new] <- as.numeric( DAT[,col.new] )
  #N# Standardize
#  DAT[,col.new] <- scale(DAT[,col.new])
  #N# QC
  print( col.new )
  print( table( DAT[,col] , round(DAT[,col.new],3) , useNA='a' ) )
  
}

##############################################################
##Recode Age
DAT$PREP.Age <- NA
DAT$PREP.Age[DAT$Age == '18 to 34'] <- 27
DAT$PREP.Age[DAT$Age == '35 to 54'] <- 44.5
DAT$PREP.Age[DAT$Age == '55 & over'] <- 65
table(DAT$PREP.Age)
DAT$PREP.Age <- scale(DAT$PREP.Age)
DAT$PREP.Age <- DAT$PREP.Age - min(DAT$PREP.Age, na.rm = TRUE)
DAT$PREP.Age <- DAT$PREP.Age / max(DAT$PREP.Age, na.rm = TRUE)

##############################################################
##Recode Gender
table(DAT$Gender)
DAT$PREP.Gender <- NA
DAT$PREP.Gender[DAT$Gender == 'Male'] <- 0
DAT$PREP.Gender[DAT$Gender == 'Female'] <- 1
table(DAT$PREP.Gender, useNA='a')

##############################################################
##Recode Brand Used

table (DAT$`Brand Used`, useNA='a')
Brand.Used <- unique (DAT$`Brand Used`)
for (Brand in Brand.Used){
  col.new <- paste0("PREP.Brand.", Brand)
  DAT[,col.new] <- ifelse(DAT$`Brand Used` == Brand,1,0)
#N#QC
  print(Brand)
  print(table(DAT$`Brand Used`, DAT[,col.new], useNA='a'))
}

##################################################################################
##Data set with recoded variables
colnames(DAT)
x1 <- DAT[38:70]
colnames(x1)
x2 <- na.omit(x1)


#### Determining the number of factors
library(nFactors)
ev <- eigen(cor(x2)) # get eigenvalues
ap <- parallel(subject=nrow(x2),var=ncol(x2),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)


##### Running rotated PCA
library(psych)
###### 8 factors
fit <- principal(x2,nfactors=6,rotate="varimax")
fit.loadings <- fit$loadings
write.csv(fit.loadings,"Factor Loadings (check2).csv")
data <- cbind(x2,fit$scores)
data2 <- data[34:39]

######################################################
#Subset variables we recoded (Stuff with "PREP." in the beginning) 
Cluster.Data <- DAT[,(grep("PREP.",names(DAT),fixed=TRUE, value=TRUE))]

###################################

# Diagnostic testing

library(NbClust)
nb <- NbClust(Cluster.Data, min.nc = 2,
              max.nc = 8, method = "complete", index = "all")
# 
 library(clValid)
 library(kohonen)
intern <- clValid(Cluster.Data,
                  2:8,
                  clMethods = c("hierarchical",
                                "kmeans"),
                  validation = "internal",
                  maxitems = 5000,
                  metric = "euclidean",
                  method = "ward")
summary(intern)


##################################################################
# Create Cluster Solutions (Hierarchical Clustering) with indexed variables
#N# Identify the clustering columns
col.cluster <- DAT[,(grep("PREP.",names(DAT),fixed=TRUE, value=TRUE))]

#N# Create the distance matrix
dist.euclidean <- proxy::dist( data2[,col.cluster] , method="Euclidean" )

#N# Visualize distance matrix
image( as.matrix(dist.euclidean) )

#N# Create the dendogram
##Wards Distance
hclust.ward <- hclust( dist.euclidean, method="ward.D")

##Dendrogram 6 clusters Wards Method
png("Ward Dendrogram.png", width=10, height=8, units='in',res=300,pointsize=8)
plot( hclust.ward )
dev.off()


#N#Clustering variable
hclust.ward.6 <- cutree(hclust.ward,6)
