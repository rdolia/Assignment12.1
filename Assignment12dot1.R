#Assignment12.1
my_data <- read.delim("community.txt",sep = ",")

#a Find top attributes having highest correlation (select only Numeric)
options(max.print = 99999)
nums <- unlist(lapply(my_data, is.numeric))  
numeric_attributes<- my_data[,nums]
correlation <-as.data.frame(cor(numeric_attributes))

#replacing all values of 1 with 0.
correlation[correlation==1]<-0
correlation1<-as.matrix(correlation)
## function to return n largest values and position for matrix 
nlargest <- function(correlation1, n, sim = TRUE) {
  mult <- 1;
  if (sim) mult <- 2;
  res <- order(correlation1)[seq_len(n) * mult];
  pos <- arrayInd(res, dim(correlation1), useNames = TRUE);
  list(values = correlation1[res],
       position = pos)
}

nlargest(correlation1, 5);
#indices for 5 largest correlations are 

#$position
#row col
#[1,]  62  63
#[2,]  16  19
#[3,]   8  62
#[4,]  62  93
#[5,]  46  52

#Values for the above 5 correlations are as below :
correlation1[62,63]
correlation1[16,19]
correlation1[8,62]
correlation1[62,93]
correlation1[46,52]
#b Find out top 3 reasons for having  more crime in city
#The column X0.2.2 is ViolentCrimesPerPop which best identifies having more crime.
# Hence we will take the 3 highest correlations for this column and identify the 3 reasons.
violent3<-correlation1[,102]
violent3<-as.matrix(violent3)
sort(violent3,decreasing = TRUE)
#The 3 highest values are 0.73796471  0.63127917  0.57468959 which are for 
#X.0.14,X.0.02,X.0.15.
#Highest reason -      - PctRecImmig5- 0.73796471
#Second highest reason - agePct65up  - 0.63127917
#Third highest reason  - AsianPerCap - 0.57468959

#Which all attributes have high attribute with crime rate.
library(sqldf)
correlation1 <- as.data.frame(correlation1)
#Choosing correlations that are higher than 0.5
high_corr <- sqldf("SELECT * 
      FROM  correlation1
      WHERE  `X0.2.2` > '0.5'")
high_corr
#Choosing correlations that are less than -0.5
high_corr_neg <- sqldf("SELECT * 
                   FROM  correlation1
                   WHERE  `X0.2.2` < '-0.5'")
high_corr_neg
