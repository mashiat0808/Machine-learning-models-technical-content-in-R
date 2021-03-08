# 1.Read the data "oj.csv".

oj <- read.csv(file.choose())

# 2.Find the structure of the data, and analyze the type of the variables.

str(oj)
#this function shows the metadata and how the table is structured and the variable type as well
 
# 3.Check the number of rows in this data.

dim(oj)
#This fuction displays the columns and rows of the dataframe respectively

# 4.Print the first 10 rows of the Data
head(oj)
# this fuction by default displays the first 10 rows of the dataset
 
# 5.Rename Store as Shop
colnames(oj) <- c("shop","brand","week","logmove","feat","price","AGE60","EDUC","ETHNIC",
                  "INCOME","HHLARGE","WORKWOM","HVAL150","SSTRDIST","SSTRVOL","CPDIST5","CPWVOL5")

# 6.Filter the data and create a subset using  week, brand, store, income from Data where brand = Tropicana and store >50

dat1 <- subset(oj, oj$brand== "dominicks" & shop>50, select= c(week, brand, shop, INCOME) )
 

# 7.Make different Datasets where
# 
#   i)    brand starts with d,

dat2 <- oj[startsWith(oj$brand,"d"),]

#   ii)   brand ends with maid,

dat3 <- oj[endsWith(oj$brand,"maid"),]

#   iii)  brand must contain man,
#adding tidyverse package
dat4<- oj[str_subset(oj$brand, "man"),]

#   iv)   income is between 8 and 10

dt5<- oj[(oj$INCOME>=8 & oj$INCOME<=10),]
