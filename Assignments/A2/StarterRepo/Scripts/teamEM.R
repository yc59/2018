load("/Users/zhangliang/Desktop/FishLengths.RData")

fish<- x[order(x$Length),]
rm(x)

#######################################
# Change all the data to 1 before the first age＝2

# Find the of the location for the first 2 occurrence
r<- which(fish$Age == 2)
r1<- r[1]

##1. Figure the length of the data belongting to the group of age=2
length.1<- as.numeric(r1)-1
# use is.na function to change the na value
group.1<- fish$Age[1:length.1]
seq(along = group.1)[is.na(group.1)]
group.1[is.na(group.1)]<-1

# take place of the orginal data which belong to the group of age=2
fish$Age[1:length.1]<-group.1

##2.Change all the data to 2 before the first age＝3 occures

# Find the of the location for the first 2 occurrence
r2<- which(fish$Age == 3)
r3<- r2[1]

# Figure the location of the last missing value 
# before the first age=3 appearing
length.2<- as.numeric(r3)-1

# use is.na function to change the na for the group of age=2
group.2<- fish$Age[(length.1+1):length.2]
seq(along = group.2)[is.na(group.2)]
group.2[is.na(group.2)]<-2

# take place of the orginal data which belong to the group of age=2
fish$Age[(length.1+1):length.2]<-group.2

## 3.change the rest na which may be located in age group3
group.3<- fish$Age[(length.2+1):1000]
seq(along = group.3)[is.na(group.3)]
group.3[is.na(group.3)]<-3
# take place of the orginal data which belong to the group of age=2
fish$Age[(length.2+1):1000]<-group.3

#######################################
## 1.compute the mean of initial values
for (i in 1:3) {
 fish.value<- vector()
 fish.mean<- vector() 
 fish.value<- fish$Length[which(fish$Age==i)]
 fish.mean[i]<- mean(fish.value)
 print(fish.mean[i])
 i<- i+1
} 

## 2.compute the variances of initial values
for (i in 1:3) {
  fish.values<- vector()
  fish.variance<- vector() 
  fish.values<- fish$Length[which(fish$Age==i)]
  fish.variance[i]<- var(fish.values)
  print(fish.variance[i])
  i<- i+1
} 


teamEM <- function(){
  
}
