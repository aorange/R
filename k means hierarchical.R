get_type <-function(name){
  type<- "none" #this is the code for no activity, declared like this because idk how to make var without decs in r
  if(grepl("brush_teeth",name)){
    type="brush_teeth"
  }else if(grepl("climb_stairs",name)){
    type="climb_stairs"
  }else if(grepl("comb_hair",name)){
    type="comb_hair"
  }else if(grepl("descend_stairs",name)){
    type="descend_stairs"
  }else if(grepl("drink_glass",name)){
    type="drink_glass"
  }else if(grepl("eat_meat",name)){
    type="eat_meat"
  }else if(grepl("eat_soup",name)){
    type="eat_soup"
  }else if(grepl("getup_bed",name)){
    type="getup_bed"
  }else if(grepl("liedown_bed",name)){
    type="liedown_bed"
  }else if(grepl("pour_water",name)){
    type="pour_water"
  }else if(grepl("sitdown_chair",name)){
    type="sitdown_chair"
  }else if(grepl("standup_chair",name)){
    type="standup_chair"
  }else if(grepl("use_telephone",name)){
    type="use_telephone"
  }else if(grepl("walk",name)){
    type="walk"
  }#14 conditions because that's how many classes there are
  return(type)
}

closest_cluster <-function(sig, centers){
  if(is.null(centers)){#just a check because some first layer might not have had 20 elems
    return(-1)
  }
  closest_index<-0
  smallest_dist<--1
  for(x in c(1:nrow(centers))){
    curr_dist = as.numeric(dist(rbind(sig, centers[x,])))
    if(curr_dist < smallest_dist || smallest_dist==-1){
      smallest_dist = curr_dist
      closest_index = x;
    }
  }
  return(closest_index)
}

setwd("/Users/aorange/Documents/CS 361/cs361 final") #this is where all my data files are
filenames = list.files()  #get the name of all the files
#sample and split into test files and training files
testing = sample(filenames, size=168, replace=F) #839*.2 = ~168
training = setdiff(filenames, testing) 

#obtain subsignals
subsignals <- matrix(ncol=96, nrow=14000) #preallocated to avoid the 2nd circle of hell
i<-1
for(name in training){  #for each file, get 32 subsignals of 1 second length
  data <-read.table(name, sep="", header=F)  #get all the values from the file
  data <-as.numeric(unlist(data, use.names=F)) #make the dataframe into a vector
  amt_signals = floor(length(data)/96)  #signal lengths of 1 second, 32Hz*3 dimensions
  for(signal in 1:amt_signals){ 
     subsignals[i,]<-data[(1+96*(signal-1)):(96*signal)] #our subsignals are 96 numbers
     i=i+1
  }
}

#hierarchical k-means starting here
main_clusters = 20 #change values to experiment
sub_clusters = 20

subsignals<-na.omit(subsignals) #remove empty rows that we allocated at the start
first_layer = kmeans(subsignals, centers=main_clusters)#start with k=20
#separate subsignals into their clusters and begin calculating subclusters
subsignals<-cbind(subsignals, first_layer$cluster)
second_layer <-vector("list", main_clusters) #same size as first layer centers
#generate kmeans for the second layer
for(i in c(1:sub_clusters)){
  temp = subsignals[subsignals[,97]==i,]
  if(nrow(temp) >= sub_clusters){
    temp <- temp[,-97]
    second_layer[[i]] = kmeans(temp, centers=sub_clusters)
  }
}

subsignals<-subsignals[,-97] #remove the last column for later calculations

#feature vector creation for test set and training set
feature_vectors_test <- matrix(0,ncol=main_clusters*sub_clusters, nrow=168) #declare matrices at 0 because that means no cluster hit
feature_vectors_train <-matrix(0,ncol=main_clusters*sub_clusters, nrow=671)
feature_vectors_test<-as.data.frame(feature_vectors_test)
feature_vectors_train<-as.data.frame(feature_vectors_train)
feature_vectors_test_labels <-matrix(ncol=1, nrow=168)
feature_vectors_train_labels <-matrix(ncol=1, nrow=671)
for(i in c(1:671)){
  name <- training[[i]]
  type <- get_type(name)
  #get all the subsignals in the file
  data <-read.table(name, sep="", header=F)  #get all the values from the file
  data <-as.numeric(unlist(data, use.names=F)) #make the dataframe into a vector
  amt_signals = floor(length(data)/96)  #signal lengths of 1 second, 32Hz*3 dimensions
  for(signal in 1:amt_signals){ 
    temp<- data[(1+96*(signal-1)):(96*signal)] #this is our subsignal
    #calculate closest cluster center here
    closest_main<- closest_cluster(temp, first_layer$centers)
    #now calculate closest secondary cluster within main cluster
    closest_sub<- closest_cluster(temp, second_layer[[closest_main]]$centers)
    if(closest_sub == -1){
      next
    }
    #increment the corresponding cluster center
    feature_vectors_train[i, ((closest_main-1)*sub_clusters)+closest_sub]<- feature_vectors_train[i, ((closest_main-1)*20)+closest_sub]+1
  }

  #assign the class label
  feature_vectors_train_labels[i,1] <- type
}
#repeat for testing data
for(i in c(1:168)){
  name <- testing[[i]]
  type <- get_type(name)
  #get all the subsignals in the file
  data <-read.table(name, sep="", header=F)  #get all the values from the file
  data <-as.numeric(unlist(data, use.names=F)) #make the dataframe into a vector
  amt_signals = floor(length(data)/96)  #signal lengths of 1 second, 32Hz*3 dimensions
  for(signal in 1:amt_signals){ 
    temp<- data[(1+96*(signal-1)):(96*signal)] #this is our subsignal
    #calculate closest cluster center here
    closest_main<- closest_cluster(temp, first_layer$centers)
    #now calculate closest secondary cluster within main cluster
    closest_sub<- closest_cluster(temp, second_layer[[closest_main]]$centers)
    if(closest_sub == -1){
      next
    }
    #increment the corresponding cluster center
    feature_vectors_test[i, ((closest_main-1)*sub_clusters)+closest_sub]<- feature_vectors_test[i, ((closest_main-1)*20)+closest_sub]+1
  }
  
  #assign the class label
  feature_vectors_test_labels[i,1] <- type
}
feature_vectors_test_labels = factor(feature_vectors_test_labels
                                     , levels=c("brush_teeth", "climb_stairs","comb_hair", "descend_stairs",
                                                "drink_glass", "eat_meat", "eat_soup", "getup_bed"
                                                ,"liedown_bed", "pour_water", "sitdown_chair",
                                                "standup_chair", "use_telephone", "walk"))
feature_vectors_train_labels = factor(feature_vectors_train_labels
                                     , levels=c("brush_teeth", "climb_stairs","comb_hair", "descend_stairs",
                                                "drink_glass", "eat_meat", "eat_soup", "getup_bed"
                                                ,"liedown_bed", "pour_water", "sitdown_chair",
                                                "standup_chair", "use_telephone", "walk"))
#feature_vectors_test[,401]<-as.factor(feature_vectors_test[,401])
#feature_vectors_train[,401]<-as.factor(feature_vectors_train[,401])
#classification using random forest
library(randomForest)
rf<- randomForest(x=feature_vectors_train, y=feature_vectors_train_labels
             , xtest=feature_vectors_test, ytest=feature_vectors_test_labels)



