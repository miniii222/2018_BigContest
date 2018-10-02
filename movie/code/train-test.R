#preprocessing

library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)

#movie
movie<-read_csv('movie4.csv')

#main character na -> blank
movie[is.na(movie$main1),'main1']<-''
movie[is.na(movie$main2),'main2']<-''
movie[is.na(movie$main3),'main3']<-''

#factor
columns<-c('genre','grade','type2','type3','month','dist_factor2','dist_factor1')
us_train_data[,columns]<-lapply(us_train_data[,columns], factor)
us_test_data[,columns]<-lapply(us_test_data[,columns], factor)

#main character 괄호 뒤 지우기
movie$main1<-gsub("\\(.*","",movie$main1)
movie$main2<-gsub("\\(.*","",movie$main2)
movie$main3<-gsub("\\(.*","",movie$main3)

movie<-movie %>% filter(movie$y>=10000)

#moviek 
moviek<-read_csv('moviek28.csv')
moviek<-moviek %>% select(-main1_to_m,-main1_to_s,-main2_to_m,-main2_to_s,
                          -main3_to_m,-main3_to_s,-med_dir,-mean_dir,
                          -dir_to_s,-country,-actor)

moviek[is.na(moviek$main1),'main1']<-''
moviek[is.na(moviek$main2),'main2']<-''
moviek[is.na(moviek$main3),'main3']<-''


#movieus
movieus<-read_csv('usmovie2.csv')
movieus<-movieus %>% select(-med_dir,-mean_dir,-dir_to_s,-country)

movieus[is.na(movieus$main1),'main1']<-''
movieus[is.na(movieus$main2),'main2']<-''
movieus[is.na(movieus$main3),'main3']<-''

n<-nrow(moviek)
p<-0.8

train.ind<-sample(n,as.integer(n*p))  #get train index randomly
train.data<-moviek[train.ind,]       #get train data
test.data<-moviek[-train.ind,]

write.csv(train.data, 'kor_train_data.csv',row.names = F)
write.csv(test.data, 'kor_test_data.csv',row.names = F)
