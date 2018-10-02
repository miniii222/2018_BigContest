#us movie
library(readr)
library(dplyr)
library(stringr)
us_train_data <- read_csv("us_train_data.csv")


##main_to_numeric function
us_train_data<-main_to_numeric(us_train_data)
us_train_data %>% head()

############main1,2,3 NA처리 생각
us_train_data[is.na(us_train_data$main1),'main1_to_m']
us_train_data[is.na(us_train_data$main2),'main2']
us_train_data[is.na(us_train_data$main3),'main3']

#mean by main
main123_mean<-main_train_mean(us_train_data)

##dist_to_numeric
us_train_data<-us_dist_numeric(us_train_data)


#director_to_numeric
us_train_data<-dir_to_numeric(us_train_data)

#NA 
###delete
us_train_data2<-us_train_data[complete.cases(us_train_data),]

###lm
lm1<-lm(main1_to_m ~ first_count, data = us_train_data2)
us_train_data[is.na(us_train_data$main1_to_m),'main1_to_m']<-lm1$coef[1]+
      lm1$coef[2]*us_train_data[is.na(us_train_data$main1_to_m),'first_count']
lm12<-lm(main1_to_s ~ first_count, data = us_train_data2)
us_train_data[is.na(us_train_data$main1_to_s),'main1_to_s']<-lm12$coef[1]+
  lm12$coef[2]*us_train_data[is.na(us_train_data$main1_to_s),'first_count']

lm2<-lm(main2_to_m ~ first_count, data = us_train_data2)
us_train_data[is.na(us_train_data$main2_to_m),'main2_to_m']<-lm2$coef[1]+
  lm2$coef[2]*us_train_data[is.na(us_train_data$main2_to_m),'first_count']
lm22<-lm(main2_to_s ~ first_count, data = us_train_data2)
us_train_data[is.na(us_train_data$main2_to_s),'main2_to_s']<-lm22$coef[1]+
  lm22$coef[2]*us_train_data[is.na(us_train_data$main2_to_s),'first_count']

lm3<-lm(main3_to_m ~ first_count, data = us_train_data2)
us_train_data[is.na(us_train_data$main3_to_m),'main3_to_m']<-lm3$coef[1]+
  lm3$coef[2]*us_train_data[is.na(us_train_data$main3_to_m),'first_count']
lm32<-lm(main3_to_s ~ first_count, data = us_train_data2)
us_train_data[is.na(us_train_data$main3_to_s),'main3_to_s']<-lm32$coef[1]+
  lm32$coef[2]*us_train_data[is.na(us_train_data$main3_to_s),'first_count']




####test
us_test_data<-read_csv('us_test_data.csv')

#preprocessing
us_test_data<-left_join(us_test_data,main123_mean,c('main1' = 'actor_name')) %>% 
  rename(main1_to_m=mean_y, main1_to_s = stand_y) %>% 
  left_join(main123_mean,c('main2' = 'actor_name')) %>% 
  rename(main2_to_m=mean_y, main2_to_s = stand_y) %>%
  left_join(main123_mean,c('main3' = 'actor_name')) %>% 
  rename(main3_to_m=mean_y, main3_to_s = stand_y)

us_test_data %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.)))) %>% select(20:26)

us_test_data[is.na(us_test_data$main1_to_m),'main1_to_m']<-lm1$coef[1]+
  lm1$coef[2]*us_test_data[is.na(us_test_data$main1_to_m),'first_count']
us_test_data[is.na(us_test_data$main1_to_s),'main1_to_s']<-lm12$coef[1]+
  lm12$coef[2]*us_test_data[is.na(us_test_data$main1_to_s),'first_count']
us_test_data[is.na(us_test_data$main2_to_m),'main2_to_m']<-lm2$coef[1]+
  lm2$coef[2]*us_test_data[is.na(us_test_data$main2_to_m),'first_count']
us_test_data[is.na(us_test_data$main2_to_s),'main2_to_s']<-lm22$coef[1]+
  lm22$coef[2]*us_test_data[is.na(us_test_data$main2_to_s),'first_count']
us_test_data[is.na(us_test_data$main3_to_m),'main3_to_m']<-lm3$coef[1]+
  lm3$coef[2]*us_test_data[is.na(us_test_data$main3_to_m),'first_count']
us_test_data[is.na(us_test_data$main3_to_s),'main3_to_s']<-lm32$coef[1]+
  lm32$coef[2]*us_test_data[is.na(us_test_data$main3_to_s),'first_count']


#director
dir_list<-dir_to_list(us_test_data)
us_test_data<-left_join(us_test_data, dir_list,by=c('director'='dir_name'))


#dist
us_test_data<-us_dist_numeric(us_test_data)

rmse<-function(y,y_hat){
  
  sqrt(mean((y-y_hat)^2))
}


###modeling

#1.randomForest
library(randomForest)
rf1<-randomForest(y~b_screen+b_play+b_count+
          main1_to_s+main2_to_s+main3_to_s+dir_to_m+dist_factor2+
          dist_factor1+first_count, data = us_train_data)
varImpPlot(rf1)

rf1.pred<-predict(rf1, us_test_data)

rmse(us_test_data$y,rf1.pred)

#2.lm
lm1<-lm(y~b_screen+b_play+b_count+
          main1_to_s+main2_to_s+main3_to_s+dir_to_m+dist_factor2+
          dist_factor1+first_count, data = us_train_data)
lm1.pred<-predict(lm1,us_test_data)
rmse(us_test_data$y,lm1.pred)

lm1.s<-step(lm1)
lm1.s.pred<-predict(lm1.s,us_test_data)
rmse(us_test_data$y,lm1.s.pred)


#3.glm - gaussian
glm1<-glm(y~b_screen+b_play+b_count+
  main1_to_s+main2_to_s+main3_to_s+dir_to_m+dist_factor2+
    dist_factor1+first_count, family=gaussian, data = us_train_data)

glm1.pred<-predict(glm1,us_test_data)
glm1.s<-step(glm1); glm1.s.pred<-predict(glm1.s, us_test_data)
rmse(us_test_data$y,glm1.pred)
rmse(us_test_data$y,glm1.s.pred)

write.csv(us_train_data, 'us_train_data_lm.csv', row.names = F)
write.csv(us_test_data, 'us_test_data_lm.csv', row.names = F)






#spy data(나를 차버린 스파이)
spy<-data.frame(b_screen = 20, b_play = 61, b_count=10317,first_count=29368,
                dist_factor2=0,dist_factor1=0,grade='15세이상관람가',month=8,
                director="수잔나 포겔", main1="밀라 쿠니스", main2="케이트 맥키넌"
)


levels(spy$dist_factor2) <- levels(us_test_data$dist_factor2)
levels(spy$dist_factor1) <- levels(us_test_data$dist_factor1)

spy<-left_join(spy, main123_mean, by=c('main1' = 'actor_name')) %>% 
  rename(main1_to_m=mean_y, main1_to_s = stand_y) %>% 
  left_join(main123_mean, c('main2' = 'actor_name'))%>%
  rename(main2_to_m=mean_y, main2_to_s = stand_y)
colSums(is.na(spy))
spy$main3_to_m<-predict(lm3,spy); spy$main3_to_s<-predict(lm32,spy)

#director NA -> lm
lm_dir1<-lm(dir_to_m~first_count, data = us_train_data2)
lm_dir2<-lm(dir_to_s~first_count, data = us_train_data2)

spy$dir_to_m<-predict(lm_dir1, spy); spy$dir_to_s<-predict(lm_dir2, spy)
write.csv(spy, 'spy.csv', row.names = F)

lmf<-lm(y ~ grade + b_screen + b_play + first_count + main1_to_m + 
  main2_to_m + main3_to_m + dir_to_m, data = us_train_data)
rmse(predict(lmf, spy),spy$y)#321351.3

spy$dist_factor1<-as.factor(spy$dist_factor1)
spy$dist_factor2<-as.factor(spy$dist_factor2)

spy$y<-235062 #not_final

rmse(spy$y, predict(rf1, spy))
rmse(spy$y, predict(lm1.s, spy))


#korea movie
library(readr)
library(dplyr)
library(stringr)

ko_train_data<-read_csv("final_train_data_KO.csv")
ko_train_data<-ko_train_data[,-c(20:28)]

##main_to_numeric function
ko_train_data<-main_to_numeric(ko_train_data)
ko_train_data %>% head()

############main1,2,3 NA처리 생각
ko_train_data[is.na(ko_train_data$main1),'main1_to_m']
ko_train_data[is.na(ko_train_data$main2),'main2']
ko_train_data[is.na(ko_train_data$main3),'main3']

#mean by main
main123_mean<-main_train_mean(ko_train_data)

##dist_to_numeric
ko_train_data<-ko_dist_numeric(ko_train_data)


#director_to_numeric
ko_train_data<-dir_to_numeric(ko_train_data)

#NA 
###delete
ko_train_data2<-ko_train_data[complete.cases(ko_train_data),]

###lm
lm1<-lm(main1_to_m ~ first_count, data = ko_train_data2)
ko_train_data[is.na(ko_train_data$main1_to_m),'main1_to_m']<-lm1$coef[1]+
  lm1$coef[2]*ko_train_data[is.na(ko_train_data$main1_to_m),'first_count']
lm12<-lm(main1_to_s ~ first_count, data = ko_train_data2)
ko_train_data[is.na(ko_train_data$main1_to_s),'main1_to_s']<-lm12$coef[1]+
  lm12$coef[2]*ko_train_data[is.na(ko_train_data$main1_to_s),'first_count']

lm2<-lm(main2_to_m ~ first_count, data = ko_train_data2)
ko_train_data[is.na(ko_train_data$main2_to_m),'main2_to_m']<-lm2$coef[1]+
  lm2$coef[2]*ko_train_data[is.na(ko_train_data$main2_to_m),'first_count']
lm22<-lm(main2_to_s ~ first_count, data = ko_train_data2)
ko_train_data[is.na(ko_train_data$main2_to_s),'main2_to_s']<-lm22$coef[1]+
  lm22$coef[2]*ko_train_data[is.na(ko_train_data$main2_to_s),'first_count']

lm3<-lm(main3_to_m ~ first_count, data = ko_train_data2)
ko_train_data[is.na(ko_train_data$main3_to_m),'main3_to_m']<-lm3$coef[1]+
  lm3$coef[2]*ko_train_data[is.na(ko_train_data$main3_to_m),'first_count']
lm32<-lm(main3_to_s ~ first_count, data = ko_train_data2)
ko_train_data[is.na(ko_train_data$main3_to_s),'main3_to_s']<-lm32$coef[1]+
  lm32$coef[2]*ko_train_data[is.na(ko_train_data$main3_to_s),'first_count']

colSums(is.na(ko_train_data))


####test
ko_test_data<-read_csv('fianl_test_data_KO.csv')
ko_test_data<-ko_test_data[,-c(20:28)]
#preprocessing
ko_test_data<-left_join(ko_test_data,main123_mean,c('main1' = 'actor_name')) %>% 
  rename(main1_to_m=mean_y, main1_to_s = stand_y) %>% 
  left_join(main123_mean,c('main2' = 'actor_name')) %>% 
  rename(main2_to_m=mean_y, main2_to_s = stand_y) %>%
  left_join(main123_mean,c('main3' = 'actor_name')) %>% 
  rename(main3_to_m=mean_y, main3_to_s = stand_y)

colSums(is.na(ko_test_data))

ko_test_data[is.na(ko_test_data$main1_to_m),'main1_to_m']<-lm1$coef[1]+
  lm1$coef[2]*ko_test_data[is.na(ko_test_data$main1_to_m),'first_count']
ko_test_data[is.na(ko_test_data$main1_to_s),'main1_to_s']<-lm12$coef[1]+
  lm12$coef[2]*ko_test_data[is.na(ko_test_data$main1_to_s),'first_count']
ko_test_data[is.na(ko_test_data$main2_to_m),'main2_to_m']<-lm2$coef[1]+
  lm2$coef[2]*ko_test_data[is.na(ko_test_data$main2_to_m),'first_count']
ko_test_data[is.na(ko_test_data$main2_to_s),'main2_to_s']<-lm22$coef[1]+
  lm22$coef[2]*ko_test_data[is.na(ko_test_data$main2_to_s),'first_count']
ko_test_data[is.na(ko_test_data$main3_to_m),'main3_to_m']<-lm3$coef[1]+
  lm3$coef[2]*ko_test_data[is.na(ko_test_data$main3_to_m),'first_count']
ko_test_data[is.na(ko_test_data$main3_to_s),'main3_to_s']<-lm32$coef[1]+
  lm32$coef[2]*ko_test_data[is.na(ko_test_data$main3_to_s),'first_count']


#director
dir_list<-dir_to_list(ko_test_data)
ko_test_data<-left_join(ko_test_data, dir_list,by=c('director'='dir_name'))

colSums(is.na(ko_test_data))

#dist
ko_test_data<-ko_dist_numeric(ko_test_data)

write.csv(ko_train_data, 'ko_train_data_lm.csv', row.names = F)
write.csv(ko_test_data, 'ko_test_data_lm.csv', row.names = F)

rmse<-function(y,y_hat){
  
  sqrt(mean((y-y_hat)^2))
}

ko_train_data<-read_csv('ko_train_data_lm.csv')
urmarry<-data.frame(b_screen = 21, b_play = 145, b_count=31020,first_count=99315,
                    dist_factor1=0,grade='12세이상관람가',month=8,
                    director="이석근", main1="박보영", main2="김영광"
)

forest<-data.frame(b_screen = 35, b_play = 84, b_count=15598,first_count=131350,
                   dist_factor1=0,grade='전체관람',month=2,
                   director="임순례", main1="김태리", main2="류준열",main3="문소리"
)

forest$y<-1505560
levels(urmarry$dist_factor1) <- levels(ko_test_data$dist_factor1)

forest<-left_join(forest, main123_mean, by=c('main1' = 'actor_name')) %>% 
  rename(main1_to_m=mean_y, main1_to_s = stand_y) %>% 
  left_join(main123_mean, c('main2' = 'actor_name'))%>%
  rename(main2_to_m=mean_y, main2_to_s = stand_y)

#dir
ko_train_data %>% filter(director=="임순례") %>% select(starts_with('dir'))
forest$dir_to_m<-1278779; forest$dir_to_s<-0.0623
colSums(is.na(forest))
urmarry$main2_to_m<-predict(lm2,urmarry); urmarry$main2_to_s<-predict(lm22,urmarry)

#director NA -> lm
lm_dir1<-lm(dir_to_m~first_count, data = ko_train_data2)
lm_dir2<-lm(dir_to_s~first_count, data = ko_train_data2)

urmarry$dir_to_m<-predict(lm_dir1, urmarry); urmarry$dir_to_s<-predict(lm_dir2, urmarry)

urmarry$dist_factor1<-as.factor(urmarry$dist_factor1)


urmarry$y<-2081252 #not_final

forest$main3_to_m<-predict(lm3,forest)
forest$main3_to_s<-predict(lm32,forest)
rmse<-function(y,y_hat){
  
  sqrt(mean((y-y_hat)^2))
}

write.csv(urmarry,'urmarry.csv', row.names = F)
write.csv(forest,'forest.csv', row.names = F)
