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



#1.randomForest
library(randomForest)
rf1<-randomForest(y~b_screen+b_play+b_count+
                    main1_to_s+main2_to_s+main3_to_s+dir_to_m+
                    dist_factor1+first_count, data = ko_train_data)
varImpPlot(rf1)

rf1.pred<-predict(rf1, ko_test_data)

rmse(ko_test_data$y,rf1.pred)


2.lm
lm1<-lm(y~b_screen+b_play+b_count+
                    main1_to_s+main2_to_s+main3_to_s+dir_to_m+
                    dist_factor1+first_count, data = ko_train_data)

lm1.pred<-predict(rf1, ko_train_data)

rmse(ko_train_data$y,rf1.pred)

