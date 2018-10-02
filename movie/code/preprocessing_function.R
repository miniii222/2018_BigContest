#######main_to_numeric function
actor_i<-function(name,many_list){
  index<-vector()
  for (i in 1:length(many_list)) {
    if(name %in% many_list[[i]]) {
      index = c(index,i)
      
    }
  }
  return(index)
}

main_to_numeric<-function(df){
  
  df$actor<-paste(df$main1,df$main2,df$main3,sep=',')
  actor_list = str_split(df$actor, ',')

  unique_actor <- unique(unlist(actor_list[])) #unique actors list
  numeric1<-vector()

  
#impetation with mean
for (i in 1 : (length(unique_actor))){
  numeric1[i]<-mean(df[actor_i(unique_actor[i],actor_list),]$y)
}


actor1<-data.frame('actor_name' = unique_actor, 'mean_y' = numeric1,
                   'stand_y'=(numeric1-mean(numeric1))/sd(numeric1))

#NA ¾î¶»°Ô Ã¤¿ïÁö »ý°¢

df2<-left_join(df, actor1, c('main1' = 'actor_name')) %>% 
 rename(main1_to_m=mean_y, main1_to_s = stand_y) %>% 
 left_join(actor1, c('main2' = 'actor_name'))%>%
 rename(main2_to_m=mean_y, main2_to_s = stand_y) %>%
 left_join(actor1, c('main3' = 'actor_name'))%>%
 rename(main3_to_m=mean_y, main3_to_s = stand_y)




  return(df2)
}

main_train_mean<-function(df){
  
  df$actor<-paste(df$main1,df$main2,df$main3,sep=',')
  actor_list = str_split(df$actor, ',')
  
  unique_actor <- unique(unlist(actor_list[])) #unique actors list
  numeric1<-vector()
  
  
  #impetation with mean
  for (i in 1 : (length(unique_actor))){
    numeric1[i]<-mean(df[actor_i(unique_actor[i],actor_list),]$y)
  }
  
  
  actor1<-data.frame('actor_name' = unique_actor, 'mean_y' = numeric1,
                     'stand_y'=(numeric1-mean(numeric1))/sd(numeric1))
  return(actor1)
}


###dist_to_numeric function
####us
us_dist_numeric<-function(df){
  
  #factor = 2 index
  f2<-c(grep('µðÁî´Ï',df$dist),grep('¼Ò´Ï',df$dist),
        grep('ÀÌ½Ê¼¼±â',df$dist),grep('¿ö³Êºê',df$dist))
  
  #factor = 1 index
  f1<-c(grep('¾¾Á¦ÀÌ',df$dist),grep('³Ø½ºÆ®',df$dist),
        grep('·Ôµ¥',df$dist),grep('¼î¹Ú½º',df$dist))
  
  df2<-df
  
  df2$dist_factor2<-0; df2[f2,'dist_factor2']<-1
  df2$dist_factor1<-0; df2[f1,'dist_factor1']<-1
  
  return(df2)
  
}

us_dist_numeric(us)

####ko
ko_dist_numeric<-function(df){
  
  #factor = 1 index
  f1<-c(grep('¾¾Á¦ÀÌ',df$dist),grep('³Ø½ºÆ®',df$dist),
        grep('·Ôµ¥',df$dist),grep('¼î¹Ú½º',df$dist))
  
  df2<-df
  
  df2$dist_factor1<-0; df2[f1,'dist_factor1']<-1
  
  return(df2)
  
}

#######dir_to_numeric function
dir_to_numeric<-function(df){
  
  unique_dir <- unique(df$director) #unique dirs list
  
  numeric1<-vector()
  #impetation with mean
  for (i in 1 : (length(unique_dir))){
    numeric1[i]<-mean(df[df$director==unique_dir[i],]$y)
  }
  
  
  dir1<-data.frame('dir_name' = unique_dir, 'dir_to_m' = numeric1,
                     'dir_to_s'=(numeric1-mean(numeric1))/sd(numeric1))
  

  df2<-left_join(df, dir1, c('director' = 'dir_name'))
 
  return(df2)
}


dir_to_list<-function(df){
  
  unique_dir <- unique(df$director) #unique dirs list
  
  numeric1<-vector()
  #impetation with mean
  for (i in 1 : (length(unique_dir))){
    numeric1[i]<-mean(df[df$director==unique_dir[i],]$y)
  }
  
  
  dir1<-data.frame('dir_name' = unique_dir, 'dir_to_m' = numeric1,
                   'dir_to_s'=(numeric1-mean(numeric1))/sd(numeric1))

  return(dir1)
}
