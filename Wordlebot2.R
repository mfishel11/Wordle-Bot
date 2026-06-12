library(readxl)
library(tidyverse)
#pull the possible answers into R
answer<-read.delim("C:/Your File Path/all possible words.txt")
trainer<-read.delim("C:/Your File Path/wordle-answers-alphabetical.txt")
## create a smarter dataset so it will eliminate the most words with each guess
test<-as.data.frame(str_split_fixed(trainer$Key,pattern = "",n = 5))
for(i in 1:dim(test)[1]){
  b<-1
for(j in 1:(dim(test)[2]-1)){
  for(k in 1:(5-j)){
    if(test[i,j]==test[i,(j+k)]){
      b<-b+1
    }
  }
  test$factor[i]<-b
}
}
  
f<-test %>%
  group_by(V1) %>%
  summarise(count1 = n()) %>%
  arrange(desc(count1))
f<-as.data.frame(f)

s<-test %>%
  group_by(V2) %>%
  summarise(count2 = n()) %>%
  arrange(desc(count2))
s<-as.data.frame(s)

t<-test %>%
  group_by(V3) %>%
  summarise(count3 = n()) %>%
  arrange(desc(count3))
t<-as.data.frame(t)


fo<-test %>%
  group_by(V4) %>%
  summarise(count4 = n()) %>%
  arrange(desc(count4))
fo<-as.data.frame(fo)

fi<-test %>%
  group_by(V5) %>%
  summarise(count5 = n()) %>%
  arrange(desc(count5))
fi<-as.data.frame(fi)


## prioritzes letter placement
test_clean<-merge(x = test, y = f, by.x = 'V1', by.y = 'V1',all.x = T)
test_clean<-merge(x = test_clean, y = s, by.x = 'V2', by.y = 'V2',all.x = T)
test_clean<-merge(x = test_clean, y = t, by.x = 'V3', by.y = 'V3',all.x = T)
test_clean<-merge(x = test_clean, y = fo, by.x = 'V4', by.y = 'V4',all.x = T)
test_clean<-merge(x = test_clean, y = fi, by.x = 'V5', by.y = 'V5',all.x = T)
test_clean$word<-paste(test_clean$V1,test_clean$V2,test_clean$V3,test_clean$V4,test_clean$V5,
                       sep = "")
test_clean<-test_clean %>%
  select(word, count1, count2, count3, count4, count5, factor)

test_clean[is.na(test_clean),]<-0

for(i in 1:nrow(test_clean)){
  test_clean$sum[i]<-((test_clean$count1[i]+test_clean$count2[i]+test_clean$count3[i]
                       +test_clean$count4[i]+test_clean$count5[i])/(test_clean$factor[i]*.7))
}
test_clean_pos<-test_clean%>%
  select(word,sum)%>%
  arrange(desc(sum))

answer5<-test_clean_pos%>%
  select(word) %>%
  rename(Key = word)
answer3<-as.data.frame(answer5)

### prioritizes letter being used
test<-as.data.frame(str_split_fixed(answer$Key,pattern = "",n = 5))
for(i in 1:dim(test)[1]){
  b<-1
  for(j in 1:(dim(test)[2]-1)){
    for(k in 1:(5-j)){
      if(test[i,j]==test[i,(j+k)]){
        b<-b+1
      }
    }
    test$factor[i]<-b
  }
}

f<-test %>%
  group_by(V1) %>%
  summarise(count1 = n()) %>%
  arrange(desc(count1))
f<-as.data.frame(f)

s<-test %>%
  group_by(V2) %>%
  summarise(count2 = n()) %>%
  arrange(desc(count2))
s<-as.data.frame(s)

t<-test %>%
  group_by(V3) %>%
  summarise(count3 = n()) %>%
  arrange(desc(count3))
t<-as.data.frame(t)


fo<-test %>%
  group_by(V4) %>%
  summarise(count4 = n()) %>%
  arrange(desc(count4))
fo<-as.data.frame(fo)

fi<-test %>%
  group_by(V5) %>%
  summarise(count5 = n()) %>%
  arrange(desc(count5))
fi<-as.data.frame(fi)
test_clean<-merge(x = f, y = s, by.x = 'V1', by.y = 'V2')
test_clean<-merge(x = test_clean, y = t, by.x = 'V1', by.y = 'V3',all.x = T)
test_clean<-merge(x = test_clean, y = fo, by.x = 'V1', by.y = 'V4',all.x = T)
test_clean<-merge(x = test_clean, y = fi, by.x = 'V1', by.y = 'V5',all.x = T)

for(i in 1:nrow(test_clean)){
  test_clean$average[i]<-((test_clean$count1[i]+test_clean$count2[i]+test_clean$count3[i]
                       +test_clean$count4[i]+test_clean$count5[i])/5)
}
test_ave<-test_clean%>%
  select(V1, average)
test_ave2<-test_ave%>%
  rename(average2 = average)
test_ave3<-test_ave%>%
  rename(average3 = average)
test_ave4<-test_ave%>%
  rename(average4 = average)
test_ave5<-test_ave%>%
  rename(average5 = average)
test_clean<-merge(x = test, y = test_ave, by.x = 'V1', by.y = 'V1',all.x = T)
test_clean<-merge(x = test_clean, y = test_ave2, by.x = 'V2', by.y = 'V1',all.x = T)
test_clean<-merge(x = test_clean, y = test_ave3, by.x = 'V3', by.y = 'V1',all.x = T)
test_clean<-merge(x = test_clean, y = test_ave4, by.x = 'V4', by.y = 'V1',all.x = T)
test_clean<-merge(x = test_clean, y = test_ave5, by.x = 'V5', by.y = 'V1',all.x = T)

test_clean$word<-paste(test_clean$V1,test_clean$V2,test_clean$V3,test_clean$V4,test_clean$V5, 
                       sep = "")
test_clean<-test_clean %>%
  select(word, average, average2, average3, average4, average5, factor)

test_clean[is.na(test_clean)]<-0

for(i in 1:nrow(test_clean)){
  test_clean$sum[i]<-((test_clean$average[i]+test_clean$average2[i]+test_clean$average3[i]
                      +test_clean$average4[i]+test_clean$average5[i])/(test_clean$factor[i]*.7))
}
test_clean_ave<-test_clean%>%
  select(word, sum)%>%
  arrange(desc(sum))

answer<-test_clean_ave%>%
  select(word) %>%
  rename(Key = word)
answer4<-as.data.frame(answer)
##both method
# answer<-merge(x = test_clean_pos,y = test_clean_ave, by.x = "word", by.y = "word")
# for(i in 1:nrow(answer)){
#   answer$sum2[i]<-((answer$sum.x[i]+(answer$sum.y[i]*.7))/2)
# }
# answer<-answer%>%
#   arrange(desc(sum2))%>%
#   select(word)%>%
#   rename(Key = word)
# answer<-as.data.frame(answer)

## create a function that knows the rules of Wordle
wordle<-function(){
b<-""
c<-0
x<-""
answer<-answer3
answer2<-answer4
while(x != "ggggg"){
  if(c< 1){
    m<-readline(prompt = "Enter your first word:")
  }
  if(c == 1 & nrow(answer2) == 0){
    m<-answer[1,]
  }
  if(c == 1 & nrow(answer2) != 0){
    m<-answer2[1,]
  }
  if(c==2 & nrow(answer2)<100){
    m<- answer[1,]}
  if(c==2 & nrow(answer2)>10 & nrow(answer)>2){
    m<- answer2[1,]}
  if(c==2 & nrow(answer2)>10 & nrow(answer)<2){
    m<- answer[1,]}
  if(c>2){
    m<- answer[1,]}
  print(m)
  n<-strsplit(m,"")
x=readline(prompt = "enter results of previous word:")
c <- c+1
y<-strsplit(x,"")
if(y[[1]][1] == "g"){
  answer<-answer$Key[grep(paste("",n[[1]][1],"....", sep = ""),answer$Key)]
  answer<-as.data.frame(answer)
  answer<-rename(answer,Key = answer)
}
if(y[[1]][2] == "g"){
  answer<-answer$Key[grep(paste(".",n[[1]][2],"...", sep = ""),answer$Key)]
  answer<-as.data.frame(answer)
  answer<-rename(answer,Key = answer)
}
if(y[[1]][3] == "g"){
  answer<-answer$Key[grep(paste("..",n[[1]][3],"..", sep = ""),answer$Key)]
  answer<-as.data.frame(answer)
  answer<-rename(answer,Key = answer)
}
if(y[[1]][4] == "g"){
  answer<-answer$Key[grep(paste("...",n[[1]][4],".", sep = ""),answer$Key)]
  answer<-as.data.frame(answer)
  answer<-rename(answer,Key = answer)
}
if(y[[1]][5] == "g"){
  answer<-answer$Key[grep(paste("....",n[[1]][5],"", sep = ""),answer$Key)]
  answer<-as.data.frame(answer)
  answer<-rename(answer,Key = answer)
}
for(i in 1:5){
  if(y[[1]][i] == "g" | y[[1]][i] == "y"){
    b<-paste(b,n[[1]][i], sep = "")
  }
}
for(i in 1:5){
  if(y[[1]][i] == "b" & str_detect(b,n[[1]][i]) == F){
    answer<-answer$Key[grep(n[[1]][i],answer$Key, invert = T)]
    answer<-as.data.frame(answer)
    answer<-rename(answer,Key = answer)
  }
}
if(y[[1]][1] == "b" & str_detect(b,n[[1]][1]) == T){
  answer<-answer$Key[grep(paste("",n[[1]][1],"....", sep = ""),answer$Key, invert = T)]
  answer<-as.data.frame(answer)
  answer<-rename(answer,Key = answer)
}
if(y[[1]][2] == "b" & str_detect(b,n[[1]][2]) == T){
  answer<-answer$Key[grep(paste(".",n[[1]][2],"...", sep = ""),answer$Key, invert = T)]
  answer<-as.data.frame(answer)
  answer<-rename(answer,Key = answer)
}
if(y[[1]][3] == "b" & str_detect(b,n[[1]][3]) == T){
  answer<-answer$Key[grep(paste("..",n[[1]][3],"..", sep = ""),answer$Key, invert = T)]
  answer<-as.data.frame(answer)
  answer<-rename(answer,Key = answer)
}
if(y[[1]][4] == "b" & str_detect(b,n[[1]][4]) == T){
  answer<-answer$Key[grep(paste("...",n[[1]][4],".", sep = ""),answer$Key, invert = T)]
  answer<-as.data.frame(answer)
  answer<-rename(answer,Key = answer)
}
if(y[[1]][5] == "b" & str_detect(b,n[[1]][5]) == T){
  answer<-answer$Key[grep(paste("....",n[[1]][5],"", sep = ""),answer$Key, invert = T)]
  answer<-as.data.frame(answer)
  answer<-rename(answer,Key = answer)
  
}
if(y[[1]][1] == "y"){
  answer<-answer$Key[grep(paste("",n[[1]][1],"....", sep = ""),answer$Key, invert = T)]
  answer<-as.data.frame(answer)
  answer<-rename(answer,Key = answer)
}
if(y[[1]][2] == "y"){
  answer<-answer$Key[grep(paste(".",n[[1]][2],"...", sep = ""),answer$Key, invert = T)]
  answer<-as.data.frame(answer)
  answer<-rename(answer,Key = answer)
}
if(y[[1]][3] == "y"){
  answer<-answer$Key[grep(paste("..",n[[1]][3],"..", sep = ""),answer$Key, invert = T)]
  answer<-as.data.frame(answer)
  answer<-rename(answer,Key = answer)
}
if(y[[1]][4] == "y"){
  answer<-answer$Key[grep(paste("...",n[[1]][4],".", sep = ""),answer$Key, invert = T)]
  answer<-as.data.frame(answer)
  answer<-rename(answer,Key = answer)
}
if(y[[1]][5] == "y"){
  answer<-answer$Key[grep(paste("....",n[[1]][5],"", sep = ""),answer$Key, invert = T)]
  answer<-as.data.frame(answer)
  answer<-rename(answer,Key = answer)
  
}
for(i in 1:5){
  if(y[[1]][i] == "y"){
    answer<-answer$Key[grep(n[[1]][i],answer$Key)]
    answer<-as.data.frame(answer)
    answer<-rename(answer,Key = answer)
  }
}
answer<-answer$Key[grep(m,answer$Key, invert = T)]
answer<-as.data.frame(answer)
answer<-rename(answer,Key = answer)
for(i in 1:5){
  if(y[[1]][i] == "g"){
    answer2<-answer2$Key[grep(n[[1]][i],answer2$Key, invert = T)]
    answer2<-as.data.frame(answer2)
    answer2<-rename(answer2,Key = answer2)
  }
}
# for(i in 1:5){
#   if(y[[1]][i] == "g" | y[[1]][i] == "y"){
#     b<-paste(b,n[[1]][i], sep = "")
#   }
# }
for(i in 1:5){
  if(y[[1]][i] == "b" & str_detect(b,n[[1]][i]) == F){
    answer2<-answer2$Key[grep(n[[1]][i],answer2$Key, invert = T)]
    answer2<-as.data.frame(answer2)
    answer2<-rename(answer2,Key = answer2)
  }
}
# if(y[[1]][1] == "b" & str_detect(b,n[[1]][1]) == T){
#   answer<-answer$Key[grep(paste("",n[[1]][1],"....", sep = ""),answer$Key, invert = T)]
#   answer<-as.data.frame(answer)
#   answer<-rename(answer,Key = answer)
# }
# if(y[[1]][2] == "b" & str_detect(b,n[[1]][2]) == T){
#   answer<-answer$Key[grep(paste(".",n[[1]][2],"...", sep = ""),answer$Key, invert = T)]
#   answer<-as.data.frame(answer)
#   answer<-rename(answer,Key = answer)
# }
# if(y[[1]][3] == "b" & str_detect(b,n[[1]][3]) == T){
#   answer<-answer$Key[grep(paste("..",n[[1]][3],"..", sep = ""),answer$Key, invert = T)]
#   answer<-as.data.frame(answer)
#   answer<-rename(answer,Key = answer)
# }
# if(y[[1]][4] == "b" & str_detect(b,n[[1]][4]) == T){
#   answer<-answer$Key[grep(paste("...",n[[1]][4],".", sep = ""),answer$Key, invert = T)]
#   answer<-as.data.frame(answer)
#   answer<-rename(answer,Key = answer)
# }
# if(y[[1]][5] == "b" & str_detect(b,n[[1]][5]) == T){
#   answer<-answer$Key[grep(paste("....",n[[1]][5],"", sep = ""),answer$Key, invert = T)]
#   answer<-as.data.frame(answer)
#   answer<-rename(answer,Key = answer)
#   
# }
for(i in 1:5){
  if(y[[1]][i] == "y"){
    b<-paste(b,n[[1]][i], sep = "")
  }
}
f<-strsplit(b,"")
for(i in 1:length(f)){
  if(y[[1]][1] == "g"){
    answer2<-answer2$Key[grep(paste("",f[[1]][i],"....", sep = ""),answer2$Key, invert = T)]
    answer2<-as.data.frame(answer2)
    answer2<-rename(answer2,Key = answer2)
  }
  if(y[[1]][2] == "g"){
    answer2<-answer2$Key[grep(paste(".",f[[1]][i],"...", sep = ""),answer2$Key, invert = T)]
    answer2<-as.data.frame(answer2)
    answer2<-rename(answer2,Key = answer2)
  }
  if(y[[1]][3] == "g"){
    answer2<-answer2$Key[grep(paste("..",f[[1]][i],"..", sep = ""),answer2$Key, invert = T)]
    answer2<-as.data.frame(answer2)
    answer2<-rename(answer2,Key = answer2)
  }
  if(y[[1]][4] == "g"){
    answer2<-answer2$Key[grep(paste("...",f[[1]][i],".", sep = ""),answer2$Key, invert = T)]
    answer2<-as.data.frame(answer2)
    answer2<-rename(answer2,Key = answer2)
  }
  if(y[[1]][5] == "g"){
    answer2<-answer2$Key[grep(paste("....",f[[1]][i],"", sep = ""),answer2$Key, invert = T)]
    answer2<-as.data.frame(answer2)
    answer2<-rename(answer2,Key = answer2)
  }
}
if(y[[1]][1] == "y"){
  answer2<-answer$Key[grep(paste("",n[[1]][1],"....", sep = ""),answer2$Key, invert = T)]
  answer2<-as.data.frame(answer2)
  answer2<-rename(answer2,Key = answer2)
}
if(y[[1]][2] == "y"){
  answer2<-answer2$Key[grep(paste(".",n[[1]][2],"...", sep = ""),answer2$Key, invert = T)]
  answer2<-as.data.frame(answer2)
  answer2<-rename(answer2,Key = answer2)
}
if(y[[1]][3] == "y"){
  answer2<-answer2$Key[grep(paste("..",n[[1]][3],"..", sep = ""),answer2$Key, invert = T)]
  answer2<-as.data.frame(answer2)
  answer2<-rename(answer2,Key = answer2)
}
if(y[[1]][4] == "y"){
  answer2<-answer2$Key[grep(paste("...",n[[1]][4],".", sep = ""),answer2$Key, invert = T)]
  answer2<-as.data.frame(answer2)
  answer2<-rename(answer2,Key = answer2)
}
if(y[[1]][5] == "y"){
  answer2<-answer2$Key[grep(paste("....",n[[1]][5],"", sep = ""),answer2$Key, invert = T)]
  answer2<-as.data.frame(answer2)
  answer2<-rename(answer2,Key = answer2)
  
}
for(i in 1:5){
  if(y[[1]][i] == "y"){
    answer2<-answer2$Key[grep(n[[1]][i],answer2$Key)]
    answer2<-as.data.frame(answer2)
    answer2<-rename(answer2,Key = answer2)
  }
}
answer2<-answer2$Key[grep(m,answer2$Key, invert = T)]
answer2<-as.data.frame(answer2)
answer2<-rename(answer2,Key = answer2)
}
print("Good Job!")
}
####################################################################
############### Play the Game #########################
#########################################################
wordle()
#################################################
answer[sample(nrow(answer),1),1]
















#testing ground
answer<-read.delim("C:/Users/maxfi/Documents/R/Wordle/wordle-answers-alphabetical.txt")
if(y[[1]][1] == "g"){
  answer<-answer$Key[grep(paste("",n[[1]][1],"....", sep = ""),answer$Key)]
  answer<-as.data.frame(answer)
  answer<-rename(answer,Key = answer)
}
if(y[[1]][2] == "g"){
  answer<-answer$Key[grep(paste(".",n[[1]][2],"...", sep = ""),answer$Key)]
  answer<-as.data.frame(answer)
  answer<-rename(answer,Key = answer)
}

b <-""
for(i in 1:5){
if(y[[1]][i] == "g"){
  b<-paste(b,n[[1]][i], sep = "")
}
} 

answer<-read.delim("C:/Users/maxfi/Documents/R/Wordle/wordle-answers-alphabetical.txt")

test<-as.data.frame(str_split_fixed(answer$Key,pattern = "",n = 5))

f<-test %>%
  group_by(V1) %>%
  summarise(count1 = n()) %>%
  arrange(desc(count1))
f<-as.data.frame(f)

s<-test %>%
  group_by(V2) %>%
  summarise(count2 = n()) %>%
  arrange(desc(count2))
s<-as.data.frame(s)

t<-test %>%
  group_by(V3) %>%
  summarise(count3 = n()) %>%
  arrange(desc(count3))
t<-as.data.frame(t)


fo<-test %>%
  group_by(V4) %>%
  summarise(count4 = n()) %>%
  arrange(desc(count4))
fo<-as.data.frame(fo)

fi<-test %>%
  group_by(V5) %>%
  summarise(count5 = n()) %>%
  arrange(desc(count5))
fi<-as.data.frame(fi)

test_clean<-merge(x = test, y = f, by.x = 'V1', by.y = 'V1',all.x = T)
test_clean<-merge(x = test_clean, y = s, by.x = 'V2', by.y = 'V2',all.x = T)
test_clean<-merge(x = test_clean, y = t, by.x = 'V3', by.y = 'V3',all.x = T)
test_clean<-merge(x = test_clean, y = fo, by.x = 'V4', by.y = 'V4',all.x = T)
test_clean<-merge(x = test_clean, y = fi, by.x = 'V5', by.y = 'V5',all.x = T)

test_clean$word<-paste(test_clean$V1,test_clean$V2,test_clean$V3,test_clean$V4,test_clean$V5, 
                       sep = "")
test_clean<-test_clean %>%
  select(word, count1, count2, count3, count4, count5)

test_clean[is.na(test_clean),]<-0

for(i in 1:nrow(test_clean)){
  test_clean$sum[i]<-(test_clean$count1[i]+test_clean$count2[i]+test_clean$count3[i]
                      +test_clean$count4[i]+test_clean$count5[i])
}
test_clean<-test_clean%>%
  arrange(desc(sum))

answer<-test_clean%>%
  select(word)

?merge

str_detect(b,n[[1]][1])
          
?str_detect

trainer[str_detect("pinot", trainer$Key),]
  
