install.packages("jsonlite")
install.packages("dplyr")

library(jsonlite)
library(dplyr)

career107 <- fromJSON("107年各教育程度別初任人員每人每月經常性薪資─按大職類分.json")
career104 <- fromJSON("http://ipgod.nchc.org.tw/dataset/b6f36b72-0c4a-4b60-9254-1904e180ddb1/resource/63ecb4a9-f634-45f4-8b38-684b72cf95ba/download/0df38b73f75962d5468a11942578cce5.json")

View(data.frame(career104[,"大職業別"],career107[,"大職業別"]) ) #判斷兩年的差異
career104$大職業別 <- career107$大職業別 #統一職業名稱

career_join<-inner_join(career104 ,career107,by = "大職業別")
for (n in grep("…|—",career_join)){
  career_join[grep("…|—", career_join[,n]),n] <- ""
} #清除所有在薪資欄位的特殊字元
career_join[3:27]<-as.data.frame(lapply(career_join[3:27],as.numeric)) #轉換成數值方便後續使用


#Q1
career_join$提高比率 <-  career_join$`經常性薪資-薪資.y` / career_join$`經常性薪資-薪資.x`
Q1_1_answer <- filter(career_join, `經常性薪資-薪資.y`>`經常性薪資-薪資.x`) %>% 
  arrange(desc(提高比率)) %>%
  head(10)

Q1_2_answer <- filter(career_join, 提高比率>1.05)
index <- regexpr("-", paste0(Q1_2_answer$大職業別, "-"))
Q1_3_answer <-substr(Q1_2_answer$大職業別, 0, index-1 ) 
table(Q1_3_answer)


#Q2
Q2_1_104 <-career_join %>% arrange(`大學-女/男.x`) %>%
  head(10) %>%
  select(1:14)
Q2_1_107 <-career_join %>% arrange(`大學-女/男.y`) %>%
  head(10) %>%
  select(-1,-(3:14),-28)

Q2_2_104 <-career_join %>% arrange(desc(`大學-女/男.x`)) %>%
  select(1:14) %>%
  head(10)
Q2_2_107 <-career_join %>% arrange(desc(`大學-女/男.y`)) %>%
  select(-1,-(3:14),-28) %>%
  head(10)

#Q3
clean_career107 <- select(career_join, -1,-(3:14),-28)
clean_career107$碩士薪資增加比率 <- clean_career107$`研究所-薪資` / clean_career107$`大學-薪資.y` 
MasterHighCP107 <- clean_career107 %>% arrange(desc(碩士薪資增加比率)) %>% 
  head(10)

#Q4
My_choices <- career_join[c(114, 100, 86, 79),c(2,11,13,24,26,28)] 
My_choices$碩士107CP值 <- My_choices$`研究所-薪資` / My_choices$`大學-薪資.y` 
names(My_choices)[2:5] <- c("104大學薪資","104研究所以上薪資","107大學薪資","107研究所以上薪資")

