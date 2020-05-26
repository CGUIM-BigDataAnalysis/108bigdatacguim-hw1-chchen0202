#載入套件
library(readr)
library(dplyr)

#分別讀入107與104年的資料
Data107 <- read_csv("~/Downloads/4133da254dbcdba28a2097de48d8d606_csv/107年各教育程度別初任人員每人每月經常性薪資─按大職類分.csv")
Data104 <- read_csv("~/Downloads/a17000000j-020066-mah.csv")

#另外存成變數以防原始資料被更動
DataWork107<-Data107
DataWork104<-Data104
#用迴圈將底線和...取代成空字串
for(n in 1:14)
  {DataWork107[[n]]<-gsub("—|…","",DataWork107[[n]])
  DataWork104[[n]]<-gsub("—|…","",DataWork104[[n]])}
#用迴圈將所有資料轉換成數值
for(n in 3:14)
  {DataWork107[[n]]<-as.numeric(DataWork107[[n]])
  DataWork104[[n]]<-as.numeric(DataWork104[[n]])}

#1
#統一兩個表當中的用詞
DataWork104[[2]]<-gsub("部門","",DataWork104[[2]])
DataWork104[[2]]<-gsub("、","_",DataWork104[[2]])
DataWork107[[2]]<-gsub("營建工程","營造業",DataWork107[[2]])
#因104年的職業別不一定出現在107年(如教育業)，因此使用left_join()
DataJoin<-left_join(DataWork104,DataWork107,by="大職業別")

#將研究所薪資除以大學薪資的比例結果儲存到新的欄位
DataJoin$PriceUpPercentage<-DataJoin$`大學-薪資.y`/DataJoin$`大學-薪資.x`
#計算薪資有調漲的個數
DataJoin$UP<-ifelse(DataJoin$PriceUpPercentage>0,"yes","no")
table(DataJoin$UP)

#使用order排序並做成子集，利用head()取出前十名
head(DataJoin[order(DataJoin$PriceUpPercentage,decreasing = T),c(2,28)],10)

#篩選漲薪超過5％(1.05)的職業
DataJoin$Over_1.05<-ifelse(DataJoin$PriceUpPercentage>1.05,"yes","no")
DataUP1.05<-DataJoin[grepl("yes",DataJoin$Over_1.05),"大職業別"]

#主要的職業類別，取出大職業別中"-" 前面的字串，並分析出現次數
upjobclass<-strsplit(DataUP1.05$大職業別,"-")
upjobclass2<-lapply(upjobclass, "[", 1)
table(unlist(upjobclass2))


#2

#104年大學畢業男女薪資排序(小到大)
head(DataWork104[order(DataWork104$`大學-女/男`,decreasing = F),c(2,12)],10)

#107年大學畢業男女薪資排序(小到大)
head(DataWork107[order(DataWork107$`大學-女/男`,decreasing = F),c(2,12)],10)


#女生薪資大於男性的行業(104年)
DataWork104$FemaleMore<-ifelse(DataWork104$`大學-女/男`>100,"yes","no")
FemaleMoretable104<-DataWork104[grepl("yes",DataWork104$FemaleMore),"大職業別"]
#女生薪資大於男性的行業(107年)
DataWork107$FemaleMore<-ifelse(DataWork107$`大學-女/男`>100,"yes","no")
FemaleMoretable107<-DataWork107[grepl("yes",DataWork107$FemaleMore),"大職業別"]




#3
#將研究所薪資除以大學薪資的比例結果儲存到新的欄位
DataWork107$PriceUpPercentage<-DataWork107$`研究所-薪資`/DataWork107$`大學-薪資`
#將完整的欄位（即職業別）取出並另存到新dataframe
DataWork107COM<-DataWork107[complete.cases(DataWork107),]

#使用order排序並做成子集，利用head()取出前十名
head(DataWork107COM[order(DataWork107COM$PriceUpPercentage,decreasing = T),c(2)],10)


#4
#呈現有興趣的職業別，並顯示大學薪資與研究所薪資
LoveJob<-DataWork107COM[grepl(c("出版、影音製作、傳播及資通訊服務業|工業及服務業"),DataWork107COM[[2]]),c(2,11,13)]
#計算新資差異
LoveJob$range<-LoveJob$`研究所-薪資`-LoveJob$`大學-薪資`

